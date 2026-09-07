// Runs a testthat file from tests/testthat/ against an rwasm::build()-built
// stanr package, inside webR (Node), and exits non-zero on any failure.
//
// This exists because the "stanli" backend interprets Stan programs rather
// than compiling them, so it's the one backend that's actually exercisable
// in a wasm/webR context without a C++ toolchain at runtime -- unlike the
// default "compiled" backend used by most of the test suite, which JIT
// compiles a native shared library per model via R CMD SHLIB.
//
// Usage: node run-stanli-tests.mjs <path-to-pkg-tarball> <path-to-repo-root> <filter>

import { WebR } from 'webr';
import { mkdtempSync, existsSync, readdirSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { execFileSync } from 'node:child_process';

const [, , tarballPath, repoRoot, filter] = process.argv;
if (!tarballPath || !repoRoot || !filter) {
  console.error('Usage: node run-stanli-tests.mjs <path-to-pkg-tarball> <path-to-repo-root> <filter>');
  process.exit(1);
}

const absTarball = path.resolve(tarballPath);
const absRepoRoot = path.resolve(repoRoot);
const testthatDir = path.join(absRepoRoot, 'tests', 'testthat');
if (!existsSync(absTarball) || !existsSync(testthatDir)) {
  console.error(`Missing input: ${absTarball} or ${testthatDir}`);
  process.exit(1);
}

const libDir = mkdtempSync(path.join(tmpdir(), 'webr-pkg-'));
execFileSync('tar', ['xzf', absTarball, '-C', libDir]);
const pkgName = readdirSync(libDir).filter((e) => statSync(path.join(libDir, e)).isDirectory())[0];

const webR = new WebR();
await webR.init();

console.log('Installing R-level dependencies from the wasm repo...');
// Mirrors the dependency list installed for the other cross-compile jobs
// (see .github/workflows/R-CMD-check-cross.yaml).
await webR.installPackages(['R6', 'QuickJSR', 'posterior', 'withr', 'testthat', 'loo']);

const libMount = '/host_lib';
const testsMount = '/host_tests';
await webR.FS.mkdir(libMount);
await webR.FS.mount('NODEFS', { root: libDir }, libMount);
await webR.FS.mkdir(testsMount);
await webR.FS.mount('NODEFS', { root: testthatDir }, testsMount);

const rCode = `
.libPaths(c(${JSON.stringify(libMount)}, .libPaths()))
library(testthat)
library(${JSON.stringify(pkgName)}, character.only = TRUE)

# stan_model()'s default backend, unless a test passes `backend =` itself.
Sys.setenv(STANR_BACKEND = "stanli")

setwd(${JSON.stringify(testsMount)})
results <- test_dir(
  ${JSON.stringify(testsMount)},
  filter = ${JSON.stringify(filter)},
  reporter = "summary",
  stop_on_failure = FALSE
)

df <- as.data.frame(results)
n_fail <- sum(df$failed) + sum(df$error)
n_pass <- sum(df$passed)
cat(sprintf("\\n%d passed, %d failed/errored\\n", n_pass, n_fail))
if (n_fail > 0L) {
  cat("FAILURES:\\n")
  print(df[df$failed > 0 | df$error, c("file", "test", "failed", "error")])
}
n_fail == 0L
`;

const shelter = await new webR.Shelter();
let ok = false;
try {
  const result = await shelter.captureR(rCode, { withAutoprint: false });
  for (const out of result.output) {
    if (out.type === 'stdout' || out.type === 'stderr') {
      console.log(out.data);
    }
  }
  const value = await result.result.toJs();
  ok = !!(value.values ? value.values[0] : value);
} catch (e) {
  console.error('ERROR running tests:', e.message);
  ok = false;
} finally {
  shelter.purge();
}

await webR.close();
process.exit(ok ? 0 : 1);
