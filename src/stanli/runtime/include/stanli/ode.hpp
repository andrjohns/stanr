// Compile-time description of an ODE solve, attached to its op through the
// graph's user-data channel. Owns everything the integrator needs that is
// fixed at lowering time: the right-hand side's MIR, the function table it
// may call into, default/data-only solve times, data arrays, and tolerances.
// Modern calls may instead supply t0 and ts as runtime operands.
#ifndef STANLI_ODE_HPP
#define STANLI_ODE_HPP

#include <stanli/callback.hpp>

#include <memory>
#include <vector>

namespace stanli {

struct OdeSpec : RetainedCallback {
  // The spec outlives lowering (the graph does), so it owns copies of the
  // functions it may call rather than pointing into the parsed program.
  // Compatibility name retained for tooling/tests; storage and lookup live
  // in the shared RetainedCallback base.
  std::string rhs_name;
  const mir::FunDef* rhs() const { return callback(rhs_name); }
  double t0 = 0;
  std::vector<double> ts;
  double rtol = 1e-6, atol = 1e-6;
  long max_steps = 1000000;
  // Whether this solver uses the legacy multistep defaults
  // (BDF/Adams: 1e-10, 1e8 rather than RK45's 1e-6, 1e6).
  bool stiff = false;
  // Which integrator, for the modern ode_* family. These are genuinely
  // different methods, not aliases: Adams and BDF are both CVODES
  // multistep but with different stability, and CKRK is a different
  // Runge-Kutta tableau from RK45. On an easy system they agree to
  // solver tolerance, which is exactly why running the wrong one would
  // pass a casual test and fail the user who chose it for stiffness.
  enum Solver { RK45, BDF, ADAMS, CKRK };
  Solver solver = RK45;
  // True for integrate_ode_*. The kernel preserves that interface's solver
  // defaults and function-name labels while calling the same *_tol_impl the
  // deprecated Stan Math wrappers delegate to, avoiding their per-callback
  // std::vector/Eigen adapter.
  bool legacy = false;
  // The right-hand side's arguments after (t, y), in declaration order.
  // The deprecated interface always fills this with exactly three --
  // theta, x_r, x_i -- and the variadic ode_* interface with however many
  // the call passed. It is what lets the INTERPRETER fallback split the
  // packed theta and x_r back into the individual formal parameters; the
  // compiled program has the same information baked into its register
  // ranges. Without it the fallback could only serve the deprecated
  // shape, and a right-hand side the compiler cannot take would lose
  // coverage rather than lose speed.
  // The right-hand side, compiled. Falls back to the MIR interpreter when
  // `prog.ok` is false; `prog.why` says what stopped it.
  // Branchless double-forward/generated-reverse derivative payload for the
  // direct coupled RK45/CKRK path. Null is an ordinary structural refusal;
  // `prog` remains the exact nested-autodiff oracle and fallback.
  std::shared_ptr<const IslandProg> direct_rk;
  std::string direct_rk_why;
  // Selection is fixed at lowering so the repeated kernel does no environment
  // lookup. STANLI_NO_ODE_DIRECT_RK leaves the payload present for a clean
  // same-binary oracle comparison while disabling its use.
  bool direct_rk_enabled = false;
};

}  // namespace stanli

#endif
