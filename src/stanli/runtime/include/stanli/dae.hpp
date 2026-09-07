// Compile-time description of a DAE solve retained by OP_DAE.
#ifndef STANLI_DAE_HPP
#define STANLI_DAE_HPP

#include <stanli/callback.hpp>

#include <string>
#include <vector>

namespace stanli {

struct DaeSpec : RetainedCallback {
  std::string residual_name;
  const mir::FunDef* residual() const { return callback(residual_name); }
  double t0 = 0;
  std::vector<double> ts;
  double rtol = 1e-10;
  double atol = 1e-10;
  long max_steps = 100000000;
};

}  // namespace stanli

#endif
