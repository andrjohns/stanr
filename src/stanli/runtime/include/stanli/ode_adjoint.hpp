// Compile-time controls and retained callback for CVODES adjoint ODE solves.
#ifndef STANLI_ODE_ADJOINT_HPP
#define STANLI_ODE_ADJOINT_HPP

#include <stanli/callback.hpp>

#include <string>
#include <vector>

namespace stanli {

struct OdeAdjointSpec : RetainedCallback {
  std::string rhs_name;
  const mir::FunDef* rhs() const { return callback(rhs_name); }

  double relative_tolerance_forward = 0;
  std::vector<double> absolute_tolerance_forward;
  double relative_tolerance_backward = 0;
  std::vector<double> absolute_tolerance_backward;
  double relative_tolerance_quadrature = 0;
  double absolute_tolerance_quadrature = 0;
  long max_num_steps = 0;
  long num_steps_between_checkpoints = 0;
  int interpolation_polynomial = 0;
  int solver_forward = 0;
  int solver_backward = 0;
};

}  // namespace stanli

#endif
