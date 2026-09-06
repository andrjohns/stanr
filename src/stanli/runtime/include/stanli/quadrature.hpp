#ifndef STANLI_QUADRATURE_HPP
#define STANLI_QUADRATURE_HPP

#include <stanli/callback.hpp>

#include <cmath>
#include <limits>

namespace stanli {

struct QuadratureSpec : RetainedCallback {
  mir::QuadratureMethod method = mir::QuadratureMethod::Integrate1D;
  double relative_tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  double absolute_tolerance = 0.0;
  int max_steps = 15;
  int parameter_count = 0;
};

}  // namespace stanli

#endif
