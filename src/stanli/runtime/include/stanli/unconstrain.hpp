#ifndef STANLI_UNCONSTRAIN_HPP
#define STANLI_UNCONSTRAIN_HPP

#include <stanli/mir.hpp>

#include <cstdint>
#include <string>
#include <vector>

// The inverse of the parameter transforms the log_prob graph applies forward.
// Every direction here goes through stan-math's own *_free, so the two
// directions cannot drift apart -- the same discipline the MIR interpreter's
// bound transforms already follow.
//
// This unit is deliberately per-LEAF and layout-free. It converts one
// contiguous constrained leaf into its contiguous free values and knows
// nothing about outer array batches, where a leaf sits in a draw, or which
// parameter it belongs to. Those belong to the caller, which has the
// declaration in hand.
//
// Only `double` exists here. Unconstraining a starting value needs no
// derivatives, which is why this is a fraction of the size of the forward
// kernels in runtime/kernels/constrain.cpp.
namespace stanli {

// One evaluated transform argument: a lower bound, upper bound, offset, or
// multiplier. A length of 1 broadcasts across the leaf; any other length must
// match the leaf element for element.
struct TransformArg {
  const double* data = nullptr;
  int64_t len = 0;
};

// How many trailing declared dimensions the transform's leaf occupies: 2 for
// the matrix-shaped transforms, 1 for the vector-shaped ones, and 0 for the
// elementwise ones, which apply to a value of any shape. SumToZero is 1 or 2
// depending on whether the declaration is a vector or a matrix, so it reports
// 0 and the caller passes the leaf it declared.
int transform_leaf_rank(mir::Transform::Kind kind);

// Number of free values one leaf of these constrained dimensions produces.
// Throws CompileError when the transform and the dimensions disagree, which
// is a malformed model rather than anything a user supplied.
int64_t free_leaf_size(mir::Transform::Kind kind,
                       const std::vector<int64_t>& leaf_dims);

// Invert one transform over one leaf. `constrained` holds the product of
// `leaf_dims` values in stanli's single first-index-fast convention, which for
// a matrix leaf is Eigen's own column-major order, and `out` receives
// free_leaf_size() values.
//
// `args` supplies the evaluated bounds in transform-argument order and is
// empty for the structured transforms. A value that violates its own
// constraint -- a simplex that does not sum to one, a lower bound the value
// sits below -- raises the exception stan-math raises, whose message describes
// the violation but not which parameter it came from; callers add the name.
void unconstrain_leaf(mir::Transform::Kind kind,
                      const std::vector<int64_t>& leaf_dims,
                      const double* constrained,
                      const std::vector<TransformArg>& args, double* out);

}  // namespace stanli

#endif
