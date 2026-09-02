// The scalar continuous densities and distribution functions, once, for
// everything that is not a graph kernel.
//
// There used to be two lists. The graph had all of them
// (STANLI_SCALAR_DENSITY_LIST, optable.hpp); the register machine carried
// a hand-picked twelve of its own, and the MIR interpreter generated from
// that twelve and then hand-added `student_t_lpdf` back because it did not
// fit. The subset was not a design -- it was whatever the corpus needed
// when the register machine was written -- and it carried weight in the
// worst place: a region whose control flow depends on a parameter has to
// compile to the register machine or not at all, so a density outside the
// twelve was a hard compile error for a model the runtime handles
// everywhere else. `target += chi_square_lpdf(y | nu)` inside an
// `if (theta > 0)` did not compile; the same line outside the `if` did.
//
// So there is one shared dispatch now, and this is the one place that
// switches on it. CDF/LCDF/LCCDF functions use the same scalar argument and
// partials contract and are included as well, which lets truncation inside a
// runtime-control region use the same implementation as the graph.
// Three callers share these definitions rather than instantiating the whole
// set apiece: the register machine's interpreter (program.hpp), its generated
// adjoint (adjoint.cpp), and the MIR interpreter (mir_interp.hpp).
#ifndef STANLI_PROGRAM_DENSITY_HPP
#define STANLI_PROGRAM_DENSITY_HPP

#include <stan/math/rev/core.hpp>

#include <cstdint>
#include <string>

namespace stanli {

// The widest scalar probability functions here take four arguments
// (student_t, skew_normal, exp_mod_normal, pareto_type_2, and
// skew_double_exponential).
constexpr int kMaxDensityArgs = 4;

// How many densities there are; ids run over [0, count).
int program_density_count();

// How many arguments density `id` takes, or 0 if `id` names none. The
// caller does not need to know which density it holds to lay out its
// arguments.
int program_density_arity(int id);

// Density id for a Stan function name (`"normal_lpdf"`) or an opcode
// (`OP_NORMAL_LPDF`), or -1. The name form is what the MIR front ends
// look up; the opcode form is what the island carver translates.
int program_density_id_by_name(const std::string& name);
int program_density_id_by_opcode(uint16_t opcode);

// Its name, for diagnostics.
const char* program_density_name(int id);

// Evaluate density `id` over `arity` contiguous arguments. propto-OFF:
// with no term-dropping the value does not depend on which arguments are
// autodiff, so one call serves the double and var passes alike.
template <typename T>
T program_density(int id, const T* args);

extern template double program_density<double>(int, const double*);
extern template stan::math::var program_density<stan::math::var>(
    int, const stan::math::var*);

// The same densities differentiated: stan-math computes value and partials
// in doubles through the recorder scalar (recorder.hpp), with no tape.
// `partials` receives one double per argument. Nothing here differentiates
// a density by hand.
// Returns whether Stan Math built a dependency edge. A constant early return
// fills zero partials and returns false so reverse mode can skip, rather than
// form an indeterminate infinite-adjoint-times-zero product.
//
// Bit k of `mask` says argument k needs a partial; a clear bit binds it as a
// plain double, which is what makes stan-math drop that argument's partial
// expression, and leaves partials[k] untouched. The value is not affected --
// propto is off and this function's value is discarded anyway, the forward
// having computed it -- so a mask only removes arithmetic whose result the
// caller discards. Masks are dispatched for the densities whose tier carries
// STANLI_DENSITY_FULL_MASKS (optable.hpp) and ignored for the rest, the same
// trade the graph's density kernels make.
bool program_density_partials(int id, unsigned mask, const double* args,
                              double* partials);

// Whether density `id` is affordable to also instantiate over container
// arguments (program_density_vec): the same COMMON_A/COMMON_B tier line
// program_density_partials draws for mask dispatch, reused here rather than
// adding a second cost axis for a related reason -- both trade compiled
// size for a density call the graph can already do exactly, on the
// functions models actually lean on.
bool program_density_container_capable(int id);

// Density `id` over up to four arguments, one or more of which is a
// `len`-element container rather than a scalar (bit k of `mask` says
// argument k is one): one propto-OFF call built the way CmdStan's generated
// code would build it (stan-math's own broadcasting), not `len` scalar
// calls summed by hand, which would not sum in the same order. `reg` is the
// register file; `arg_reg[k]` is a container argument's first register, or
// a scalar argument's only one.
template <typename T>
T program_density_vec(int id, unsigned mask, int32_t len, const T* reg,
                      const int32_t* arg_reg);

extern template double program_density_vec<double>(int, unsigned, int32_t,
                                                   const double*,
                                                   const int32_t*);
extern template stan::math::var program_density_vec<stan::math::var>(
    int, unsigned, int32_t, const stan::math::var*, const int32_t*);

}  // namespace stanli

#endif
