#ifndef STANLI_HIGHER_ORDER_EVAL_HPP
#define STANLI_HIGHER_ORDER_EVAL_HPP

#include <stanli/compile.hpp>
#include <stanli/mir.hpp>

#include <functional>
#include <map>

namespace stanli {

// Value-only bridge used by every MIR interpreter. It normalizes a retained
// higher-order call once and invokes the same registered kernel used by graph
// and runtime-control execution. False means the expression is not one of the
// retained-kernel families (reduce_sum and map_rect remain structural calls).
bool evaluate_retained_higher_order(
    const std::map<std::string, const mir::FunDef*>& funs,
    const mir::Expr& call,
    const std::function<DataMap::Entry(const mir::Expr&)>& eval,
    DataMap::Entry* out);

}  // namespace stanli

#endif
