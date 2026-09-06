// Shared state and argument packing for user callbacks retained by runtime
// algorithms (ODEs, algebra solvers, quadrature, and future map/DAE kernels).
#ifndef STANLI_CALLBACK_HPP
#define STANLI_CALLBACK_HPP

#include <stanli/mir.hpp>
#include <stanli/ode_prog.hpp>

#include <map>
#include <limits>
#include <string>
#include <utility>
#include <vector>

namespace stanli {

struct RetainedCallback {
  std::map<std::string, mir::FunDef> owned;
  std::map<std::string, const mir::FunDef*> funs_map;
  std::string callback_name;

  // Real callback arguments are packed in source order: active values into
  // the kernel's parameter input and data values into x_r. Integer values
  // remain in each RhsArg. The register compiler uses the same bindings to
  // reconstruct the callback's original positional signature.
  std::vector<RhsArg> args;
  std::vector<double> x_r;
  std::vector<int> x_i;
  RhsProgram prog;

  void adopt(const std::map<std::string, const mir::FunDef*>& src) {
    for (const auto& [name, def] : src) owned[name] = *def;
    for (const auto& [name, def] : owned) funs_map[name] = &def;
  }
  const mir::FunDef* callback() const {
    auto it = owned.find(callback_name);
    return it == owned.end() ? nullptr : &it->second;
  }
  const mir::FunDef* callback(const std::string& name) const {
    auto it = owned.find(name);
    return it == owned.end() ? nullptr : &it->second;
  }
  const std::map<std::string, const mir::FunDef*>* funs() const {
    return &funs_map;
  }
};

// Backend-neutral classification and packing of retained callback arguments.
// A backend supplies only value acquisition: graph lowering returns Val
// parts, Program lowering returns Range parts, and both receive identical
// RhsArg/x_r ordering and validation.
template <typename Active, typename GetActive, typename GetReals,
          typename GetInts, typename Fail>
std::vector<Active> pack_callback_arguments(RetainedCallback& retained,
                                            const std::vector<mir::Expr>& exprs,
                                            size_t begin, size_t end,
                                            GetActive&& get_active,
                                            GetReals&& get_reals,
                                            GetInts&& get_ints, Fail&& fail) {
  std::vector<Active> active;
  for (size_t i = begin; i < end; ++i) {
    const mir::Expr& arg = exprs[i];
    RhsArg binding;
    if (arg.unsized.leaf == mir::UnsizedLeaf::Int) {
      if (!arg.data_only) {
        fail("integer callback argument " + std::to_string(i - begin + 1) +
             " is not data");
        continue;
      }
      binding.is_int = true;
      binding.ints = get_ints(i);
    } else if (arg.data_only) {
      std::vector<double> values = get_reals(i);
      if (values.size() >
          static_cast<size_t>(std::numeric_limits<int>::max())) {
        fail("callback argument " + std::to_string(i - begin + 1) +
             " is too large");
        continue;
      }
      binding.len = static_cast<int>(values.size());
      retained.x_r.insert(retained.x_r.end(), values.begin(), values.end());
    } else {
      auto value = get_active(i);
      binding.is_param = true;
      binding.len = value.second;
      active.push_back(std::move(value.first));
    }
    retained.args.push_back(std::move(binding));
  }
  return active;
}

}  // namespace stanli

#endif
