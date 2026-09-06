// MIR-specific half of shared print()/reject() lowering.  It owns no backend
// handles: the callback lowers each non-literal argument using the caller's
// normal expression dispatcher.
#ifndef STANLI_MIR_MESSAGE_HPP
#define STANLI_MIR_MESSAGE_HPP

#include <stanli/message.hpp>
#include <stanli/mir.hpp>

#include <string>
#include <vector>

namespace stanli {

template <typename LowerValue>
MessageSpec lower_message_arguments(const std::vector<mir::Expr>& args,
                                    LowerValue lower_value) {
  MessageSpec spec;
  std::string pending;
  for (const auto& arg : args) {
    if (arg.kind == mir::Expr::LitStr) {
      pending += arg.lit_s;
      continue;
    }
    spec.chunks.push_back(std::move(pending));
    pending.clear();
    lower_value(arg);
  }
  spec.chunks.push_back(std::move(pending));
  return spec;
}

}  // namespace stanli

#endif
