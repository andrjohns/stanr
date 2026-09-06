// Helpers shared by the graph passes. Not installed.
#ifndef STANLI_PASS_UTIL_HPP
#define STANLI_PASS_UTIL_HPP

#include <stanli/graph.hpp>

#include <cstdint>
#include <unordered_map>
#include <utility>
#include <vector>

namespace stanli {

using Fills = std::vector<std::pair<int, std::vector<double>>>;

inline bool is_element_store(const Op& op) {
  return (op.opcode == OP_SET_INDEX || op.opcode == OP_SET_INDEX_INPLACE) &&
         op.n_in == 2 && op.n_idata == 1 && op.out == op.in[0];
}

struct Key {
  std::vector<int64_t> w;
  bool operator==(const Key& o) const { return w == o.w; }
};

struct KeyHash {
  size_t operator()(const Key& k) const {
    size_t h = 1469598103934665603ull;
    for (int64_t v : k.w) {
      h ^= static_cast<size_t>(v);
      h *= 1099511628211ull;
    }
    return h;
  }
};

inline std::vector<size_t>::const_iterator first_at_or_after(
    const std::vector<size_t>& v, size_t x, int64_t& steps) {
  size_t lo = 0, hi = v.size();
  while (lo < hi) {
    const size_t mid = lo + (hi - lo) / 2;
    ++steps;
    if (v[mid] < x)
      lo = mid + 1;
    else
      hi = mid;
  }
  return v.begin() + (ptrdiff_t)lo;
}

inline bool any_at_or_after(const std::vector<size_t>& v, size_t x,
                            int64_t& steps) {
  return first_at_or_after(v, x, steps) != v.end();
}

inline std::unordered_map<int, double> scalar_constants(const Fills& fills) {
  std::unordered_map<int, double> const_val;
  for (const auto& f : fills)
    if (f.second.size() == 1) const_val.emplace(f.first, f.second[0]);
  return const_val;
}

}  // namespace stanli

#endif
