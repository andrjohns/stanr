// Private preparation diagnostics for the re-roll pass. This header is not
// installed; the public RerollStats layout and reroll() entrypoint stay
// unchanged.
#ifndef STANLI_REROLL_PROFILE_HPP
#define STANLI_REROLL_PROFILE_HPP

#include <stanli/reroll.hpp>

namespace stanli {
namespace detail {

struct RerollDispositionStats {
  int64_t packed_rows = 0;
  int64_t term_density = 0;
  int64_t element_density = 0;
  int64_t term_widen = 0;
  int64_t element_store = 0;
};

struct ProfiledRerollStats {
  RerollStats work;
  RerollDispositionStats dispositions;
};

// The lowerer calls this only for STANLI_PROFILE_PREP. Ordinary compilation
// continues through the public reroll() function and does not count
// dispositions.
ProfiledRerollStats reroll_profiled(
    Graph& g, std::vector<std::pair<int, std::vector<double>>>& fills,
    std::vector<int>& target_terms, const std::vector<int>& extra_roots);

}  // namespace detail
}  // namespace stanli

#endif
