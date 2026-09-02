#ifndef STANLI_CONTAINER_SHAPE_HPP
#define STANLI_CONTAINER_SHAPE_HPP

#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

namespace stanli {

// Validate every extent before multiplying: zero or paired negative extents
// must not hide an invalid dimension. Used before allocating either layout.
inline int64_t checked_container_size(const std::vector<int64_t>& dims,
                                      const std::string& function) {
  for (int64_t d : dims)
    if (d < 0) throw std::domain_error(function + ": negative extent");
  int64_t size = 1;
  for (int64_t d : dims) {
    if (d != 0 && size > std::numeric_limits<int64_t>::max() / d)
      throw std::domain_error(function + ": overflowing extent");
    size *= d;
  }
  return size;
}

// Stan Math checks both the start and inclusive end of each block axis,
// even when its length is zero. Subtraction avoids overflowing i+n-1.
inline void check_block_shape(int64_t rows, int64_t cols, int64_t i, int64_t j,
                              int64_t nr, int64_t nc) {
  const auto valid = [](int64_t extent, int64_t start, int64_t count) {
    return start >= 1 && start <= extent && count >= 0 &&
           (count == 0 ? start > 1 : count <= extent - start + 1);
  };
  if (!valid(rows, i, nr) || !valid(cols, j, nc))
    throw std::out_of_range("block: row or column range is out of bounds");
}

}  // namespace stanli

#endif
