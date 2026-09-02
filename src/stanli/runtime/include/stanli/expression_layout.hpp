// Conservative description of the expression layout seen by generated
// Stan Math code before stanli materializes a graph slot.
#ifndef STANLI_EXPRESSION_LAYOUT_HPP
#define STANLI_EXPRESSION_LAYOUT_HPP

#include <cstdint>
#include <limits>

namespace stanli {

// The graph always stores values in contiguous slots.  This describes the
// source expression's evaluator instead: reduction grouping can distinguish a
// scalar-only access, a packet evaluator without direct coefficient access,
// and a direct owning/view expression whose first coefficient has a known
// offset.  Unknown is deliberately a first-class state; callers must not
// guess when floating-point grouping or extrema tie handling is observable.
struct ExpressionLayout {
  enum class Kind : uint8_t { Unknown, Scalar, Packet, Direct };

  Kind kind = Kind::Unknown;
  int64_t element_offset = 0;

  static constexpr ExpressionLayout unknown() { return {}; }

  static constexpr ExpressionLayout scalar() { return {Kind::Scalar, 0}; }

  static constexpr ExpressionLayout packet() { return {Kind::Packet, 0}; }

  static constexpr ExpressionLayout direct(int64_t offset = 0) {
    return {Kind::Direct, offset};
  }

  constexpr bool known() const { return kind != Kind::Unknown; }
  constexpr bool packet_access() const {
    return kind == Kind::Packet || kind == Kind::Direct;
  }
  constexpr bool direct_access() const { return kind == Kind::Direct; }

  friend constexpr bool operator==(ExpressionLayout a, ExpressionLayout b) {
    return a.kind == b.kind && a.element_offset == b.element_offset;
  }

  friend constexpr bool operator!=(ExpressionLayout a, ExpressionLayout b) {
    return !(a == b);
  }
};

namespace expression_layout {

// Layout composition is intentionally independent of MIR and graph slots so
// the policy can be checked against the pinned Stan Math expressions. The
// lowerer supplies the graph-specific facts (for example, whether an operand
// is a language scalar) and this helper decides the conservative result.
constexpr ExpressionLayout contiguous(ExpressionLayout source, int64_t offset) {
  if (offset < 0) return ExpressionLayout::unknown();
  switch (source.kind) {
    case ExpressionLayout::Kind::Unknown:
      return ExpressionLayout::unknown();
    case ExpressionLayout::Kind::Scalar:
      return ExpressionLayout::scalar();
    case ExpressionLayout::Kind::Packet:
      // A packet expression starts its own fold at lane zero after it is
      // materialized as a contiguous block.
      return ExpressionLayout::packet();
    case ExpressionLayout::Kind::Direct:
      if (source.element_offset > std::numeric_limits<int64_t>::max() - offset)
        return ExpressionLayout::unknown();
      return ExpressionLayout::direct(source.element_offset + offset);
  }
  return ExpressionLayout::unknown();
}

constexpr ExpressionLayout elementwise(bool all_scalar, bool packet_supported,
                                       bool all_known, bool all_packet_access) {
  if (all_scalar) return ExpressionLayout::scalar();
  if (!packet_supported) return ExpressionLayout::scalar();
  if (!all_known) return ExpressionLayout::unknown();
  if (!all_packet_access) return ExpressionLayout::scalar();
  return ExpressionLayout::packet();
}

}  // namespace expression_layout

}  // namespace stanli

#endif
