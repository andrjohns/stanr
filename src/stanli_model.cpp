#include <stan/model/model_base.hpp>
#include <stan/math/rev/core.hpp>
#include <stanr/cpp11_tuple_interop.hpp>
#include <stanr/model_methods.hpp>
#include <stanli/compile.hpp>
#include <stanli/executor_pool.hpp>
#include <stanli/function.hpp>
#include <stanli/wa_interp.hpp>

#include <cpp11.hpp>
#include <cpp11/declarations.hpp>

#include <R.h>
#include <Rinternals.h>

#include <Eigen/Dense>

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <limits>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace stanr {
namespace {

std::vector<int64_t> sexp_dims(const std::string& name, SEXP value) {
  SEXP dim = Rf_getAttrib(value, R_DimSymbol);
  std::vector<int64_t> out;
  if (Rf_xlength(dim) > 0) {
    if (TYPEOF(dim) != INTSXP)
      throw std::runtime_error("invalid dimensions for stanli data: " + name);
    out.reserve(static_cast<size_t>(Rf_xlength(dim)));
    for (R_xlen_t i = 0; i < Rf_xlength(dim); ++i) {
      const int d = INTEGER_ELT(dim, i);
      if (d == NA_INTEGER || d < 0)
        throw std::runtime_error("invalid dimensions for stanli data: " + name);
      out.push_back(d);
    }
  } else if (Rf_xlength(value) != 1) {
    out.push_back(static_cast<int64_t>(Rf_xlength(value)));
  }
  return out;
}

struct copied_reals {
  std::vector<double> values;
  std::vector<int> integer_values;
  bool all_integer = true;
};

copied_reals copy_reals(const std::string& name, SEXP value) {
  const R_xlen_t n = Rf_xlength(value);
  const double* source = REAL(value);
  copied_reals out;
  out.values.resize(static_cast<size_t>(n));
  out.integer_values.reserve(static_cast<size_t>(n));
  for (R_xlen_t i = 0; i < n; ++i) {
    if (!std::isfinite(source[i]))
      throw std::runtime_error(
          "stanli data cannot contain NA, NaN, or Inf: " + name);
    out.values[static_cast<size_t>(i)] = source[i];
    if (out.all_integer && std::trunc(source[i]) == source[i] &&
        source[i] >= static_cast<double>(std::numeric_limits<int>::min()) &&
        source[i] <= static_cast<double>(std::numeric_limits<int>::max())) {
      out.integer_values.push_back(static_cast<int>(source[i]));
    } else {
      out.all_integer = false;
      out.integer_values.clear();
    }
  }
  return out;
}

std::vector<int> copy_integers(const std::string& name, SEXP value) {
  const bool logical = TYPEOF(value) == LGLSXP;
  const R_xlen_t n = Rf_xlength(value);
  const int* source = logical ? LOGICAL(value) : INTEGER(value);
  std::vector<int> out(static_cast<size_t>(n));
  for (R_xlen_t i = 0; i < n; ++i) {
    if (source[i] == NA_INTEGER)
      throw std::runtime_error("stanli data cannot contain NA: " + name);
    out[static_cast<size_t>(i)] = source[i];
  }
  return out;
}

void add_data_frame(stanli::DataMap& out, const std::string& name,
                    SEXP value) {
  const R_xlen_t n_columns = Rf_xlength(value);
  const R_xlen_t n_rows = n_columns == 0 ? 0 : Rf_xlength(VECTOR_ELT(value, 0));
  for (R_xlen_t column = 0; column < n_columns; ++column) {
    SEXP x = VECTOR_ELT(value, column);
    if (Rf_xlength(x) != n_rows)
      throw std::runtime_error("stanli data frame columns have unequal lengths: " +
                               name);
    if (TYPEOF(x) == CPLXSXP) {
      throw std::runtime_error(
          "stanli data does not support complex values yet: " + name);
    } else if (TYPEOF(x) != INTSXP && TYPEOF(x) != REALSXP) {
      throw std::runtime_error(
          "stanli data frames must contain only integer or numeric columns: " +
          name);
    }
  }

  const std::vector<int64_t> dims = {static_cast<int64_t>(n_rows),
                                     static_cast<int64_t>(n_columns)};
  const size_t size = static_cast<size_t>(n_rows * n_columns);
  std::vector<double> values;
  std::vector<int> integer_values;
  values.reserve(size);
  integer_values.reserve(size);
  bool all_integer = true;
  for (R_xlen_t column = 0; column < n_columns; ++column) {
    SEXP x = VECTOR_ELT(value, column);
    if (TYPEOF(x) == REALSXP) {
      copied_reals one = copy_reals(
          name + "[[" + std::to_string(column + 1) + "]]", x);
      values.insert(values.end(), one.values.begin(), one.values.end());
      if (all_integer && one.all_integer) {
        integer_values.insert(integer_values.end(), one.integer_values.begin(),
                              one.integer_values.end());
      } else {
        all_integer = false;
        integer_values.clear();
      }
    } else {
      std::vector<int> one = copy_integers(
          name + "[[" + std::to_string(column + 1) + "]]", x);
      values.insert(values.end(), one.begin(), one.end());
      if (all_integer)
        integer_values.insert(integer_values.end(), one.begin(), one.end());
    }
  }
  if (all_integer) {
    out.set_int_array(name, std::move(integer_values), dims);
    return;
  }
  out.set_real_array(name, std::move(values), dims);
}

// Build DataMap directly so each R value is validated and copied once. The
// compiled backend still uses r_data_context for tuple/complex support; those
// types are deliberately rejected here because stanli cannot lower them.
stanli::DataMap sexp_to_data_map(SEXP data) {
  if (Rf_isNull(data) || XLENGTH(data) == 0) return stanli::DataMap();
  SEXP names = Rf_getAttrib(data, R_NamesSymbol);
  if (TYPEOF(data) != VECSXP || Rf_isNull(names) ||
      XLENGTH(names) != XLENGTH(data))
    throw std::runtime_error("stanli data must be a named list");
  stanli::DataMap out;
  for (R_xlen_t i = 0; i < XLENGTH(data); ++i) {
    SEXP name_sexp = STRING_ELT(names, i);
    if (name_sexp == NA_STRING || CHAR(name_sexp)[0] == '\0')
      throw std::runtime_error("stanli data list names must be non-empty");
    const std::string name = CHAR(name_sexp);
    if (out.has(name))
      throw std::runtime_error("stanli data list names must be unique: " + name);
    SEXP value = VECTOR_ELT(data, i);
    if (Rf_isNull(value))
      throw std::runtime_error("stanli data value cannot be NULL: " + name);
    if (Rf_inherits(value, "data.frame")) {
      add_data_frame(out, name, value);
      continue;
    }
    if (TYPEOF(value) == CPLXSXP)
      throw std::runtime_error(
          "stanli data does not support complex values yet: " + name);
    if (TYPEOF(value) == REALSXP) {
      copied_reals values = copy_reals(name, value);
      std::vector<int64_t> dims = sexp_dims(name, value);
      if (values.all_integer) {
        if (dims.empty())
          out.set_int(name, values.integer_values.front());
        else
          out.set_int_array(name, std::move(values.integer_values),
                            std::move(dims));
      } else if (dims.empty()) {
        out.set_real(name, values.values.front());
      } else {
        out.set_real_array(name, std::move(values.values), std::move(dims));
      }
      continue;
    }
    if (TYPEOF(value) == INTSXP || TYPEOF(value) == LGLSXP) {
      std::vector<int> values = copy_integers(name, value);
      std::vector<int64_t> dims = sexp_dims(name, value);
      if (dims.empty())
        out.set_int(name, values.front());
      else
        out.set_int_array(name, std::move(values), std::move(dims));
      continue;
    }
    throw std::runtime_error(
        "stanli data must be numeric, logical, or an array (tuple-typed "
        "data is not yet supported): " + name);
  }
  return out;
}

SEXP data_entry_to_sexp(const stanli::DataMap::Entry& entry) {
  const R_xlen_t size = static_cast<R_xlen_t>(
      entry.is_int ? entry.i.size() : entry.r.size());
  SEXP out = PROTECT(Rf_allocVector(entry.is_int ? INTSXP : REALSXP, size));
  if (entry.is_int) {
    std::copy(entry.i.begin(), entry.i.end(), INTEGER(out));
  } else {
    std::copy(entry.r.begin(), entry.r.end(), REAL(out));
  }
  if (!entry.dims.empty()) {
    SEXP dims = PROTECT(Rf_allocVector(INTSXP, entry.dims.size()));
    for (size_t i = 0; i < entry.dims.size(); ++i) {
      if (entry.dims[i] < 0 || entry.dims[i] > std::numeric_limits<int>::max()) {
        UNPROTECT(2);
        throw std::runtime_error("stanli function result has invalid dimensions");
      }
      INTEGER(dims)[i] = static_cast<int>(entry.dims[i]);
    }
    Rf_setAttrib(out, R_DimSymbol, dims);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return out;
}

void append_unc_names(const stanli::CompiledModel::UncParam& p,
                      std::vector<std::string>& out) {
  int64_t product = 1;
  bool sane = true;
  for (const int64_t d : p.dims) {
    if (d < 0) sane = false;
    product *= d;
  }
  if (sane && product == p.len && !p.dims.empty()) {
    std::vector<int64_t> index(p.dims.size(), 0);
    for (int64_t k = 0; k < p.len; ++k) {
      std::string name = p.name;
      for (const int64_t i : index) name += "." + std::to_string(i + 1);
      out.push_back(std::move(name));
      for (size_t d = 0; d < index.size(); ++d) {
        if (++index[d] < p.dims[d]) break;
        index[d] = 0;
      }
    }
    return;
  }
  if (p.dims.empty() && p.len == 1) {
    out.push_back(p.name);
    return;
  }
  for (int64_t i = 0; i < p.len; ++i)
    out.push_back(p.name + "." + std::to_string(i + 1));
}

bool require_jacobian(SEXP value) {
  if (!stanr::as_cpp<bool>(value))
    throw std::runtime_error(
        "stanli supports only jacobian = TRUE; its density graph includes "
        "the change-of-variables terms");
  return true;
}

}  // namespace

// Package-only adapter. Runtime-compiled model libraries link the generic
// runner archive but never this class or the stanli runtime.
class __attribute__((visibility("hidden"))) stanli_model_base final
    : public stan::model::model_base {
 public:
  stanli_model_base(const std::string& mir, const stanli::DataMap& data,
                    std::string model_name)
      : stan::model::model_base(0), name_(std::move(model_name)) {
    cm_ = stanli::compile_model(mir, data);
    proto_ = std::make_unique<stanli::Executor>(std::move(cm_.graph));
    cm_.bind(*proto_);
    num_params_r__ = static_cast<size_t>(proto_->n_params());
    pool_ = std::make_unique<stanli::ExecutorPool>(*proto_);
    columns_ = cm_.views;
    n_tp_start_ = columns_.size();
    n_gq_start_ = columns_.size();
    prepare_write_array();
    prepare_column_offsets();
  }

  std::string model_name() const override { return name_; }

  std::vector<std::string> model_compile_info() const override {
    return {"stanli graph interpreter", "model construction without C++ compilation"};
  }

  void get_param_names(std::vector<std::string>& names,
                       bool include_tparams = true,
                       bool include_gqs = true) const override {
    names.clear();
    for_each_selected(include_tparams, include_gqs,
                      [&](const auto& v) { names.push_back(v.name); });
  }

  void get_dims(std::vector<std::vector<size_t>>& dims,
                bool include_tparams = true,
                bool include_gqs = true) const override {
    dims.clear();
    for_each_selected(include_tparams, include_gqs, [&](const auto& v) {
      dims.push_back(has_write_array() ? column_dims(v) : view_dims(v));
    });
  }

  void constrained_param_names(std::vector<std::string>& names,
                               bool include_tparams = true,
                               bool include_gqs = true) const override {
    for_each_selected(include_tparams, include_gqs,
                      [&](const auto& v) { v.append_names(names); });
  }

  void unconstrained_param_names(std::vector<std::string>& names,
                                 bool = true, bool = true) const override {
    for (const auto& p : cm_.unc_params) append_unc_names(p, names);
  }

  double density(Eigen::VectorXd& q) const {
    auto lease = pool_->acquire();
    check_size(q.size());
    for (Eigen::Index i = 0; i < q.size(); ++i) lease->params_data()[i] = q(i);
    try {
      return lease->forward();
    } catch (const std::exception&) {
      return -std::numeric_limits<double>::infinity();
    }
  }

  stan::math::var density(
      Eigen::Matrix<stan::math::var, -1, 1>& q) const {
    auto lease = pool_->acquire();
    check_size(q.size());
    // The workspace is independent of a model/executor and is safe to reuse
    // after precomputed_gradients() has copied it onto the autodiff arena.
    static thread_local std::vector<double> gradient;
    gradient.resize(static_cast<size_t>(q.size()));
    for (Eigen::Index i = 0; i < q.size(); ++i)
      lease->params_data()[i] = q(i).val();
    double value;
    try {
      value = lease->gradient(gradient.data());
    } catch (const std::exception&) {
      return stan::math::var(-std::numeric_limits<double>::infinity());
    }
    std::vector<stan::math::var> operands;
    if (q.size() != 0) operands.assign(q.data(), q.data() + q.size());
    return stan::math::precomputed_gradients(value, operands, gradient);
  }

#define STANLI_EIGEN_LOG_PROB(NAME) \
  double NAME(Eigen::VectorXd& q, std::ostream*) const override { \
    return density(q); \
  } \
  stan::math::var NAME( \
      Eigen::Matrix<stan::math::var, -1, 1>& q, std::ostream*) const override { \
    return density(q); \
  }

  STANLI_EIGEN_LOG_PROB(log_prob)
  STANLI_EIGEN_LOG_PROB(log_prob_jacobian)
  STANLI_EIGEN_LOG_PROB(log_prob_propto)
  STANLI_EIGEN_LOG_PROB(log_prob_propto_jacobian)

#undef STANLI_EIGEN_LOG_PROB

#define STANLI_VECTOR_LOG_PROB(NAME, T) \
  T NAME(std::vector<T>& q, std::vector<int>&, \
         std::ostream* msgs) const override { \
    Eigen::Matrix<T, -1, 1> mapped = Eigen::Map<Eigen::Matrix<T, -1, 1>>( \
        q.data(), static_cast<Eigen::Index>(q.size())); \
    return NAME(mapped, msgs); \
  }

  STANLI_VECTOR_LOG_PROB(log_prob, double)
  STANLI_VECTOR_LOG_PROB(log_prob, stan::math::var)
  STANLI_VECTOR_LOG_PROB(log_prob_jacobian, double)
  STANLI_VECTOR_LOG_PROB(log_prob_jacobian, stan::math::var)
  STANLI_VECTOR_LOG_PROB(log_prob_propto, double)
  STANLI_VECTOR_LOG_PROB(log_prob_propto, stan::math::var)
  STANLI_VECTOR_LOG_PROB(log_prob_propto_jacobian, double)
  STANLI_VECTOR_LOG_PROB(log_prob_propto_jacobian, stan::math::var)

#undef STANLI_VECTOR_LOG_PROB

  void transform_inits(const stan::io::var_context& context,
                       Eigen::VectorXd& params_r,
                       std::ostream*) const override {
    const std::vector<double> values = unconstrain_context(context);
    params_r.resize(static_cast<Eigen::Index>(values.size()));
    std::copy(values.begin(), values.end(), params_r.data());
  }

  void transform_inits(const stan::io::var_context& context,
                       std::vector<int>& params_i,
                       std::vector<double>& params_r,
                       std::ostream*) const override {
    params_i.clear();
    params_r = unconstrain_context(context);
  }

  void unconstrain_array(const Eigen::VectorXd& constrained,
                         Eigen::VectorXd& unconstrained,
                         std::ostream*) const override {
    const std::vector<double> values = unconstrain_flat(
        constrained.data(), static_cast<size_t>(constrained.size()));
    unconstrained.resize(static_cast<Eigen::Index>(values.size()));
    std::copy(values.begin(), values.end(), unconstrained.data());
  }

  void unconstrain_array(const std::vector<double>& constrained,
                         std::vector<double>& unconstrained,
                         std::ostream*) const override {
    unconstrained = unconstrain_flat(constrained.data(), constrained.size());
  }

  void write_array(stan::rng_t& rng, Eigen::VectorXd& q, Eigen::VectorXd& out,
                   bool include_tparams = true, bool include_gqs = true,
                   std::ostream* msgs = nullptr) const override {
    out.resize(static_cast<Eigen::Index>(
        selected_output_size(include_tparams, include_gqs)));
    write_array_impl(rng, q.data(), static_cast<size_t>(q.size()), out.data(),
                     include_tparams, include_gqs, msgs);
  }

  void write_array(stan::rng_t& rng, std::vector<double>& q,
                   std::vector<int>&, std::vector<double>& out,
                   bool include_tparams = true, bool include_gqs = true,
                   std::ostream* msgs = nullptr) const override {
    out.resize(selected_output_size(include_tparams, include_gqs));
    write_array_impl(rng, q.data(), q.size(), out.data(), include_tparams,
                     include_gqs, msgs);
  }

 private:
  const stanli::InitInterp& init_interpreter() const {
    if (!cm_.transform_inits) {
      throw std::runtime_error(
          "this model has no inverse parameter transforms: its MIR carries "
          "no transform_inits section");
    }
    if (!cm_.transform_inits->interp) {
      throw std::runtime_error(
          "this model's inverse parameter transforms are unavailable: " +
          cm_.transform_inits->truncated);
    }
    return *cm_.transform_inits->interp;
  }

  std::vector<double> check_unconstrained_size(
      std::vector<double> values) const {
    if (values.size() != num_params_r__)
      throw std::runtime_error(
          "the inverse transforms produced the wrong number of unconstrained "
          "values");
    return values;
  }

  std::vector<double> unconstrain_context(
      const stan::io::var_context& context) const {
    const stanli::DataMap supplied =
        stanli::DataMap::from_var_context(context);
    const std::map<std::string, stanli::DataMap::Entry> inits(
        supplied.entries().begin(), supplied.entries().end());
    return check_unconstrained_size(init_interpreter().eval(inits));
  }

  std::vector<double> unconstrain_flat(const double* values, size_t size) const {
    const stanli::InitInterp& interp = init_interpreter();
    std::map<std::string, stanli::DataMap::Entry> inits;
    size_t offset = 0;
    for (const stanli::InitParam& param : interp.params()) {
      if (param.constrained_len < 0 ||
          static_cast<uint64_t>(param.constrained_len) > size - offset) {
        throw std::runtime_error(
            "the constrained parameter vector has the wrong number of values");
      }
      stanli::DataMap::Entry entry;
      entry.dims = param.dims;
      const size_t length = static_cast<size_t>(param.constrained_len);
      if (length != 0)
        entry.r.assign(values + offset, values + offset + length);
      inits.emplace(param.name, std::move(entry));
      offset += length;
    }
    if (offset != size)
      throw std::runtime_error(
          "the constrained parameter vector has the wrong number of values");
    return check_unconstrained_size(interp.eval(inits));
  }

  static std::vector<size_t> view_dims(const stanli::CompiledModel::ParamView& v) {
    return std::vector<size_t>(v.dims.begin(), v.dims.end());
  }

  static std::vector<size_t> column_dims(const stanli::CompiledModel::ParamView& v) {
    if (v.naming == stanli::CompiledModel::ParamView::Naming::Matrix && v.rows > 0)
      return {static_cast<size_t>(v.rows), static_cast<size_t>(v.len / v.rows)};
    if (v.len == 1 && v.naming == stanli::CompiledModel::ParamView::Naming::Scalar)
      return {};
    return {static_cast<size_t>(v.len)};
  }

  bool has_write_array() const { return wa_interp_ || wa_pool_; }

  using Range = std::pair<size_t, size_t>;

  size_t selected_ranges(bool include_tparams, bool include_gqs,
                         std::array<Range, 2>& ranges) const {
    ranges[0] = {0, include_tparams ? n_gq_start_ : n_tp_start_};
    if (!include_gqs) return 1;
    ranges[1] = {n_gq_start_, columns_.size()};
    return 2;
  }

  template <typename F>
  void for_each_selected(bool include_tparams, bool include_gqs, F&& f) const {
    std::array<Range, 2> ranges;
    const size_t n_ranges =
        selected_ranges(include_tparams, include_gqs, ranges);
    for (size_t r = 0; r < n_ranges; ++r)
      for (size_t i = ranges[r].first; i < ranges[r].second; ++i)
        f(columns_[i]);
  }

  void prepare_column_offsets() {
    column_offsets_.resize(columns_.size() + 1);
    column_offsets_[0] = 0;
    for (size_t i = 0; i < columns_.size(); ++i) {
      column_offsets_[i + 1] =
          column_offsets_[i] + static_cast<size_t>(columns_[i].len);
    }
  }

  size_t selected_output_size(bool include_tparams, bool include_gqs) const {
    std::array<Range, 2> ranges;
    const size_t n_ranges =
        selected_ranges(include_tparams, include_gqs, ranges);
    size_t size = 0;
    for (size_t r = 0; r < n_ranges; ++r)
      size += column_offsets_[ranges[r].second] -
              column_offsets_[ranges[r].first];
    return size;
  }

  void check_size(Eigen::Index n) const {
    if (n != static_cast<Eigen::Index>(num_params_r__))
      throw std::invalid_argument("stanli received the wrong number of unconstrained parameters");
  }

  void prepare_write_array() {
    if (!cm_.write_array) return;
    auto& wa = *cm_.write_array;
    if (wa.interp) {
      std::vector<double> q(static_cast<size_t>(proto_->n_params()));
      bool found = false;
      for (int variant = 0; variant < 3 && !found; ++variant) {
        for (size_t i = 0; i < q.size(); ++i)
          q[i] = stanli::wa_probe_point(static_cast<int64_t>(i), variant);
        try {
          stanli::WaRng probe(1);
          std::copy(q.begin(), q.end(), proto_->params_data());
          proto_->run_forward_only();
          (void)wa.interp->eval(cm_.constrained_env(*proto_), probe);
          found = true;
        } catch (const std::exception&) {
        }
      }
      if (!found) return;
      wa_interp_ = wa.interp;
      columns_ = wa_interp_->columns();
      n_tp_start_ = wa_interp_->n_tp_start();
      n_gq_start_ = wa_interp_->n_gq_start();
    } else if (!wa.columns.empty()) {
      wa_proto_ = std::make_unique<stanli::Executor>(std::move(wa.graph));
      wa.bind(*wa_proto_);
      columns_ = wa.columns;
      n_tp_start_ = wa.n_tp_start;
      n_gq_start_ = wa.n_gq_start;
      wa_pool_ = std::make_unique<stanli::ExecutorPool>(*wa_proto_);
    }
  }

  void write_array_impl(stan::rng_t& rng, const double* q, size_t q_size,
                        double* out, bool include_tparams, bool include_gqs,
                        std::ostream*) const {
    check_size(static_cast<Eigen::Index>(q_size));
    std::array<Range, 2> ranges;
    const size_t n_ranges =
        selected_ranges(include_tparams, include_gqs, ranges);

    if (wa_interp_) {
      stanli::WaRng wa_rng(include_gqs ? static_cast<unsigned>(rng()) : 1);
      auto lease = pool_->acquire();
      if (q_size != 0) std::copy(q, q + q_size, lease->params_data());
      lease->run_forward_only();
      const std::vector<double> row = wa_interp_->eval(
          cm_.constrained_env(*lease), wa_rng);
      for (size_t r = 0; r < n_ranges; ++r) {
        const size_t first = column_offsets_[ranges[r].first];
        const size_t last = column_offsets_[ranges[r].second];
        out = std::copy(row.begin() + static_cast<std::ptrdiff_t>(first),
                        row.begin() + static_cast<std::ptrdiff_t>(last), out);
      }
      return;
    }

    auto lease = (wa_pool_ ? *wa_pool_ : *pool_).acquire();
    if (q_size != 0) std::copy(q, q + q_size, lease->params_data());
    lease->run_forward_only();
    for (size_t r = 0; r < n_ranges; ++r) {
      for (size_t i = ranges[r].first; i < ranges[r].second; ++i) {
        const auto& v = columns_[i];
        const double* p = lease->value_ptr(v.slot);
        for (int64_t j = 0; j < v.len; ++j)
          *out++ = p[v.storage_index(j)];
      }
    }
  }

  std::string name_;
  stanli::CompiledModel cm_;
  std::unique_ptr<stanli::Executor> proto_;
  mutable std::unique_ptr<stanli::ExecutorPool> pool_;
  std::unique_ptr<stanli::Executor> wa_proto_;
  mutable std::unique_ptr<stanli::ExecutorPool> wa_pool_;
  std::shared_ptr<stanli::WaInterp> wa_interp_;
  std::vector<stanli::CompiledModel::ParamView> columns_;
  std::vector<size_t> column_offsets_;
  size_t n_tp_start_ = 0;
  size_t n_gq_start_ = 0;
};

extern "C" SEXP stanr_stanli_new_model(SEXP mir, SEXP data, SEXP model_name) {
  BEGIN_CPP11
  if (TYPEOF(mir) != STRSXP || XLENGTH(mir) != 1)
    cpp11::stop("stanli MIR must be a single string");
  if (TYPEOF(model_name) != STRSXP || XLENGTH(model_name) != 1)
    cpp11::stop("stanli model name must be a single string");
  stanli::DataMap data_map = sexp_to_data_map(data);
  auto* model = new stanli_model_base(
      CHAR(STRING_ELT(mir, 0)), data_map, CHAR(STRING_ELT(model_name, 0)));
  return cpp11::external_pointer<stan::model::model_base>(model);
  END_CPP11
}

extern "C" SEXP stanr_stanli_run_model(SEXP model, SEXP args) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::run_model(*m, cpp11::list(args));
  END_CPP11
}

extern "C" SEXP stanr_stanli_new_function(SEXP mir, SEXP function_name) {
  BEGIN_CPP11
  if (TYPEOF(mir) != STRSXP || XLENGTH(mir) != 1)
    cpp11::stop("stanli function MIR must be a single string");
  if (TYPEOF(function_name) != STRSXP || XLENGTH(function_name) != 1)
    cpp11::stop("stanli function name must be a single string");
  auto* function = new stanli::Function(stanli::Function::from_mir(
      CHAR(STRING_ELT(mir, 0)), CHAR(STRING_ELT(function_name, 0))));
  return cpp11::external_pointer<stanli::Function>(function);
  END_CPP11
}

extern "C" SEXP stanr_stanli_call_function(SEXP function, SEXP arguments) {
  BEGIN_CPP11
  cpp11::external_pointer<stanli::Function> fn(function);
  return data_entry_to_sexp((*fn)(sexp_to_data_map(arguments)));
  END_CPP11
}

extern "C" SEXP stanr_stanli_constrained_param_names(SEXP model) {
  BEGIN_CPP11
  cpp11::external_pointer<stan::model::model_base> m(model);
  return stanr::model_constrained_names(*m, false, false);
  END_CPP11
}

extern "C" SEXP stanr_stanli_new_base_rng(SEXP seed) {
  BEGIN_CPP11
  return stanr::make_base_rng(stanr::as_cpp<unsigned int>(seed));
  END_CPP11
}

#define STANLI_MODEL_METHOD(name, signature, expression) \
  extern "C" SEXP stanr_stanli_##name signature { \
    BEGIN_CPP11 \
    cpp11::external_pointer<stan::model::model_base> m(model); \
    expression; \
    END_CPP11 \
  }

STANLI_MODEL_METHOD(model_num_upars, (SEXP model),
                    return cpp11::as_sexp(stanr::model_num_upars(*m)))
STANLI_MODEL_METHOD(model_param_metadata, (SEXP model),
                    return stanr::model_param_metadata(*m))
STANLI_MODEL_METHOD(model_constrained_names,
                    (SEXP model, SEXP tp, SEXP gq),
                    return stanr::model_constrained_names(
                        *m, stanr::as_cpp<bool>(tp), stanr::as_cpp<bool>(gq)))
STANLI_MODEL_METHOD(model_unconstrained_names, (SEXP model),
                    return stanr::model_unconstrained_names(*m))
STANLI_MODEL_METHOD(model_log_prob,
                    (SEXP model, SEXP upars, SEXP jacobian),
                    return stanr::model_log_prob(
                        *m, cpp11::doubles(upars), require_jacobian(jacobian)))
STANLI_MODEL_METHOD(model_grad_log_prob,
                    (SEXP model, SEXP upars, SEXP jacobian),
                    return stanr::model_grad_log_prob(
                        *m, cpp11::doubles(upars), require_jacobian(jacobian)))
STANLI_MODEL_METHOD(model_hessian,
                    (SEXP model, SEXP upars, SEXP jacobian),
                    return stanr::model_hessian(
                        *m, cpp11::doubles(upars), require_jacobian(jacobian)))
STANLI_MODEL_METHOD(model_unconstrain,
                    (SEXP model, SEXP variables, SEXP declarations),
                    return stanr::model_unconstrain(
                        *m, cpp11::list(variables), declarations))
STANLI_MODEL_METHOD(model_unconstrain_matrix,
                    (SEXP model, SEXP values),
                    return stanr::model_unconstrain_matrix(
                        *m, cpp11::doubles_matrix<>(values)))
STANLI_MODEL_METHOD(model_constrain,
                    (SEXP model, SEXP rng, SEXP upars, SEXP tp, SEXP gq),
                    return stanr::model_constrain(
                        *m, *cpp11::external_pointer<stan::rng_t>(rng),
                        cpp11::doubles(upars), stanr::as_cpp<bool>(tp),
                        stanr::as_cpp<bool>(gq)))
STANLI_MODEL_METHOD(model_constrain_matrix,
                    (SEXP model, SEXP rng, SEXP upars, SEXP tp, SEXP gq),
                    return stanr::model_constrain_matrix(
                        *m, *cpp11::external_pointer<stan::rng_t>(rng),
                        cpp11::doubles_matrix<>(upars), stanr::as_cpp<bool>(tp),
                        stanr::as_cpp<bool>(gq)))
STANLI_MODEL_METHOD(model_constrain_variables,
                    (SEXP model, SEXP rng, SEXP upars, SEXP tp, SEXP gq,
                     SEXP declarations),
                    return stanr::model_constrain_variables(
                        *m, *cpp11::external_pointer<stan::rng_t>(rng),
                        cpp11::doubles(upars), stanr::as_cpp<bool>(tp),
                        stanr::as_cpp<bool>(gq), declarations))
STANLI_MODEL_METHOD(model_variable_skeleton,
                    (SEXP model, SEXP tp, SEXP gq, SEXP declarations),
                    return stanr::model_variable_skeleton(
                        *m, stanr::as_cpp<bool>(tp), stanr::as_cpp<bool>(gq),
                        cpp11::list(declarations)))
}  // namespace stanr
