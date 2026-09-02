#include <Rinternals.h>
#include <R_ext/Visibility.h>

extern "C" SEXP stanr_xptr_is_null(SEXP ptr);
extern "C" SEXP stanr_hash_strings(SEXP strings);
extern "C" SEXP stanr_max_concurrency(void);
extern "C" SEXP stanr_stanli_new_model(SEXP, SEXP, SEXP);
extern "C" SEXP stanr_stanli_run_model(SEXP, SEXP);
extern "C" SEXP stanr_stanli_new_function(SEXP, SEXP);
extern "C" SEXP stanr_stanli_call_function(SEXP, SEXP);
extern "C" SEXP stanr_stanli_constrained_param_names(SEXP);
extern "C" SEXP stanr_stanli_new_base_rng(SEXP);
extern "C" SEXP stanr_stanli_model_num_upars(SEXP);
extern "C" SEXP stanr_stanli_model_param_metadata(SEXP);
extern "C" SEXP stanr_stanli_model_constrained_names(SEXP, SEXP, SEXP);
extern "C" SEXP stanr_stanli_model_unconstrained_names(SEXP);
extern "C" SEXP stanr_stanli_model_log_prob(SEXP, SEXP, SEXP);
extern "C" SEXP stanr_stanli_model_grad_log_prob(SEXP, SEXP, SEXP);
extern "C" SEXP stanr_stanli_model_hessian(SEXP, SEXP, SEXP);
extern "C" SEXP stanr_stanli_model_unconstrain(SEXP, SEXP, SEXP);
extern "C" SEXP stanr_stanli_model_unconstrain_matrix(SEXP, SEXP);
extern "C" SEXP stanr_stanli_model_constrain(SEXP, SEXP, SEXP, SEXP, SEXP);
extern "C" SEXP stanr_stanli_model_constrain_matrix(SEXP, SEXP, SEXP, SEXP, SEXP);
extern "C" SEXP stanr_stanli_model_constrain_variables(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern "C" SEXP stanr_stanli_model_variable_skeleton(SEXP, SEXP, SEXP, SEXP);

extern "C"  {
  static const R_CallMethodDef CallEntries[] = {
    {"stanr_xptr_is_null", (DL_FUNC) &stanr_xptr_is_null, 1},
    {"stanr_hash_strings", (DL_FUNC) &stanr_hash_strings, 1},
    {"stanr_max_concurrency", (DL_FUNC) &stanr_max_concurrency, 0},
    {"stanr_stanli_new_model", (DL_FUNC) &stanr_stanli_new_model, 3},
    {"stanr_stanli_run_model", (DL_FUNC) &stanr_stanli_run_model, 2},
    {"stanr_stanli_new_function", (DL_FUNC) &stanr_stanli_new_function, 2},
    {"stanr_stanli_call_function", (DL_FUNC) &stanr_stanli_call_function, 2},
    {"stanr_stanli_constrained_param_names", (DL_FUNC) &stanr_stanli_constrained_param_names, 1},
    {"stanr_stanli_new_base_rng", (DL_FUNC) &stanr_stanli_new_base_rng, 1},
    {"stanr_stanli_model_num_upars", (DL_FUNC) &stanr_stanli_model_num_upars, 1},
    {"stanr_stanli_model_param_metadata", (DL_FUNC) &stanr_stanli_model_param_metadata, 1},
    {"stanr_stanli_model_constrained_names", (DL_FUNC) &stanr_stanli_model_constrained_names, 3},
    {"stanr_stanli_model_unconstrained_names", (DL_FUNC) &stanr_stanli_model_unconstrained_names, 1},
    {"stanr_stanli_model_log_prob", (DL_FUNC) &stanr_stanli_model_log_prob, 3},
    {"stanr_stanli_model_grad_log_prob", (DL_FUNC) &stanr_stanli_model_grad_log_prob, 3},
    {"stanr_stanli_model_hessian", (DL_FUNC) &stanr_stanli_model_hessian, 3},
    {"stanr_stanli_model_unconstrain", (DL_FUNC) &stanr_stanli_model_unconstrain, 3},
    {"stanr_stanli_model_unconstrain_matrix", (DL_FUNC) &stanr_stanli_model_unconstrain_matrix, 2},
    {"stanr_stanli_model_constrain", (DL_FUNC) &stanr_stanli_model_constrain, 5},
    {"stanr_stanli_model_constrain_matrix", (DL_FUNC) &stanr_stanli_model_constrain_matrix, 5},
    {"stanr_stanli_model_constrain_variables", (DL_FUNC) &stanr_stanli_model_constrain_variables, 6},
    {"stanr_stanli_model_variable_skeleton", (DL_FUNC) &stanr_stanli_model_variable_skeleton, 4},
    {NULL, NULL, 0}
  };

  attribute_visible void R_init_stanr(DllInfo* dll){
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
  }
}
