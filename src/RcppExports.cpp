// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// c_test
double c_test(double test);
RcppExport SEXP _statidea_c_test(SEXP testSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type test(testSEXP);
    rcpp_result_gen = Rcpp::wrap(c_test(test));
    return rcpp_result_gen;
END_RCPP
}
// c_two_arm_diff
NumericMatrix c_two_arm_diff(NumericMatrix dat);
RcppExport SEXP _statidea_c_two_arm_diff(SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dat(datSEXP);
    rcpp_result_gen = Rcpp::wrap(c_two_arm_diff(dat));
    return rcpp_result_gen;
END_RCPP
}
// c_mtp_single
IntegerVector c_mtp_single(NumericVector p_values, NumericVector alphas, NumericMatrix mat_g, bool log);
RcppExport SEXP _statidea_c_mtp_single(SEXP p_valuesSEXP, SEXP alphasSEXP, SEXP mat_gSEXP, SEXP logSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p_values(p_valuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alphas(alphasSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_g(mat_gSEXP);
    Rcpp::traits::input_parameter< bool >::type log(logSEXP);
    rcpp_result_gen = Rcpp::wrap(c_mtp_single(p_values, alphas, mat_g, log));
    return rcpp_result_gen;
END_RCPP
}
// c_mtp
IntegerMatrix c_mtp(NumericMatrix p_values, NumericVector alphas, NumericMatrix mat_g);
RcppExport SEXP _statidea_c_mtp(SEXP p_valuesSEXP, SEXP alphasSEXP, SEXP mat_gSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type p_values(p_valuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alphas(alphasSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_g(mat_gSEXP);
    rcpp_result_gen = Rcpp::wrap(c_mtp(p_values, alphas, mat_g));
    return rcpp_result_gen;
END_RCPP
}
// c_mtp_step
int c_mtp_step(NumericMatrix mat_g, NumericVector weights, IntegerVector h_ind, NumericVector p_values, double alpha);
RcppExport SEXP _statidea_c_mtp_step(SEXP mat_gSEXP, SEXP weightsSEXP, SEXP h_indSEXP, SEXP p_valuesSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat_g(mat_gSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type h_ind(h_indSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p_values(p_valuesSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(c_mtp_step(mat_g, weights, h_ind, p_values, alpha));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_stan_fit4hier_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4logn_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4mixture_mod();

static const R_CallMethodDef CallEntries[] = {
    {"_statidea_c_test", (DL_FUNC) &_statidea_c_test, 1},
    {"_statidea_c_two_arm_diff", (DL_FUNC) &_statidea_c_two_arm_diff, 1},
    {"_statidea_c_mtp_single", (DL_FUNC) &_statidea_c_mtp_single, 4},
    {"_statidea_c_mtp", (DL_FUNC) &_statidea_c_mtp, 3},
    {"_statidea_c_mtp_step", (DL_FUNC) &_statidea_c_mtp_step, 5},
    {"_rcpp_module_boot_stan_fit4hier_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4hier_mod, 0},
    {"_rcpp_module_boot_stan_fit4logn_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4logn_mod, 0},
    {"_rcpp_module_boot_stan_fit4mixture_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4mixture_mod, 0},
    {NULL, NULL, 0}
};

void my_package_init(DllInfo *dll);
RcppExport void R_init_statidea(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    my_package_init(dll);
}
