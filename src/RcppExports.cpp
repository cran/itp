// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/itp.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// itp_cpp
List itp_cpp(const SEXP& f, const List& pars, double& a, double& b, double& ya, double& yb, const double& epsilon, const double& k1, const double& k2, double& for_rk, double& inc);
static SEXP _itp_itp_cpp_try(SEXP fSEXP, SEXP parsSEXP, SEXP aSEXP, SEXP bSEXP, SEXP yaSEXP, SEXP ybSEXP, SEXP epsilonSEXP, SEXP k1SEXP, SEXP k2SEXP, SEXP for_rkSEXP, SEXP incSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const SEXP& >::type f(fSEXP);
    Rcpp::traits::input_parameter< const List& >::type pars(parsSEXP);
    Rcpp::traits::input_parameter< double& >::type a(aSEXP);
    Rcpp::traits::input_parameter< double& >::type b(bSEXP);
    Rcpp::traits::input_parameter< double& >::type ya(yaSEXP);
    Rcpp::traits::input_parameter< double& >::type yb(ybSEXP);
    Rcpp::traits::input_parameter< const double& >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< const double& >::type k1(k1SEXP);
    Rcpp::traits::input_parameter< const double& >::type k2(k2SEXP);
    Rcpp::traits::input_parameter< double& >::type for_rk(for_rkSEXP);
    Rcpp::traits::input_parameter< double& >::type inc(incSEXP);
    rcpp_result_gen = Rcpp::wrap(itp_cpp(f, pars, a, b, ya, yb, epsilon, k1, k2, for_rk, inc));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_itp_cpp(SEXP fSEXP, SEXP parsSEXP, SEXP aSEXP, SEXP bSEXP, SEXP yaSEXP, SEXP ybSEXP, SEXP epsilonSEXP, SEXP k1SEXP, SEXP k2SEXP, SEXP for_rkSEXP, SEXP incSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_itp_cpp_try(fSEXP, parsSEXP, aSEXP, bSEXP, yaSEXP, ybSEXP, epsilonSEXP, k1SEXP, k2SEXP, for_rkSEXP, incSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// xptr_eval
double xptr_eval(const double& x, const List& pars, SEXP xpsexp);
static SEXP _itp_xptr_eval_try(SEXP xSEXP, SEXP parsSEXP, SEXP xpsexpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const double& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const List& >::type pars(parsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type xpsexp(xpsexpSEXP);
    rcpp_result_gen = Rcpp::wrap(xptr_eval(x, pars, xpsexp));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_xptr_eval(SEXP xSEXP, SEXP parsSEXP, SEXP xpsexpSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_xptr_eval_try(xSEXP, parsSEXP, xpsexpSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// wiki_cpp
double wiki_cpp(const double& x, const List& pars);
static SEXP _itp_wiki_cpp_try(SEXP xSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const double& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const List& >::type pars(parsSEXP);
    rcpp_result_gen = Rcpp::wrap(wiki_cpp(x, pars));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_wiki_cpp(SEXP xSEXP, SEXP parsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_wiki_cpp_try(xSEXP, parsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// neg_wiki_cpp
double neg_wiki_cpp(const double& x, const List& pars);
static SEXP _itp_neg_wiki_cpp_try(SEXP xSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const double& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const List& >::type pars(parsSEXP);
    rcpp_result_gen = Rcpp::wrap(neg_wiki_cpp(x, pars));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_neg_wiki_cpp(SEXP xSEXP, SEXP parsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_neg_wiki_cpp_try(xSEXP, parsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// lambert_cpp
double lambert_cpp(const double& x, const List& pars);
static SEXP _itp_lambert_cpp_try(SEXP xSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const double& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const List& >::type pars(parsSEXP);
    rcpp_result_gen = Rcpp::wrap(lambert_cpp(x, pars));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_lambert_cpp(SEXP xSEXP, SEXP parsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_lambert_cpp_try(xSEXP, parsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// trig1_cpp
double trig1_cpp(const double& x, const List& pars);
static SEXP _itp_trig1_cpp_try(SEXP xSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const double& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const List& >::type pars(parsSEXP);
    rcpp_result_gen = Rcpp::wrap(trig1_cpp(x, pars));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_trig1_cpp(SEXP xSEXP, SEXP parsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_trig1_cpp_try(xSEXP, parsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// poly3_cpp
double poly3_cpp(const double& x, const List& pars);
static SEXP _itp_poly3_cpp_try(SEXP xSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const double& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const List& >::type pars(parsSEXP);
    rcpp_result_gen = Rcpp::wrap(poly3_cpp(x, pars));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_poly3_cpp(SEXP xSEXP, SEXP parsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_poly3_cpp_try(xSEXP, parsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// linear_cpp
double linear_cpp(const double& x, const List& pars);
static SEXP _itp_linear_cpp_try(SEXP xSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const double& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const List& >::type pars(parsSEXP);
    rcpp_result_gen = Rcpp::wrap(linear_cpp(x, pars));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_linear_cpp(SEXP xSEXP, SEXP parsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_linear_cpp_try(xSEXP, parsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// staircase_cpp
double staircase_cpp(const double& x, const List& pars);
static SEXP _itp_staircase_cpp_try(SEXP xSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const double& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const List& >::type pars(parsSEXP);
    rcpp_result_gen = Rcpp::wrap(staircase_cpp(x, pars));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_staircase_cpp(SEXP xSEXP, SEXP parsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_staircase_cpp_try(xSEXP, parsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// xptr_create
SEXP xptr_create(std::string fstr);
static SEXP _itp_xptr_create_try(SEXP fstrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::string >::type fstr(fstrSEXP);
    rcpp_result_gen = Rcpp::wrap(xptr_create(fstr));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _itp_xptr_create(SEXP fstrSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_itp_xptr_create_try(fstrSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _itp_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("List(*itp_cpp)(const SEXP&,const List&,double&,double&,double&,double&,const double&,const double&,const double&,double&,double&)");
        signatures.insert("double(*xptr_eval)(const double&,const List&,SEXP)");
        signatures.insert("double(*wiki_cpp)(const double&,const List&)");
        signatures.insert("double(*neg_wiki_cpp)(const double&,const List&)");
        signatures.insert("double(*lambert_cpp)(const double&,const List&)");
        signatures.insert("double(*trig1_cpp)(const double&,const List&)");
        signatures.insert("double(*poly3_cpp)(const double&,const List&)");
        signatures.insert("double(*linear_cpp)(const double&,const List&)");
        signatures.insert("double(*staircase_cpp)(const double&,const List&)");
        signatures.insert("SEXP(*xptr_create)(std::string)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _itp_RcppExport_registerCCallable() { 
    R_RegisterCCallable("itp", "_itp_itp_cpp", (DL_FUNC)_itp_itp_cpp_try);
    R_RegisterCCallable("itp", "_itp_xptr_eval", (DL_FUNC)_itp_xptr_eval_try);
    R_RegisterCCallable("itp", "_itp_wiki_cpp", (DL_FUNC)_itp_wiki_cpp_try);
    R_RegisterCCallable("itp", "_itp_neg_wiki_cpp", (DL_FUNC)_itp_neg_wiki_cpp_try);
    R_RegisterCCallable("itp", "_itp_lambert_cpp", (DL_FUNC)_itp_lambert_cpp_try);
    R_RegisterCCallable("itp", "_itp_trig1_cpp", (DL_FUNC)_itp_trig1_cpp_try);
    R_RegisterCCallable("itp", "_itp_poly3_cpp", (DL_FUNC)_itp_poly3_cpp_try);
    R_RegisterCCallable("itp", "_itp_linear_cpp", (DL_FUNC)_itp_linear_cpp_try);
    R_RegisterCCallable("itp", "_itp_staircase_cpp", (DL_FUNC)_itp_staircase_cpp_try);
    R_RegisterCCallable("itp", "_itp_xptr_create", (DL_FUNC)_itp_xptr_create_try);
    R_RegisterCCallable("itp", "_itp_RcppExport_validate", (DL_FUNC)_itp_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_itp_itp_cpp", (DL_FUNC) &_itp_itp_cpp, 11},
    {"_itp_xptr_eval", (DL_FUNC) &_itp_xptr_eval, 3},
    {"_itp_wiki_cpp", (DL_FUNC) &_itp_wiki_cpp, 2},
    {"_itp_neg_wiki_cpp", (DL_FUNC) &_itp_neg_wiki_cpp, 2},
    {"_itp_lambert_cpp", (DL_FUNC) &_itp_lambert_cpp, 2},
    {"_itp_trig1_cpp", (DL_FUNC) &_itp_trig1_cpp, 2},
    {"_itp_poly3_cpp", (DL_FUNC) &_itp_poly3_cpp, 2},
    {"_itp_linear_cpp", (DL_FUNC) &_itp_linear_cpp, 2},
    {"_itp_staircase_cpp", (DL_FUNC) &_itp_staircase_cpp, 2},
    {"_itp_xptr_create", (DL_FUNC) &_itp_xptr_create, 1},
    {"_itp_RcppExport_registerCCallable", (DL_FUNC) &_itp_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_itp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}