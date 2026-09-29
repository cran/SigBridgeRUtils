#include <Rcpp.h>
#include <cstring>

using namespace Rcpp;

namespace {

bool is_c_call(SEXP expr) {
  return TYPEOF(expr) == LANGSXP && CAR(expr) == Rf_install("c");
}

std::string symbol_name(SEXP symbol) {
  return std::string(CHAR(PRINTNAME(symbol)));
}

bool is_supported_rhs(SEXP x) {
  switch (TYPEOF(x)) {
  case VECSXP:
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
    return true;
  default:
    return false;
  }
}

void check_index(SEXP x, R_xlen_t index) {
  if (!is_supported_rhs(x)) {
    stop("Unsupported rhs type");
  }
  if (index < 0 || index >= XLENGTH(x)) {
    stop("Cannot unpack element %d from an object of length %d", index + 1, XLENGTH(x));
  }
}

SEXP get_element(SEXP x, R_xlen_t index) {
  check_index(x, index);

  if (TYPEOF(x) == VECSXP) {
    return VECTOR_ELT(x, index);
  }

  switch (TYPEOF(x)) {
  case LGLSXP:
    return Rf_ScalarLogical(LOGICAL(x)[index]);
  case INTSXP:
    return Rf_ScalarInteger(INTEGER(x)[index]);
  case REALSXP:
    return Rf_ScalarReal(REAL(x)[index]);
  case CPLXSXP:
    return Rf_ScalarComplex(COMPLEX(x)[index]);
  case STRSXP:
    return Rf_ScalarString(STRING_ELT(x, index));
  case RAWSXP:
    return Rf_ScalarRaw(RAW(x)[index]);
  default:
    stop("Unsupported rhs type");
  }
}

SEXP get_named_element(SEXP x, SEXP name) {
  SEXP names = Rf_getAttrib(x, R_NamesSymbol);
  if (TYPEOF(names) != STRSXP) {
    stop("Cannot unpack named element '%s' from an unnamed object",
         CHAR(PRINTNAME(name)));
  }

  for (R_xlen_t i = 0; i < XLENGTH(names); ++i) {
    if (STRING_ELT(names, i) != NA_STRING &&
        std::strcmp(CHAR(STRING_ELT(names, i)), CHAR(PRINTNAME(name))) == 0) {
      return get_element(x, i);
    }
  }

  stop("Named element '%s' was not found", CHAR(PRINTNAME(name)));
}

SEXP collect_elements(SEXP x, R_xlen_t start, R_xlen_t length) {
  if (!is_supported_rhs(x)) {
    stop("Unsupported rhs type");
  }

  const R_xlen_t size = XLENGTH(x);
  if (start < 0 || length < 0 || start > size || length > size - start) {
    stop("Invalid collector range");
  }

  SEXP out = PROTECT(Rf_allocVector(TYPEOF(x), length));

  switch (TYPEOF(x)) {
  case VECSXP:
    for (R_xlen_t i = 0; i < length; ++i) {
      SET_VECTOR_ELT(out, i, VECTOR_ELT(x, start + i));
    }
    break;
  case LGLSXP:
    std::memcpy(LOGICAL(out), LOGICAL(x) + start, length * sizeof(int));
    break;
  case INTSXP:
    std::memcpy(INTEGER(out), INTEGER(x) + start, length * sizeof(int));
    break;
  case REALSXP:
    std::memcpy(REAL(out), REAL(x) + start, length * sizeof(double));
    break;
  case CPLXSXP:
    std::memcpy(COMPLEX(out), COMPLEX(x) + start, length * sizeof(Rcomplex));
    break;
  case STRSXP:
    for (R_xlen_t i = 0; i < length; ++i) {
      SET_STRING_ELT(out, i, STRING_ELT(x, start + i));
    }
    break;
  case RAWSXP:
    std::memcpy(RAW(out), RAW(x) + start, length * sizeof(Rbyte));
    break;
  default:
    UNPROTECT(1);
    stop("Unsupported rhs type");
  }

  SEXP names = Rf_getAttrib(x, R_NamesSymbol);
  if (TYPEOF(names) == STRSXP) {
    SEXP out_names = PROTECT(Rf_allocVector(STRSXP, length));
    for (R_xlen_t i = 0; i < length; ++i) {
      SET_STRING_ELT(out_names, i, STRING_ELT(names, start + i));
    }
    Rf_setAttrib(out, R_NamesSymbol, out_names);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return out;
}

void unpack_target(SEXP target, SEXP value, Environment env);

void assign_symbol(SEXP target, SEXP value, Environment env) {
  std::string name = symbol_name(target);
  if (name == "." || name == "_") {
    return;
  }
  env.assign(name, value);
}

bool is_collector(SEXP argument) {
  if (TYPEOF(argument) != SYMSXP) {
    return false;
  }

  std::string name = symbol_name(argument);
  return name == ".." || (name.size() > 2 && name.rfind("..", 0) == 0);
}

void unpack_call(SEXP target, SEXP value, Environment env) {
  SEXP collector = R_NilValue;
  R_xlen_t trailing_targets = 0;

  for (SEXP node = CDR(target); node != R_NilValue; node = CDR(node)) {
    SEXP argument = CAR(node);
    if (TAG(node) != R_NilValue) {
      continue;
    }

    if (is_collector(argument)) {
      if (collector != R_NilValue) {
        stop("Only one collector is allowed in each c() target");
      }
      collector = node;
      trailing_targets = 0;
    } else if (collector != R_NilValue) {
      ++trailing_targets;
    }
  }

  R_xlen_t position = 0;
  for (SEXP node = CDR(target); node != R_NilValue; node = CDR(node)) {
    SEXP argument = CAR(node);
    SEXP tag = TAG(node);

    if (tag != R_NilValue) {
      if (argument != R_MissingArg) {
        stop("Named unpacking targets must omit their right-hand side");
      }
      Shield<SEXP> selected(get_named_element(value, tag));
      assign_symbol(tag, selected, env);
      continue;
    }

    if (node == collector) {
      if (position + trailing_targets > XLENGTH(value)) {
        stop("Cannot unpack %d trailing elements from an object of length %d",
             trailing_targets, XLENGTH(value));
      }
      const R_xlen_t length = XLENGTH(value) - position - trailing_targets;

      Shield<SEXP> collected(collect_elements(value, position, length));
      std::string name = symbol_name(argument);
      if (name != "..") {
        env.assign(name.substr(2), collected);
      }
      position += length;
      continue;
    }

    Shield<SEXP> selected(get_element(value, position));
    unpack_target(argument, selected, env);
    ++position;
  }
}

void unpack_target(SEXP target, SEXP value, Environment env) {
  if (TYPEOF(target) == SYMSXP) {
    assign_symbol(target, value, env);
    return;
  }
  if (is_c_call(target)) {
    unpack_call(target, value, env);
    return;
  }
  stop("Invalid destructuring target");
}

} // namespace

// [[Rcpp::export]]
void unpack_assign_cpp(SEXP lhs, SEXP rhs, Environment env) {
  if (!is_c_call(lhs)) {
    stop("Invalid destructuring target");
  }
  unpack_call(lhs, rhs, env);
}
