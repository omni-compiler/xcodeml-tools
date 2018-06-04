/**
 * \file F-intrinsics-table.c
 */

#include "F-intrinsics-types.h"
#include "F-front.h"

/*
 * NOTE:
 *
 *      If INTR_KIND(ep) == 1, INTR_RETURN_TYPE_SAME_AS(ep) must be
 *      -1, -3, -6, or -9.
 */

intrinsic_entry intrinsic_table[] = {
  /*
   * FORTRAN77 intrinsic functions
   */

  /* 2. Numeric functions */

  // ABS (A)
  { INTR_ABS,         INTR_NAME_GENERIC,      "abs",          {INTR_TYPE_NUMERICS},                          INTR_TYPE_NUMERICS,     1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ABS,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_COMPLEX},                           INTR_TYPE_REAL,         1, -1, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ABS,         INTR_NAME_SPECIFIC,     "dabs",         {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ABS,         INTR_NAME_SPECIFIC,     "iabs",         {INTR_TYPE_INT},                               INTR_TYPE_INT,          1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ABS,         INTR_NAME_SPECIFIC,     "cabs",         {INTR_TYPE_COMPLEX},                           INTR_TYPE_REAL,         1, -1, LANGSPEC_F77,    INTR_CLASS_E },

  // AIMAG (Z)
  { INTR_AIMAG,       INTR_NAME_SPECIFIC,     "aimag",        {INTR_TYPE_COMPLEX},                           INTR_TYPE_REAL,         1, -5, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_AIMAG,       INTR_NAME_SPECIFIC,     "",             {INTR_TYPE_DCOMPLEX},                          INTR_TYPE_DREAL,        1, -5, LANGSPEC_F77,    INTR_CLASS_E },
																					                                
  // DIMAG (Z)
  { INTR_DIMAG,       INTR_NAME_GENERIC,      "dimag",        {INTR_TYPE_DCOMPLEX},                          INTR_TYPE_DREAL,        1, -5, LANGSPEC_NONSTD, INTR_CLASS_E },
  { INTR_DIMAG,       INTR_NAME_GENERIC,      "",             {INTR_TYPE_COMPLEX},                           INTR_TYPE_REAL,         1, -5, LANGSPEC_NONSTD, INTR_CLASS_E },
																					                                
  // AINT (A  [, KIND])																			                                
  { INTR_AINT,        INTR_NAME_GENERIC,      "aint",         {INTR_TYPE_REAL,  INTR_TYPE_INT},              INTR_TYPE_REAL,         2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,    {"a", "kind"} },
  { INTR_AINT,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL, INTR_TYPE_INT},              INTR_TYPE_DREAL,        2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,    {"a", "kind"} },
  { INTR_AINT,        INTR_NAME_SPECIFIC,     "dint",         {INTR_TYPE_DREAL, INTR_TYPE_INT},              INTR_TYPE_DREAL,        2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,    {"a", "kind"} },
																					                                
  // ANINT (A [, KIND])																			                                
  { INTR_ANINT,       INTR_NAME_GENERIC,      "anint",        {INTR_TYPE_REAL,  INTR_TYPE_INT},              INTR_TYPE_REAL,         2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,    {"a", "kind"} },
  { INTR_ANINT,       INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL, INTR_TYPE_INT},              INTR_TYPE_DREAL,        2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,    {"a", "kind"} },
  { INTR_ANINT,       INTR_NAME_SPECIFIC,     "dnint",        {INTR_TYPE_DREAL, INTR_TYPE_INT},              INTR_TYPE_DREAL,        2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,    {"a", "kind"} },
																					                                
  // CMPLX (X [, Y][, KIND])																		                                
  { INTR_CMPLX,       INTR_NAME_GENERIC,      "cmplx",        {INTR_TYPE_ALL_NUMERICS, INTR_TYPE_NUMERICS,								                                
							       INTR_TYPE_INT},                               INTR_TYPE_COMPLEX,      3, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0, {"x", "y", "kind"} },
  { INTR_DCMPLX,      INTR_NAME_SPECIFIC,     "dcmplx",       {INTR_TYPE_NUMERICS, INTR_TYPE_NUMERICS,									                                
                                                               INTR_TYPE_INT},                               INTR_TYPE_DCOMPLEX,     3, -1, LANGSPEC_NONSTD, INTR_CLASS_E, ARG0, {"x", "y", "kind"} },

  // CONJG (Z)  ???
  { INTR_CONJG,       INTR_NAME_SPECIFIC,     "conjg",        {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // DCONJG (Z) ???
  { INTR_DCONJG,      INTR_NAME_SPECIFIC,     "dconjg",       {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // DBLE (A)
  { INTR_DBLE,        INTR_NAME_GENERIC,      "dble",         {INTR_TYPE_ALL_NUMERICS},                      INTR_TYPE_DREAL,        1, -1, LANGSPEC_F77,    INTR_CLASS_E },

  // DREAL (A)
  { INTR_REAL,        INTR_NAME_GENERIC,      "dreal",        {INTR_TYPE_ALL_NUMERICS},                      INTR_TYPE_DREAL,        1, -1, LANGSPEC_NONSTD, INTR_CLASS_E },

  // DIM (X, Y)
  { INTR_DIM,         INTR_NAME_GENERIC,      "dim",          {INTR_TYPE_NUMERICS, INTR_TYPE_NUMERICS},      INTR_TYPE_NUMERICS,     2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"x", "y"} },
  { INTR_DIM,         INTR_NAME_SPECIFIC,     "idim",         {INTR_TYPE_INT, INTR_TYPE_INT},                INTR_TYPE_INT,          2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"x", "y"} },
  { INTR_DIM,         INTR_NAME_SPECIFIC,     "ddim",         {INTR_TYPE_DREAL, INTR_TYPE_DREAL},            INTR_TYPE_DREAL,        2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"x", "y"} },

  // DPROD (X, Y)
  { INTR_DPROD,       INTR_NAME_GENERIC,      "dprod",        {INTR_TYPE_REAL, INTR_TYPE_REAL},              INTR_TYPE_DREAL,        2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"x", "y"} },
  { INTR_DPROD,       INTR_NAME_SPECIFIC,     "",             {INTR_TYPE_REAL, INTR_TYPE_REAL},              INTR_TYPE_DREAL,        2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"x", "y"} },

  // INT (A [, KIND])
  { INTR_INT,         INTR_NAME_GENERIC,      "int",          {INTR_TYPE_ALL_NUMERICS},                      INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_INT,         INTR_NAME_SPECIFIC_NA,  "",             {INTR_TYPE_REAL},                              INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_INT,         INTR_NAME_SPECIFIC_NA,  "ifix",         {INTR_TYPE_REAL},                              INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_INT,         INTR_NAME_SPECIFIC_NA,  "idint",        {INTR_TYPE_DREAL},                             INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  
  // MAX (A1, A2 [, A3, ...])
  { INTR_MAX,         INTR_NAME_GENERIC,      "max",          {INTR_TYPE_NUMERICS},                          INTR_TYPE_NUMERICS,    -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MAX,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_CHAR},                              INTR_TYPE_CHAR,        -1,  0, LANGSPEC_F2003,  INTR_CLASS_E, ARG0|ARG1 },

  { INTR_MAX,         INTR_NAME_SPECIFIC_NA,  "max0",         {INTR_TYPE_INT},                               INTR_TYPE_INT,         -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MAX,         INTR_NAME_SPECIFIC_NA,  "amax1",        {INTR_TYPE_REAL},                              INTR_TYPE_REAL,        -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MAX,         INTR_NAME_SPECIFIC_NA,  "dmax1",        {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,       -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MAX,         INTR_NAME_SPECIFIC_NA,  "amax0",        {INTR_TYPE_INT},                               INTR_TYPE_REAL,        -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MAX,         INTR_NAME_SPECIFIC_NA,  "max1",         {INTR_TYPE_REAL},                              INTR_TYPE_INT,         -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },

  // MIN (A1, A2 [, A3, ...])
  { INTR_MIN,         INTR_NAME_GENERIC,      "min",          {INTR_TYPE_NUMERICS},                          INTR_TYPE_NUMERICS,    -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MIN,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_CHAR},                              INTR_TYPE_CHAR,        -1,  0, LANGSPEC_F2003,  INTR_CLASS_E, ARG0|ARG1 },

  { INTR_MIN,         INTR_NAME_SPECIFIC_NA,  "min0",         {INTR_TYPE_INT},                               INTR_TYPE_INT,         -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MIN,         INTR_NAME_SPECIFIC_NA,  "amin1",        {INTR_TYPE_REAL},                              INTR_TYPE_REAL,        -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MIN,         INTR_NAME_SPECIFIC_NA,  "dmin1",        {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,       -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MIN,         INTR_NAME_SPECIFIC_NA,  "amin0",        {INTR_TYPE_INT},                               INTR_TYPE_REAL,        -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },
  { INTR_MIN,         INTR_NAME_SPECIFIC_NA,  "min1",         {INTR_TYPE_REAL},                              INTR_TYPE_INT,         -1,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1 },

  // MOD (A, P)
  { INTR_MOD,         INTR_NAME_GENERIC,      "mod",          {INTR_TYPE_NUMERICS, INTR_TYPE_NUMERICS},      INTR_TYPE_NUMERICS,     2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"a", "p"} },
  { INTR_MOD,         INTR_NAME_SPECIFIC,     "amod",         {INTR_TYPE_REAL, INTR_TYPE_REAL},              INTR_TYPE_REAL,         2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"a", "p"} },
  { INTR_MOD,         INTR_NAME_SPECIFIC,     "dmod",         {INTR_TYPE_DREAL, INTR_TYPE_DREAL},            INTR_TYPE_DREAL,        2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"a", "p"} },

  // NINT (A [, KIND])
  { INTR_NINT,        INTR_NAME_GENERIC,      "nint",         {INTR_TYPE_REAL},                              INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_NINT,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_NINT,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_ALL_NUMERICS},                      INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_NINT,        INTR_NAME_SPECIFIC,     "idnint",       {INTR_TYPE_DREAL},                             INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },

  // REAL (A [, KIND])
  { INTR_REAL,        INTR_NAME_GENERIC,      "real",         {INTR_TYPE_ALL_NUMERICS},                      INTR_TYPE_REAL,         2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_REAL,        INTR_NAME_SPECIFIC_NA,  "float",        {INTR_TYPE_INT},                               INTR_TYPE_REAL,         2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_REAL,        INTR_NAME_SPECIFIC_NA,  "dfloat",       {INTR_TYPE_INT},                               INTR_TYPE_DREAL,        2, -1, LANGSPEC_NONSTD, INTR_CLASS_E, ARG0,      {"a", "kind"} },	/* non-standard */
  { INTR_REAL,        INTR_NAME_SPECIFIC_NA,  "sngl",         {INTR_TYPE_DREAL},                             INTR_TYPE_REAL,         2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"a", "kind"} },

  // SIGN (A, B)
  { INTR_SIGN,        INTR_NAME_GENERIC,      "sign",         {INTR_TYPE_NUMERICS, INTR_TYPE_NUMERICS},      INTR_TYPE_NUMERICS,     2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"a", "b"} },
  { INTR_SIGN,        INTR_NAME_SPECIFIC,     "isign",        {INTR_TYPE_INT, INTR_TYPE_INT},                INTR_TYPE_INT,          2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"a", "b"} },
  { INTR_SIGN,        INTR_NAME_SPECIFIC,     "dsign",        {INTR_TYPE_DREAL, INTR_TYPE_DREAL},            INTR_TYPE_DREAL,        2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"a", "b"} },



  /* 3. Mathematical functions */

  // ACOS (X)
  { INTR_ACOS,        INTR_NAME_GENERIC,      "acos",         {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ACOS,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ACOS,        INTR_NAME_SPECIFIC,     "dacos",        {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // ASIN (X)
  { INTR_ASIN,        INTR_NAME_GENERIC,      "asin",         {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ASIN,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ASIN,        INTR_NAME_SPECIFIC,     "dasin",        {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // ATAN (X) or ATAN(Y, X)
  { INTR_ATAN,        INTR_NAME_GENERIC,      "atan",         {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ATAN,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_REAL,  INTR_TYPE_REAL},             INTR_TYPE_REAL,         2,  0, LANGSPEC_F2008,  INTR_CLASS_E, ARG0|ARG1, {"y", "x"} },
  { INTR_ATAN,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ATAN,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL, INTR_TYPE_DREAL},            INTR_TYPE_DREAL,        2,  0, LANGSPEC_F2008,  INTR_CLASS_E, ARG0|ARG1, {"y", "x"} },
  { INTR_ATAN,        INTR_NAME_SPECIFIC,     "datan",        {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_ATAN,        INTR_NAME_SPECIFIC,     "",             {INTR_TYPE_DREAL, INTR_TYPE_DREAL},            INTR_TYPE_DREAL,        2,  0, LANGSPEC_F2008,  INTR_CLASS_E, ARG0|ARG1, {"y", "x"} },

  // ATAN2 (Y, X)
  { INTR_ATAN2,       INTR_NAME_GENERIC,      "atan2",        {INTR_TYPE_REAL,  INTR_TYPE_REAL},             INTR_TYPE_REAL,         2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"y", "x"} },
  { INTR_ATAN2,       INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL, INTR_TYPE_DREAL},            INTR_TYPE_DREAL,        2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"y", "x"} },
  { INTR_ATAN2,       INTR_NAME_SPECIFIC,     "datan2",       {INTR_TYPE_DREAL, INTR_TYPE_DREAL},            INTR_TYPE_DREAL,        2,  0, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"y", "x"} },

  // COS (X)
  { INTR_COS,         INTR_NAME_GENERIC,      "cos",          {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_COS,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_COS,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_COS,         INTR_NAME_SPECIFIC,     "dcos",         {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_COS,         INTR_NAME_SPECIFIC,     "ccos",         {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // COSH (X)
  { INTR_COSH,        INTR_NAME_GENERIC,      "cosh",         {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_COSH,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_COSH,        INTR_NAME_SPECIFIC,     "dcosh",        {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // EXP (X)
  { INTR_EXP,         INTR_NAME_GENERIC,      "exp",          {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_EXP,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_EXP,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_EXP,         INTR_NAME_SPECIFIC,     "dexp",         {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_EXP,         INTR_NAME_SPECIFIC,     "cexp",         {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // LOG (X)
  { INTR_LOG,         INTR_NAME_GENERIC,      "log",          {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_LOG,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_LOG,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_LOG,         INTR_NAME_SPECIFIC,     "alog",         {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_LOG,         INTR_NAME_SPECIFIC,     "dlog",         {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_LOG,         INTR_NAME_SPECIFIC,     "clog",         {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // LOG10 (X)
  { INTR_LOG10,       INTR_NAME_GENERIC,      "log10",        {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_LOG10,       INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_LOG10,       INTR_NAME_SPECIFIC,     "alog10",       {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_LOG10,       INTR_NAME_SPECIFIC,     "dlog10",       {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // SIN (X)
  { INTR_SIN,         INTR_NAME_GENERIC,      "sin",          {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SIN,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SIN,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SIN,         INTR_NAME_SPECIFIC,     "dsin",         {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SIN,         INTR_NAME_SPECIFIC,     "csin",         {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // SINH (X)
  { INTR_SINH,        INTR_NAME_GENERIC,      "sinh",         {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SINH,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SINH,        INTR_NAME_SPECIFIC,     "dsinh",        {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // SQRT (X)
  { INTR_SQRT,        INTR_NAME_GENERIC,      "sqrt",         {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SQRT,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SQRT,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SQRT,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_ALL_NUMERICS},                      INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SQRT,        INTR_NAME_SPECIFIC,     "dsqrt",        {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_SQRT,        INTR_NAME_SPECIFIC,     "csqrt",        {INTR_TYPE_COMPLEX},                           INTR_TYPE_COMPLEX,      1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // TAN (X)
  { INTR_TAN,         INTR_NAME_GENERIC,      "tan",          {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_TAN,         INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_TAN,         INTR_NAME_SPECIFIC,     "dtan",         {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },

  // TANH (X)
  { INTR_TANH,        INTR_NAME_GENERIC,      "tanh",         {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_TANH,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },
  { INTR_TANH,        INTR_NAME_SPECIFIC,     "dtanh",        {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F77,    INTR_CLASS_E },



  /* 4. Character functions */

  // CHAR (I [, KIND])
  { INTR_CHAR,        INTR_NAME_GENERIC,      "char",         {INTR_TYPE_INT},                               INTR_TYPE_CHAR,         2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"i", "kind"} },
  //  { INTR_CHAR,        INTR_NAME_SPECIFIC_NA,  "",             {INTR_TYPE_INT},                               INTR_TYPE_CHAR,         2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"i", "kind"} },

  // ICHAR (C [, KIND])
  { INTR_ICHAR,       INTR_NAME_GENERIC,      "ichar",        {INTR_TYPE_CHAR},                              INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"c", "kind"} },
  //  { INTR_ICHAR,       INTR_NAME_SPECIFIC_NA,  "",             {INTR_TYPE_CHAR},                              INTR_TYPE_INT,          2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0,      {"c", "kind"} },

  // INDEX (STRING, SUBSTRING [, BACK, KIND])
  { INTR_INDEX,       INTR_NAME_GENERIC,      "index",        {INTR_TYPE_CHAR, INTR_TYPE_CHAR,
                                                               INTR_TYPE_LOGICAL, INTR_TYPE_INT},            INTR_TYPE_INT,          4, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string", "substring", "back", "kind"} },
  //  { INTR_INDEX,       INTR_NAME_SPECIFIC,     "",             {INTR_TYPE_CHAR, INTR_TYPE_CHAR,
  //							       INTR_TYPE_LOGICAL, INTR_TYPE_INT},            INTR_TYPE_INT,          4, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string", "substring", "back", "kind"} },

  // LGE (STRING_A, STRING_B)
  { INTR_LGE,         INTR_NAME_GENERIC,      "lge",          {INTR_TYPE_CHAR, INTR_TYPE_CHAR},              INTR_TYPE_LOGICAL,      2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string_a", "string_b"} },
  //  { INTR_LGE,         INTR_NAME_SPECIFIC_NA,  "",             {INTR_TYPE_CHAR, INTR_TYPE_CHAR},              INTR_TYPE_LOGICAL,      2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string_a", "string_b"} },

  // LGT (STRING_A, STRING_B)
  { INTR_LGT,         INTR_NAME_GENERIC,      "lgt",          {INTR_TYPE_CHAR, INTR_TYPE_CHAR},              INTR_TYPE_LOGICAL,      2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string_a", "string_b"} },
  //  { INTR_LGT,         INTR_NAME_SPECIFIC_NA,  "",             {INTR_TYPE_CHAR, INTR_TYPE_CHAR},              INTR_TYPE_LOGICAL,      2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string_a", "string_b"} },

  // LLE (STRING_A, STRING_B)
  { INTR_LLE,         INTR_NAME_GENERIC,      "lle",          {INTR_TYPE_CHAR, INTR_TYPE_CHAR},              INTR_TYPE_LOGICAL,      2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string_a", "string_b"} },
  //  { INTR_LLE,         INTR_NAME_SPECIFIC_NA,  "",             {INTR_TYPE_CHAR, INTR_TYPE_CHAR},              INTR_TYPE_LOGICAL,      2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string_a", "string_b"} },

  // LLT (STRING_A, STRING_B)
  { INTR_LLT,         INTR_NAME_GENERIC,      "llt",          {INTR_TYPE_CHAR, INTR_TYPE_CHAR},              INTR_TYPE_LOGICAL,      2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string_a", "string_b"} },
  //  { INTR_LLT,         INTR_NAME_SPECIFIC_NA,  "",             {INTR_TYPE_CHAR, INTR_TYPE_CHAR},              INTR_TYPE_LOGICAL,      2, -1, LANGSPEC_F77,    INTR_CLASS_E, ARG0|ARG1, {"string_a", "string_b"} },



  /* 5. Character inquiry function */

  // LEN (STRING [, KIND])
  { INTR_LEN,         INTR_NAME_GENERIC,      "len",          {INTR_TYPE_CHAR},                              INTR_TYPE_INT,          2, -6, LANGSPEC_F77,    INTR_CLASS_I, ARG0,      {"string", "kind"} },

  /* 6. Fortran77 non-standard */
  { INTR_LOC,         INTR_NAME_GENERIC,      "loc",          {INTR_TYPE_ANY},                               INTR_TYPE_INT,          1, -9, LANGSPEC_NONSTD, INTR_CLASS_I },


  /*
   * Fortran90 intrinsic
   */

  /* 2. Numeric functions */

  /* Entries of ceiling and floor for DREAL added (by Hitoshi Murai). */

  // CEILING (A  [, KIND])
  { INTR_CEILING,     INTR_NAME_GENERIC,      "ceiling",      {INTR_TYPE_REAL},                              INTR_TYPE_INT,          2, -1, LANGSPEC_F90,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_CEILING,     INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_INT,          2, -1, LANGSPEC_F90,    INTR_CLASS_E, ARG0,      {"a", "kind"} },

  // FLOOR (A  [, KIND])
  { INTR_FLOOR,       INTR_NAME_GENERIC,      "floor",        {INTR_TYPE_REAL},                              INTR_TYPE_INT,          2, -1, LANGSPEC_F90,    INTR_CLASS_E, ARG0,      {"a", "kind"} },
  { INTR_FLOOR,       INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_INT,          2, -1, LANGSPEC_F90,    INTR_CLASS_E, ARG0,      {"a", "kind"} },

  // MODULO (A, P)
  { INTR_MODULO,         INTR_NAME_GENERIC,   "modulo",       {INTR_TYPE_NUMERICS, INTR_TYPE_NUMERICS},      INTR_TYPE_NUMERICS,     2,  0, LANGSPEC_F90,    INTR_CLASS_E, ARG0|ARG1, {"a", "p"} },



  /* 4. Character functions */

  // ACHAR (I [, KIND])
  { INTR_ACHAR,       INTR_NAME_GENERIC,      "achar",        {INTR_TYPE_INT},                               INTR_TYPE_CHAR,         1, -1, LANGSPEC_F2003,  INTR_CLASS_E, ARG0,      {"i", "kind"} },

  // ADJUSTL (STRING)
  { INTR_ADJUSTL,     INTR_NAME_GENERIC,      "adjustl",      {INTR_TYPE_CHAR},                              INTR_TYPE_CHAR,         1,  0, LANGSPEC_F90,    INTR_CLASS_E },

  // ADJUSTR (STRING)
  { INTR_ADJUSTR,     INTR_NAME_GENERIC,      "adjustr",      {INTR_TYPE_CHAR},                              INTR_TYPE_CHAR,         1,  0, LANGSPEC_F90,    INTR_CLASS_E },

  // IACHAR (C)
  { INTR_IACHAR,      INTR_NAME_GENERIC,      "iachar",       {INTR_TYPE_CHAR},                              INTR_TYPE_INT,          1, -1, LANGSPEC_F90,    INTR_CLASS_E },

  // LEN_TRIM (STRING [, KIND])
  { INTR_LEN_TRIM,    INTR_NAME_GENERIC,      "len_trim",     {INTR_TYPE_CHAR},                              INTR_TYPE_INT,          1, -1, LANGSPEC_F90,    INTR_CLASS_E, ARG0,      {"string", "kind"} },

  // REPEAT (STRING, NCOPIES)
  { INTR_REPEAT,  INTR_NAME_GENERIC,          "repeat",       {INTR_TYPE_CHAR, INTR_TYPE_INT},               INTR_TYPE_INT,          2,  0, LANGSPEC_F90,    INTR_CLASS_T, ARG0|ARG1, {"string", "ncopies"} },

  // SCAN (STRING, SET [, BACK, KIND ])
  { INTR_SCAN,        INTR_NAME_GENERIC,      "scan",         {INTR_TYPE_CHAR, INTR_TYPE_CHAR,
							       INTR_TYPE_LOGICAL, INTR_TYPE_INT},            INTR_TYPE_INT,          4, -1, LANGSPEC_F90,    INTR_CLASS_E, ARG0|ARG1, {"string", "set", "back", "kind"} },

  // TRIM (STRING)
  { INTR_TRIM,        INTR_NAME_GENERIC,      "trim",         {INTR_TYPE_CHAR},                              INTR_TYPE_CHAR,         1,  0, LANGSPEC_F90,    INTR_CLASS_E },

  // VERIFY (STRING, SET [, BACK, KIND])
  { INTR_VERIFY,      INTR_NAME_GENERIC,      "verify",       {INTR_TYPE_CHAR, INTR_TYPE_CHAR,
							       INTR_TYPE_LOGICAL, INTR_TYPE_INT},            INTR_TYPE_INT,          3, -1, LANGSPEC_F90,    INTR_CLASS_E, ARG0|ARG1, {"string", "set", "back", "kind"} },


  /* 6. Kind functions */

  // KIND (X)
  { INTR_KIND,        INTR_NAME_GENERIC,      "kind",         {INTR_TYPE_ANY},                               INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },

  // SELECTED_INT_KIND (R)
  { INTR_SELECTED_INT_KIND,
                      INTR_NAME_GENERIC, "selected_int_kind", {INTR_TYPE_INT},                               INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_T },

  // SELECTED_REAL_KIND ([P, R])
  { INTR_SELECTED_REAL_KIND,
                     INTR_NAME_GENERIC, "selected_real_kind", {INTR_TYPE_INT, INTR_TYPE_INT},                INTR_TYPE_INT,          2,  0, LANGSPEC_F90,    INTR_CLASS_T, 0,         {"p", "r"} },


  /* 7. Logical function */

  // LOGICAL (L [, KIND])
  { INTR_LOGICAL,     INTR_NAME_GENERIC,      "logical",      {INTR_TYPE_LOGICAL},                           INTR_TYPE_LOGICAL,      2, -1, LANGSPEC_F90,    INTR_CLASS_E, ARG0,      {"l", "kind"} },



  /* 8. Numeric inquiry functions */

  // DIGITS (X)
  { INTR_DIGITS,      INTR_NAME_GENERIC,      "digits",       {INTR_TYPE_NUMERICS_ARRAY},                    INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_DIGITS,      INTR_NAME_GENERIC,      "",             {INTR_TYPE_NUMERICS},                          INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },

  // EPSILON (X)
  { INTR_EPSILON,     INTR_NAME_GENERIC,      "epsilon",      {INTR_TYPE_REAL_ARRAY},                        INTR_TYPE_REAL,         1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_EPSILON,     INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL_ARRAY},                       INTR_TYPE_DREAL,        1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_EPSILON,     INTR_NAME_GENERIC,      "",             {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_EPSILON,     INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F90,    INTR_CLASS_I },

  // HUGE (X)
  { INTR_HUGE,        INTR_NAME_GENERIC,      "huge",         {INTR_TYPE_INT_ARRAY},                         INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_HUGE,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_REAL_ARRAY},                        INTR_TYPE_REAL,         1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_HUGE,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL_ARRAY},                       INTR_TYPE_DREAL,        1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_HUGE,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_INT},                               INTR_TYPE_INT,          1,  0, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_HUGE,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_HUGE,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F90,    INTR_CLASS_I },

  // MAXEXPONENT (X)
  { INTR_MAXEXPONENT, INTR_NAME_GENERIC,      "maxexponent",  {INTR_TYPE_REAL_ARRAY},                        INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_MAXEXPONENT, INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL_ARRAY},                       INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_MAXEXPONENT, INTR_NAME_GENERIC,      "",             {INTR_TYPE_REAL},                              INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_MAXEXPONENT, INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },

  // MINEXPONENT (X)
  { INTR_MINEXPONENT, INTR_NAME_GENERIC,      "minexponent",  {INTR_TYPE_REAL_ARRAY},                        INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_MINEXPONENT, INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL_ARRAY},                       INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_MINEXPONENT, INTR_NAME_GENERIC,      "",             {INTR_TYPE_REAL},                              INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_MINEXPONENT, INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },

  // PRECISION (X)
  { INTR_PRECISION,   INTR_NAME_GENERIC,      "precision",    {INTR_TYPE_ALL_REAL_ARRAY},                    INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_PRECISION,   INTR_NAME_GENERIC,      "",             {INTR_TYPE_ALL_COMPLEX_ARRAY},                 INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_PRECISION,   INTR_NAME_GENERIC,      "",             {INTR_TYPE_ALL_REAL},                          INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_PRECISION,   INTR_NAME_GENERIC,      "",             {INTR_TYPE_ALL_COMPLEX},                       INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },

  // RADIX (X)
  { INTR_RADIX,       INTR_NAME_GENERIC,      "radix",        {INTR_TYPE_ALL_REAL_ARRAY},                    INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_RADIX,       INTR_NAME_GENERIC,      "",             {INTR_TYPE_INT_ARRAY},                         INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_RADIX,       INTR_NAME_GENERIC,      "",             {INTR_TYPE_ALL_REAL},                          INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_RADIX,       INTR_NAME_GENERIC,      "",             {INTR_TYPE_INT},                               INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },

  // RANGE (X)
  { INTR_RANGE,       INTR_NAME_GENERIC,      "range",        {INTR_TYPE_ALL_NUMERICS_ARRAY},                INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_RANGE,       INTR_NAME_GENERIC,      "",             {INTR_TYPE_ALL_NUMERICS},                      INTR_TYPE_INT,          1, -6, LANGSPEC_F90,    INTR_CLASS_I },

  // TINY (X)
  { INTR_TINY,        INTR_NAME_GENERIC,      "tiny",         {INTR_TYPE_REAL_ARRAY},                        INTR_TYPE_REAL,         1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_TINY,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL_ARRAY},                       INTR_TYPE_DREAL,        1, -6, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_TINY,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_REAL},                              INTR_TYPE_REAL,         1,  0, LANGSPEC_F90,    INTR_CLASS_I },
  { INTR_TINY,        INTR_NAME_GENERIC,      "",             {INTR_TYPE_DREAL},                             INTR_TYPE_DREAL,        1,  0, LANGSPEC_F90,    INTR_CLASS_I },


/* /\*  *\/ */
/* /\*     /\\* 9. Bit inquiry function *\\/ *\/ */

/* /\*     // BGE (I, J) *\/ */
/* /\*     { INTR_BGE,         INTR_NAME_GENERIC,      "bge",          0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_LOGICAL,                      2, -1, LANGSPEC_F2008, INTR_CLASS_E, {"i", "j"} }, *\/ */

/* /\*     // BGT (I, J) *\/ */
/* /\*     { INTR_BGT,         INTR_NAME_GENERIC,      "bgt",          0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_LOGICAL,                      2, -1, LANGSPEC_F2008, INTR_CLASS_E, {"i", "j"} }, *\/ */

/* /\*     // BLE (I, J) *\/ */
/* /\*     { INTR_BLE,         INTR_NAME_GENERIC,      "ble",          0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_LOGICAL,                      2, -1, LANGSPEC_F2008, INTR_CLASS_E, {"i", "j"} }, *\/ */

/* /\*     // BLT (I, J) *\/ */
/* /\*     { INTR_BLT,         INTR_NAME_GENERIC,      "blt",          0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_LOGICAL,                      2, -1, LANGSPEC_F2008, INTR_CLASS_E, {"i", "j"} }, *\/ */

/* /\*     // BIT_SIZE (I) *\/ */
/* /\*     { INTR_BIT_SIZE,    INTR_NAME_GENERIC,      "bit_size",     0,      {INTR_TYPE_INT},                                        INTR_TYPE_INT,                          1,  0, LANGSPEC_F90,  INTR_CLASS_I }, *\/ */


/* /\*  *\/ */
/* /\*     /\\* 10. Bit manipulation functions *\\/ *\/ */

/* /\*     // BTEST (I, POS) *\/ */
/* /\*     { INTR_BTEST,       INTR_NAME_GENERIC,      "btest",        0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_LOGICAL,                      2, -1, LANGSPEC_F90 | LANGSPEC_NONSTD, *\/ */
/* /\*                                                                                                                                                                                                INTR_CLASS_E, {"i", "pos"} }, *\/ */

/* /\*     // DSHIFTL (I, J, SHIFT) *\/ */
/* /\*     { INTR_DSHIFTL,     INTR_NAME_GENERIC,      "dshiftl",      0,      {INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT},          INTR_TYPE_INT,                          3, -9, LANGSPEC_F2008, INTR_CLASS_E, {"i", "j", "shift"} }, *\/ */

/* /\*     // DSHIFTR (I, J, SHIFT) *\/ */
/* /\*     { INTR_DSHIFTR,     INTR_NAME_GENERIC,      "dshiftr",      0,      {INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT},          INTR_TYPE_INT,                          3, -9, LANGSPEC_F2008, INTR_CLASS_E, {"i", "j", "shift"} }, *\/ */

/* /\*     // IALL (ARRAY, DIM [, MASK]) or IALL (ARRAY [, MASK]) *\/ */
/* /\*     { INTR_IALL,        INTR_NAME_GENERIC,      "iall",         0,      {INTR_TYPE_INT_ARRAY, INTR_TYPE_INT},                   INTR_TYPE_INT_ARRAY,                    2,  0, LANGSPEC_F2008, INTR_CLASS_T, {"array", "dim"} }, *\/ */
/* /\*     { INTR_IALL,        INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_INT_ARRAY, INTR_TYPE_INT, *\/ */
/* /\* 									 INTR_TYPE_LOGICAL_ARRAY},                              INTR_TYPE_INT_ARRAY,                    3,  0, LANGSPEC_F2008, INTR_CLASS_T, {"array", "dim", "mask"} }, *\/ */
/* /\*     { INTR_IALL,        INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_INT_ARRAY},                                  INTR_TYPE_INT_ARRAY,                    1,  0, LANGSPEC_F2008, INTR_CLASS_T }, *\/ */
/* /\*     { INTR_IALL,        INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_INT_ARRAY, INTR_TYPE_LOGICAL_ARRAY},         INTR_TYPE_INT_ARRAY,                    2,  0, LANGSPEC_F2008, INTR_CLASS_T, {"array", "mask"} }, *\/ */

/* /\*     // IAND (I, J) *\/ */
/* /\*     { INTR_IAND,        INTR_NAME_GENERIC,      "iand",         0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_INT,                          2,  0, LANGSPEC_F90 | LANGSPEC_NONSTD, *\/ */
/* /\*                                                                                                                                                                                                INTR_CLASS_E, {"i", "j"} }, *\/ */

/* /\*     // IANY (ARRAY, DIM [, MASK]) or IALL (ARRAY [, MASK]) *\/ */
/* /\*     { INTR_IANY,        INTR_NAME_GENERIC,      "iany",         0,      {INTR_TYPE_INT_ARRAY, INTR_TYPE_INT},                   INTR_TYPE_INT_ARRAY,                    2,  0, LANGSPEC_F2008, INTR_CLASS_T, {"array", "dim"} }, *\/ */
/* /\*     { INTR_IANY,        INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_INT_ARRAY, INTR_TYPE_INT, *\/ */
/* /\* 									 INTR_TYPE_LOGICAL_ARRAY},                              INTR_TYPE_INT_ARRAY,                    3,  0, LANGSPEC_F2008, INTR_CLASS_T, {"array", "dim", "mask"} }, *\/ */
/* /\*     { INTR_IANY,        INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_INT_ARRAY},                                  INTR_TYPE_INT_ARRAY,                    1,  0, LANGSPEC_F2008, INTR_CLASS_T }, *\/ */
/* /\*     { INTR_IANY,        INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_INT_ARRAY, INTR_TYPE_LOGICAL_ARRAY},         INTR_TYPE_INT_ARRAY,                    2,  0, LANGSPEC_F2008, INTR_CLASS_T, {"array", "mask"} }, *\/ */

/* /\*     // IBCLR (I, POS) *\/ */
/* /\*     { INTR_IBCLR,       INTR_NAME_GENERIC,      "ibclr",        0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_INT,                          2,  0, LANGSPEC_F90,   INTR_CLASS_E, {"i", "pos"} }, *\/ */

/* /\*     // IBITS (I, POS, LEN) *\/ */
/* /\*     { INTR_IBITS,       INTR_NAME_GENERIC,      "ibits",        0,      {INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT},          INTR_TYPE_INT,                          3,  0, LANGSPEC_F90,   INTR_CLASS_E, {"i", "pos", "len"} }, *\/ */

/* /\*     // IBSET (I, POS) *\/ */
/* /\*     { INTR_IBSET,       INTR_NAME_GENERIC,      "ibset",        0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_INT,                          2,  0, LANGSPEC_F90,   INTR_CLASS_E, {"i", "pos"} }, *\/ */

/* /\*     // IEOR (I, J) *\/ */
/* /\*     { INTR_IEOR,        INTR_NAME_GENERIC,      "ieor",         0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_INT,                          2,  0, LANGSPEC_F90 | LANGSPEC_NONSTD, *\/ */
/* /\*                                                                                                                                                                                                INTR_CLASS_E, {"i", "j"} }, *\/ */
/* /\*     // IOR (I, J) *\/ */
/* /\*     { INTR_IOR,         INTR_NAME_GENERIC,      "ior",          0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_INT,                          2,  0, LANGSPEC_F90,   INTR_CLASS_E, {"i", "j"} }, *\/ */

/* /\*     // ISHFT (I, SHIFT) *\/ */
/* /\*     { INTR_ISHFT,       INTR_NAME_GENERIC,      "ishft",        0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_INT,                          2,  0, LANGSPEC_F90,   INTR_CLASS_E, {"i", "shift"} }, *\/ */

/* /\*     // ISHFTC (I, SHIFT [, SIZE]) *\/ */
/* /\*     { INTR_ISHFTC,      INTR_NAME_GENERIC,      "ishftc",       0,      {INTR_TYPE_INT, INTR_TYPE_INT},                         INTR_TYPE_INT,                          2,  0, LANGSPEC_F90,   INTR_CLASS_E, {"i", "shift"} }, *\/ */
/* /\*     { INTR_ISHFTC,      INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT},          INTR_TYPE_INT,                          3,  0, LANGSPEC_F90,   INTR_CLASS_E, {"i", "shift", "size"} }, *\/ */

/* /\*     // NOT (I) *\/ */
/* /\*     { INTR_NOT,         INTR_NAME_GENERIC,      "not",          0,      {INTR_TYPE_INT},                                        INTR_TYPE_INT,                          1,  0, LANGSPEC_F90,   INTR_CLASS_E }, *\/ */


/* /\*  *\/ */
/* /\*     /\\* 11. Transfer function *\\/ *\/ */

/* /\*     // TRANSFER (SOURCE, MOLD [, SIZE]) *\/ */
/* /\*     { INTR_TRANSFER,    INTR_NAME_GENERIC,      "transfer",     0,      {INTR_TYPE_ANY_ARRAY, INTR_TYPE_ANY_ARRAY},             INTR_TYPE_ANY_ARRAY, 2, 1, LANGSPEC_F90, INTR_CLASS_T }, *\/ */
/* /\*     { INTR_TRANSFER,    INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY_ARRAY, INTR_TYPE_ANY},                   INTR_TYPE_ANY,  2, 1, LANGSPEC_F90, INTR_CLASS_T }, *\/ */
/* /\*     { INTR_TRANSFER,    INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY, INTR_TYPE_ANY_ARRAY},                   INTR_TYPE_ANY_ARRAY,    2, 1, LANGSPEC_F90, INTR_CLASS_T }, *\/ */
/* /\*     { INTR_TRANSFER,    INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY, INTR_TYPE_ANY},                         INTR_TYPE_ANY,  2, 1, LANGSPEC_F90, INTR_CLASS_T }, *\/ */

/* /\*     { INTR_TRANSFER,    INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY_ARRAY, INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT},      INTR_TYPE_ANY_ARRAY, 3, 1, LANGSPEC_F90, INTR_CLASS_T }, *\/ */
/* /\*     { INTR_TRANSFER,    INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY_ARRAY, INTR_TYPE_ANY, INTR_TYPE_INT},    INTR_TYPE_ANY,  3, 1, LANGSPEC_F90, INTR_CLASS_T }, *\/ */
/* /\*     { INTR_TRANSFER,    INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY, INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT},    INTR_TYPE_ANY_ARRAY, 3, 1, LANGSPEC_F90, INTR_CLASS_T }, *\/ */
/* /\*     { INTR_TRANSFER,    INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY, INTR_TYPE_ANY, INTR_TYPE_INT},          INTR_TYPE_ANY,  3, 1, LANGSPEC_F90, INTR_CLASS_T }, *\/ */


/* /\*  *\/ */
/* /\*     /\\* 12. Floating-point manipulation functions *\\/ *\/ */

/* /\*     // EXPONENT (X) *\/ */
/* /\*     { INTR_EXPONENT,    INTR_NAME_GENERIC,      "exponent",     0,      {INTR_TYPE_REAL},              INTR_TYPE_INT,  1, -6, LANGSPEC_F90, INTR_CLASS_E }, *\/ */
/* /\*     { INTR_EXPONENT,    INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_DREAL},             INTR_TYPE_INT,  1, -6, LANGSPEC_F90, INTR_CLASS_E }, *\/ */

/* /\*     // FRACTION (X) *\/ */
/* /\*     { INTR_FRACTION,    INTR_NAME_GENERIC,      "fraction",     0,      {INTR_TYPE_ALL_REAL},          INTR_TYPE_ALL_REAL,  1, 0, LANGSPEC_F90, INTR_CLASS_E }, *\/ */

/* /\*     // NEAREST (X, S) *\/ */
/* /\*     { INTR_NEAREST,     INTR_NAME_GENERIC,      "nearest",      0,      {INTR_TYPE_ALL_REAL, INTR_TYPE_ALL_REAL},        INTR_TYPE_ALL_REAL,         2, 0, LANGSPEC_F90, INTR_CLASS_E }, *\/ */

/* /\*     // RRSPACING (X) *\/ */
/* /\*     { INTR_RRSPACING,   INTR_NAME_GENERIC,      "rrspacing",    0,      {INTR_TYPE_ALL_REAL},          INTR_TYPE_ALL_REAL,  1, 0, LANGSPEC_F90, INTR_CLASS_E }, *\/ */

/* /\*     // SCALE (X, I) *\/ */
/* /\*     { INTR_SCALE,       INTR_NAME_GENERIC,      "scale",        0,      {INTR_TYPE_REAL, INTR_TYPE_INT},        INTR_TYPE_REAL,         2, 0, LANGSPEC_F90, INTR_CLASS_E }, *\/ */
/* /\*     { INTR_SCALE,       INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_DREAL, INTR_TYPE_INT},       INTR_TYPE_DREAL,        2, 0, LANGSPEC_F90, INTR_CLASS_E }, *\/ */

/* /\*     // SET_EXPONENT (X, I) *\/ */
/* /\*     { INTR_SET_EXPONENT, INTR_NAME_GENERIC,     "set_exponent", 0,      {INTR_TYPE_ALL_REAL, INTR_TYPE_INT},        INTR_TYPE_ALL_REAL,         2, 0, LANGSPEC_F90, INTR_CLASS_E }, *\/ */

/* /\*     // SPACING (X) *\/ */
/* /\*     { INTR_SPACING,     INTR_NAME_GENERIC,      "spacing",      0,      {INTR_TYPE_REAL},               INTR_TYPE_REAL,         1, 0, LANGSPEC_F90, INTR_CLASS_E }, *\/ */
/* /\*     { INTR_SPACING,     INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_DREAL},              INTR_TYPE_DREAL,        1, 0, LANGSPEC_F90, INTR_CLASS_E }, *\/ */


/* /\*  *\/ */
/* /\*     /\\* 13. Vector and matrix multiply functions *\\/ *\/ */

/* /\*     // DOT_PRODUCT (VECTOR_A, VECTOR_B) *\/ */
/* /\*     { INTR_DOT_PRODUCT, INTR_NAME_GENERIC,      "dot_product",  0,      {INTR_TYPE_ALL_NUMERICS_ARRAY, *\/ */
/* /\* 									 INTR_TYPE_ALL_NUMERICS_ARRAY},                         INTR_TYPE_ALL_NUMERICS,                 2, -6, LANGSPEC_F90,   INTR_CLASS_T, {"vector_a", "vector_b"} }, *\/ */
/* /\*     { INTR_DOT_PRODUCT, INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_LOGICAL_ARRAY},     INTR_TYPE_LOGICAL,                      2, -6, LANGSPEC_F90,   INTR_CLASS_T, {"vector_a", "vector_b"} }, *\/ */
/* /\* #if 0 *\/ */
/* /\*     { INTR_DOT_PRODUCT, INTR_NAME_GENERIC,      "dot_product",  0,      {INTR_TYPE_INT_ARRAY, INTR_TYPE_INT_ARRAY},             INTR_TYPE_INT_ARRAY,                    2,  0, LANGSPEC_F90,   INTR_CLASS_T, {"vector_a", "vector_b"} }, *\/ */
/* /\*     { INTR_DOT_PRODUCT, INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_REAL_ARRAY, INTR_TYPE_REAL_ARRAY},           INTR_TYPE_REAL_ARRAY,                   2,  0, LANGSPEC_F90,   INTR_CLASS_T, {"vector_a", "vector_b"} }, *\/ */
/* /\*     { INTR_DOT_PRODUCT, INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_DREAL_ARRAY, INTR_TYPE_DREAL_ARRAY},         INTR_TYPE_DREAL_ARRAY,                  2,  0, LANGSPEC_F90,   INTR_CLASS_T, {"vector_a", "vector_b"} }, *\/ */
/* /\*     { INTR_DOT_PRODUCT, INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_COMPLEX_ARRAY, INTR_TYPE_COMPLEX_ARRAY},     INTR_TYPE_COMPLEX_ARRAY,                2,  0, LANGSPEC_F90,   INTR_CLASS_T, {"vector_a", "vector_b"} }, *\/ */
/* /\* #endif *\/ */

/* /\*     // MATMUL (MATRIX_A, MATRIX_B) *\/ */
/* /\*     { INTR_MATMUL,      INTR_NAME_GENERIC,      "matmul",       0,      {INTR_TYPE_ALL_NUMERICS_ARRAY, *\/ */
/* /\* 									 INTR_TYPE_ALL_NUMERICS_ARRAY},                         INTR_TYPE_ALL_NUMERICS_DYNAMIC_ARRAY,   2, -1, LANGSPEC_F90,   INTR_CLASS_T, {"matrix_a", "matrix_b"} }, *\/ */
/* /\*     { INTR_MATMUL,      INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_LOGICAL_ARRAY},     INTR_TYPE_LOGICAL_DYNAMIC_ARRAY,        2, -1, LANGSPEC_F90,   INTR_CLASS_T, {"matrix_a", "matrix_b"} }, *\/ */

/* /\* #if 0 *\/ */
/* /\*     { INTR_MATMUL,      INTR_NAME_GENERIC,      "matmul",       0,      {INTR_TYPE_INT_ARRAY, INTR_TYPE_INT_ARRAY},             INTR_TYPE_INT_ARRAY,                    2,  1, LANGSPEC_F90,   INTR_CLASS_T, {"matrix_a", "matrix_b"} }, *\/ */
/* /\*     { INTR_MATMUL,      INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_REAL_ARRAY, INTR_TYPE_REAL_ARRAY},           INTR_TYPE_REAL_ARRAY,                   2,  1, LANGSPEC_F90,   INTR_CLASS_T, {"matrix_a", "matrix_b"} }, *\/ */
/* /\*     { INTR_MATMUL,      INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_DREAL_ARRAY, INTR_TYPE_DREAL_ARRAY},         INTR_TYPE_REAL_ARRAY,                   2,  1, LANGSPEC_F90,   INTR_CLASS_T, {"matrix_a", "matrix_b"} }, *\/ */
/* /\*     { INTR_MATMUL,      INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_COMPLEX_ARRAY,INTR_TYPE_COMPLEX_ARRAY},      INTR_TYPE_COMPLEX_ARRAY,                2,  1, LANGSPEC_F90,   INTR_CLASS_T, {"matrix_a", "matrix_b"} }, *\/ */
/* /\* #endif *\/ */

/* /\*  *\/ */
/* /\*     /\\* 14. Array reduction functions *\\/ *\/ */

/* /\*     //ALL (MASK [, DIM]) *\/ */
/* /\*     { INTR_ALL,         INTR_NAME_GENERIC,      "all",          0,      { INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_LOGICAL,                      1, -1, LANGSPEC_F90,   INTR_CLASS_T }, *\/ */
/* /\*     { INTR_ALL,         INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },             INTR_TYPE_LOGICAL_DYNAMIC_ARRAY,        2, -1, LANGSPEC_F90,   INTR_CLASS_T, { "mask", "dim" } }, *\/ */

/* /\*     //ANY (MASK [, DIM]) *\/ */
/* /\*     { INTR_ANY,         INTR_NAME_GENERIC,      "any",          0,      { INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_LOGICAL,                      1, -1, LANGSPEC_F90,   INTR_CLASS_T }, *\/ */
/* /\*     { INTR_ANY,         INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },             INTR_TYPE_LOGICAL_DYNAMIC_ARRAY,        2, -1, LANGSPEC_F90,   INTR_CLASS_T, { "mask", "dim" } }, *\/ */

/* /\*     // COUNT (MASK [, DIM]) *\/ */
/* /\*     { INTR_COUNT,       INTR_NAME_GENERIC,      "count",        0,      { INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_INT,                          1, -1, LANGSPEC_F90,   INTR_CLASS_T }, *\/ */
/* /\*     { INTR_COUNT,       INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT},              INTR_TYPE_INT_DYNAMIC_ARRAY,            2, -1, LANGSPEC_F90,   INTR_CLASS_T, { "mask", "dim" } }, *\/ */

/* /\*     // MAXVAL (ARRAY [, MASK]) *\/ */
/* /\*     { INTR_MAXVAL,      INTR_NAME_GENERIC,      "maxval",       0,      { INTR_TYPE_ALL_NUMERICS_ARRAY },                       INTR_TYPE_ALL_NUMERICS,                 1, -2, LANGSPEC_F90,   INTR_CLASS_T }, *\/ */
/* /\*     { INTR_MAXVAL,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_NUMERICS_ARRAY, *\/ */
/* /\* 									  INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_ALL_NUMERICS,                 2, -2, LANGSPEC_F90,   INTR_CLASS_T, { "array", "mask" } }, *\/ */
/* /\*     // MAXVAL (ARRAY, DIM [, MASK]) *\/ */
/* /\*     { INTR_MAXVAL,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_NUMERICS_ARRAY, INTR_TYPE_INT },        INTR_TYPE_ALL_NUMERICS_DYNAMIC_ARRAY,   2, -1, LANGSPEC_F90,   INTR_CLASS_T, { "array", "dim" } }, *\/ */
/* /\*     { INTR_MAXVAL,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_NUMERICS_ARRAY, INTR_TYPE_INT, *\/ */
/* /\* 									  INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_ALL_NUMERICS_DYNAMIC_ARRAY,   3, -1, LANGSPEC_F90,   INTR_CLASS_T, { "array", "dim", "mask" } }, *\/ */

/* /\*     // MINVAL (ARRAY [, MASK]) *\/ */
/* /\*     { INTR_MINVAL,      INTR_NAME_GENERIC,      "minval",       0,      { INTR_TYPE_NUMERICS_ARRAY },                           INTR_TYPE_NUMERICS,                     1, -2, LANGSPEC_F90,   INTR_CLASS_T },  *\/ */
/* /\*     { INTR_MINVAL,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_NUMERICS_ARRAY,  INTR_TYPE_LOGICAL_ARRAY }, INTR_TYPE_NUMERICS,                     2, -2, LANGSPEC_F90,   INTR_CLASS_T, { "array", "mask" } }, *\/ */
/* /\*     { INTR_MINVAL,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY,      INTR_TYPE_LOGICAL_ARRAY }, INTR_TYPE_NUMERICS,                     2, -2, LANGSPEC_F2003, INTR_CLASS_T, { "array", "mask" } }, *\/ */
/* /\*     // MINVAL (ARRAY, DIM [, MASK]) *\/ */
/* /\*     { INTR_MINVAL,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_NUMERICS_ARRAY, INTR_TYPE_INT },            INTR_TYPE_NUMERICS_DYNAMIC_ARRAY,       2, -1, LANGSPEC_F90,   INTR_CLASS_T, { "array", "dim" } }, *\/ */
/* /\*     { INTR_MINVAL,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_NUMERICS_ARRAY, INTR_TYPE_INT, *\/ */
/* /\* 									  INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_NUMERICS_DYNAMIC_ARRAY,       3, -1, LANGSPEC_F90,   INTR_CLASS_T, { "array", "dim", "mask" } }, *\/ */
/* /\*     { INTR_MINVAL,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY,     INTR_TYPE_INT, *\/ */
/* /\* 									  INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_NUMERICS_DYNAMIC_ARRAY,       3, -1, LANGSPEC_F2003, INTR_CLASS_T, { "array", "dim", "mask" } }, *\/ */

/* /\*     // PRODUCT (ARRAY [, MASK]) *\/ */
/* /\*     { INTR_PRODUCT,     INTR_NAME_GENERIC,      "product",      0,      { INTR_TYPE_ALL_NUMERICS_ARRAY },                       INTR_TYPE_ALL_NUMERICS,                 1, -2, LANGSPEC_F90,   INTR_CLASS_T }, *\/ */
/*     { INTR_PRODUCT,     INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_NUMERICS_ARRAY, */
/* 									  INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_ALL_NUMERICS,                 2, -2, LANGSPEC_F90,   INTR_CLASS_T, { "array", "mask" } }, */
/*     // PRODUCT (ARRAY, DIM [, MASK]) */
/*     { INTR_PRODUCT,     INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_NUMERICS_ARRAY, INTR_TYPE_INT },        INTR_TYPE_ALL_NUMERICS_DYNAMIC_ARRAY,   2, -1, LANGSPEC_F90,   INTR_CLASS_T, { "array", "dim" } }, */
/*     { INTR_PRODUCT,     INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_NUMERICS_ARRAY, INTR_TYPE_INT, */
/* 									  INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_ALL_NUMERICS_DYNAMIC_ARRAY,   3, -1, LANGSPEC_F90,   INTR_CLASS_T, { "array", "dim", "mask" } }, */

/*     // SUM (ARRAY [, MASK]) */
/*     { INTR_SUM,         INTR_NAME_GENERIC,      "sum",          0,      { INTR_TYPE_ALL_NUMERICS_ARRAY },                       INTR_TYPE_ALL_NUMERICS,                 1, -2, LANGSPEC_F90,   INTR_CLASS_T }, */
/*     { INTR_SUM,         INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_NUMERICS_ARRAY, */
/* 									  INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_ALL_NUMERICS,                 2, -2, LANGSPEC_F90,   INTR_CLASS_T, { "array", "mask" } }, */
/*     // SUM (ARRAY, DIM [, MASK]) */
/*     { INTR_SUM,         INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_NUMERICS_ARRAY, INTR_TYPE_INT },        INTR_TYPE_ALL_NUMERICS_DYNAMIC_ARRAY,   2, -1, LANGSPEC_F90,   INTR_CLASS_T, { "array", "dim" } }, */
/*     { INTR_SUM,         INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_NUMERICS_ARRAY, INTR_TYPE_INT, */
/* 									  INTR_TYPE_LOGICAL_ARRAY },                            INTR_TYPE_ALL_NUMERICS_DYNAMIC_ARRAY,   3, -1, LANGSPEC_F90,   INTR_CLASS_T, { "array", "dim", "mask" } }, */

/*  */
/*     /\\* 15. Array inquiry functions *\\/ */

/*     // ALLOCATED (ARRAY) */
/*     { INTR_ALLOCATED,   INTR_NAME_GENERIC,      "allocated",    0,      { INTR_TYPE_ANY_ARRAY_ALLOCATABLE },                    INTR_TYPE_LOGICAL,      1, -6, LANGSPEC_F90, INTR_CLASS_I }, */

/*     // ALLOCATED (SCALAR) for F2003 */
/*     { INTR_ALLOCATED,   INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY },                                      INTR_TYPE_LOGICAL,      1, -6, LANGSPEC_F2003, INTR_CLASS_I }, */

/*     // LBOUND (ARRAY [, DIM, KIND]) */
/*     { INTR_LBOUND,      INTR_NAME_GENERIC,      "lbound",       1,      { INTR_TYPE_ANY_ARRAY },                                INTR_TYPE_INT_ARRAY,                    1, -3, LANGSPEC_F90,   INTR_CLASS_I }, */
/*     { INTR_LBOUND,      INTR_NAME_GENERIC,      "",             1,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT },                 INTR_TYPE_INT,                          2, -1, LANGSPEC_F90,   INTR_CLASS_I, {"array", "dim"} }, */

/*     // SHAPE (SOURCE) */
/*     { INTR_SHAPE,       INTR_NAME_GENERIC,      "shape",        0,      { INTR_TYPE_ANY_ARRAY },                                INTR_TYPE_INT_ARRAY,    1, -3, LANGSPEC_F90, INTR_CLASS_I }, */

/*     // SIZE (ARRAY [, DIM]) */
/*     { INTR_SIZE,        INTR_NAME_GENERIC,      "size",         0,      { INTR_TYPE_ANY_ARRAY },                                INTR_TYPE_INT,  1, -1, LANGSPEC_F90, INTR_CLASS_I }, */
/*     { INTR_SIZE,        INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT },                 INTR_TYPE_INT,  2, -1, LANGSPEC_F90, INTR_CLASS_I }, */

/*     // UBOUND (ARRAY [, DIM, KIND]) */
/*     { INTR_UBOUND,      INTR_NAME_GENERIC,      "ubound",       1,      { INTR_TYPE_ANY_ARRAY },                                INTR_TYPE_INT_ARRAY,                    1, -3, LANGSPEC_F90,   INTR_CLASS_I }, */
/*     { INTR_UBOUND,      INTR_NAME_GENERIC,      "",             1,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT },                 INTR_TYPE_INT,                          2, -1, LANGSPEC_F90,   INTR_CLASS_I, {"array", "dim"} }, */


/*  */
/*     /\\* 16. Array construction functions *\\/ */

/*     // MERGE (TSOURCE, FSOURCE, MASK) */
/*     { INTR_MERGE,       INTR_NAME_GENERIC,      "merge",        0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_ANY_ARRAY, INTR_TYPE_LOGICAL },        INTR_TYPE_ANY_ARRAY,    3, 0, LANGSPEC_F90, INTR_CLASS_E }, */
/*     { INTR_MERGE,       INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY, INTR_TYPE_ANY, INTR_TYPE_LOGICAL },                    INTR_TYPE_ANY,          3, 0, LANGSPEC_F90, INTR_CLASS_E }, */

/*     // PACK (ARRAY, MASK [, VECTOR]) */
/*     //    { INTR_PACK,        INTR_NAME_GENERIC,      "pack",         0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_LOGICAL_ARRAY, },              INTR_TYPE_ANY_DYNAMIC_ARRAY,            2, -1, LANGSPEC_F90, INTR_CLASS_T }, */
/*     //    { INTR_PACK,        INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_ANY_ARRAY},              INTR_TYPE_ANY_DYNAMIC_ARRAY,            3, -1, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_PACK,        INTR_NAME_GENERIC,      "pack",         0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_LOGICAL, },              INTR_TYPE_ANY_DYNAMIC_ARRAY,            2, -1, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_PACK,        INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_LOGICAL, INTR_TYPE_ANY_ARRAY},              INTR_TYPE_ANY_DYNAMIC_ARRAY,            3, -1, LANGSPEC_F90, INTR_CLASS_T }, */

/*     // SPREAD (SOURCE, DIM, NCOPIES) */
/*     { INTR_SPREAD,      INTR_NAME_GENERIC,      "spread",       0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT, INTR_TYPE_INT },          INTR_TYPE_ANY_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_SPREAD,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT },                INTR_TYPE_ANY_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F90, INTR_CLASS_T }, */

/*     // UNPACK (VECTOR, MASK, FIELD) */
/*     { INTR_UNPACK,      INTR_NAME_GENERIC,      "unpack",       0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_ANY_ARRAY},              INTR_TYPE_ANY_DYNAMIC_ARRAY,            3, -1, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_UNPACK,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_ANY},              INTR_TYPE_ANY_DYNAMIC_ARRAY,            3, -1, LANGSPEC_F90, INTR_CLASS_T }, */


/*  */
/*     /\\* 17. Array reshape function *\\/ */

/*     // RESHAPE (SOURCE, SHAPE [, PAD, ORDER]) */
/*     { INTR_RESHAPE,     INTR_NAME_GENERIC,      "reshape",      0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY },   INTR_TYPE_ANY_DYNAMIC_ARRAY,    2, -1, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_RESHAPE,     INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY, INTR_TYPE_INT_ARRAY },      INTR_TYPE_ANY_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_RESHAPE,     INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY, INTR_TYPE_ANY_ARRAY },      INTR_TYPE_ANY_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_RESHAPE,     INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY, INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY }, INTR_TYPE_ANY_DYNAMIC_ARRAY,    4, -1, LANGSPEC_F90, INTR_CLASS_T }, */


/*  */
/*     /\\* 18. Array manipulation functions *\\/ */

/*     // CSHIFT (ARRAY, SHIFT [, DIM]) */
/*     { INTR_CSHIFT,      INTR_NAME_GENERIC,      "cshift",       0,      {INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY},             INTR_TYPE_ANY_ARRAY,                    2, 0, LANGSPEC_F90, INTR_CLASS_T, {"array", "shift"} }, */
/*     { INTR_CSHIFT,      INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT},                   INTR_TYPE_ANY_ARRAY,                    2, 0, LANGSPEC_F90, INTR_CLASS_T, {"array", "shift"} }, */
/*     { INTR_CSHIFT,      INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY, */
/* 									 INTR_TYPE_INT},                                        INTR_TYPE_ANY_ARRAY,                    3, 0, LANGSPEC_F90, INTR_CLASS_T, {"array", "shift", "dim"} }, */
/*     { INTR_CSHIFT,      INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT, INTR_TYPE_INT},    INTR_TYPE_ANY_ARRAY,                    3, 0, LANGSPEC_F90, INTR_CLASS_T, {"array", "shift", "dim"} }, */

/*     // TRANSPOSE (MATRIX) */
/*     { INTR_TRANSPOSE,   INTR_NAME_GENERIC,      "transpose",    0,      { INTR_TYPE_ANY_ARRAY },                                INTR_TYPE_ANY_ARRAY,    1, -4, LANGSPEC_F90, INTR_CLASS_T }, */


/*  */
/*     /\\* 19. Array location functions *\\/ */

/*     // MINLOC (ARRAY [, MASK, KIND, BACK]) */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "minloc",       0,      { INTR_TYPE_INT_ARRAY },                                                                  INTR_TYPE_INT_ARRAY,    1, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_LOGICAL_ARRAY },                                         INTR_TYPE_INT_ARRAY,    2, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },                          INTR_TYPE_INT_ARRAY,    3, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },       INTR_TYPE_INT_ARRAY,    4, -3, LANGSPEC_F90, INTR_CLASS_T }, */

/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY },                                                             INTR_TYPE_INT_ARRAY,    1, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_LOGICAL_ARRAY },                                    INTR_TYPE_INT_ARRAY,    2, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },                     INTR_TYPE_INT_ARRAY,    3, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },  INTR_TYPE_INT_ARRAY,    4, -3, LANGSPEC_F90, INTR_CLASS_T }, */

/*     //// F2003 accepts CHARACTER ARRAY */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY },                                                                 INTR_TYPE_INT_ARRAY,    1, -3, LANGSPEC_F2003, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_LOGICAL_ARRAY },                                        INTR_TYPE_INT_ARRAY,    2, -3, LANGSPEC_F2003, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },                         INTR_TYPE_INT_ARRAY,    3, -3, LANGSPEC_F2003, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },      INTR_TYPE_INT_ARRAY,    4, -3, LANGSPEC_F2003, INTR_CLASS_T }, */

/*     // MINLOC (ARRAY, DIM [, MASK, KIND, BACK]) (Fortran 95) */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_INT },                                                   INTR_TYPE_INT_DYNAMIC_ARRAY,    2, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY },                          INTR_TYPE_INT_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },           INTR_TYPE_INT_DYNAMIC_ARRAY,    4, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },   INTR_TYPE_INT_DYNAMIC_ARRAY,    5, -1, LANGSPEC_F95, INTR_CLASS_T }, */

/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_INT },                                              INTR_TYPE_INT_DYNAMIC_ARRAY,    2, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY },                     INTR_TYPE_INT_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },      INTR_TYPE_INT_DYNAMIC_ARRAY,    4, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL }, INTR_TYPE_INT_DYNAMIC_ARRAY,    5, -1, LANGSPEC_F95, INTR_CLASS_T }, */

/*     //// F2003 accepts CHARACTER ARRAY */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_INT },                                                  INTR_TYPE_INT_DYNAMIC_ARRAY,    2, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY },                         INTR_TYPE_INT_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },          INTR_TYPE_INT_DYNAMIC_ARRAY,    4, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MINLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL }, INTR_TYPE_INT_DYNAMIC_ARRAY,    5, -1, LANGSPEC_F95, INTR_CLASS_T }, */

/*     // MAXLOC (ARRAY [, MASK, KIND, BACK]) */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "maxloc",       0,      { INTR_TYPE_INT_ARRAY },                                                                  INTR_TYPE_INT_ARRAY,    1, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_LOGICAL_ARRAY },                                         INTR_TYPE_INT_ARRAY,    2, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },                          INTR_TYPE_INT_ARRAY,    3, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },       INTR_TYPE_INT_ARRAY,    4, -3, LANGSPEC_F90, INTR_CLASS_T }, */

/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY },                                                             INTR_TYPE_INT_ARRAY,    1, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_LOGICAL_ARRAY },                                    INTR_TYPE_INT_ARRAY,    2, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },                     INTR_TYPE_INT_ARRAY,    3, -3, LANGSPEC_F90, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },  INTR_TYPE_INT_ARRAY,    4, -3, LANGSPEC_F90, INTR_CLASS_T }, */

/*     //// F2003 accepts CHARACTER ARRAY */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY },                                                                 INTR_TYPE_INT_ARRAY,    1, -3, LANGSPEC_F2003, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_LOGICAL_ARRAY },                                        INTR_TYPE_INT_ARRAY,    2, -3, LANGSPEC_F2003, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },                         INTR_TYPE_INT_ARRAY,    3, -3, LANGSPEC_F2003, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },      INTR_TYPE_INT_ARRAY,    4, -3, LANGSPEC_F2003, INTR_CLASS_T }, */

/*     // MAXLOC (ARRAY, DIM, [MASK, KIND, BACK]) (Fortran 95) */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_INT },                                                   INTR_TYPE_INT_DYNAMIC_ARRAY,    2, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY },                          INTR_TYPE_INT_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },           INTR_TYPE_INT_DYNAMIC_ARRAY,    4, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_INT_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },       INTR_TYPE_INT_DYNAMIC_ARRAY,    5, -1, LANGSPEC_F95, INTR_CLASS_T }, */

/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_INT },                                              INTR_TYPE_INT_DYNAMIC_ARRAY,    2, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY },                     INTR_TYPE_INT_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },      INTR_TYPE_INT_DYNAMIC_ARRAY,    4, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_ALL_REAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },  INTR_TYPE_INT_DYNAMIC_ARRAY,    5, -1, LANGSPEC_F95, INTR_CLASS_T }, */


/*     //// F2003 accepts CHARACTER ARRAY */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_INT },                                                  INTR_TYPE_INT_DYNAMIC_ARRAY,    2, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY },                         INTR_TYPE_INT_DYNAMIC_ARRAY,    3, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT },          INTR_TYPE_INT_DYNAMIC_ARRAY,    4, -1, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_MAXLOC,      INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_CHAR_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL_ARRAY, INTR_TYPE_INT, INTR_TYPE_LOGICAL },       INTR_TYPE_INT_DYNAMIC_ARRAY,    5, -1, LANGSPEC_F95, INTR_CLASS_T }, */

/*  */
/*     /\\* 20. Pointer association status functions *\\/ */

/*     // ASSOCIATED (POINTER [, TARGET]) */
/*     { INTR_ASSOCIATED,  INTR_NAME_GENERIC,      "associated",   0,      { INTR_TYPE_POINTER },                                  INTR_TYPE_LOGICAL,                      1, -6, LANGSPEC_F90,   INTR_CLASS_I }, */
/*     { INTR_ASSOCIATED,  INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_POINTER, INTR_TYPE_PASSIGNABLE },           INTR_TYPE_LOGICAL,                      2, -6, LANGSPEC_F90,   INTR_CLASS_I, { "pointer", "target" } }, */

/*  */
/*     /\\* 21. Intrinsic subroutines *\\/ */

/*     // DATE_AND_TIME ([DATE, TIME, ZONE, VALUES]) */
/*     { INTR_DATE_AND_TIME, INTR_NAME_GENERIC,    "date_and_time",0,      {INTR_TYPE_NONE},                                       INTR_TYPE_NONE,                         0, -1, LANGSPEC_F90,   INTR_CLASS_S }, */
/*     { INTR_DATE_AND_TIME, INTR_NAME_GENERIC,    "",             0,      {INTR_TYPE_CHAR},                                       INTR_TYPE_NONE,                         1, -1, LANGSPEC_F90,   INTR_CLASS_S }, */
/*     { INTR_DATE_AND_TIME, INTR_NAME_GENERIC,    "",             0,      {INTR_TYPE_INT_ARRAY},                                  INTR_TYPE_NONE,                         1, -1, LANGSPEC_F90,   INTR_CLASS_S }, */
/*     { INTR_DATE_AND_TIME, INTR_NAME_GENERIC,    "",             0,      {INTR_TYPE_CHAR, INTR_TYPE_CHAR},                       INTR_TYPE_NONE,                         2, -1, LANGSPEC_F90,   INTR_CLASS_S, {"date", "time", "zone"} }, */
/*     { INTR_DATE_AND_TIME, INTR_NAME_GENERIC,    "",             0,      {INTR_TYPE_CHAR, INTR_TYPE_CHAR, INTR_TYPE_CHAR},       INTR_TYPE_NONE,                         3, -1, LANGSPEC_F90,   INTR_CLASS_S, {"date", "time", "zone"} }, */
/*     { INTR_DATE_AND_TIME, INTR_NAME_GENERIC,    "",             0,      {INTR_TYPE_CHAR, INTR_TYPE_CHAR, INTR_TYPE_CHAR, */
/* 									 INTR_TYPE_INT_ARRAY},                                  INTR_TYPE_NONE,                         4, -1, LANGSPEC_F90,   INTR_CLASS_S, {"date", "time", "zone", "values"} }, */

/*     // MVBITS (FROM, FROMPOS, LEN, TO, TOPOS) */
/*     { INTR_MVBITS,              INTR_NAME_GENERIC,      "mvbits",               0,      { INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT },        INTR_TYPE_NONE, 5, -1, LANGSPEC_F90, INTR_CLASS_ES }, */

/*     // RANDOM_NUMBER (HARVEST) */
/*     { INTR_RANDOM_NUMBER,      INTR_NAME_GENERIC,      "",                     0,      { INTR_TYPE_ALL_REAL_ARRAY },                           INTR_TYPE_NONE, 1, -1, LANGSPEC_F90, INTR_CLASS_S }, */
/*     { INTR_RANDOM_NUMBER,      INTR_NAME_GENERIC,      "random_number",        0,      { INTR_TYPE_ALL_REAL },                                 INTR_TYPE_NONE, 1, -1, LANGSPEC_F90, INTR_CLASS_S }, */

/*     // RANDOM_SEED ([SIZE, PUT, GET]) */
/*     { INTR_RANDOM_SEED,         INTR_NAME_GENERIC,      "random_seed",          0,      { INTR_TYPE_NONE },                                     INTR_TYPE_NONE, 0, -1, LANGSPEC_F90, INTR_CLASS_S }, */
/*     { INTR_RANDOM_SEED,         INTR_NAME_GENERIC,      "",                     0,      { INTR_TYPE_INT },                                      INTR_TYPE_NONE, 1, -1, LANGSPEC_F90, INTR_CLASS_S }, */
/*     { INTR_RANDOM_SEED,         INTR_NAME_GENERIC,      "",                     0,      { INTR_TYPE_INT, INTR_TYPE_INT_ARRAY },                  INTR_TYPE_NONE, 2, -1, LANGSPEC_F90, INTR_CLASS_S }, */
/*     { INTR_RANDOM_SEED,         INTR_NAME_GENERIC,      "",                     0,      { INTR_TYPE_INT, INTR_TYPE_INT_ARRAY, INTR_TYPE_INT_ARRAY },     INTR_TYPE_NONE, 3, -1, LANGSPEC_F90, INTR_CLASS_S }, */


/*  */
/*     // SYSTEM_CLOCK ([COUNT, COUNT_RATE, COUNT_MAX]) */
/*     { INTR_SYSTEM_CLOCK,        INTR_NAME_GENERIC,      "system_clock",         0,      { INTR_TYPE_NONE },                             INTR_TYPE_NONE, 0, -6, LANGSPEC_F90, INTR_CLASS_S }, */
/*     { INTR_SYSTEM_CLOCK,        INTR_NAME_GENERIC,      "",                     0,      { INTR_TYPE_INT },                              INTR_TYPE_NONE, 1, -6, LANGSPEC_F90, INTR_CLASS_S }, */
/*     { INTR_SYSTEM_CLOCK,        INTR_NAME_GENERIC,      "",                     0,      { INTR_TYPE_INT, INTR_TYPE_INT },               INTR_TYPE_NONE, 2, -6, LANGSPEC_F90, INTR_CLASS_S }, */
/*     { INTR_SYSTEM_CLOCK,        INTR_NAME_GENERIC,      "",                     0,      { INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT },        INTR_TYPE_NONE, 3, -6, LANGSPEC_F90, INTR_CLASS_S }, */
/*     { INTR_SYSTEM_CLOCK,        INTR_NAME_GENERIC,      "",                     0,      { INTR_TYPE_INT, INTR_TYPE_REAL },              INTR_TYPE_NONE, 2, -6, LANGSPEC_F2003, INTR_CLASS_S }, */
/*     { INTR_SYSTEM_CLOCK,        INTR_NAME_GENERIC,      "",                     0,      { INTR_TYPE_INT, INTR_TYPE_REAL, INTR_TYPE_INT },       INTR_TYPE_NONE, 3, -6, LANGSPEC_F2003, INTR_CLASS_S }, */

/*     /\\* */
/*      * Fortran90 intrinsics */
/*      *\\/ */

/*     /\\* 1. Argument presence inquiry function *\\/ */

/*     // PRESENT (A) */

/*     { INTR_PRESENT,    INTR_NAME_GENERIC,      "present",       0,      { INTR_TYPE_ANY_OPTIONAL },                            INTR_TYPE_LOGICAL, 1, -6, LANGSPEC_F95, INTR_CLASS_I }, */

/*     /\\* 18. Array manipulation functions *\\/ */

/*     // EOSHIFT (ARRAY, SHIFT [, BOUNDARY, DIM]) */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "eoshift",      0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY },           INTR_TYPE_ANY_ARRAY,                    2,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift"} }, */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT },                 INTR_TYPE_ANY_ARRAY,                    2,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift"} }, */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY, */
/* 									  INTR_TYPE_ANY_ARRAY },                                INTR_TYPE_ANY_ARRAY,                    3,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift", "boundary"} }, */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY, */
/* 									  INTR_TYPE_ANY },                                      INTR_TYPE_ANY_ARRAY,                    3,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift", "boundary"} }, */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT, */
/* 									  INTR_TYPE_ANY_ARRAY },                                INTR_TYPE_ANY_ARRAY,                    3,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift", "boundary"} }, */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT, INTR_TYPE_ANY },  INTR_TYPE_ANY_ARRAY,                    3,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift", "boundary"} }, */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY, */
/* 									  INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT },                 INTR_TYPE_ANY_ARRAY,                    4,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift", "boundary", "dim"} }, */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT_ARRAY, */
/* 									  INTR_TYPE_ANY, INTR_TYPE_INT },                       INTR_TYPE_ANY_ARRAY,                    4,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift", "boundary", "dim"} }, */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT, */
/* 									  INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT },                 INTR_TYPE_ANY_ARRAY,                    4,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift", "boundary", "dim"} }, */
/*     { INTR_EOSHIFT,    INTR_NAME_GENERIC,       "",             0,      { INTR_TYPE_ANY_ARRAY, INTR_TYPE_INT, INTR_TYPE_ANY, */
/* 									  INTR_TYPE_INT },                                      INTR_TYPE_ANY_ARRAY,                    4,  0, LANGSPEC_F95,   INTR_CLASS_T, {"array", "shift", "boundary", "dim"} }, */

/*     /\\* */
/*      * Fortran95 intrinsic */
/*      *\\/ */

/*     /\\* 20. Pointer association status functions *\\/ */

/*     // NULL ([MOLD]) */
/*     { INTR_NULL,       INTR_NAME_GENERIC,      "null",          0,      { INTR_TYPE_NONE },                     INTR_TYPE_LHS, 0, -7, LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_NULL,       INTR_NAME_GENERIC,      "",              0,      { INTR_TYPE_ANY_ARRAY_ALLOCATABLE },    INTR_TYPE_TARGET, 1, 0,  LANGSPEC_F95, INTR_CLASS_T }, */
/*     { INTR_NULL,       INTR_NAME_GENERIC,      "",              0,      { INTR_TYPE_POINTER },                  INTR_TYPE_TARGET, 1, 0,  LANGSPEC_F95, INTR_CLASS_T }, */

/*     /\\* 21. Intrinsic subroutines *\\/ */

/*     // CPU_TIME (TIME) */
/*     { INTR_CPU_TIME,    INTR_NAME_GENERIC,      "cpu_time",     0,      { INTR_TYPE_REAL },                                    INTR_TYPE_NONE, 1, -6, LANGSPEC_F95, INTR_CLASS_S }, */
/*     { INTR_CPU_TIME,    INTR_NAME_GENERIC,      "",             0,      { INTR_TYPE_DREAL },                                   INTR_TYPE_NONE, 1, -6, LANGSPEC_F95, INTR_CLASS_S }, */


/*     /\\* */
/*      * CAF1.0 (subset of Fortran2008) intrinic functions */
/*      *\\/ */

/*     { INTR_NUM_IMAGES,    INTR_NAME_GENERIC,   "num_images",    0,      {INTR_TYPE_NONE},                                       INTR_TYPE_INT,                          0, -6, LANGSPEC_F2008, INTR_CLASS_T }, */

/*     { INTR_THIS_IMAGE,    INTR_NAME_GENERIC,   "this_image",    0,      {INTR_TYPE_COARRAY_ANY},                                INTR_TYPE_INT,                          0, -6, LANGSPEC_F2008, INTR_CLASS_T }, */
/*     { INTR_THIS_IMAGE,    INTR_NAME_GENERIC,   "",              0,      {INTR_TYPE_COARRAY_ANY},                                INTR_TYPE_INT_DYNAMIC_ARRAY,            1, -1, LANGSPEC_F2008, INTR_CLASS_T }, */
/*     { INTR_THIS_IMAGE,    INTR_NAME_GENERIC,   "",              0,      {INTR_TYPE_COARRAY_ANY, INTR_TYPE_INT},                 INTR_TYPE_INT,                          2, -6, LANGSPEC_F2008, INTR_CLASS_T, {"coarray", "dim"} }, */

/*     { INTR_IMAGE_INDEX,   INTR_NAME_GENERIC,   "image_index",   0,      {INTR_TYPE_COARRAY_ANY, INTR_TYPE_INT_ARRAY},           INTR_TYPE_INT,                          2, -6, LANGSPEC_F2008, INTR_CLASS_T, {"coarray", "sub"} }, */

/*     { INTR_LCOBOUND,      INTR_NAME_GENERIC,   "lcobound",      1,      {INTR_TYPE_COARRAY_ANY},                                INTR_TYPE_INT_ARRAY,                    1, -9, LANGSPEC_F2008, INTR_CLASS_I }, */
/*     { INTR_LCOBOUND,      INTR_NAME_GENERIC,   "",              1,      {INTR_TYPE_COARRAY_ANY, INTR_TYPE_INT},                 INTR_TYPE_INT,                          2, -6, LANGSPEC_F2008, INTR_CLASS_I, {"coarray", "dim"} }, */
/*     //    { INTR_LCOBOUND,      INTR_NAME_GENERIC,   "",              0,      {INTR_TYPE_COARRAY_ANY, INTR_TYPE_INT, INTR_TYPE_INT},  INTR_TYPE_INT,                          3, -6, LANGSPEC_F2008, INTR_CLASS_I }, */

/*     { INTR_UCOBOUND,      INTR_NAME_GENERIC,   "ucobound",      1,      {INTR_TYPE_COARRAY_ANY},                                INTR_TYPE_INT_ARRAY,                    1, -9, LANGSPEC_F2008, INTR_CLASS_I }, */
/*     { INTR_UCOBOUND,      INTR_NAME_GENERIC,   "",              1,      {INTR_TYPE_COARRAY_ANY, INTR_TYPE_INT},                 INTR_TYPE_INT,                          2, -6, LANGSPEC_F2008, INTR_CLASS_I, {"coarray", "dim"} }, */
/*     //    { INTR_UCOBOUND,      INTR_NAME_GENERIC,   "",              0,      {INTR_TYPE_COARRAY_ANY, INTR_TYPE_INT, INTR_TYPE_INT},  INTR_TYPE_INT,                          3, -6, LANGSPEC_F2008, INTR_CLASS_I }, */

/*     { INTR_ATOMIC_DEFINE, INTR_NAME_GENERIC,   "atomic_define", 0,      {INTR_TYPE_COARRAY_INT, INTR_TYPE_INT},                 INTR_TYPE_NONE,                         2,  0, LANGSPEC_F2008, INTR_CLASS_A, {"atom", "value" } }, */
/*     { INTR_ATOMIC_DEFINE, INTR_NAME_GENERIC,   "",              0,      {INTR_TYPE_COARRAY_LOGICAL, INTR_TYPE_LOGICAL},         INTR_TYPE_NONE,                         2,  0, LANGSPEC_F2008, INTR_CLASS_A, {"atom", "value" } }, */

/*     { INTR_ATOMIC_DEFINE, INTR_NAME_GENERIC,   "atomic_ref",    0,      {INTR_TYPE_INT, INTR_TYPE_COARRAY_INT},                 INTR_TYPE_NONE,                         2,  0, LANGSPEC_F2008, INTR_CLASS_A, {"value", "atom" } }, */
/*     { INTR_ATOMIC_DEFINE, INTR_NAME_GENERIC,   "",              0,      {INTR_TYPE_LOGICAL, INTR_TYPE_COARRAY_LOGICAL},         INTR_TYPE_NONE,                         2,  0, LANGSPEC_F2008, INTR_CLASS_A, {"value", "atom" } }, */

/*     // SELECTED_CHAR_KIND(NAME) */
/*     { INTR_SELECTED_CHAR_KIND, */
/*                           INTR_NAME_GENERIC,   "selected_char_kind", */
/*                                                                 0,      {INTR_TYPE_CHAR},                                       INTR_TYPE_INT,                          1, -6, LANGSPEC_F2008, INTR_CLASS_T }, */


/*     /\\* */
/*      * Fortran 2003 intrinsics */
/*      *\\/ */
/*     { INTR_MOVE_ALLOC,  INTR_NAME_SPECIFIC,     "move_alloc",   0,      {INTR_TYPE_ANY, INTR_TYPE_ANY},                         INTR_TYPE_NONE,                         2,  -6, LANGSPEC_F2003, INTR_CLASS_PS }, */
/*     { INTR_EXTENDS_TYPE_OF, */
/*                         INTR_NAME_SPECIFIC,  "extends_type_of", 0,      {INTR_TYPE_ANY, INTR_TYPE_ANY},                         INTR_TYPE_LOGICAL,                      2,  -6, LANGSPEC_F2003, INTR_CLASS_I, {"a", "mold"} }, */
/*     { INTR_SAME_TYPE_AS,INTR_NAME_SPECIFIC,     "same_type_as", 0,      {INTR_TYPE_ANY, INTR_TYPE_ANY},                         INTR_TYPE_LOGICAL,                      2,  -6, LANGSPEC_F2003, INTR_CLASS_I }, */
/*     { INTR_IS_IOSTAT_END, */
/*                         INTR_NAME_SPECIFIC,    "is_iostat_end", 0,      {INTR_TYPE_INT},                                        INTR_TYPE_LOGICAL,                      1,  -6, LANGSPEC_F2003, INTR_CLASS_E }, */
/*     { INTR_IS_IOSTAT_EOR, */
/*                         INTR_NAME_SPECIFIC,    "is_iostat_ior", 0,      {INTR_TYPE_INT},                                        INTR_TYPE_LOGICAL,                      1,  -6, LANGSPEC_F2003, INTR_CLASS_E }, */
/*     { INTR_NEW_LINE,    INTR_NAME_SPECIFIC,     "new_line",     0,      {INTR_TYPE_CHAR},                                       INTR_TYPE_CHAR,                         1,  -6, LANGSPEC_F2003, INTR_CLASS_I }, */
/*     { INTR_NEW_LINE,    INTR_NAME_SPECIFIC,     "",             0,      {INTR_TYPE_CHAR_ARRAY},                                 INTR_TYPE_CHAR,                         1,  -6, LANGSPEC_F2003, INTR_CLASS_I }, */

/*     /\\* */
/*      * Fortran 2008 intrinsics */
/*      *\\/ */

/*     { INTR_COMMAND_ARUGMENT_COUNT, */
/*                   INTR_NAME_SPECIFIC, "command_argument_count", 0,      {INTR_TYPE_NONE},                                       INTR_TYPE_INT,                          0, -6, LANGSPEC_F2008, INTR_CLASS_T }, */

/*     { INTR_GET_COMMAND, INTR_NAME_GENERIC,      "get_command",  0,      {INTR_TYPE_NONE},                                       INTR_TYPE_NONE,                         0, -6, LANGSPEC_F2008, INTR_CLASS_S }, */
/*     { INTR_GET_COMMAND, INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_CHAR},                                       INTR_TYPE_NONE,                         1, -6, LANGSPEC_F2008, INTR_CLASS_S, {"command"} }, */
/*     { INTR_GET_COMMAND, INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_CHAR, INTR_TYPE_INT},                        INTR_TYPE_NONE,                         2, -6, LANGSPEC_F2008, INTR_CLASS_S, {"command", "length"} }, */
/*     { INTR_GET_COMMAND, INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_CHAR, INTR_TYPE_INT, INTR_TYPE_INT},         INTR_TYPE_NONE,                         3, -6, LANGSPEC_F2008, INTR_CLASS_S, {"command", "length", "status"} }, */

/*     // EXECUTE_COMMAND_LINE (COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ]) */
/*     //{ INTR_EXECUTE_COMMAND_LINE, */
/*     //INTR_NAME_GENERIC, "execute_command_line", 0, {INTR_TYPE_CHAR}, INTR_TYPE_NONE, ?, -6, LANGSPEC_F2008, INTR_CLASS_S, {} } */
    
/*     { INTR_GET_COMMAND_ARUGMENT, */
/*                      INTR_NAME_GENERIC, "get_command_argument", 0,      {INTR_TYPE_INT},                                        INTR_TYPE_NONE,                         1, -6, LANGSPEC_F2008, INTR_CLASS_S, {"number"} }, */
/*     { INTR_GET_COMMAND_ARUGMENT, */
/*                      INTR_NAME_GENERIC,         "",             0,      {INTR_TYPE_INT, INTR_TYPE_CHAR},                        INTR_TYPE_NONE,                         2, -6, LANGSPEC_F2008, INTR_CLASS_S, {"number", "value"} }, */
/*     { INTR_GET_COMMAND_ARUGMENT, */
/*                      INTR_NAME_GENERIC,         "",             0,      {INTR_TYPE_INT, INTR_TYPE_CHAR, INTR_TYPE_INT},         INTR_TYPE_NONE,                         3, -6, LANGSPEC_F2008, INTR_CLASS_S, {"number", "value", "length"} }, */
/*     { INTR_GET_COMMAND_ARUGMENT, */
/*                      INTR_NAME_GENERIC,         "",             0,      {INTR_TYPE_INT, INTR_TYPE_CHAR, INTR_TYPE_INT, */
/* 									 INTR_TYPE_INT},                                        INTR_TYPE_NONE,                         4, -6, LANGSPEC_F2008, INTR_CLASS_S, {"number", "value", "length", "status"} }, */

/*     { INTR_GET_ENVIRONMENT_VARIABLE, */
/*                  INTR_NAME_GENERIC, "get_environment_variable", 0,      {INTR_TYPE_CHAR},                                       INTR_TYPE_INT,                          1, -6, LANGSPEC_F2008, INTR_CLASS_T, {"name"} }, */
/*     { INTR_GET_ENVIRONMENT_VARIABLE, */
/*                         INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_CHAR, INTR_TYPE_CHAR},                       INTR_TYPE_INT,                          2, -6, LANGSPEC_F2008, INTR_CLASS_T, {"name", "value"} }, */
/*     { INTR_GET_ENVIRONMENT_VARIABLE, */
/*                         INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_CHAR, INTR_TYPE_CHAR, INTR_TYPE_INT},        INTR_TYPE_INT,                          3, -6, LANGSPEC_F2008, INTR_CLASS_T, {"name", "value", "length"} }, */
/*     { INTR_GET_ENVIRONMENT_VARIABLE, */
/*                         INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_CHAR, INTR_TYPE_CHAR, INTR_TYPE_INT, */
/* 									 INTR_TYPE_INT},                                        INTR_TYPE_INT,                          4, -6, LANGSPEC_F2008, INTR_CLASS_T, {"name", "value", "length", "status"} }, */
/*     { INTR_GET_ENVIRONMENT_VARIABLE, */
/*                         INTR_NAME_GENERIC,      "",             0,      {INTR_TYPE_CHAR, INTR_TYPE_CHAR, INTR_TYPE_INT, */
/* 									 INTR_TYPE_INT, INTR_TYPE_LOGICAL },                    INTR_TYPE_INT,                          5, -6, LANGSPEC_F2008, INTR_CLASS_T, {"name", "value", "length", "status", "trim_name"} }, */

/*     { INTR_GAMMA,       INTR_NAME_GENERIC,      "gamma",        0,      {INTR_TYPE_REAL},                                       INTR_TYPE_REAL,                         1, -6, LANGSPEC_F2008, INTR_CLASS_T}, */
/*     { INTR_LOGGAMMA,    INTR_NAME_GENERIC,      "log_gamma",    0,      {INTR_TYPE_REAL},                                       INTR_TYPE_REAL,                         1, -6, LANGSPEC_F2008, INTR_CLASS_T}, */

/*     { INTR_BESSEL_JO,   INTR_NAME_GENERIC,                     "bessel_j0",    0,      {INTR_TYPE_REAL},                                       INTR_TYPE_REAL,                         1,  0, LANGSPEC_F2008, INTR_CLASS_E }, */
/*     { INTR_BESSEL_J1,   INTR_NAME_GENERIC,                     "bessel_j1",    0,      {INTR_TYPE_REAL},                                       INTR_TYPE_REAL,                         1,  0, LANGSPEC_F2008, INTR_CLASS_E }, */
/*     { INTR_BESSEL_JN,   INTR_NAME_GENERIC,                     "bessel_jn",    0,      {INTR_TYPE_INT, INTR_TYPE_REAL},                        INTR_TYPE_REAL,                         2,  1, LANGSPEC_F2008, INTR_CLASS_E, {"n", "x"} }, */
/*     { INTR_BESSEL_JN,   INTR_NAME_GENERIC,                     "",             0,      {INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_REAL},         INTR_TYPE_REAL_ARRAY,                   3, -1, LANGSPEC_F2008, INTR_CLASS_T, {"n1", "n2", "x"} }, */
/*     { INTR_BESSEL_Y0,   INTR_NAME_GENERIC,                     "bessel_y0",    0,      {INTR_TYPE_REAL},                                       INTR_TYPE_REAL,                         1,  0, LANGSPEC_F2008, INTR_CLASS_E }, */
/*     { INTR_BESSEL_Y1,   INTR_NAME_GENERIC,                     "bessel_y1",    0,      {INTR_TYPE_REAL},                                       INTR_TYPE_REAL,                         1,  0, LANGSPEC_F2008, INTR_CLASS_E }, */
/*     { INTR_BESSEL_YN,   INTR_NAME_GENERIC,                     "bessel_yn",    0,      {INTR_TYPE_INT, INTR_TYPE_REAL},                        INTR_TYPE_REAL,                         2,  1, LANGSPEC_F2008, INTR_CLASS_E, {"n", "x"} }, */
/*     { INTR_BESSEL_YN,   INTR_NAME_GENERIC,                     "",             0,      {INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_REAL},         INTR_TYPE_REAL_ARRAY,                   3, -1, LANGSPEC_F2008, INTR_CLASS_T, {"n1", "n2", "x"} }, */
    
/* #ifdef GNU_INTRINSIC_EXTENSION */
/*     { INTR_GNU_ABORT,     INTR_NAME_GENERIC, "abort",     0, { INTR_TYPE_NONE }, INTR_TYPE_NONE, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
    
/*     { INTR_GNU_ACCESS,    INTR_NAME_GENERIC, "access",    0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_INT,  2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */
    
/*     { INTR_GNU_ACOSD,     INTR_NAME_GENERIC, "acosd",     0, { INTR_TYPE_REAL }, INTR_TYPE_REAL, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_ACOSD,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_COMPLEX, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
    
/*     { INTR_GNU_ALARM,     INTR_NAME_GENERIC, "alarm",     0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_ALARM,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT}, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
    
/*     { INTR_GNU_AND,       INTR_NAME_GENERIC, "and",       0, { INTR_TYPE_INT, INTR_TYPE_INT}, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */
/*     { INTR_GNU_AND,       INTR_NAME_GENERIC, "",          0, { INTR_TYPE_LOGICAL, INTR_TYPE_LOGICAL}, INTR_TYPE_LOGICAL, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */
    
/*     { INTR_GNU_ASIND,     INTR_NAME_GENERIC, "asind",     0, { INTR_TYPE_REAL }, INTR_TYPE_REAL, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_ASIND,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_COMPLEX, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
    
/*     { INTR_GNU_ATAND,     INTR_NAME_GENERIC, "atand",     0, { INTR_TYPE_REAL }, INTR_TYPE_REAL, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_ATAND,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_COMPLEX, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
    
/*     { INTR_GNU_ATAN2D,    INTR_NAME_GENERIC, "atan2d",    0, { INTR_TYPE_REAL, INTR_TYPE_REAL }, INTR_TYPE_REAL, 2, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
    
/*     { INTR_GNU_BACKTRACE, INTR_NAME_GENERIC, "backtrace", 0, { INTR_TYPE_NONE }, INTR_TYPE_NONE, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
    
/*     { INTR_GNU_CHDIR,     INTR_NAME_GENERIC, "chdir",     0, { INTR_TYPE_CHAR }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */
/*     { INTR_GNU_CHDIR,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */
/*     { INTR_GNU_CHDIR,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_CHMOD,     INTR_NAME_GENERIC, "chmod",     0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_CHMOD,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR, INTR_TYPE_INT }, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_CHMOD,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_COMPLEX,   INTR_NAME_GENERIC, "complex",   0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_COMPLEX,   INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL, INTR_TYPE_REAL }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_COMPLEX,   INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL, INTR_TYPE_INT }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_COMPLEX,   INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_REAL }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */

/*     { INTR_GNU_COSD,      INTR_NAME_GENERIC, "cosd",      0, { INTR_TYPE_REAL }, INTR_TYPE_REAL, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_COSD,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_COMPLEX, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */

/*     { INTR_GNU_COTAN,     INTR_NAME_GENERIC, "cotan",     0, { INTR_TYPE_REAL }, INTR_TYPE_REAL, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_COTAN,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_COMPLEX, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */

/*     { INTR_GNU_COTAND,    INTR_NAME_GENERIC, "cotand",    0, { INTR_TYPE_REAL }, INTR_TYPE_REAL, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_COTAND,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_COMPLEX, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E}, */

/*     { INTR_GNU_CTIME,     INTR_NAME_GENERIC, "ctime",     0, { INTR_TYPE_INT, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, 0, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_CTIME,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT }, INTR_TYPE_CHAR, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_DCMPLX,    INTR_NAME_GENERIC, "dcmplx",    0, { INTR_TYPE_INT }, INTR_TYPE_COMPLEX, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_DCMPLX,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL }, INTR_TYPE_COMPLEX, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_DCMPLX,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_COMPLEX, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_DCMPLX,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX, INTR_TYPE_INT }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_DCMPLX,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX, INTR_TYPE_REAL }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_DCMPLX,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL, INTR_TYPE_REAL }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_DCMPLX,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL, INTR_TYPE_INT }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_DCMPLX,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_DCMPLX,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_REAL }, INTR_TYPE_COMPLEX, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */

/*     { INTR_GNU_DREAL,     INTR_NAME_GENERIC, "dreal",     0, { INTR_TYPE_COMPLEX }, INTR_TYPE_REAL, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, // TODO CHECK CLASS */

/*     { INTR_GNU_DTIME,     INTR_NAME_GENERIC, "dtime",     0, { INTR_TYPE_REAL_ARRAY, INTR_TYPE_REAL }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_DTIME,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL_ARRAY }, INTR_TYPE_REAL, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_ETIME,     INTR_NAME_GENERIC, "etime",     0, { INTR_TYPE_REAL_ARRAY, INTR_TYPE_REAL }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_ETIME,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL_ARRAY }, INTR_TYPE_REAL, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_EXIT,      INTR_NAME_GENERIC, "exit",      0, { INTR_TYPE_NONE }, INTR_TYPE_NONE, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_EXIT,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_FDATE,     INTR_NAME_GENERIC, "fdate",     0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FDATE,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_NONE }, INTR_TYPE_CHAR, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_FGET,      INTR_NAME_GENERIC, "fget",      0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FGET,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FGET,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_FGETC,     INTR_NAME_GENERIC, "fgetc",     0, { INTR_TYPE_INT, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FGETC,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_CHAR, INTR_TYPE_INT }, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FGETC,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_CHAR }, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_FLUSH,     INTR_NAME_GENERIC, "flush",     0, { INTR_TYPE_INT }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_FNUM,      INTR_NAME_GENERIC, "fnum",      0, { INTR_TYPE_INT }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_FPUT,      INTR_NAME_GENERIC, "fput",      0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FPUT,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_INT}, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FPUT,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR}, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_FPUTC,     INTR_NAME_GENERIC, "fputc",     0, { INTR_TYPE_INT, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FPUTC,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_CHAR, INTR_TYPE_INT}, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FPUTC,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_CHAR}, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_FREE,      INTR_NAME_GENERIC, "free",      0, { INTR_TYPE_INT}, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_FSEEK,     INTR_NAME_GENERIC, "fseek",     0, { INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT}, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FSEEK,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT}, INTR_TYPE_NONE, 4, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_FSTAT,     INTR_NAME_GENERIC, "fstat",     0, { INTR_TYPE_INT, INTR_TYPE_INT_ARRAY}, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FSTAT,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT_ARRAY, INTR_TYPE_INT}, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FSTAT,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT}, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_FTELL,     INTR_NAME_GENERIC, "ftell",     0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_FTELL,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_GERROR,    INTR_NAME_GENERIC, "gerror",    0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_GETARG,    INTR_NAME_GENERIC, "getarg",    0, { INTR_TYPE_INT, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_GETCWD,    INTR_NAME_GENERIC, "getcwd",    0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_GETCWD,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_GETCWD,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_GETENV,    INTR_NAME_GENERIC, "getenv",    0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_GETGID,    INTR_NAME_GENERIC, "getgid",    0, { INTR_TYPE_NONE }, INTR_TYPE_INT, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_GETLOG,    INTR_NAME_GENERIC, "getlog",    0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_GETPID,    INTR_NAME_GENERIC, "getpid",    0, { INTR_TYPE_NONE }, INTR_TYPE_INT, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_GETUID,    INTR_NAME_GENERIC, "getuid",    0, { INTR_TYPE_NONE }, INTR_TYPE_INT, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_GMTIME,    INTR_NAME_GENERIC, "gmtime",    0, { INTR_TYPE_INT, INTR_TYPE_INT_ARRAY }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_HOSTNM,    INTR_NAME_GENERIC, "hostnm",    0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_HOSTNM,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */
/*     { INTR_GNU_HOSTNM,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_IARGC,     INTR_NAME_GENERIC, "iargc",     0, { INTR_TYPE_NONE }, INTR_TYPE_INT, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_IDATE,     INTR_NAME_GENERIC, "idate",     0, { INTR_TYPE_INT_ARRAY }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_IERRNO,    INTR_NAME_GENERIC, "ierrno",    0, { INTR_TYPE_NONE }, INTR_TYPE_INT, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_INT2,      INTR_NAME_GENERIC, "int2",      0, { INTR_TYPE_INT }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_INT2,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */
/*     { INTR_GNU_INT2,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E}, */

/*     { INTR_GNU_INT8,      INTR_NAME_GENERIC, "int8",      0, { INTR_TYPE_INT }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E }, */
/*     { INTR_GNU_INT8,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E }, */
/*     { INTR_GNU_INT8,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E }, */
    
/*     { INTR_GNU_IRAND,     INTR_NAME_GENERIC, "irand",     0, { INTR_TYPE_INT }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_ISATTY,    INTR_NAME_GENERIC, "isatty",    0, { INTR_TYPE_INT }, INTR_TYPE_LOGICAL, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_ISNAN,     INTR_NAME_GENERIC, "isnan",     0, { INTR_TYPE_REAL }, INTR_TYPE_LOGICAL, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_ITIME,     INTR_NAME_GENERIC, "itime",     0, { INTR_TYPE_INT_ARRAY }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */

/*     { INTR_GNU_KILL,      INTR_NAME_GENERIC, "kill",      0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_KILL,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_KILL,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_LINK,      INTR_NAME_GENERIC, "link",      0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_LINK,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR , INTR_TYPE_INT }, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_LINK,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_LNBLNK,    INTR_NAME_GENERIC, "lnblnk",    0, { INTR_TYPE_CHAR }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E }, */

/*     { INTR_GNU_LOC,       INTR_NAME_GENERIC, "loc",       0, { INTR_TYPE_ANY }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_I }, */

/*     { INTR_GNU_LONG,      INTR_NAME_GENERIC, "long",      0, { INTR_TYPE_INT }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E }, */
/*     { INTR_GNU_LONG,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_REAL }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E }, */
/*     { INTR_GNU_LONG,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_E }, */

/*     { INTR_GNU_LSHIFT,    INTR_NAME_GENERIC, "lshift",    0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_INT, 2, 0, LANGSPEC_NONSTD, INTR_CLASS_E }, */

/*     { INTR_GNU_LSTAT,     INTR_NAME_GENERIC, "lstat",     0, { INTR_TYPE_CHAR, INTR_TYPE_INT_ARRAY }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_LSTAT,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_INT_ARRAY, INTR_TYPE_INT }, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_LSTAT,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_INT_ARRAY }, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */

/*     { INTR_GNU_LTIME,     INTR_NAME_GENERIC, "ltime",     0, { INTR_TYPE_INT, INTR_TYPE_INT_ARRAY }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */

/*     { INTR_GNU_MALLOC,    INTR_NAME_GENERIC, "malloc",    0, { INTR_TYPE_INT }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_MCLOCK,    INTR_NAME_GENERIC, "mclock",    0, { INTR_TYPE_NONE }, INTR_TYPE_INT, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_MCLOCK8,   INTR_NAME_GENERIC, "mclock8",   0, { INTR_TYPE_NONE }, INTR_TYPE_INT, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_OR,        INTR_NAME_GENERIC, "or",        0, { INTR_TYPE_INT, INTR_TYPE_INT}, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */
/*     { INTR_GNU_OR,        INTR_NAME_GENERIC, "",          0, { INTR_TYPE_LOGICAL, INTR_TYPE_LOGICAL}, INTR_TYPE_LOGICAL, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_PERROR,    INTR_NAME_GENERIC, "perror",    0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_RAN,       INTR_NAME_GENERIC, "ran",       0, { INTR_TYPE_INT }, INTR_TYPE_REAL, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_RAND,      INTR_NAME_GENERIC, "rand",      0, { INTR_TYPE_INT }, INTR_TYPE_REAL, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_RENAME,    INTR_NAME_GENERIC, "rename",    0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_RENAME,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR , INTR_TYPE_INT }, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_RENAME,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_RSHIFT,    INTR_NAME_GENERIC, "rshift",    0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_INT, 2, 0, LANGSPEC_NONSTD, INTR_CLASS_E }, */

/*     { INTR_GNU_SECNDS,    INTR_NAME_GENERIC, "secnds",    0, { INTR_TYPE_REAL }, INTR_TYPE_REAL, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_SECOND,    INTR_NAME_GENERIC, "second",    0, { INTR_TYPE_REAL }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_SECOND,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_NONE }, INTR_TYPE_REAL, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_SIGNAL,    INTR_NAME_GENERIC, "signal",    0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_SIGNAL,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_SIGNAL,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_SIND,      INTR_NAME_GENERIC, "sind",      0, { INTR_TYPE_REAL }, INTR_TYPE_REAL, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E }, */
/*     { INTR_GNU_SIND,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_COMPLEX, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E }, */

/*     { INTR_GNU_SIZEOF,    INTR_NAME_GENERIC, "sizeof",    0, { INTR_TYPE_ANY }, INTR_TYPE_INT,  1, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/*     { INTR_GNU_SLEEP,     INTR_NAME_GENERIC, "sleep",     0, { INTR_TYPE_INT }, INTR_TYPE_NONE,  1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_SRAND,     INTR_NAME_GENERIC, "srand",     0, { INTR_TYPE_INT }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S}, */

/*     { INTR_GNU_STAT,      INTR_NAME_GENERIC, "stat",      0, { INTR_TYPE_CHAR, INTR_TYPE_INT_ARRAY }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_STAT,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_INT_ARRAY, INTR_TYPE_INT }, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_STAT,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_INT_ARRAY }, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */

/*     { INTR_GNU_SYMLNK,    INTR_NAME_GENERIC, "symlnk",    0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_SYMLNK,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR , INTR_TYPE_INT }, INTR_TYPE_NONE, 3, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_SYMLNK,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR, INTR_TYPE_CHAR }, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_SYSTEM,    INTR_NAME_GENERIC, "system",    0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_SYSTEM,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR , INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_SYSTEM,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_TAND,      INTR_NAME_GENERIC, "tand",      0, { INTR_TYPE_REAL }, INTR_TYPE_REAL, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E }, */
/*     { INTR_GNU_TAND,      INTR_NAME_GENERIC, "",          0, { INTR_TYPE_COMPLEX }, INTR_TYPE_COMPLEX, 1, 0, LANGSPEC_NONSTD, INTR_CLASS_E }, */

/*     { INTR_GNU_TIME,      INTR_NAME_GENERIC, "time",      0, { INTR_TYPE_NONE }, INTR_TYPE_INT, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_TIME8,     INTR_NAME_GENERIC, "time8",     0, { INTR_TYPE_NONE }, INTR_TYPE_INT, 0, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_TTYNAM,    INTR_NAME_GENERIC, "ttynam",    0, { INTR_TYPE_INT, INTR_TYPE_CHAR }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_TTYNAM,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT }, INTR_TYPE_CHAR, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */

/*     { INTR_GNU_UMASK,     INTR_NAME_GENERIC, "umask",     0, { INTR_TYPE_INT }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_UMASK,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT, INTR_TYPE_INT }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_UMASK,     INTR_NAME_GENERIC, "",          0, { INTR_TYPE_INT }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_UNLINK,    INTR_NAME_GENERIC, "unlink",    0, { INTR_TYPE_CHAR }, INTR_TYPE_NONE, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_UNLINK,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR , INTR_TYPE_INT }, INTR_TYPE_NONE, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_S }, */
/*     { INTR_GNU_UNLINK,    INTR_NAME_GENERIC, "",          0, { INTR_TYPE_CHAR }, INTR_TYPE_INT, 1, -6, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GNU_XOR,       INTR_NAME_GENERIC, "xor",       0, { INTR_TYPE_INT, INTR_TYPE_INT}, INTR_TYPE_INT, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */
/*     { INTR_GNU_XOR,       INTR_NAME_GENERIC, "",          0, { INTR_TYPE_LOGICAL, INTR_TYPE_LOGICAL}, INTR_TYPE_LOGICAL, 2, -6, LANGSPEC_NONSTD, INTR_CLASS_N}, */

/* #endif */

/*     /\\* hidden interfaces for debugging *\\/ */
/*     { INTR_COARRAY_MALLOC_BYTES,      INTR_NAME_GENERIC,   "xmpf_coarray_malloc_bytes",        0,   {INTR_TYPE_NONE},   INTR_TYPE_INT,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */
/*     { INTR_COARRAY_ALLOCATED_BYTES,   INTR_NAME_GENERIC,   "xmpf_coarray_allocated_bytes",     0,   {INTR_TYPE_NONE},   INTR_TYPE_INT,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */
/*     { INTR_COARRAY_GARBAGE_BYTES,     INTR_NAME_GENERIC,   "xmpf_coarray_garbage_bytes",       0,   {INTR_TYPE_NONE},   INTR_TYPE_INT,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     /\\* XMP/F *\\/ */

/*     { INTR_DESC_OF,        INTR_NAME_GENERIC,      "xmp_desc_of",         0,      {INTR_TYPE_ANY},       INTR_TYPE_DREAL,  1, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_GET_MPI_COMM,        INTR_NAME_GENERIC,      "xmp_get_mpi_comm",         0,      {INTR_TYPE_NONE},       INTR_TYPE_INT,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_NUM_NODES,        INTR_NAME_GENERIC,      "xmp_num_nodes",         0,      {INTR_TYPE_NONE},       INTR_TYPE_INT,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_NODE_NUM,         INTR_NAME_GENERIC,      "xmp_node_num",          0,      {INTR_TYPE_NONE},       INTR_TYPE_INT,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ALL_NUM_NODES,        INTR_NAME_GENERIC,      "xmp_all_num_nodes",         0,      {INTR_TYPE_NONE},       INTR_TYPE_INT,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ALL_NODE_NUM,         INTR_NAME_GENERIC,      "xmp_all_node_num",          0,      {INTR_TYPE_NONE},       INTR_TYPE_INT,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_WTIME,         INTR_NAME_GENERIC,      "xmp_wtime",          0,      {INTR_TYPE_NONE},       INTR_TYPE_DREAL,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_WTICK,         INTR_NAME_GENERIC,      "xmp_wtick",          0,      {INTR_TYPE_NONE},       INTR_TYPE_DREAL,  0, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ARRAY_NDIMS,         INTR_NAME_GENERIC,      "xmp_array_ndims",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT},       INTR_TYPE_INT, 2, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ARRAY_LBOUND,         INTR_NAME_GENERIC,      "xmp_array_lbound",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ARRAY_UBOUND,         INTR_NAME_GENERIC,      "xmp_array_ubound",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ARRAY_LSIZE,         INTR_NAME_GENERIC,      "xmp_array_lsize",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ARRAY_USHADOW,         INTR_NAME_GENERIC,      "xmp_array_ushadow",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ARRAY_LSHADOW,         INTR_NAME_GENERIC,      "xmp_array_lshadow",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ARRAY_LEAD_DIM,         INTR_NAME_GENERIC,      "xmp_array_lead_dim",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT_ARRAY},       INTR_TYPE_INT, 2, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ARRAY_GTOL,         INTR_NAME_GENERIC,      "xmp_array_gtol",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 4, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ALIGN_AXIS,         INTR_NAME_GENERIC,      "xmp_align_axis",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ALIGN_OFFSET,         INTR_NAME_GENERIC,      "xmp_align_offset",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ALIGN_REPLICATED,         INTR_NAME_GENERIC,      "xmp_align_replicated",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_LOGICAL},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_ALIGN_TEMPLATE,         INTR_NAME_GENERIC,      "xmp_align_template",          0,      {INTR_TYPE_ANY, INTR_TYPE_ANY},       INTR_TYPE_INT, 2, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_TEMPLATE_FIXED,         INTR_NAME_GENERIC,      "xmp_template_fixed",          0,      {INTR_TYPE_ANY, INTR_TYPE_LOGICAL},       INTR_TYPE_INT, 2, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_TEMPLATE_NDIMS,         INTR_NAME_GENERIC,      "xmp_template_ndims",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT},       INTR_TYPE_INT, 2, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_TEMPLATE_LBOUND,         INTR_NAME_GENERIC,      "xmp_template_lbound",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_TEMPLATE_UBOUND,         INTR_NAME_GENERIC,      "xmp_template_ubound",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_DIST_FORMAT,         INTR_NAME_GENERIC,      "xmp_dist_format",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_DIST_BLOCKSIZE,         INTR_NAME_GENERIC,      "xmp_dist_blocksize",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_DIST_GBLOCKMAP,         INTR_NAME_GENERIC,      "xmp_dist_gblockmap",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT_ARRAY},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_DIST_NODES,         INTR_NAME_GENERIC,      "xmp_dist_nodes",          0,      {INTR_TYPE_ANY, INTR_TYPE_ANY},       INTR_TYPE_INT, 2, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_DIST_AXIS,         INTR_NAME_GENERIC,      "xmp_dist_axis",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_NODES_NDIMS,         INTR_NAME_GENERIC,      "xmp_nodes_ndims",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT},       INTR_TYPE_INT, 2, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_NODES_INDEX,         INTR_NAME_GENERIC,      "xmp_nodes_index",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_NODES_SIZE,         INTR_NAME_GENERIC,      "xmp_nodes_size",          0,      {INTR_TYPE_ANY, INTR_TYPE_INT, INTR_TYPE_INT},       INTR_TYPE_INT, 3, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

/*     { INTR_NODES_EQUIV,         INTR_NAME_GENERIC,      "xmp_nodes_equiv",          0,      {INTR_TYPE_ANY, INTR_TYPE_ANY, INTR_TYPE_INT_ARRAY, INTR_TYPE_INT_ARRAY, INTR_TYPE_INT_ARRAY},       INTR_TYPE_INT, 5, -8, LANGSPEC_NONSTD, INTR_CLASS_N }, */

    { INTR_END }
};
