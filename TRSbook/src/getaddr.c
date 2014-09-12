#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP getAddrDbl(SEXP ptr)   {
    double * _ptr;
    int* _addret;
    SEXP addret;

    int a[2];
    int* *p;
     
    ptr = PROTECT(coerceVector(ptr, REALSXP));
    addret = PROTECT(allocVector(INTSXP, 2));
    _ptr=REAL(ptr); _addret=INTEGER(addret);
    a[0]=a[1]=0;

    p=(int**)&a;
    *p=(int*)_ptr;
    _addret[0]=a[0];_addret[1]=a[1];
    UNPROTECT(2);
    return addret;
}

SEXP writeAtAddrDbl(SEXP addr,SEXP newval) {
    double * _newval;
    int* _addr; 
    int a[2];
    unsigned long int *raddr;

    double *p;
    // &p = adresse de p
    // p = addresse vers laquelle pointe p
    // *p = valeur de la case à l'adresse p
    addr = PROTECT(coerceVector(addr, INTSXP));
    newval = PROTECT(coerceVector(newval, REALSXP));
    _addr=INTEGER(addr); _newval=REAL(newval);
    a[0]=_addr[0];a[1]=_addr[1];
    raddr=(long unsigned int *)a;
    p = (double *)*raddr;
    *p = (double)(_newval[0]); // Identique à *p = *newval; mais sans warning à la compilation
    UNPROTECT(2);
  return R_NilValue;

}

SEXP getAddrInt(SEXP ptr)   {
    int * _ptr;
    int* _addret;
    SEXP addret;

    int a[2];
    int* *p;
     
    ptr = PROTECT(coerceVector(ptr, INTSXP));
    addret = PROTECT(allocVector(INTSXP, 2));
    _ptr=INTEGER(ptr); _addret=INTEGER(addret);
    a[0]=a[1]=0;

    p=(int**)&a;
    *p=(int*)_ptr;
    _addret[0]=a[0];_addret[1]=a[1];
    UNPROTECT(2);
    return addret;
}

SEXP writeAtAddrInt(SEXP addr,SEXP newval) {
    int * _newval;
    int* _addr; 
    int a[2];
    unsigned long int *raddr;

    int *p;
    // &p = adresse de p
    // p = addresse vers laquelle pointe p
    // *p = valeur de la case à l'adresse p
    addr = PROTECT(coerceVector(addr, INTSXP));
    newval = PROTECT(coerceVector(newval, INTSXP));
    _addr=INTEGER(addr); _newval=INTEGER(newval);
    a[0]=_addr[0];a[1]=_addr[1];
    raddr=(long unsigned int *)a;
    p = (int *)*raddr;
    *p = (int)(_newval[0]); // Identique à *p = *newval; mais sans warning à la compilation
    UNPROTECT(2);
  return R_NilValue;

}

SEXP printAddr(SEXP addr) {
    int* _addr; 
    int a[2];
    unsigned long int *raddr;
    addr = PROTECT(coerceVector(addr, INTSXP));
    _addr=INTEGER(addr);
    a[0]=_addr[0];a[1]=_addr[1];
    raddr=(long unsigned int *)a;
  Rprintf("<vector: %p>\n",*raddr);
  UNPROTECT(1);
  return R_NilValue;
}

static R_CallMethodDef callMethods[] = {
        {"getAddrDbl", (DL_FUNC) &getAddrDbl, 1},
        {"getAddrInt", (DL_FUNC) &getAddrInt, 1},
        {"writeAtAddrDbl", (DL_FUNC) &writeAtAddrDbl, 2},
        {"writeAtAddrInt", (DL_FUNC) &writeAtAddrInt, 2},
        {"printAddr", (DL_FUNC) &printAddr, 1},
        {NULL, NULL, 0}
     };

void R_init_LeLogicielR(DllInfo *info)
     {
        R_registerRoutines(info, NULL, callMethods, NULL, NULL);
     };
