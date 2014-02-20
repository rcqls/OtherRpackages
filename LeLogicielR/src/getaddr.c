// R CMD SHLIB getaddr.c
#include <R.h>

void getAddrForDbl(double *ptr, int* addret, double *valret) 
  {

    int a[2];
    int* *p;

    a[0]=a[1]=0;
    //Rprintf("1) %p %d %f\n", ptr, ptr, *ptr);

    //Rprintf("a av: %d %d (%d,%d)\n",a[0],a[1],sizeof(a[0]),sizeof(a[1]));

    p=(int**)&a;
    *p=(int*)ptr;

    //Rprintf("a ap: %d %d (%d,%d)\n",a[0],a[1],sizeof(a[0]),sizeof(a[1]));

    //Rprintf("addret init: %d %d (%d,%d)\n",addret[0],addret[1],sizeof(addret[0]),sizeof(addret[1]));

    addret[0]=a[0];addret[1]=a[1];
    //Rprintf("addret: %d %d (%d,%d)\n",addret[0],addret[1],sizeof(addret[0]),sizeof(addret[1]));

    *valret = *ptr;  

    return;
  }

void writeAtAddrForDbl(int *addr,double *newval) 
{

  double *res;

  long unsigned int *raddr;

  raddr=(long unsigned int *)addr;
  res=(double*)*raddr;

  //Rprintf("resWrite=%lf %lf (%d,%d)\n",*res,*newval,addr[0],addr[1]);

  *res=*newval;

  //Rprintf("resWriteBis=%lf %lf (%d,%d)\n",*res,*newval,addr[0],addr[1]);

  return;

}

void getAddrForInt(int *ptr, int* addret, int *valret) 
  {

    int a[2];
    int* *p;

    a[0]=a[1]=0;
    //Rprintf("1) %p %d %f\n", ptr, ptr, *ptr);

    //Rprintf("a av: %d %d (%d,%d)\n",a[0],a[1],sizeof(a[0]),sizeof(a[1]));

    p=(int**)&a;
    *p=(int*)ptr;

    //Rprintf("a ap: %d %d (%d,%d)\n",a[0],a[1],sizeof(a[0]),sizeof(a[1]));

    //Rprintf("addret init: %d %d (%d,%d)\n",addret[0],addret[1],sizeof(addret[0]),sizeof(addret[1]));

    addret[0]=a[0];addret[1]=a[1];
    //Rprintf("addret: %d %d (%d,%d)\n",addret[0],addret[1],sizeof(addret[0]),sizeof(addret[1]));

    *valret = *ptr;  

    return;
  }

void writeAtAddrForInt(int *addr,int *newval) 
{

  int *res;

  long unsigned int *raddr;

  raddr=(long unsigned int *)addr;
  res=(int*)(*raddr);

  //Rprintf("resWrite=%lf %lf (%d,%d)\n",*res,*newval,addr[0],addr[1]);

  *res=*newval;

  //Rprintf("resWriteBis=%lf %lf (%d,%d)\n",*res,*newval,addr[0],addr[1]);

  return;

}
