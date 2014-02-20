// R CMD SHLIB getaddr.c
#include <R.h>


void getaddrdble(double *ptr, int *addret, double *valret) 
  {
    //    Rprintf("%p %i\n", ptr, *ptr);

  //  Rprintf("%p %d %f\n", ptr, ptr, *ptr);

    //addret = ptr;

    addret[0] = (unsigned long int)(*(&ptr)); // adresse du 1er octet de ptr

    
     //    Rprintf("%i\n",(unsigned long int)&ptr);

    valret[0] = ptr[0];  



    //    printf("Memory %x\n",&ptr);
    //        char c[8];
    //        printf("%x\n",memcpy(c,&ptr,8));
    //        printf("%lf %f\n",ptr,c);
    //        printf("1) %x\n",c[7]);
    //        printf("2) %x\n",c[6]);
    //        printf("3) %x\n",c[5]);
    //        printf("4) %x\n",c[4]);
    //        printf("5) %x\n",c[3]);
    //        printf("6) %x\n",c[2]);
    //        printf("7) %x\n",c[1]);
    //        printf("8) %x\n",c[0]);


    return;
  }

void getaddrint(int *ptr, int *addret, int *valret) 
  {

  //  Rprintf("Test %u %u\n", ptr, *ptr);



        addret[0] = (unsigned long int)(*(&ptr)); // adresse du 1er octet de ptr
    //add[0] = ptr; // adresse du 1er octet de ptr

    //   val[0] = ptr[0];  //*(ptr+2) *ptr
    
	//*add=ptr;
    *valret=*ptr;

    return;
  }

void writeataddrdble(int *addr,double *newval) 
{

     double *p;

    // &p = adresse de p
    // p = addresse vers laquelle pointe p
    // *p = valeur de la case à l'adresse p

    p = (double *)(unsigned long int)addr[0]; // Identique à p = *addr; mais sans warning à la compilation

    *p = (double)(newval[0]); // Identique à *p = *newval; mais sans warning à la compilation

//    Rprintf("%p %d %i\n", &p, p, *p);

  return;

}

void writeataddrint(int *addr,int *newval) 
{

     int *p;

    // &p = adresse de p
    // p = addresse vers laquelle pointe p
    // *p = valeur de la case à l'adresse p

    p = (int *)(unsigned long int)addr[0]; // Identique à p = *addr; mais sans warning à la compilation

    *p = (int)(newval[0]); // Identique à *p = *newval; mais sans warning à la compilation

//    Rprintf("%p %d %i\n", &p, p, *p);

  return;

}

