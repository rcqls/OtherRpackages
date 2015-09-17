#ifndef RCPP_SIM_VAM_H
#define RCPP_SIM_VAM_H
#include <Rcpp.h>
#include "rcpp_vam_models.h"

using namespace Rcpp ;

class SimVam { 

public:

    SimVam(List models_) {
        set_models(models_);
        cache=new VamCache(2);
    };

    ~SimVam() {
        delete cache;
    };

    void set_models(List models_) {
        models=new VamModelList(models_,cache);
    }

    VamCache* cache;

private:

    VamModelList* models;

};

#endif //RCPP_SIM_VAM_H