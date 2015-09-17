#ifndef RCPP_MLE_VAM_H
#define RCPP_MLE_VAM_H
#include <Rcpp.h>
#include "rcpp_vam_models.h"
#include "rcpp_family_model.h"

using namespace Rcpp ;

class MLEVam { 

public:

    MLEVam(List models_, List data_) {
        set_models(models_);
        cache=new VamCache(data_,2);
    }

    ~MLEVam() {
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