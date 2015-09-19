#ifndef RCPP_SIM_VAM_H
#define RCPP_SIM_VAM_H
#include <Rcpp.h>
#include "rcpp_vam_models.h"

using namespace Rcpp ;

class SimVam { 

public:

    SimVam(VamCache* cache_) {
        cache=cache_;
    };

    ~SimVam() {
    };


private:
    VamCache* cache;

};

#endif //RCPP_SIM_VAM_H