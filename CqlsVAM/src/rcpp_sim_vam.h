#ifndef RCPP_SIM_VAM_H
#define RCPP_SIM_VAM_H
#include <Rcpp.h>
#include "rcpp_vam_models.h"

using namespace Rcpp ;

class SimVam { 

public:

    SimVam(List model_) {
        cache=new VamCache(model_);
    };

    ~SimVam() {
        delete cache;
    };

    DataFrame simulate(int nbsim) {
        init(nbsim);

        while(cache->k < nbsim) {
            //### modAV <- if(Type[k]<0) obj$vam.CM[[1]]$model else obj$vam.PM$models[[obj$data$Type[k]]]
            //# Here, obj$cache$k means k-1
            //#print(c(obj$cache$Vleft,obj$cache$Vright))
            double timePM, timeCM = cache->models->at(cache->idMod)->virtual_age_inverse(cache->family->inverse_cummulative_density(cache->family->cummulative_density(cache->models->at(cache->idMod)->virtual_age(cache->time[cache->k]))-log(runif(1))[0]));
            int idMod;
            List timeAndTypePM;
            if(cache->maintenance_policy != NULL) {
                timeAndTypePM = cache->maintenance_policy->update(cache->time[cache->k]); //# Peut-être ajout Vright comme argument de update
                
                NumericVector tmp=timeAndTypePM["time"];
                timePM=tmp[0];
            }
            if(cache->maintenance_policy == NULL || timeCM < timePM) {
                cache->time[cache->k + 1]=timeCM;
                cache->type[cache->k + 1]=-1;
                idMod=0;
            } else {
                cache->time[cache->k + 1]=timePM;
                NumericVector tmp2=timeAndTypePM["type"];
                int typePM=tmp2[0];
                cache->type[cache->k + 1]=typePM;
                idMod=timeAndTypePM["type"];
            }
            //# used in the next update
            cache->update_Vleft(false);
            //# update the next k, and save model in cache too!
            cache->models->at(idMod)->update(false);
        }

        return DataFrame::create(_["Time"]=cache->time,_["Type"]=cache->type);
    }

    VamCache* get_cache() {
        return cache;
    }

    NumericVector get_params() {
        return cache->get_params();
    }

    void set_params(NumericVector pars) {
        cache->set_params(pars);
    }


private:
    VamCache* cache;

    void init(int nbsim) {
        cache->Vright=0;
        cache->k=0;
        cache->idMod=0; // Since no maintenance is possible!
        cache->time=rep(0,nbsim+1);
        cache->type= rep(1,nbsim+1);
    } 

};

#endif //RCPP_SIM_VAM_H