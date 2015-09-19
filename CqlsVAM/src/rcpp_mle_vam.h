#ifndef RCPP_MLE_VAM_H
#define RCPP_MLE_VAM_H
#include <Rcpp.h>
#include "rcpp_vam_cache.h"

using namespace Rcpp ;

class MLEVam { 

public:

    MLEVam(List model_,List data_) {
        cache=new VamCache(model_,data_);
    }

    ~MLEVam() {
        delete cache;
    };


    NumericVector contrast(NumericVector param) {
    	NumericVector res;
    	return res;
    }

    NumericVector gradient(NumericVector param) {
    	NumericVector res;
    	return res;
    }

    VamCache* cache;

private:

    void update_Vleft(bool with_gradient) {
    	cache->Vleft =(cache->models->at(cache->modInd))->virtual_age(cache->time[cache->k+1]);
		if(with_gradient) cache->dVleft=(cache->models->at(cache->modInd))->virtual_age_derivative(cache->time[cache->k+1]);
    }

    void init_mle_vam(bool with_gradient) {
    	cache->Vright = 0;
    	cache->k=0;
    	cache->modInd=0;
    	cache->S1 = 0;
    	cache->S2 = 0;
    	cache->S3 = 0;for(int i=0;i<cache->type.size();i++) if(cache->type[i] < 0) (cache->S3) += 1; //TO COMPUTE from cache->type
    	if(with_gradient) {
    		cache->dVright[0]=0;
    		cache->dS1[0]=0;cache->dS1[1]=0;
    		cache->dS2[0]=0;cache->dS2[1]=0;
    		for(int i=0;i<(cache->nbPM)+1;i++) {
    			cache->dVright[i+1]=0;
    			cache->dS1[i+2]=0;
    			cache->dS2[i+2]=0;
    		}
    	}
    }

    void contrast_update(bool with_gradient) {
    	update_Vleft(with_gradient);
    	cache->hVleft=cache->family->density(cache->Vleft);
    	cache->indCM = ((cache->type)[(cache->k) + 1] < 0 ? 1.0 : 0.0);

    	cache->S1 += cache->family->cummulative_density(cache->Vleft) - cache->family->cummulative_density(cache->Vright);
    	cache->S2 += log(cache->hVleft)* cache->indCM;
    	//for(int i=0;i<(cache->nbPM)+2;i++) cache->dS1[i] += cdVleft[i] - cdVright[i];
    	//cache->dS1 += (models->at(0))
    }

    void gradient_update(bool with_gradient) {
    	contrast_update(with_gradient);
    	cache->dS1[0] += cache->family->cummulative_density_param_derivative(cache->Vleft) - cache->family->cummulative_density_param_derivative(cache->Vright);
    	cache->dS2[0] += cache->family->density_param_derivative(cache->Vleft)/cache->hVleft*cache->indCM ;
    	double hVright=cache->family->density(cache->Vright);
    	double dhVleft=cache->family->density_derivative(cache->Vleft);
    	for(int i=0;i<(cache->nbPM)+1;i++) {
    		cache->dS1[i+1] += cache->hVleft * cache->dVleft[i] - hVright * cache->dVright[i];
    		cache->dS2[i+1] +=  dhVleft * cache->dVleft[i]/cache->hVleft * cache->indCM;
    	}
    }

};

#endif //RCPP_SIM_VAM_H