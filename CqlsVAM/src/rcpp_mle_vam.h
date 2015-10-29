#ifndef RCPP_MLE_VAM_H
#define RCPP_MLE_VAM_H
#include <Rcpp.h>
#include "rcpp_vam_cache.h"

using namespace Rcpp ;

class MLEVam { 

public:

    MLEVam(List model_,List data_) {
        cache=new VamCache(model_,data_);
        dS1=new double[cache->nbPM+2];
        dS2=new double[cache->nbPM+2];
    }

    ~MLEVam() {
        delete cache;
        delete[] dS1;
        delete[] dS2;
    };

    void set_data(List data_) {
        cache->set_data(data_);
    }

    NumericVector get_params() {
        return cache->get_params();
    }

    void set_params(NumericVector pars) {
        cache->set_params(pars);
    }


    void contrast_for_current_system() {
    	init_mle_vam_for_current_system(false);
		int n=(cache->time).size() - 1;
		while(cache->k < n) {
			contrast_update_for_current_system(false);
			// previous model for the next step
			int type=cache->type[cache->k + 1 ];
			if(type < 0) type=0;
			//cache->indMode = (type < 0 ? 0 : type);
			cache->models->at(type)->update(false);
		}
        //cache updated for current system: S1,S2,S3
        S1 += cache->S1;S2 += cache->S2; S3 += cache->S3;

    }

    NumericVector contrast(NumericVector param) {
        NumericVector res(1);

        init_mle_vam(false);
        cache->set_params(param);
        contrast_for_current_system();
        //only if multi-system
        for(int i=1;i<cache->nb_system;i++) {
            //printf("system %d\n",i);
            cache->select_data(i);
            contrast_for_current_system();
        }

        // log-likelihood (with constant +S3*(log(S3)-1))
        res[0]=-log(S1) * S3 + S2 +S3*(log(S3)-1);

        return res;
    }

    void gradient_for_current_system() {
    	init_mle_vam_for_current_system(true);
    	int n=(cache->time).size() - 1;
    	while(cache->k < n) {
    		gradient_update_for_current_system();
    		int type=cache->type[cache->k + 1 ];
			if(type < 0) type=0;
			//cache->indMode = (type < 0 ? 0 : type);
			cache->models->at(type)->update(true);
    	}
        //cache updated for current system: S1,S2,S3,dS1,dS2
        S1 += cache->S1;S2 += cache->S2; S3 += cache->S3;
        for(int i=0;i<cache->nbPM + 2;i++) {
            dS1[i] += cache->dS1[i]; dS2[i] += cache->dS2[i];
        }
    }

    NumericVector gradient(NumericVector param) {
        NumericVector res(cache->nbPM + 2);

        init_mle_vam(true);
        cache->set_params(param);
        gradient_for_current_system();
        
        //only if multi-system
        for(int i=1;i<cache->nb_system;i++) {
            cache->select_data(i);
            gradient_for_current_system();
        }

        //compute gradient
        for(int i=0;i<cache->nbPM + 2;i++) {
            res[i] = -dS1[i]/S1 * S3 + dS2[i];
        }
        
        return res;
    }

    NumericVector get_alpha_est(NumericVector param) {
        NumericVector res(1);
        contrast(param); //To compute S1 and S3
        res[0]=S3/S1;
        return res;
    }

    VamCache* get_cache() {
    	return cache;
    }

private:

	VamCache* cache;

    double S1, S2, S3, *dS1, *dS2;//Accumulator!

    void init_mle_vam(bool with_gradient) {
        S1 = 0; S2 = 0; S3 = 0;
        if(with_gradient) {
            for(int i=0;i<cache->nbPM + 2;i++) {
                dS1[i] = 0; dS2[i] = 0;
            }
        }
    }

    void init_mle_vam_for_current_system(bool with_gradient) {
    	cache->Vright = 0; //100000.;
    	cache->k=0;
    	cache->idMod=0; //id of current model
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

    void contrast_update_for_current_system(bool with_gradient) {
    	cache->update_Vleft(with_gradient);
    	cache->hVleft=cache->family->density(cache->Vleft);
    	cache->indType = ((cache->type)[(cache->k) + 1] < 0 ? 1.0 : 0.0);
    	// printf("HVleft:%d,%lf,%lf\n",cache->k,cache->Vleft,cache->family->cumulative_density(cache->Vleft));
    	// printf("HVright:%lf,%lf\n",cache->Vright,cache->family->cumulative_density(cache->Vright));
    	// printf("S1:%lf\n",cache->S1);
    	// printf("indType,S2,hVleft:%lf,%lf,%lf\n",cache->indType,cache->S1,cache->hVleft);
    	cache->S1 += cache->family->cumulative_density(cache->Vleft) - cache->family->cumulative_density(cache->Vright);
    	cache->S2 += log(cache->hVleft)* cache->indType;
    	//for(int i=0;i<(cache->nbPM)+2;i++) cache->dS1[i] += cdVleft[i] - cdVright[i];
    	//cache->dS1 += (models->at(0))
    }

    void gradient_update_for_current_system() {
    	contrast_update_for_current_system(true);
    	cache->dS1[0] += cache->family->cumulative_density_param_derivative(cache->Vleft) - cache->family->cumulative_density_param_derivative(cache->Vright);
    	cache->dS2[0] += cache->family->density_param_derivative(cache->Vleft)/cache->hVleft*cache->indType ;
    	double hVright=cache->family->density(cache->Vright);
    	double dhVleft=cache->family->density_derivative(cache->Vleft);
    	//printf("k:%d,hVright:%lf,dhVleft:%lf,indType:%lf\n",cache->k,hVright,dhVleft,cache->indType);
    	for(int i=0;i<(cache->nbPM)+1;i++) {
    		cache->dS1[i+1] += cache->hVleft * cache->dVleft[i] - hVright * cache->dVright[i];
    		//printf("dS1[%d]=(%lf,%lf,%lf),%lf,",i+1,cache->hVleft,cache->dVleft[i],cache->dVright[i],cache->dS1[i+1]);
    		cache->dS2[i+1] +=  dhVleft * cache->dVleft[i]/cache->hVleft * cache->indType;
    		//printf("dS2[%d]=%lf,",i+1,cache->dS2[i+1]);
    	}
    	//printf("\n");
    }

};

#endif //RCPP_SIM_VAM_H