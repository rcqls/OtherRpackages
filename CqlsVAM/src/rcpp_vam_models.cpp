#include "rcpp_vam_models.h"

using namespace Rcpp ;

//Forward declarations
//class VamCache;
//class VamModel;
//VamModel* newVamModel(List model,VamCache* cache);

//Effective declarations
VamModelList::VamModelList(List models_,VamCache* cache) {
    int i=0;
    for(
        List::iterator lit=models_.begin();
        lit != models_.end();
        ++lit
    ) {
    	List model=*lit;
    	VamModel*  vam=newVamModel(model,cache);
        vam->set_id(i++);
        model_list.push_back(vam);
    }
}

VamModelList::~VamModelList() {
	for(
		std::vector<VamModel*>::iterator vit=model_list.begin();
		vit != model_list.end();
        ++vit
    ) {
		delete *vit;
	}

}

void ARA1::update(bool with_gradient) {
    /*# next step
    obj$vam$cache$k <- obj$vam$cache$k + 1
    # At T(k)
    obj$vam$cache$Vright <- obj$vam$cache$Vright + (1-obj$rho)*(dVlr <-(obj$vam$cache$Vleft-obj$vam$cache$Vright))
    if(with.gradient) {
        # only the rho parameters
        #obj$vam$cache$dVright <- obj$vam$cache$dVright + rep(0,1+length(obj$vam$vam.PM$models))
        i <- match(obj$id,seq(obj$vam$vam.PM$models),nomatch=0)+1
        obj$vam$cache$dVright[i] <- obj$vam$cache$dVright[i] - dVlr
    }
    # save old model
    obj$cache$mod <- obj
    */
    cache->k += 1;
    double dVlr = cache->Vleft- cache->Vright;
    cache->Vright += (1-rho) * dVlr;
    if(with_gradient) {
        cache->dVright[id] += -dVlr;
    }
    cache->idMod = id;
}

double ARA1::virtual_age(double time) {
    //max(0.0000001,obj$vam$cache$Vright+time-obj$vam$data$Time[obj$vam$cache$k])
    //printf("virtual_age:%lf,%lf,%lf\n",cache -> Vright, time,cache->time[cache->k]);
    return cache -> Vright + time  - cache->time[cache->k];
}

double* ARA1::virtual_age_derivative(double x) {
    return cache->dVright;
}

double ARA1::virtual_age_inverse(double time) {
    return time+cache->time[cache->k] - cache->Vright;
}

void ARAInf::update(bool with_gradient) {
    cache->k += 1;
    cache->Vright = (1-rho) * cache->Vleft;
    if(with_gradient) {
        for(int i=0;i<cache->nbPM+1;i++) {
            cache->dVright[i] = (1-rho) * cache->dVright[i];
        }
        cache->dVright[id] = cache->dVright[id] - cache->Vleft;
    }
    // save old model
    cache->idMod = id;
}

double ARAInf::virtual_age(double time) {
    //max(0.0000001,obj$vam$cache$Vright+time-obj$vam$data$Time[obj$vam$cache$k])
    return cache -> Vright + time  - cache->time[cache->k];
}

double* ARAInf::virtual_age_derivative(double x) {
    return cache->dVright;
}

double ARAInf::virtual_age_inverse(double time) {
    return time+cache->time[cache->k] - cache->Vright;
}


VamModel* newVamModel(List model,VamCache* cache) {
	std::string name=model["name"];
	NumericVector params=model["params"];
	VamModel*  vam=NULL;
	if(name.compare("ARA1.va.model") == 0) {
		double rho=params[0];
		vam=new ARA1(rho,cache);
	} else if(name.compare("ARAInf.va.model") == 0) {
		double rho=params[0];
		vam=new ARAInf(rho,cache);
	}
	return vam;
}
