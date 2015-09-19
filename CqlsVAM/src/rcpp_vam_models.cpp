#include "rcpp_vam_models.h"

using namespace Rcpp ;

//Forward declarations
//class VamCache;
//class VamModel;
//VamModel* newVamModel(List model,VamCache* cache);

//Effective declarations
VamModelList::VamModelList(List models_,VamCache* cache) {
    for(
        List::iterator lit=models_.begin();
        lit != models_.end();
        ++lit
    ) {
    	List model=*lit;
    	VamModel*  vam=newVamModel(model,cache);
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

}

double ARA1::virtual_age(double time) {
    //max(0.0000001,obj$vam$cache$Vright+time-obj$vam$data$Time[obj$vam$cache$k])
    return cache -> Vright + time  - cache->time[cache->k];
}

double* ARA1::virtual_age_derivative(double x) {
    return cache->dVright;
}

double ARA1::virtual_age_inverse(double x) {
    return 0;
}

void ARAInf::update(bool with_gradient) {

}

double ARAInf::virtual_age(double time) {
    //max(0.0000001,obj$vam$cache$Vright+time-obj$vam$data$Time[obj$vam$cache$k])
    return cache -> Vright + time  - cache->time[cache->k];
}

double* ARAInf::virtual_age_derivative(double x) {
    return cache->dVright;
}

double ARAInf::virtual_age_inverse(double x) {
    return 0;
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
