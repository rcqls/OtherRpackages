#include "rcpp_vam_cache.h"
#include "rcpp_family_model.h"
#include "rcpp_vam_models.h"

using namespace Rcpp ;

NumericVector VamCache::get_params() {
	NumericVector pars(nbPM+3);
	NumericVector fam=family->get_params();
	pars[0]=fam[0];pars[1]=fam[1];
	for(int i=0;i<nbPM + 1;i++) {
		VamModel* vam=models->at(i);
		NumericVector res=vam->get_params();
		pars[2+i]=vam->get_params()[0];
	}
	return pars;
}

void VamCache::set_params(NumericVector pars) {
	family->set_params(pars[0],pars[1]);
	for(int i=0;i<nbPM + 1;i++) {
		VamModel* vam=models->at(i);
		vam->set_params(pars[2+i]);
	}
}


void VamCache::set_models(List models_) {
    models=new VamModelList(models_,this);
}

void VamCache::set_family(List family_) {
	family=newFamilyModel(family_);
}

void VamCache::init(List model_) {
	List models_=model_["models"];
	List family_=model_["family"];
    set_models(models_);
	nbPM=models->size()-1;
	
	set_family(family_);

	S1=0;S2=0;S3=0;
	Vleft=0;Vright=0;
	hVleft=0;
	dVright=new double[nbPM+1];
	dVleft=new double[nbPM+1];
	dS1=new double[nbPM+2];
	dS2=new double[nbPM+2];
};

