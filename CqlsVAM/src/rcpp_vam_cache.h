#ifndef RCPP_VAM_CACHE_H
#define RCPP_VAM_CACHE_H
#include <Rcpp.h>
#include "rcpp_family_model.h"

using namespace Rcpp ;

class VamModelList;

class VamCache {
public:

	VamCache(List model_) {
        init(model_);
    };

	VamCache(List model_,List data) {
		init(model_);
		time = data["Time"]; type = data["Type"];
	};

	~VamCache() {
		delete[] dVright;
		delete[] dVleft;
		delete[] dS1;
		delete[] dS2;
		//delete models;
	};

	int k,nbPM,idMod;

	NumericVector time, type;

	double S1, S2, S3, indType;

	double Vleft, Vright, hVleft;

	double *dVleft, *dVright, *dS1, *dS2;

	VamModelList* models;

	FamilyModel* family;

	void initMLE() {
		int i;
		k=0;
		Vleft=0;
		Vright=0;
		indType=0;hVleft=0;
		dS1[0]=0;dS2[0]=0;
		for (i=0;i<nbPM+1;i++) {
			dVright[i]=0;
			dVleft[i]=0;
			dS1[i+1]=0;
			dS2[i+1]=0;
		}
		
	};

	FamilyModel* get_family() {
		return family;
	}

	List get() {
		List ret;
		ret["S1"]=NumericVector::create(S1);ret["S2"]=NumericVector::create(S2);ret["S3"]=NumericVector::create(S3);
		ret["Vright"]=NumericVector::create(Vright);ret["Vleft"]=NumericVector::create(Vleft);
		NumericVector dS1R(nbPM+2),dS2R(nbPM+2),dVrightR(nbPM+1),dVleftR(nbPM+1);
		ret["dS1"]=dS1R;ret["dS2"]=dS2R;
		ret["dVright"]=dVrightR;ret["dVleft"]=dVleftR;
		dS1R[0]=dS1[0];dS2R[0]=dS2[0];
		for (int i=0;i<nbPM+1;i++) {
			dVrightR[i]=dVright[i];
			dVleftR[i]=dVleft[i];
			dS1R[i+1]=dS1[i+1];
			dS2R[i+1]=dS2[i+1];
		}
		return ret;
	}

	NumericVector get_params();

    void set_params(NumericVector pars);


private:
	void set_models(List models_);

    void set_family(List family_);

	void init(List model_);
};

#endif