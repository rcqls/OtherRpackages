#ifndef RCPP_VAM_MODEL_H
#define RCPP_VAM_MODEL_H
#include <Rcpp.h>
#include "rcpp_vam_cache.h"

using namespace Rcpp ;

//Forward declarations
class VamModel;

//Effective declarations
class VamModelList {//List of ModelBase (heterogeneous terms) 
public:
    VamModelList(List models_,VamCache* cache);

    ~VamModelList();

    VamModel* at(int i) {
    	return model_list[i];
    }

    int size() {
    	return model_list.size();
    }


protected:

    std::vector<VamModel*> model_list; //model list
     
};


class VamModel {
public:
    VamModel(VamCache* cache_) {
    	cache = cache_;
    }

    virtual ~VamModel() {};

    virtual NumericVector get_params() = 0;

    virtual  void set_params(double) = 0;
 
    virtual void update(bool with_gradient) = 0;

    virtual double virtual_age(double x) = 0;

    virtual double* virtual_age_derivative(double x) = 0;

    virtual double virtual_age_inverse(double x) = 0;

    VamCache* cache;

};

class ARA1 : public VamModel { 

public:

    ARA1(double rho_,VamCache* cache_) : VamModel(cache_) {
    	rho = rho_;
    }

    NumericVector get_params() {
    	NumericVector out(1);
    	out[0]=rho;
    	return out;
    }

    void set_params(double par) {
    	rho=par;
    }

    void update(bool with_gradient); 

    double virtual_age(double time);

    double* virtual_age_derivative(double x);

    double virtual_age_inverse(double x);
private:

	double rho;

};

class ARAInf : public VamModel { 

public:

    ARAInf(double rho_,VamCache* cache_) : VamModel(cache_) {
    	rho = rho_;
    }

    NumericVector get_params() {
    	NumericVector out(1);
    	out[0]=rho;
    	return out;
    }

    void set_params(double par) {
    	rho=par;
    }

    void update(bool with_gradient);

    double virtual_age(double time);

    double* virtual_age_derivative(double x);

    double virtual_age_inverse(double x);

private:

	double rho;

};

VamModel* newVamModel(List model,VamCache* cache);

#endif //RCPP_VAM_MODEL_H