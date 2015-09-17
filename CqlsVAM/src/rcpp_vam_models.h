#ifndef RCPP_VAM_MODEL_H
#define RCPP_VAM_MODEL_H
#include <Rcpp.h>

using namespace Rcpp ;

class VamCache {
public:

	VamCache(int nbCM_) {
		nbCM=nbCM_;
		dVright=new double[nbCM+1];
		dVleft=new double[nbCM+1];
		dS1=new double[nbCM+2];
		dS2=new double[nbCM+2];
	};

	VamCache(List data,int nbCM_) {
		time = data["Time"]; type = data["Type"];
		nbCM=nbCM_;
		dVright=new double[nbCM+1];
		dVleft=new double[nbCM+1];
		dS1=new double[nbCM+2];
		dS2=new double[nbCM+2];
	};

	~VamCache() {
		delete[] dVright;
		delete[] dVleft;
		delete[] dS1;
		delete[] dS2;
	};

	int k,nbCM;

	NumericVector time, type;

	double Vleft, Vright, dS3;

	double *dVleft, *dVright, *dS1, *dS2;

	void initMLE() {
		int i;
		k=0;
		Vleft=0;
		Vright=0;
		dS3=0;
		dS1[0]=0;
		for (i=0;i<nbCM+1;i++) {
			dVright[i]=0;
			dVleft[i]=0;
			dS1[i+1]=0;
			dS2[i+1]=0;
		}
		
	};
};

class VamModel {
public:
    VamModel(VamCache* cache_) {
    	cache = cache_;
    }

    virtual ~VamModel() {};

    virtual NumericVector get_params() = 0;

    virtual  void set_params(NumericVector pars) = 0;
 
    virtual void update(bool with_gradient) = 0;

    virtual double virtual_age(double x) = 0;

    virtual double virtual_age_derivative(double x) = 0;

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

    void set_params(NumericVector pars) {
    	rho=pars[0];
    }

    void update(bool with_gradient) {

    }

    double virtual_age(double time) {
		//max(0.0000001,obj$vam$cache$Vright+time-obj$vam$data$Time[obj$vam$cache$k])
    	return cache -> Vright + time  - cache->time[cache->k];
    }

    double virtual_age_derivative(double x) {
    	return 0;
    }

    double virtual_age_inverse(double x) {
    	return 0;
    }

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

    void set_params(NumericVector pars) {
    	rho=pars[0];
    }

    void update(bool with_gradient) {

    }

    double virtual_age(double time) {
		//max(0.0000001,obj$vam$cache$Vright+time-obj$vam$data$Time[obj$vam$cache$k])
    	return cache -> Vright + time  - cache->time[cache->k];
    }

    double virtual_age_derivative(double x) {
    	return 0;
    }

    double virtual_age_inverse(double x) {
    	return 0;
    }

private:

	double rho;

};

class VamModelList {//List of ModelBase (heterogeneous terms) 
public:
    VamModelList(List models_,VamCache* cache) {

        for(
            List::iterator lit=models_.begin();
            lit != models_.end();
            ++lit
        ) {
        	VamModel*  vam=new ARA1(1.0,cache);
            model_list.push_back(vam);
        }
    }

    ~VamModelList() {
    	for(
    		std::vector<VamModel*>::iterator vit=model_list.begin();
    		vit != model_list.end();
            ++vit
        ) {
    		delete *vit;
    	}

    }

    VamModel* operator[](const int i) {
    	return model_list[i];
    }

protected:

    std::vector<VamModel*> model_list; //model list
     
};





#endif //RCPP_VAM_MODEL_H