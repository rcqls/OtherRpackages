#ifndef RCPP_FAMILY_MODEL_H
#define RCPP_FAMILY_MODEL_H
#include <Rcpp.h>

using namespace Rcpp ;

class FamilyModel {
public:

	FamilyModel() {};

	virtual ~FamilyModel() {};

	virtual NumericVector get_params() = 0;

	virtual void set_params(double, double) = 0;

	virtual double density(double x) = 0; 

	virtual double cummulative_density(double x) = 0;

	virtual double inverse_cummulative_density(double x) = 0;

	virtual double density_derivative(double x) = 0;

	virtual double density_param_derivative(double x) = 0;

	virtual double cummulative_density_param_derivative(double x) = 0;
};

class WeibullFamilyModel : public FamilyModel {
public:
    WeibullFamilyModel(double alpha_, double beta_) {
    	alpha=alpha_;beta=beta_;
    }

    ~WeibullFamilyModel() {};

    double alpha, beta;

    NumericVector get_params() {
    	NumericVector out(2);
    	out[0]=alpha;out[1]=beta;
    	return out;
    }

    void set_params(double alpha_, double beta_) {
    		alpha=alpha_;beta=beta_;
    }

    double density(double x) {
    	return alpha*beta*pow(x,beta-1);
    }

	double cummulative_density(double x) {
		return alpha*pow(x,beta);
	}

	double inverse_cummulative_density(double x) {
		 return pow(x/alpha,1/beta);

	}

	double density_derivative(double x) {
		return alpha*beta*(beta-1)*pow(x,beta-2);
	}

	double density_param_derivative(double x) {
		return (x==0 ? 0 : alpha*(1+beta*log(x))*pow(x,beta-1));
	}

	double cummulative_density_param_derivative(double x) {
		return (x==0 ? 0 : alpha*log(x)*pow(x,beta));
	}
 
};

FamilyModel* newFamilyModel(List family);

#endif //RCPP_FAMILY_MODEL_H