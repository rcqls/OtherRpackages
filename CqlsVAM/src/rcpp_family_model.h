#ifndef RCPP_FAMILY_MODEL_H
#define RCPP_FAMILY_MODEL_H
#include <Rcpp.h>

using namespace Rcpp ;

class FamilyModel {
public:

	FamilyModel() {};

	virtual ~FamilyModel() {};

	virtual double density(double x) = 0; 

	virtual double cummulative_density(double x) = 0;

	virtual double inverse_cummulative_density(double x) = 0;

	virtual double density_derivative(double x) = 0;

	virtual double density_param_derivative(double x) = 0;

	virtual double cummulative_density_param_derivative(double x) = 0;
};

class WeibullFamilyModel {
public:
    WeibullFamilyModel(double alpha_, double beta_) {
    	alpha=alpha_;beta=beta_;
    }

    ~WeibullFamilyModel() {};

    double alpha, beta;

    double density(double x) {
    	return alpha*beta*pow(x,beta-1);
    }

	virtual double cummulative_density(double x) {
		return alpha*pow(x,beta);
	}

	virtual double inverse_cummulative_density(double x) {
		 return pow(x/alpha,1/beta);

	}

	virtual double density_derivative(double x) {
		return alpha*beta*(beta-1)*pow(x,beta-2);
	}

	virtual double density_param_derivative(double x) {
		return (x==0 ? 0 : alpha*(1+beta*log(x))*pow(x,beta-1));
	}

	virtual double cummulative_density_param_derivative(double x) {
		return (x==0 ? 0 : alpha*log(x)*pow(x,beta));
	}
 
};

#endif //RCPP_FAMILY_MODEL_H