#include "rcpp_vam_model.h"
#include "rcpp_family_model.h"
#include "rcpp_maintenance_model.h"

using namespace Rcpp ;

NumericVector VamModel::get_params() {
	NumericVector pars(nbPM+3);
	NumericVector fam=family->get_params();
	pars[0]=fam[0];pars[1]=fam[1];
	for(int i=0;i<nbPM + 1;i++) {
		MaintenanceModel* vam=models->at(i);
		NumericVector res=vam->get_params();
		pars[2+i]=vam->get_params()[0];
	}
	return pars;
}

void VamModel::set_params(NumericVector pars) {
	family->set_params(pars[0],pars[1]);
	for(int i=0;i<nbPM + 1;i++) {
		MaintenanceModel* vam=models->at(i);
		vam->set_params(pars[2+i]);
	}
}

void VamModel::update_Vleft(bool with_gradient) {
	/*if(model->k < 10) printf("Vleft:%lf\n", model->Vleft);*/
	Vleft =(models->at(idMod))->virtual_age(time[k+1]);
	//printf("Vleft:%lf\n", model->Vleft);
	if(with_gradient) dVleft=(models->at(idMod))->virtual_age_derivative(time[k+1]);
}

void VamModel::set_data(List data_) {
	data=data_;
	nb_system=data.size();
	//printf("Number of systems: %d\n",nb_system);
	select_data(0);//default when only one system no need to 
}

void VamModel::select_data(int i) {
	List data2=data[i];
	time = data2["Time"]; type = data2["Type"];
}


void VamModel::set_models(List models_) {
    models=new MaintenanceModelList(models_,this);
}

void VamModel::set_family(List family_) {
	family=newFamilyModel(family_);
}

void VamModel::set_maintenance_policy(List maintenance_policy_) {
	maintenance_policy=newMaintenancePolicy(maintenance_policy_);
	if(maintenance_policy==NULL) printf("maintenance_policy is NULL\n");
};

void VamModel::init(List model_) {
	List models_=model_["models"];
	List family_=model_["family"];
	List maintenance_policy_=model_["pm.policy"];
    set_models(models_);
	nbPM=models->size()-1;
	
	set_family(family_);
	set_maintenance_policy(maintenance_policy_);

	S1=0;S2=0;S3=0;
	Vleft=0;Vright=0;
	hVleft=0;
	dVright=new double[nbPM+1];
	dVleft=new double[nbPM+1];
	dS1=new double[nbPM+2];
	dS2=new double[nbPM+2];
};

