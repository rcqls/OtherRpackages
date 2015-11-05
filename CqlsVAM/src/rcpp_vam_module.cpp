#include "rcpp_vam_module.h"


RCPP_MODULE(vam_module) {
	class_<VamModel>("VamModelCpp")
	.constructor<List>()
	.constructor<List,List>()
	.field( "time", &VamModel::time, "time" )
	.field( "type", &VamModel::type, "type" )
	.method( "get", &VamModel::get, "get many informations" )
	.method( "family", &VamModel::get_family, "get family" )
	.method("get_params",&VamModel::get_params,"get params")
    .method("set_params",&VamModel::set_params,"set params")
	;

    class_<SimVam>( "SimVamCpp" )
    .constructor<List>()
    .method("model",&SimVam::get_model,"model accessor")
    .method("simulate",&SimVam::simulate,"simulate")
    .method("get_params",&SimVam::get_params,"get params")
    .method("set_params",&SimVam::set_params,"set params")
    ;

    class_<MLEVam>( "MLEVamCpp" )
    .constructor<List,List>()
    .method("model",&MLEVam::get_model,"model accessor")
    .method("set_data",&MLEVam::set_data,"set data")
    .method("contrast",&MLEVam::contrast,"compute contrast")
    .method("gradient",&MLEVam::gradient,"compute gradient")
    .method("alpha_est",&MLEVam::get_alpha_est,"get alpha estimation")
    .method("get_params",&MLEVam::get_params,"get params")
    .method("set_params",&MLEVam::set_params,"set params")
    ;

    class_<FamilyModel>("FamilyModelCpp")
    .method("density",&FamilyModel::density,"density")
    .method("cumulative_density",&FamilyModel::cumulative_density,"cumulative density")
    .method("density_derivative",&FamilyModel::density_derivative,"density_derivative")
    .method("inverse_cumulative_density",&FamilyModel::inverse_cumulative_density,"inverse cumulative density")
    .method("density_param_derivative",&FamilyModel::density_param_derivative,"density derivative with respect to beta")
    .method("cumulative_density_param_derivative",&FamilyModel::cumulative_density_param_derivative,"cumulative density derivative with respect to beta")
    ;

    function( "newMaintenancePolicy", &newMaintenancePolicy );

    class_<MaintenancePolicy>("MaintenancePolicyCpp")
    .method("update",&MaintenancePolicy::update,"update")
    .method("get_params",&MaintenancePolicy::get_params,"get params")
    .method("set_params",&MaintenancePolicy::set_params,"set params")
    ;

    function( "newMaintenancePolicy", &newMaintenancePolicy );


}