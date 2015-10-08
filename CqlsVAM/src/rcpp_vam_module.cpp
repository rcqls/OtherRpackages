#include "rcpp_vam_module.h"


RCPP_MODULE(vam_module) {
	class_<VamCache>("VamCacheCpp")
	.constructor<List>()
	.constructor<List,List>()
	.field( "time", &VamCache::time, "time" )
	.field( "type", &VamCache::type, "type" )
	.method( "get", &VamCache::get, "get many informations" )
	.method( "family", &VamCache::get_family, "get family" )
	.method("get_params",&VamCache::get_params,"get params")
    .method("set_params",&VamCache::set_params,"set params")
	;

    class_<SimVam>( "SimVamCpp" )
    .constructor<List>()
    .method("cache",&SimVam::get_cache,"cache accessor")
    .method("simulate",&SimVam::simulate,"simulate")
    .method("get_params",&SimVam::get_params,"get params")
    .method("set_params",&SimVam::set_params,"set params")
    ;

    class_<MLEVam>( "MLEVamCpp" )
    .constructor<List,List>()
    .method("cache",&MLEVam::get_cache,"cache accessor")
    .method("set_data",&MLEVam::set_data,"set data")
    .method("contrast",&MLEVam::contrast,"compute contrast")
    .method("gradient",&MLEVam::gradient,"compute gradient")
    .method("alpha_est",&MLEVam::get_alpha_est,"get alpha estimation")
    .method("get_params",&MLEVam::get_params,"get params")
    .method("set_params",&MLEVam::set_params,"set params")
    ;

    class_<FamilyModel>("FamilyModelCpp")
    .method("density",&FamilyModel::density,"density")
    .method("cummulative_density",&FamilyModel::cummulative_density,"cummulative density")
    .method("density_derivative",&FamilyModel::density_derivative,"density_derivative")
    .method("inverse_cummulative_density",&FamilyModel::inverse_cummulative_density,"inverse cummulative density")
    .method("density_param_derivative",&FamilyModel::density_param_derivative,"density derivative with respect to beta")
    .method("cummulative_density_param_derivative",&FamilyModel::cummulative_density_param_derivative,"cummulative density derivative with respect to beta")
    ;

    function( "newMaintenancePolicy", &newMaintenancePolicy );

    class_<MaintenancePolicy>("MaintenancePolicyCpp")
    .method("update",&MaintenancePolicy::update,"update")
    .method("get_params",&MaintenancePolicy::get_params,"get params")
    .method("set_params",&MaintenancePolicy::set_params,"set params")
    ;

    function( "newMaintenancePolicy", &newMaintenancePolicy );


}