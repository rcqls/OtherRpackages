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

    //class_<SimVam>( "SimVamCpp" )
    //.constructor<List>()
    //;

    class_<MLEVam>( "MLEVamCpp" )
    .constructor<List,List>()
    .method("cache",&MLEVam::get_cache,"cache accessor")
    .method("contrast",&MLEVam::contrast,"compute contrast")
    .method("gradient",&MLEVam::gradient,"compute gradient")
    ;

    class_<FamilyModel>("FamilyModelCpp")
    .method("density",&FamilyModel::density,"density")
    .method("cummulative_density",&FamilyModel::cummulative_density,"cummulative density")
    .method("density_derivative",&FamilyModel::density_derivative,"density_derivative")
    .method("inverse_cummulative_density",&FamilyModel::inverse_cummulative_density,"inverse cummulative density")
    .method("density_param_derivative",&FamilyModel::density_param_derivative,"density derivative with respect to beta")
    .method("cummulative_density_param_derivative",&FamilyModel::cummulative_density_param_derivative,"cummulative density derivative with respect to beta")
    ;

    function( "newFamilyModel", &newFamilyModel );
}