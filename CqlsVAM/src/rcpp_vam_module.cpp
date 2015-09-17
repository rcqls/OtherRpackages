#include "rcpp_vam_module.h"


RCPP_MODULE(vam_module) {
    class_<SimVam>( "SimVamCpp" )
    .constructor<List>();

    class_<MLEVam>( "MLEVamCpp" )
    .constructor<List,List>();
}