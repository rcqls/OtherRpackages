params <- function(obj,...) UseMethod("params")

cumulative.density <- function(x,...) UseMethod("cumulative.density") 

inverse.cumulative.density <- function(x,...) UseMethod("inverse.cumulative.density") 

density.derivative <- function(x,...) UseMethod("density.derivative") 

density.param.derivative <- function(x,...) UseMethod("density.param.derivative") 

cumulative.density.param.derivative <- function(x,...) UseMethod("cumulative.density.param.derivative") 

virtual.age <- function(x,...) UseMethod("virtual.age") 

inverse.virtual.age <- function(x,...) UseMethod("inverse.virtual.age")

virtual.age.derivative <- function(x,...) UseMethod("virtual.age.derivative")