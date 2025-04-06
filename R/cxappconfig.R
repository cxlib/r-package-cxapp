#' Utility function to implement a global config object 
#' 
#' @param x Configuration object
#' 
#' @returns Configuration object
#' 
#' @description
#' Configurations are used extensively throughout the cxapp package and importing
#' configurations from property files every time a configuration is required will
#' include unnecessary processing for a very static reference.
#' 
#' The function will return the \emph{cached} \link[cxapp]{cxapp_config}
#' object from the global environment.
#' 
#' The configuration object is `.cxapp.wrkcache.appconfig` in the
#' \link[base]{.GlobalEnv} environment.
#' 
#' 
#' If a configuration object is specified as input `x`, the global configuration
#' is updated before it is returned.
#' 
#'   
#' @export

.cxappconfig <- function( x ) {
  
  # -- configuration not specified as input
  
  if ( missing(x) || any(is.na(x)) ) {
  
    # - use default configuration if cached configuraion does not exist  
    if ( ! base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      assign( ".cxapp.wrkcache.config", cxapp::cxapp_config(), envir = .GlobalEnv )
    
    # - return configuration (pass-by-value for now)
    return(invisible( base::get( ".cxapp.wrkcache.config", envir = .GlobalEnv ) ))
  }

  
  # -- configuration specified as input
    
  if ( ! inherits( x, "cxapp_config") )
    stop( "The specified input is not valid" )
  
  
  # -- cache configuration
  assign( ".cxapp.wrkcache.config", x, envir = .GlobalEnv )
  
  
  # -- return configuration
  return(invisible( base::get( ".cxapp.wrkcache.config", envir = .GlobalEnv ) ))
}
