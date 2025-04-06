#' Utility function representing a global application cache object
#'
#' @param x Application cache object
#'
#' @return Application cache object
#'
#' @description
#' Caching is used extensively throughout apps using the cxapp package and
#' importing cache configurations from property files every time the cache
#' is required will include unnecessary processing for a very static reference.
#'
#' The function will return the \emph{cached} \link[cxapp]{cxapp_applicationcache}
#' object from the global environment.
#'
#' The configured application cache object is `.cxapp.wrkcache.appcache` in the
#' \link[base]{.GlobalEnv} environment.
#'
#' If an application cache object is specified as input `x`, the global
#' environment cache object is updated before it is returned.
#'
#'
#' @export

.cxappcache <- function(x) {


  # -- cache object not specified as input

  if ( missing(x) || any(is.na(x)) ) {

    # - use default application cache if "cached" application cache does not exist
    if ( ! base::exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      assign( ".cxapp.wrkcache.appcache", cxapp::cxapp_applicationcache(), envir = .GlobalEnv )

    # - return configuration (pass-by-value for now)
    return(invisible( base::get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) ))
  }


  # -- configuration specified as input

  if ( ! inherits( x, "cxapp_applicationcache") )
    stop( "The specified input is not valid" )


  # -- cache configuration
  assign( ".cxapp.wrkcache.appcache", x, envir = .GlobalEnv )


  # -- return configuration
  return(invisible( base::get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) ))
}