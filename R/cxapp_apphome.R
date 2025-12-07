#' Utility function to get app home configuration if set
#' 
#' @return The app home  directory path
#' 
#' @description
#' The utility function searches for the app home directory.
#' 
#' The function first performs a search for the environmental variable 
#' `APP_HOME`. If more than one environmental variable is found with a case
#' insensitive match to `APP_HOME` (on some systems variables names are case
#' sensitive), an error is returned. 
#' 
#' If the environmental variable `APP_HOME` is not set, the current working
#' directory \link[base]{getwd} is returned. 
#' 
#' 
#' @examples
#' 
#' # -- APP_HOME (upper case)
#' Sys.setenv( "APP_HOME" = base::tempdir() )
#' myapp_home <- cxapp::cxapp_apphome()
#' 
#' Sys.unsetenv( "APP_HOME" )
#' 
#' 
#' # -- APP_HOME (lower case)
#' Sys.setenv( "app_home" = base::tempdir() )
#' myapp_home <- cxapp::cxapp_apphome()
#' 
#' Sys.unsetenv( "app_home" )
#' 
#' 
#' # -- APP_HOME (mixed case)
#' Sys.setenv( "App_hOmE" = base::tempdir() )
#' myapp_home <- cxapp::cxapp_apphome()
#' 
#' Sys.unsetenv( "App_hOmE" )
#' 
#' # -- Subdirectory of APP_HOME (upper case as example)
#' Sys.setenv( "APP_HOME" = base::tempdir() )
#' 
#' myapp_home <- cxapp::cxapp_apphome( "/config/here" )
#' myapp_home <- cxapp::cxapp_apphome( "config/here" )
#' 
#' Sys.unsetenv( "APP_HOME" )
#' 
#' 
#' 
#' 
#' @export


cxapp_apphome <- function() {
  
  # -- environmental vars
  lst_envnames <- base::names( Sys.getenv() )
  
  
  # -- app_home not set
  if ( ! "app_home" %in% base::tolower(lst_envnames) )
    return(invisible( cxapp::cxapp_standardpath( base::getwd() ) ))
  
  
  # -- identify APP_HOME
  apphome_envname <- lst_envnames[ grepl( "APP_HOME", lst_envnames, ignore.case = TRUE) ]
  
  if ( length(apphome_envname) > 1 )
    stop( "One or more environmental variables are names APP_HOME (case insentitive) [", paste(apphome_envname, collapse = ", "), "]" )
  
  if ( length(apphome_envname) == 1)
    return(invisible( cxapp::cxapp_standardpath( Sys.getenv( apphome_envname, unset = base::getwd()) ) ))

  
  # -- if all else fails for some reason
  return(invisible( cxapp::cxapp_standardpath( base::getwd() ) ))
}
  
  