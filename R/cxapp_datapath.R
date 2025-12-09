#' Utility function to get the path in the data area
#' 
#' @param ... Path elements
#' 
#' @return A vector of length one with the path 
#' 
#' @description
#' The utility function `cxapp_datapath` is synonymous with the function
#' \link[base]{file.path}.
#' 
#' The root of the data path is defined with the app property `APP.DATA`. If 
#' `APP.DATA` is not defined, If `APP.DATA` is not defined, a transient 
#' application cache directory `.application-data-<node>` is created in the R
#' session temporary directory \link[base]{tempdir}. 
#' 
#' The arguments in the call to the function `cxapp_datapath` are passed to
#' \link[base]{file.path}.  
#' 
#' 
#' @export


cxapp_datapath <- function( ... ) {

  
  # -- get the specified arguments remoing names
  # fxargs <- unlist(list( ... ), use.names = FALSE)
  fxargs <- list( ... )

  
  # -- path arguments
  fpath_args <- fxargs
  fpath_args[["fsep"]] <- "/"  # force delimiter
  

  # -- configuration
  cfg <- cxapp::cxapp_config()
  
  
  # -- default root path
  xroot <- cfg$option( "APP.DATA", unset = NA )
  
  if ( is.na(xroot) ) {
    
    # - transient root directory
    xroot <- cxapp::cxapp_standardpath( file.path( base::tempdir(), paste0( ".application-data-", cxapp::cxapp_appnode() ), fsep = "/" ) )
    
    if ( ! dir.exists(xroot) && ! dir.create( xroot, recursive = TRUE ) )
      stop( "Could not create transient data path directory" )
    
  }

  if ( ! dir.exists( xroot ) )
    stop( "The data path directory or its parent does not exist" )
  
  
  # -- no arguments .. return xroot as data path
  if ( length(fpath_args) == 0 )
    return(invisible( cxapp::cxapp_standardpath( xroot ) ))
  

  # - inject root path at the start of args
  fpath_args <- append( xroot, fpath_args )

  # -- derive full datapath
  data_path <-  do.call( base::file.path, fpath_args)
          
  
  # -- return
  return(invisible( cxapp::cxapp_standardpath( data_path ) ))
}


