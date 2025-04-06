#' Utility function to get the path in the data area
#' 
#' @param ... Vector of path elements
#' 
#' @return A vector of length one with the path 
#' 
#' @description
#' The utility function `cxapp_datapath()` is synonymous with the function
#' \link[base]{file.path}.
#' 
#' The root of the data path is defined in the cxapp property `DATA`. The first 
#' path in `DATA` that exists is the root of the returned path. If no specified 
#' paths exists, the first specified path is used.
#' 
#' If the `DATA` property is not defined, the function returns the path equal to
#' that of \link[base]{file.path}.
#' 
#' 
#' @export


cxapp_datapath <- function( ... ) {

  
  # -- get the specified arguments remoing names
  # fxargs <- unlist(list( ... ), use.names = FALSE)
  fxargs <- list( ... )


  # -- define root path
  xroot <- character(0)
  
  cfg <- cxapp::cxapp_config()
  
  if ( ! is.na(cfg$option( "DATA", unset = NA )) ) {
    xpaths <- base::unlist( base::strsplit( cfg$option("DATA"), .Platform$path.sep, fixed = TRUE ) )
   
    # if any directory in xpaths exists ... use the first existing
    # if no directory in xpaths exists ... use the first specified
    xroot <- ifelse( any(dir.exists( xpaths )), 
                     utils::head( xpaths[ dir.exists(xpaths) ], n = 1 ),
                     utils::head( xpaths, n = 1 ) )
    
  }


  # -- generate path 
  fpath_args <- append( as.list(xroot), fxargs)
  fpath_args[["fsep"]] <- "/"

  data_path <-  do.call( base::file.path, fpath_args)
          
  
  # -- return
  return( cxapp::cxapp_standardpath( data_path ) )
}


