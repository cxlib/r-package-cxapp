#' Pseudo-random string to identify an application node by unique ID
#' 
#' @return Vector of identifying attributes
#' 
#' @description
#' The intent is to generate a sufficiently unique string such that the likelihood
#' that any two nodes active at the same time will not have the same identify. 
#' 
#' 
#' @export

cxapp_appnode <- function() {
  
  
  if ( ! base::exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
    base::assign( ".cxapp.wrkcache.appnode",
                  digest::digest( paste( c( base::tempdir(), 
                                            as.character(Sys.info()),
                                            as.character(as.POSIXct( Sys.time(), tz = "UTC")),
                                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 200, replace = TRUE), collapse = "") ),
                                         collapse = ":" ), 
                                  algo = "crc32", file = FALSE ),
                  envir = .GlobalEnv )
  
  
  return(invisible( base::get(".cxapp.wrkcache.appnode", envir = .GlobalEnv) ))
}



