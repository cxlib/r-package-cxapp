#' Utility to standardize path separators in path references
#' 
#' @param x A vector of paths
#' 
#' @return A vector of standardized paths
#'  
#' @description 
#' A path uses separators of directories and file references. On Microsoft
#' Windows this is a backslash (`\`). On Linux the separator is a forward slash.
#' 
#' In addition, the use of the backward slash will also need to be escaped as the 
#' backward slash is an escape character.
#' 
#' R supports the forward slash on Windows environments, however not all functions
#' allows the path separator to be specified. 
#' 
#' The function will translate all backward slashes into a forward slash.
#' 
#' @export

cxapp_standardpath <- function(x) {
  
  if ( missing(x) || is.null(x) )
    return(character(0))
  
  return( gsub( "\\\\", "/", x) )
}

