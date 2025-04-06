#' Utility class to store and retrieve content in the application cache
#' 
#' @method initialize initialize
#' @method add add
#' @method exists exists
#' @method get get
#' @method drop drop
#' @method touch touch
#' @method purge purge
#' @method show show
#' 
#' 
#' @description
#' The application cache is a normalized file caching utility. An item is 
#' referenced by a path syntax that maps to the item. 
#' 
#' The location of the cache is set by the configuration option `CACHEPATH` in 
#' `app.properties`. If `CACHEPATH` is not defined, the `.cache` sub-directory  
#' in the temporary directory \link[base]{tempdir} for the current R session is 
#' used.
#' 
#' The `add(x)` method will add the specified `file` to the cache using `x` as a 
#' reference. The time to expire for the item in the cache is defined using 
#' the configuration option `APPCACHE.EXPIRE` in minutes. If the expire duration 
#' is updated, the duration in effect when the item was add or last touched is
#' used.
#' 
#' The `exists(x)` method returns a logical vector of the same length as `x` 
#' indicating if the object reference exists in the cache (`TRUE`) or not 
#' (`FALSE`). An object exists in the cache if it physically exists and the 
#' object has not expired in the cache.
#' 
#' The `get(x)` method returns the path to the referenced object in the cache. Note
#' that the preference and the path is not the same. The returned path does not 
#' preserve directory structure, file names or file extension. 
#' 
#' The `drop(x)` method drops (deletes) an object from the cache.
#' 
#' The `touch(x)` method will reset the date and time for an object to expire in 
#' the cache. The new date and time of expiration is derived from the current
#' value of the configuration option `APPCACHE.EXPIRE`.
#' 
#' The `purge()` method deletes all objects in the cache.
#' 
#' @exportClass cxapp_applicationcache
#' @export cxapp_applicationcache


cxapp_applicationcache <- methods::setRefClass( "cxapp_applicationcache", 
                                      fields = list( ".attr" = "list" ) )




cxapp_applicationcache$methods( "initialize" = function() {
  "Initialize"
  
  # -- initialize internals
  
  .self$.attr <- list()
  
  
  # -- cache directory
  
  cache_root <- cxapp::cxapp_standardpath( cxapp::.cxappconfig()$option( "app/cachepath", unset = NA) )
  
  if ( is.na( cache_root) ) {

    # - force temporary directory uniqueness    
    cache_root <- cxapp::cxapp_standardpath( base::tempfile(".application-cache-", tmpdir = base::tempdir(), fileext = "" ) )
    
    if ( ! dir.exists(cache_root) && ! dir.create( cache_root, recursive = TRUE ) )
      stop( "Could not create temporary root directory for cache" )
     
  }
  
  
  if ( ! dir.exists(cache_root) )
    stop( "Cache directory does not exist" )
  
  
  .self$.attr[["cache.path"]] <- cache_root

})




cxapp_applicationcache$methods( "add" = function( x ) {
  "Add one or more files to cache"

  
  if ( missing(x) || is.null(x) || any(is.na(x)) )
    stop( "Vector of files missing" )
  
  # -- futility ... nothing to do
  if ( length(x) == 0 )
    return(invisible(TRUE))
  
  # -- valid files
  if ( ! inherits( x, "character") || ! all( file.exists(x) ) )
    stop( "One or more files do not exist" )
  
  
  # -- setup process
  
  lst_files <- x
  
  # - no names .. use file path as ref
  if ( is.null( names(lst_files) ) )
    names(lst_files) <- lst_files
  


  # - generate references
  
  lst_names <- base::tolower( base::trimws(names(lst_files)) )

  if ( length(match( "", lst_names )) > 0 ) { 
    
    lst_unnamed <- which( lst_names == "" ) 

    for ( xidx in lst_unnamed )
      lst_names[xidx] <- base::tolower( unname(lst_files[ xidx ]) )
    
  }

  

  lst_refs <- character(0)
  
  for ( xname in lst_names ) 
    lst_refs <- append( lst_refs, 
                        digest::digest( xname, algo = "sha1", file = FALSE ) )

  names(lst_files) <- lst_refs


  # -- derive TTL/expire
  
  expire_minutes <- try( as.integer(.cxappconfig()$option( "app/appcache.expire", unset = 1440 )), silent = TRUE )
  
  if ( inherits( expire_minutes, "try-error" ) )
    stop( "Configuration error in that APPCACHE.EXPIRE is not an integer" )
  
  
  expire_str <- base::format(  base::as.POSIXct( Sys.time() + 60*expire_minutes, tx = "UTC" ), format = "%Y%m%d-%H%M" )
  
  
  
  # -- set up temporary processing area
  
  cache_tmp <- cxapp::cxapp_standardpath( base::tempfile( pattern = "appcache-temp-", tmpdir = base::tempdir(), file = "" ) )
  
  if ( ! dir.exists( cache_tmp ) && ! dir.create( cache_tmp, recursive = TRUE ) )
    stop( "Could not create temporary staging area" )
  
  
  # -- process files
  for ( xref in lst_refs ) {
    
    # - file
    src <- unname(lst_files[xref])
    trgt <- file.path( .self$.attr[["cache.path"]], xref, fsep = "/" ) 
    
    tmp_src  <- file.path( cache_tmp, base::basename(src), fsep = "/" ) 
    tmp_trgt <- file.path( cache_tmp, base::basename(trgt), fsep = "/" ) 
    
    
    # - lck file
    lck <- file.path( .self$.attr[["cache.path"]], paste0( xref, "-", expire_str, ".lck"), fsep = "/" ) 
    
    
    # - generate SHA-1 at source
    src_sha1 <- digest::digest( src, algo = "sha1", file = TRUE )
    
    
    # - add file to cache
    #   note: using multi-step copy process to traverse file systems on some platforms instead of straight rename
    
    if ( ! file.exists(src) ||
         ! file.copy( src, base::dirname(tmp_src), copy.mode = FALSE, copy.date = FALSE ) ||
         ! file.rename( tmp_src, tmp_trgt ) ||
         ! file.copy( tmp_trgt, base::dirname(trgt), copy.mode = FALSE, copy.date = FALSE ) ||
         ( digest::digest( trgt, algo = "sha1", file = TRUE) != src_sha1 ) )
      stop( "Could not stage file ", src, " in app cache" )
      
    # - remove temporary file
    base::unlink( tmp_trgt, force = TRUE, recursive = TRUE )
    
    
    # - write lck
    write_lck <- try( base::writeLines("", con = lck ), silent = FALSE )
    
    if ( inherits( write_lck, "try-error" ) ) {
      
      if ( file.exists( trgt ) )
        base::unlink( trgt, force = TRUE, recursive = TRUE )
      
      stop( "Cache rollback for file ", src )
    }
    
  }
  

  
  # -- drop temporary processing area  
  
  base::unlink( cache_tmp, force = TRUE, recursive = TRUE )
  

  return(invisible(TRUE))
  
})



cxapp_applicationcache$methods( "exists" = function(x) {
  "Check if item exists in cache"
  
  if ( missing(x) || is.null(x) || any(is.na(x)) )
    stop( "Vector of references missing" )
  
  # -- futility ... nothing to do
  if ( length(x) == 0 )
    return(invisible(FALSE))
  
  # -- valid files
  if ( ! inherits( x, "character") )
    stop( "Expecting a character vector" )
  
  
  # -- derive references
  
  lst_refs <- character(0)
  
  for ( xitem in x )
    lst_refs <- append( lst_refs, 
                        digest::digest( base::tolower(base::trimws(xitem)), algo = "sha1", file = FALSE ) )
  
  
  # -- process refs
  
  lst_exists <- base::rep_len( FALSE, length(lst_refs) )
  names(lst_exists) <- lst_refs
  
  for ( xref in names(lst_exists) ) {
    
    cache_objpath <- file.path( .self$.attr[["cache.path"]], xref, fsep = "/" )

    if ( ! file.exists( cache_objpath ) ) 
      next()  # we set everything to FALSE initially ... right ...
    
    
    # -- consider expired object
    
    lck_pattern <- paste0( "^", xref, "\\-\\d{8}\\-\\d{4}\\.lck$")

    lck_files <- list.files( .self$.attr[["cache.path"]],
                             pattern = lck_pattern,
                             full.names = FALSE,
                             recursive = FALSE )


    if ( length(lck_files) > 0 ) {
      
      # - determine object expiration
      
      obj_lck <- utils::tail( lck_files, n = 1 )

      obj_expire <- as.POSIXct( gsub( "^[a-f0-9]{40}-(\\d{8})-(\\d{4})\\.lck$" , 
                                      "\\1-\\2" , 
                                      obj_lck), 
                                format = "%Y%m%d-%H%M", tz = "UTC" )

      # - is expired ?      

      if ( as.POSIXct( Sys.time(), tz = "UTC" ) < obj_expire ) {
        lst_exists[ xref ] <- TRUE
        next()
      }

    } # end of if-statement for processing lck files
    
    
    
    # -- delete cached object and lck files if they are expired
    
    files_to_delete <- list.files( .self$.attr[["cache.path"]],
                                   pattern = paste0( "^", xref ),
                                   full.names = FALSE,
                                   recursive = FALSE ) 
    
    base::unlink( file.path( .self$.attr[["cache.path"]], files_to_delete, fsep = "/"), recursive = FALSE, force = TRUE )
    
  }  # end of for-statement on entries in lst_exists
  


  # -- return 

  if ( length(lst_exists) == 1 )
    return(invisible(unname(lst_exists)))
    
  
  names(lst_exists) <- x
  return(invisible(lst_exists))
  
})
 



cxapp_applicationcache$methods( "get" = function(x) {
  "Get the path to the referenced item in cache"

  
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || ! inherits( x, "character" ) )
    stop( "Object reference invalid" )

  if ( ! .self$exists(x) )
    stop( "File does not exist in cache")
  

  # -- reference
  objref <- digest::digest( base::tolower(base::trimws(x)), algo = "sha1", file = FALSE )
  
  
  objpath <- file.path( .self$.attr[["cache.path"]], objref, fsep = "/" )
  
  if ( ! file.exists( objpath ) )
    stop( "Object not found in cache" )
  
    
  return(invisible(objpath))
  
})




cxapp_applicationcache$methods( "drop" = function(x) {
  "Drop an item from cache"

  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || ! inherits( x, "character" ) )
    stop( "Object reference invalid" )
  
  # -- futility .. nothing to do
  if ( ! .self$exists(x) )
    return(invisible(TRUE))
  
  
  # -- reference
  objref <- digest::digest( base::tolower(base::trimws(x)), algo = "sha1", file = FALSE )
  
  
  # -- delete cached object and lck files if they are expired
  files_to_delete <- list.files( .self$.attr[["cache.path"]],
                                 pattern = paste0( "^", objref ),
                                 full.names = FALSE,
                                 recursive = FALSE ) 
  
  base::unlink( file.path( .self$.attr[["cache.path"]], files_to_delete, fsep = "/"), recursive = FALSE, force = TRUE )  
  
  
  return(invisible(TRUE))
})





cxapp_applicationcache$methods( "touch" = function(x) {
  "Extend expiration for an item in cache"

  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || ! inherits( x, "character" ) )
    stop( "Object reference invalid" )
  
  if ( ! .self$exists(x) )
    stop( "Referenced item does not exist in cache")
  

  # -- reference
  objref <- digest::digest( base::tolower(base::trimws(x)), algo = "sha1", file = FALSE )
  
  
  # -- derive TTL/expire
  expire_minutes <- try( as.integer(.cxappconfig()$option( "app/appcache.expire", unset = 1440 )), silent = TRUE )
  
  if ( inherits( expire_minutes, "try-error" ) )
    stop( "Configuration error in that APPCACHE.EXPIRE is not an integer" )

  expire_str <- base::format(  base::as.POSIXct( Sys.time() + 60*expire_minutes, tx = "UTC" ), format = "%Y%m%d-%H%M" )
  
  

  # - write lck file
  lck <- file.path( .self$.attr[["cache.path"]], paste0( objref, "-", expire_str, ".lck"), fsep = "/" ) 

  write_lck <- try( base::writeLines("", con = lck ), silent = FALSE )
  
  if ( inherits( write_lck, "try-error" ) ) 
    return(invisible(FALSE))
  
  
  
  return(invisible(TRUE))
})



cxapp_applicationcache$methods( "purge" = function() {
  "Delete all items in cache"
  
  # -- delete all cached object and lck files 
  files_to_delete <- list.files( .self$.attr[["cache.path"]], full.names = TRUE, recursive = FALSE ) 
  
  base::unlink( files_to_delete, recursive = FALSE, force = TRUE )  

})



cxapp_applicationcache$methods( "show" = function() {
  "Print cache details"
  
  info <- c( " ", 
             "Application cache", 
             paste( rep_len("-", 60), collapse = "" ) )
             
  
  # -- add cache path
  info <- append( info, paste( "Cache path                         ", .self$.attr[["cache.path"]] ) )
  

  # -- add expire duration setting
  info <- append( info, paste( "Object time to expire (minutes)    ", .cxappconfig()$option( "app/appcache.expire", unset = 1440 ) ),)
  
  
  # -- add number of objects
  obj_lst <- list.files( .self$.attr[["cache.path"]], pattern  = "^[a-f0-9]{40}$", recursive = FALSE )
  info <- append( info, paste( "Number of objects                  ", length(obj_lst) ) ) 
  


  cat( info, sep = "\n" )  

})


