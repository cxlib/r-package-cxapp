#
#  Tests for application cache
#
#  Expire content
#


#' @cx.testsfor cxapp::cxapp_applicationcache()




testthat::test_that( "appcache.configDefaultExpire", {
  
  
  #' @cx.tests Files added to an application cache item expires in cache as defined by the cache default expire duration when no duration is specified
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - APP_HOME
  
  #   note: align with cxapp_apphome()
  #   note: precedence of APP_HOME all caps
  prev_apphome <- Sys.getenv( "APP_HOME", unset = NA, names = TRUE )
  
  on.exit({
    
    if ( ! is.na( prev_apphome ) )
      do.call( Sys.setenv, as.list(prev_apphome) )
    
  }, add = TRUE )
  
  
  if ( ! is.na( prev_apphome ) )
    Sys.unsetenv( base::names(prev_apphome))
  
  
  test_apphome <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-app-home-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( file.path( test_apphome, "config", fsep = "/" ) ) && ! dir.create( file.path( test_apphome, "config", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage APP_HOME directory" )
  
  Sys.setenv( "APP_HOME" = test_apphome )
  
  if ( is.na(Sys.getenv("APP_HOME", unset = NA ) ) )
    testthat::fail( "Could not stage APP_HOME" )
  
  
  
  # - move cached config out of the way
  
  prev_cachedconfig <- NA
  
  if ( base::exists( ".cxapp.wrkcache.config", envir = base::.GlobalEnv ) )
    prev_cachedconfig <- base::get( ".cxapp.wrkcache.config", envir = base::.GlobalEnv )
  
  on.exit({
    
    # note: the cached content is .self$.attr of cxapp::cxapp_config() 
    if ( inherits( prev_cachedconfig, "list") ) 
      base::assign( ".cxapp.wrkcache.config", prev_cachedconfig, envir = base::.GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( base::exists( ".cxapp.wrkcache.config", envir = base::.GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = base::.GlobalEnv )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  # - test expire duration minutes
  test_expire_mins <- 5
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.EXPIRE =", as.character(test_expire_mins)),
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  #   note: use single file for this scenario
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  
  test_file <- cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") )
  
  test_content <- paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" )
  
  
  base::writeLines( test_content, con = test_file )
  
  if ( ! file.exists( test_file ) )
    testthat::fail( "Could not stage test file")
  
  
  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # - capture time of test
  
  test_time <- as.POSIXct( Sys.time(), tz = "UTC" )
  
  
  # -- test
  
  result <- test_cache$add( test_file )
  
  
  
  # -- expected
  
  lst_cacheobjects <- digest::digest( base::tolower(base::trimws(test_file)), algo = "sha1", file = FALSE )
  
  
  
  # - expected object files
  
  expected_files <- file.path( test_cachepath, lst_cacheobjects, fsep = "/" )
  
  
  
  # - expected lck files (regex)
  
  expected_lckfile_patterns <- paste0("^", lst_cacheobjects, "\\-\\d{8}\\-\\d{4}\\.lck$" )
  
  
  
  # - test reference time to use in duration calcs
  #   note: poor mans floor of date/time to minute
  
  expected_reftime <- as.POSIXct( format( test_time, format = "%Y%m%d-%H%M" ), tz = "UTC", format = "%Y%m%d-%H%M" )
  
  
  
  # -- assertions
  
  # - result returned is boolean
  testthat::expect_true( result )
  
  
  # - cached files exist
  testthat::expect_true( all(file.exists(expected_files)) )
  
  
  # - lck files for cached items
  
  lst_lckfiles <- cxapp::cxapp_standardpath( list.files( test_cachepath,
                                                         pattern = "^[a-z0-9]{40}\\-\\d{8}\\-\\d{4}\\.lck$",
                                                         full.names = FALSE,
                                                         recursive = FALSE ) )
  
  testthat::expect_length(lst_lckfiles, length(expected_files) )
  
  for ( xpattern in expected_lckfile_patterns )
    testthat::expect_true( any(grepl( xpattern, lst_lckfiles, ignore.case = TRUE, perl = TRUE )) )
  
  
  # - verify cache lock is 5 min ... or so
  #   expecting something like 60 second diff
  
  obj_lck <- as.POSIXct( gsub( "^[a-f0-9]{40}-(\\d{8})-(\\d{4})\\.lck$", "\\1-\\2", lst_lckfiles ), tz = "UTC", format = "%Y%m%d-%H%M" )
  
  # read this ... difference to obj_lck from current_time in minutes
  expire_diff <- as.numeric( base::difftime( obj_lck, expected_reftime, units = "mins" ) )
  
  
  # note: we floored our reference time
  # note: if reference time is floored and we pass the minute mark in processing .. the lck time is floored to the "next" minute so diff in 5 or 6
  testthat::expect_true( expire_diff %in% c( test_expire_mins, test_expire_mins + 1 ) )
  
  
})






testthat::test_that( "appcache.touch", {
  
  #' @cx.tests Touch an entry in the cache to extend expiration by the cache default expire duration when no extend duration is specified
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - APP_HOME
  
  #   note: align with cxapp_apphome()
  #   note: precedence of APP_HOME all caps
  prev_apphome <- Sys.getenv( "APP_HOME", unset = NA, names = TRUE )
  
  on.exit({
    
    if ( ! is.na( prev_apphome ) )
      do.call( Sys.setenv, as.list(prev_apphome) )
    
  }, add = TRUE )
  
  
  if ( ! is.na( prev_apphome ) )
    Sys.unsetenv( base::names(prev_apphome))
  
  
  test_apphome <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-app-home-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( file.path( test_apphome, "config", fsep = "/" ) ) && ! dir.create( file.path( test_apphome, "config", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage APP_HOME directory" )
  
  Sys.setenv( "APP_HOME" = test_apphome )
  
  if ( is.na(Sys.getenv("APP_HOME", unset = NA ) ) )
    testthat::fail( "Could not stage APP_HOME" )
  
  
  
  # - move cached config out of the way
  
  prev_cachedconfig <- NA
  
  if ( base::exists( ".cxapp.wrkcache.config", envir = base::.GlobalEnv ) )
    prev_cachedconfig <- base::get( ".cxapp.wrkcache.config", envir = base::.GlobalEnv )
  
  on.exit({
    
    # note: the cached content is .self$.attr of cxapp::cxapp_config() 
    if ( inherits( prev_cachedconfig, "list") ) 
      base::assign( ".cxapp.wrkcache.config", prev_cachedconfig, envir = base::.GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( base::exists( ".cxapp.wrkcache.config", envir = base::.GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = base::.GlobalEnv )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       "# expire in 30 days or 30*24*60 minutes",
                       "APP.CACHE.EXPIRE = 43200",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  test_file_refs <- replicate( 10,
                               base::tolower(base::trimws( cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ) )),
                               simplify = TRUE )
  
  
  # - test objects
  
  test_object_refs <- character(0)
  
  for ( xfileref in test_file_refs )
    test_object_refs <- append( test_object_refs,
                                digest::digest( xfileref, algo = "sha1", file = FALSE ) )
  
  
  names(test_object_refs) <- test_file_refs
  
  
  
  # - stage test objects
  
  for ( xobj in test_object_refs ) {
    
    # object
    base::writeLines( paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                      con = file.path( test_cachepath, xobj, fsep  = "/" ) )
    
    # lck file
    base::writeLines( "", con = file.path( test_cachepath,
                                           paste0( xobj, format( as.POSIXct( Sys.time() + 5*60*60, tz = "UTC" ), format = "-%Y%m%d-%H%M" ), ".lck"),
                                           fsep  = "/" ) )
    
  }
  
  
  
  
  # - random select test reference
  
  test_reference <- sample( test_file_refs, 1 )
  
  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # - ensure object discoverable
  if ( ! test_cache$exists( test_reference ) )
    testthat::fail( "Unexpected could not ensure item exists in cache" )
  
  
  
  # - inventory test cache area
  test_cacheinv <- list.files( test_cachepath, full.names = FALSE, recursive = FALSE )
  
  
  # -- test
  
  result <- test_cache$touch( test_reference )
  
  
  
  # -- expected
  
  # - object reference
  expected_objref <- digest::digest( base::tolower(base::trimws(test_reference)), algo = "sha1", file = FALSE )
  
  
  # - expected path
  expected_path <- file.path( test_cachepath, expected_objref, fsep = "/" )
  
  
  
  # -- assertions
  
  # - object does exists
  testthat::expect_true( file.exists( expected_path ) )
  
  
  # - use new lock file as surrogate
  result_cacheinv <- list.files( test_cachepath, full.names = FALSE, recursive = FALSE )
  new_lckfile <- result_cacheinv[ ! result_cacheinv %in% test_cacheinv ]
  
  testthat::expect_true( grepl( paste0( "^", expected_objref, "\\-\\d{8}-\\d{4}\\.lck" ), new_lckfile, perl = TRUE ) )
  
})
