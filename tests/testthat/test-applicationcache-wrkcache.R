#
#  Tests for cxapp::.cxappcache()
#
#
#
#


testthat::test_that( "appcache.noConfig", {
  
  
  # -- stage 

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  

  
  # - test cache root  
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )
  
  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  
  # - move global in-memory cached config 
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
      
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )

  
  # - move global in-memory cached application cache 
  
  prev_appcache <- NA
  
  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )
 
    if ( inherits( prev_appcache, "cxapp_applicationcache" ) ) 
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )
      

  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")
  
  
  # - stash current APP_HOME
  
  test_apphome <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-apphome-directory-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_apphome ) || ! dir.create( file.path( test_apphome, "config", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test app home directory" )
  
  
  test_apphome_name <- "APP_HOME"
  
  # case insensitive matching
  env_names <- names(Sys.getenv())
  
  if ( test_apphome_name %in% base::toupper(env_names) )
    test_apphome_name <- utils::head( env_names[ base::toupper(env_names) == "APP_HOME" ]  , n = 1)
  
  current_apphome <- Sys.getenv( test_apphome_name, unset = NA )
  
  resetfor_apphome <- list( current_apphome )
  names(resetfor_apphome) <- test_apphome_name
  
  on.exit({
    
    if ( ! is.na(current_apphome) )
      do.call( Sys.setenv, resetfor_apphome )
    
  }, add = TRUE )
  
  names(test_apphome) <- test_apphome_name
  
  do.call( Sys.setenv, as.list(test_apphome) )
  


  # - inject configuration
  
  base::writeLines( c( "# test configuration", 
                       paste0( "CACHEPATH = ", test_cachepath ) ), 
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )
  

    
  # -- test
  
  result <- cxapp::.cxappcache()

  
  # -- expectations
  
  expected_cachepath <- test_cachepath
  

  # -- assertions
  
  testthat::expect_true( inherits(result, "cxapp_applicationcache") )
  
  testthat::expect_equal( result$.attr[["cache.path"]], expected_cachepath )

})
