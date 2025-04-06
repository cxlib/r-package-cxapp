#
#  Tests for cxapp::.cxappconfig()
#
#
#
#

testthat::test_that( "appconfig.emptyConfig", {
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")

  
  
  # - stash current config
  
  current_appconfig <- NA
  
  if ( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    current_appconfig <- base::get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  
  on.exit( {

    if ( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( current_appconfig, "cxapp_config" ) )
      assign( ".cxapp.wrkcache.config", current_appconfig, envir = .GlobalEnv )

  }, add = TRUE )

  
  if ( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash current app config" )
    

  
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
  


  # -- test
  
  result <- cxapp::.cxappconfig()

  
  
  # -- assertions
  
  testthat::expect_true( inherits( result, "cxapp_config" ) )
  testthat::expect_true( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )

})





testthat::test_that( "appconfig.globalConfig", {
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  
  # - stash current config
  
  current_appconfig <- NA
  
  if ( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    current_appconfig <- base::get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  
  on.exit( {
    
    if ( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( current_appconfig, "cxapp_config" ) )
      assign( ".cxapp.wrkcache.config", current_appconfig, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash current app config" )
  
  
  
  
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
  
  
  
  # - test reference value
  
  test_reference_name <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9)) , 10), collapse = "")

  test_reference_value <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9)) , 30), collapse = "")
  names(test_reference_value) <- test_reference_name
    
  
  
  # - inject configuration
  
  test_configfile <- file.path( test_apphome, "app.properties", fsep = "/" )
  
  base::writeLines( c( "# Test property file", 
                       paste( test_reference_name, "=", test_reference_value ) ), 
                    con = test_configfile )
  
  if ( ! file.exists( test_configfile ) )
    testthat::fail( "Could not stage app properties" )
  
  
  
  # - test configuration
  
  test_cfg <- cxapp::.cxappconfig()
  
  
  
  # - overwrite configuration
  
  test_orig_config_sha1 <- digest::digest( test_configfile, algo = "sha1", file = TRUE )

  base::writeLines( c( "# Test property file", 
                       paste( test_reference_name, "=", paste( sample( c( base::LETTERS, base::letters, as.character(0:9)) , 30), collapse = "") ) ), 
                    con = test_configfile )
  
  if ( ! file.exists( test_configfile ) ||
       ( digest::digest( test_configfile, algo = "sha1", file = TRUE ) == test_orig_config_sha1 ) )
    testthat::fail( "Could not update staged app properties" )
  
  

  
  # -- test
  
  result <- cxapp::.cxappconfig()
  

  
  # -- expectations
  
  expected_propname <- paste0( "app/", test_reference_name )
  expected_propvalue <- unname(test_reference_value)
  

  # -- assertions

  # - global object  
  testthat::expect_true( inherits( result, "cxapp_config" ) )
  testthat::expect_true( base::exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
  
  
  # - property value
  #   note: verify global value is read and not the updated config file
  testthat::expect_equal( result$option( expected_propname, unset = NA ), expected_propvalue )
  
})




