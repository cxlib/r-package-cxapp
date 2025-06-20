#
#  High-level tests for cxapp::cxapp_vault()
#
#
#

testthat::test_that( "vault.noneConfig", {
  
  # -- test
  testthat::expect_error( cxapp::cxapp_vault(), regexp = "^No vault service is configured$" )
  
}) 



testthat::test_that( "vault.invalidVaultName", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
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
  
  
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # random vault reference
  test_reference <- "thisisnotavault"
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "VAULT = ", test_reference ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # -- test
  
  
    
  # -- test
  testthat::expect_error( cxapp::cxapp_vault(), regexp = paste( "^Vault service", base::toupper(test_reference) , "is not supported$" ) )
  
}) 
