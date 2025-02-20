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
  
  test_root <- cxapp:::.cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
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
                    con = file.path( test_cxapp_path, "cxapp.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "cxapp.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxapp.properties" )
  
  
  # -- test
  
  
    
  # -- test
  testthat::expect_error( cxapp::cxapp_vault(), regexp = paste( "^Vault service", base::toupper(test_reference) , "is not supported$" ) )
  
}) 
