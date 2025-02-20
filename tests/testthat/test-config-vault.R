#
#  test for cxapp::cxapp_config()
#
#  vault secrets
#
#  using local vault
#
#




testthat::test_that( "config.propertyRedirectVaultSecretTag", {
  
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
  
  
  
  # stage vault
  
  test_vault_path <- cxapp:::.cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  test_secrets <- base::replicate( 20, 
                                   paste( base::replicate( 5, 
                                                           paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), ".", "-", "_"), 10 ), collapse = ""),
                                                           simplify = TRUE),
                                          collapse = "/" )
                                   , simplify = TRUE )
  
  for ( xsecret in test_secrets ) {
    
    secrets_file <- file.path( test_vault_path, xsecret, fsep = "/" )
    
    if ( ! dir.create( base::dirname(secrets_file), recursive = TRUE ) )
      testthat::fail( "Failed to create hierarchy for secret" )
    
    base::writeLines( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), ".", "-", "_"), 40 ), collapse = ""),
                      con = secrets_file )
    
    if ( ! file.exists( secrets_file ) )
      testthat::fail( "Could not stage secret" )
    
  }
  
  test_secret_ref <- paste0( "/", utils::tail( test_secrets, n = 1 ) )
  

  # test property 
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_propfile_value <- paste0( "[vault]", test_secret_ref )
  

  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       "# -- vault configuration",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ),
                       "# -- property",
                       paste( test_reference_name, test_reference_propfile_value, sep = "=" ) ),
                    con = file.path( test_cxapp_path, "cxapp.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "cxapp.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxapp.properties" )
  

  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  
  expected_name <- paste0( test_reference_name )
  
  expected_value <- base::readLines( file.path( test_vault_path, utils::tail( test_secrets, n = 1 ), fsep = "/" ) )
  
  
  # -- assertions
  testthat::expect_equal( result$option( expected_name  ), expected_value )
  
})





testthat::test_that( "config.propertyRedirectVaultSecretTagSecretNotExist", {
  
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
  
  
  
  # stage vault
  
  test_vault_path <- cxapp:::.cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  test_secrets <- base::replicate( 20, 
                                   paste( base::replicate( 5, 
                                                           paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), ".", "-", "_"), 10 ), collapse = ""),
                                                           simplify = TRUE),
                                          collapse = "/" )
                                   , simplify = TRUE )
  
  # note: stage all but the last secret .. last should be undefined
  for ( xsecret in utils::head( test_secrets, n = length(test_secrets) - 1 ) ) {
    
    secrets_file <- file.path( test_vault_path, xsecret, fsep = "/" )
    
    if ( ! dir.create( base::dirname(secrets_file), recursive = TRUE ) )
      testthat::fail( "Failed to create hierarchy for secret" )
    
    base::writeLines( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), ".", "-", "_"), 40 ), collapse = ""),
                      con = secrets_file )
    
    if ( ! file.exists( secrets_file ) )
      testthat::fail( "Could not stage secret" )
    
  }
  
  test_secret_ref <- paste0( "/", utils::tail( test_secrets, n = 1 ) )
  
  
  # test property 
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_propfile_value <- paste0( "[vault]", test_secret_ref )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       "# -- vault configuration",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ),
                       "# -- property",
                       paste( test_reference_name, test_reference_propfile_value, sep = "=" ) ),
                    con = file.path( test_cxapp_path, "cxapp.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "cxapp.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxapp.properties" )
  

  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  
  expected_name <- paste0( test_reference_name )
  
  expected_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" )
  
  
  # -- assertions
  testthat::expect_equal( result$option( expected_name, unset = expected_value  ), expected_value )
  
})

