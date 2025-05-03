#
#  Tests for cxapp:::.cxapp_vaultlocal()
#
#


testthat::test_that( "vaultlocal.initNoConfig", {
  
  # .. test
  testthat::expect_error( cxapp:::.cxapp_vaultlocal(), regexp = "^Vault configuration is not local$" )
  
})




testthat::test_that( "vaultlocal.initNoVaultDirectoryPath", {

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
  
  

  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL" ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # -- test
  testthat::expect_error( cxapp:::.cxapp_vaultlocal(), regexp = "^The vault root directory is not defined or does not exist$" )
  
})




testthat::test_that( "vaultlocal.initConfigDirectoryNotExist", {

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
  
  
  # vault directory
  
  test_vault_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_vault_path ) || file.exists( test_vault_path ) )
    testthat::fail( "Unexpected test vault directory exists" )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  

  # -- test
  testthat::expect_error( cxapp:::.cxapp_vaultlocal(), regexp = "^The vault root directory is not defined or does not exist$" )
  
})




testthat::test_that( "vaultlocal.emptyList", {

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
  
  
  # vault directory
  
  test_vault_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # -- test
  
  vault <- cxapp:::.cxapp_vaultlocal()
  
  result <- vault$list()
  
  
  # -- assertions
  
  testthat::expect_true( inherits( result, "character") )
  testthat::expect_length( result , 0 )

})





testthat::test_that( "vaultlocal.list", {
  
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
  
  
  # vault directory
  
  test_vault_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # inject secrets
  
  test_secrets <- base::replicate( 20, 
                                   paste( base::replicate( 5, 
                                                           paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
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
  
  
  # -- test
  
  vault <- cxapp:::.cxapp_vaultlocal()
  
  result <- vault$list()

  
  # -- expected
  
  expected_secrets <- sort(paste0( "/", test_secrets ))


  # -- assertions

  testthat::expect_equal( result, expected_secrets )  

})





testthat::test_that( "vaultlocal.secretNotExist", {
  
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
  
  
  # vault directory
  
  test_vault_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # inject secrets
  
  test_secrets <- base::replicate( 20, 
                                   paste( base::replicate( 5, 
                                                           paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
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
  
  
  # reference secret

  test_ref_secret <- paste( base::replicate( 5, 
                                             paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
                                             simplify = TRUE),
                            collapse = "/" ) 
  
  
  if ( test_ref_secret %in% test_secrets )
    testthat::fail( "Could not generate a secret that does not exist" )
  
  

  # -- test
  
  vault <- cxapp:::.cxapp_vaultlocal()
  
  result <- vault$secret( paste0( "/", test_ref_secret ), unset = NA )
  

  # -- assertions

  testthat::expect_true( is.na(result) )  

})



testthat::test_that( "vaultlocal.secret", {
  
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
  
  
  # vault directory
  
  test_vault_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # inject secrets
  
  test_secrets <- base::replicate( 20, 
                                   paste( base::replicate( 5, 
                                                           paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
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
  
  
  # reference secret
  
  test_ref_secret <- paste( base::replicate( 5, 
                                             paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
                                             simplify = TRUE),
                            collapse = "/" ) 
  
  
  if ( test_ref_secret %in% test_secrets )
    testthat::fail( "Could not generate a secret that does not exist" )
  
  
  
  # -- test
  
  vault <- cxapp:::.cxapp_vaultlocal()
  
  result <- vault$secret( paste0( "/", utils::tail( test_secrets, n = 1 ) ), unset = NA )
  
  
  # -- expectations
  
  expected_secret_value <- base::readLines( file.path( test_vault_path, utils::tail( test_secrets, n = 1 ), fsep = "/" ) )
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_secret_value )
  
})





