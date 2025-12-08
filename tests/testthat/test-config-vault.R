#
#  test for cxapp::cxapp_config()
#
#  vault secrets
#
#  using local vault
#
#



#' @cx.testsfor cxapp::cxapp_config()


testthat::test_that( "config.propertyRedirectVaultSecretTag", {
  
  #' @cx.tests Property value returned is value of a vault sercret when property value references the vault secret and the vault secret exists
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
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
  
  
  
  
  # - cached config
  
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
  

  
  # - stage vault
  
  test_vault_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
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
  

  # - test property 
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_propfile_value <- paste0( "[vault]", test_secret_ref )
  

  
  # - stage property file
  
  base::writeLines( c( "# test properties file", 
                       "# -- vault configuration",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ),
                       "# -- property",
                       paste( test_reference_name, test_reference_propfile_value, sep = "=" ) ),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )

  
  # -- configuration
  test_obj <- cxapp::cxapp_config()
    

  # -- test
  result <- test_obj$option( test_reference_name, unset = NA, use.names = FALSE )
  
  
  # -- expected
  
  expected_name <- paste0( test_reference_name )
  
  expected_value <- base::readLines( file.path( test_vault_path, utils::tail( test_secrets, n = 1 ), fsep = "/" ) )
  
  
  # -- assertions
  testthat::expect_equal( result, expected_value )
  
})





testthat::test_that( "config.propertyRedirectVaultSecretTagSecretNotExist", {
  
  #' @cx.tests Property value returned is value of unset when property value references a vault secret and the vault secret does not exist
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
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
  
  
  
  
  # - cached config
  
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
  
  
  
  # - stage vault
  
  test_vault_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
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
  
  
  # - test property 
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_propfile_value <- paste0( "[vault]", test_secret_ref )
  
  
  # - test property file

  base::writeLines( c( "# test properties file", 
                       "# -- vault configuration",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ),
                       "# -- property",
                       paste( test_reference_name, test_reference_propfile_value, sep = "=" ) ),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  

  # -- configuration
  test_obj <- cxapp::cxapp_config()
  
  
  # -- test
  result <- test_obj$option( test_reference_name, unset = NA, use.names = FALSE )


  # -- assertions
  testthat::expect_true( is.na(result))
  
})

