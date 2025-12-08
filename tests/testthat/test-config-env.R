#
#  test for cxapp::cxapp_config()
#
#  Environmental variables
#
#


#' @cx.testsfor cxapp::cxapp_config()


testthat::test_that( "config.envPropFilePrecedenceDefault", {
  
  #' @cx.tests Property set by properties file takes precedence over resolved environmental variable
  
  
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
  
  
  
  # -- test properties
  
  test_properties <- replicate( base::sample( 1:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1), replace = TRUE ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( base::sample( c( base::LETTERS, base::letters ), base::sample( 10:25, 1), replace = TRUE ), collapse = ""),
                                               simplify = TRUE )
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome )
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  test_proplines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  base::writeLines( test_proplines, con = test_propfiles )
  
  

  # -- environment variable 
  #    note: using random name
  
  test_env_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 5:19, 1), replace = TRUE ), collapse = "")
  base::names(test_env_value) <- base::sample(base::names(test_properties), 1)
  
  if ( ! is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Unexpected environmental variable is defined" )
  
  do.call( Sys.setenv, as.list(test_env_value) )
  
  on.exit({
    Sys.unsetenv( base::names(test_env_value) )
  }, add = TRUE )
  
  
  if ( is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Staging environmental variable failed" )
  
  
  
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

  
  # - configuration object
  test_obj <- cxapp::cxapp_config()
  
  
  # -- test
  result <- test_obj$option( base::names(test_env_value), unset = NA )

  
  # -- expected
  
  expected_value <- test_properties[ base::names(test_env_value) ]
  base::names(expected_value) <- base::tolower(base::names(expected_value))
  
  # -- assertions
  testthat::expect_equal( result, expected_value )

})




testthat::test_that( "config.envResolvesWithSSearchEnvarEnabled", {
  
  #' @cx.tests Property resolved to environmental variable value with property not set and search environmental variables enabled
  
  
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
  
  

  
  # -- test properties
  
  test_properties <- replicate( base::sample( 1:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1), replace = TRUE ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( base::sample( c( base::LETTERS, base::letters ), base::sample( 10:25, 1), replace = TRUE ), collapse = ""),
                                               simplify = TRUE )
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome )
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  test_proplines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  base::writeLines( test_proplines, con = test_propfiles )
  
  
  
  # -- environment variable 
  #    note: using random name
  #    note: an environmental variable name look up is either on upper case or lower case
  
  test_env_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 5:19, 1), replace = TRUE ), collapse = "")
  
  test_env_name <- paste( base::sample( c( base::LETTERS, base::letters ), base::sample( c( 4:9, 26:30), 1), replace = TRUE ), collapse = "")
  base::names(test_env_value) <- base::sample( c( base::tolower(test_env_name), base::toupper(test_env_name) ), 1 )
  


  if ( base::names(test_env_value) %in% base::names(test_properties) )
    testthat::fail( "Random name exists as a property name")
  
  if ( ! is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Unexpected environmental variable is defined" )
  
  do.call( Sys.setenv, as.list(test_env_value) )
  
  on.exit({
    Sys.unsetenv( base::names(test_env_value) )
  }, add = TRUE )
  
  
  if ( is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Staging environmental variable failed" )
  
  
  
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
  
  
  # - configuration object
  test_obj <- cxapp::cxapp_config()
  
  
  # -- test
  result <- test_obj$option( base::names(test_env_value), unset = NA )
  
  
  # -- expected
  
  expected_value <- test_env_value
  base::names(expected_value) <- base::tolower(base::names(expected_value))
  
  # -- assertions
  testthat::expect_equal( result, expected_value )

})



testthat::test_that( "config.envUnsetWithSSearchEnvarDisabled", {
  
  #' @cx.tests Property not resolved to environmental variable value with property not set and search environmental variables disabled
  
  
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
  
  

  # -- test properties
  
  test_properties <- replicate( base::sample( 1:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1), replace = TRUE ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( base::sample( c( base::LETTERS, base::letters ), base::sample( 10:25, 1), replace = TRUE ), collapse = ""),
                                               simplify = TRUE )
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome )
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  test_proplines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  base::writeLines( test_proplines, con = test_propfiles )
  
  
  
  # -- environment variable 
  #    note: using random name
  #    note: an environmental variable name look up is either on upper case or lower case
  
  test_env_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 5:19, 1), replace = TRUE ), collapse = "")
  
  test_env_name <- paste( base::sample( c( base::LETTERS, base::letters ), base::sample( c( 4:9, 26:30), 1), replace = TRUE ), collapse = "")
  base::names(test_env_value) <- base::sample( c( base::tolower(test_env_name), base::toupper(test_env_name) ), 1 )
  
  
  
  if ( base::names(test_env_value) %in% base::names(test_properties) )
    testthat::fail( "Random name exists as a property name")
  
  if ( ! is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Unexpected environmental variable is defined" )
  
  do.call( Sys.setenv, as.list(test_env_value) )
  
  on.exit({
    Sys.unsetenv( base::names(test_env_value) )
  }, add = TRUE )
  
  
  if ( is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Staging environmental variable failed" )
  
  
  
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
  
  
  # - configuration object
  test_obj <- cxapp::cxapp_config()
  
  
  # -- test
  result <- test_obj$option( base::names(test_env_value), unset = NA, search.envars = FALSE )
  
  
  # -- assertions

  testthat::expect_true( is.na(result) )  
  

})





testthat::test_that( "config.envRedirectWithEnvExists", {
  
  #' @cx.tests Property value reference to existing environmental variable resolved when environmental variable exists
  
  
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
  
  
  
  # -- environment variable 
  #    note: using random name
  #    note: case sensitive reference
  
  test_env_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 5:19, 1), replace = TRUE ), collapse = "")
  base::names(test_env_value) <- paste( base::sample( c( base::LETTERS, base::letters ), base::sample( c( 4:9, 26:30), 1), replace = TRUE ), collapse = "")

  if ( ! is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Unexpected environmental variable is defined" )
  
  do.call( Sys.setenv, as.list(test_env_value) )
  
  on.exit({
    Sys.unsetenv( base::names(test_env_value) )
  }, add = TRUE )
  
  
  if ( is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Staging environmental variable failed" )
  
  
  
  
  
  # -- test properties
  
  test_properties <- replicate( base::sample( 5:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1), replace = TRUE ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( base::sample( c( base::LETTERS, base::letters ), base::sample( 10:25, 1), replace = TRUE ), collapse = ""),
                                               simplify = TRUE )

  # -- test property redirect
  #    note: use two properties
  #    note: first refers to "[env] <var>"
  #    note: second refers to "$<var>"
  
  test_prop_envref <- base::sample( base::names(test_properties), 2 )
  
  test_properties[ test_prop_envref[1] ] <- paste0( "[env] ", base::names(test_env_value) )
  test_properties[ test_prop_envref[2] ] <- paste0( "$", base::names(test_env_value) )
  
    
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome )
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  test_proplines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  base::writeLines( test_proplines, con = test_propfiles )
  
  
  
  
  
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
  
  
  # - configuration object
  test_obj <- cxapp::cxapp_config()
  
  
  # -- test
  results <- base::unlist(lapply( test_prop_envref, function(x) {
    test_obj$option( x, unset = NA )
  }))
  

  # -- expected
  
  expected_values <- rep_len( test_env_value, length(test_prop_envref) )
  base::names(expected_values) <- base::tolower(test_prop_envref)
  
  
  # -- assertions
  
  testthat::expect_equal( results[ base::sort(base::names(results)) ], expected_values[ base::sort(base::names(expected_values)) ] )

  
})



testthat::test_that( "config.envRedirectWithEnvNotExists", {
  
  #' @cx.tests Property value reference to existing environmental variable resolved when environmental variable exists
  
  
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
  
  
  
  # -- environment variable 
  #    note: using random name
  #    note: case sensitive reference
  
  test_env_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 5:19, 1), replace = TRUE ), collapse = "")
  base::names(test_env_value) <- paste( base::sample( c( base::LETTERS, base::letters ), base::sample( c( 4:9, 26:30), 1), replace = TRUE ), collapse = "")
  
  if ( ! is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Unexpected environmental variable is defined" )
  
  do.call( Sys.setenv, as.list(test_env_value) )
  
  on.exit({
    Sys.unsetenv( base::names(test_env_value) )
  }, add = TRUE )
  
  
  if ( is.na(Sys.getenv( base::names(test_env_value), unset = NA)) )
    testthat::fail( "Staging environmental variable failed" )
  
  
  
  
  
  # -- test properties
  
  test_properties <- replicate( base::sample( 5:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1), replace = TRUE ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( base::sample( c( base::LETTERS, base::letters ), base::sample( 10:25, 1), replace = TRUE ), collapse = ""),
                                               simplify = TRUE )
  
  # -- test property redirect
  #    note: use two properties
  #    note: random environmental variable names that should not exist
  #    note: first refers to "[env] <var>"
  #    note: second refers to "$<var>"
  
  test_prop_envref <- base::sample(base::names(test_properties), 2)
  test_prop_envnames <- replicate( 2, 
                                   paste( base::sample( c( base::LETTERS, base::letters ), base::sample( 10:25, 1), replace = TRUE ), collapse = ""), 
                                   simplify = TRUE )

  if ( any( test_prop_envnames %in% base::names(Sys.getenv()) ) )
    testthat::fail( "Unexpected random property name refers to an existing environmental variable" )

  
  test_properties[ test_prop_envref[1] ] <- paste0( "[env] ", test_prop_envnames[1] )
  test_properties[ test_prop_envref[2] ] <- paste0( "$", test_prop_envnames[2] )
  
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome )
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  test_proplines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  base::writeLines( test_proplines, con = test_propfiles )
  
  
  
  
  
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
  
  
  # - configuration object
  test_obj <- cxapp::cxapp_config()
  
  
  # -- test
  results <- base::unlist(lapply( test_prop_envref, function(x) {
    test_obj$option( x, unset = NA )
  }))
  

  # -- expected
  
  expected_values <- rep_len( NA, length(test_prop_envref) )

  
  # -- assertions
  
  testthat::expect_equal( results[ base::sort(base::names(results)) ], expected_values[ base::sort(base::names(expected_values)) ] )
  
  
})







