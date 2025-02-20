#
#  test for cxapp::cxapp_config()
#
#  environmental variables
#
#



testthat::test_that( "config.envPropFilePrecedence", {
  
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
  
  
  # test property value
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_propfile_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )
  test_reference_env_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )
  
  if ( test_reference_propfile_value == test_reference_env_value )
    testthat::fail( "Could not distinguish between property file and environmental variable value" )

    
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( test_reference_name, test_reference_propfile_value, sep = "=" ) ),
                    con = file.path( test_cxapp_path, "cxapp.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "cxapp.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxapp.properties" )
  
  
  # stage environmental variable 
  
  on.exit( {
    Sys.unsetenv( test_reference_name )
  }, add = TRUE )
  
  env_values <- as.list( test_reference_env_value )
  names(env_values) <- test_reference_name
  
  do.call( Sys.setenv, env_values )

  if ( Sys.getenv( test_reference_name, unset = NA ) != test_reference_env_value )
    testthat::fail( "Could not stage environmental variable name" )
  
  
  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected

  expected_value <- base::trimws( test_reference_propfile_value )
  
  # -- assertions
  testthat::expect_equal( result$option( test_reference_name), expected_value )

})






testthat::test_that( "config.envVar", {
  
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
  
  
  # test property value
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_propfile_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )
  test_reference_env_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )
  
  if ( test_reference_propfile_value == test_reference_env_value )
    testthat::fail( "Could not distinguish between property file and environmental variable value" )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( test_reference_name, test_reference_propfile_value, sep = "=" ) ),
                    con = file.path( test_cxapp_path, "cxapp.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "cxapp.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxapp.properties" )
  
  
  # stage environmental variable 
  
  test_context <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  
  test_env_name <- paste0( test_context, "_", test_reference_name )
  
  
  on.exit( {
    Sys.unsetenv( test_env_name )
  }, add = TRUE )
  
  env_values <- as.list( test_reference_env_value )
  names(env_values) <- test_env_name
  
  do.call( Sys.setenv, env_values )
  
  if ( Sys.getenv( test_env_name, unset = NA ) != test_reference_env_value )
    testthat::fail( "Could not stage environmental variable name" )
  
  
  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  
  # note: properties file reference is cxapp/<test_reference_name>
  # note: environmental variable reference is <test_context>/<test_reference_name>
  expected_name <- paste0( test_context, "/", test_reference_name )
  
  expected_value <- base::trimws( test_reference_env_value )
  

  # -- assertions
  testthat::expect_equal( result$option( expected_name  ), expected_value )
  
})




testthat::test_that( "config.propertyRedirectEnvVarEnvTag", {
  
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
  

  # stage environmental variable 
  
  test_env_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 20 ), collapse = "" ) )
  test_reference_env_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )

  on.exit( {
    Sys.unsetenv( test_env_name )
  }, add = TRUE )
  
  env_values <- as.list( test_reference_env_value )
  names(env_values) <- test_env_name
  
  do.call( Sys.setenv, env_values )
  
  if ( Sys.getenv( test_env_name, unset = NA ) != test_reference_env_value )
    testthat::fail( "Could not stage environmental variable name" )
  
  
    
  # test property value
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_propfile_value <- paste( "[env]", test_env_name )
  
  if ( test_reference_propfile_value == test_reference_env_value )
    testthat::fail( "Could not distinguish between property file and environmental variable value" )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( test_reference_name, test_reference_propfile_value, sep = "=" ) ),
                    con = file.path( test_cxapp_path, "cxapp.properties", fsep = "/") )
 
  if ( ! file.exists( file.path( test_cxapp_path, "cxapp.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxapp.properties" )
  

  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  
  expected_name <- paste0( test_reference_name )
  
  expected_value <- base::trimws( test_reference_env_value )
  
  
  # -- assertions
  testthat::expect_equal( result$option( expected_name  ), expected_value )
  
})



testthat::test_that( "config.propertyRedirectEnvVarEnvSymbol", {
  
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
  
  
  # stage environmental variable 
  
  test_env_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 20 ), collapse = "" ) )
  test_reference_env_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )
  
  on.exit( {
    Sys.unsetenv( test_env_name )
  }, add = TRUE )
  
  env_values <- as.list( test_reference_env_value )
  names(env_values) <- test_env_name
  
  do.call( Sys.setenv, env_values )
  
  if ( Sys.getenv( test_env_name, unset = NA ) != test_reference_env_value )
    testthat::fail( "Could not stage environmental variable name" )
  
  
  
  # test property value
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_propfile_value <- paste0( "$", test_env_name )
  
  if ( test_reference_propfile_value == test_reference_env_value )
    testthat::fail( "Could not distinguish between property file and environmental variable value" )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( test_reference_name, test_reference_propfile_value, sep = "=" ) ),
                    con = file.path( test_cxapp_path, "cxapp.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "cxapp.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxapp.properties" )
  

  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  
  expected_name <- paste0( test_reference_name )
  
  expected_value <- base::trimws( test_reference_env_value )
  
  
  # -- assertions
  testthat::expect_equal( result$option( expected_name  ), expected_value )
  
})





testthat::test_that( "config.propertyRedirectEnvVarWithEnvVarNotExist", {
  
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
  
  
  # stage environmental variable 
  
  test_env_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 20 ), collapse = "" ) )

  if ( ! is.na(Sys.getenv( test_env_name, unset = NA )) )
    testthat::fail( "Environmental variable exists unexpected" )
  
  
  
  # test property value
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_propfile_value <- paste0( "$", test_env_name )

  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
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
  testthat::expect_equal( result$option( expected_name, unset = expected_value ), expected_value )
  
})

