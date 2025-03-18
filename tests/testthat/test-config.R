#
#  tests for cxapp::cxapp_config()
#
#


testthat::test_that( "config.empty", {
 
  
  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- assertions
  testthat::expect_length( result[[".attr"]][[".internal"]][["property.files"]], 0 )
   
})



testthat::test_that( "config.cxappConfigEmpty", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - update .libPaths

  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  # - inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test library path area")
  
  base::writeLines( "# empty test properties file", con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  expected_propfiles <- file.path( test_cxapp_path, "app.properties", fsep = "/")
  
  
  # -- assertions
  testthat::expect_equal( result[[".attr"]][[".internal"]][["property.files"]], expected_propfiles )

})





testthat::test_that( "config.cxappSimpleConfigSingle", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  # - test property value
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_value <- base::trimws(paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ))
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test library path area")
  
  base::writeLines( c( "# test properties file", 
                       paste( test_reference_name, test_reference_value, sep = "=" ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  expected_propfiles <- file.path( test_cxapp_path, "app.properties", fsep = "/")
  
  expected_props <- base::trimws(test_reference_value)
  names(expected_props) <- base::tolower(test_reference_name)
  
  expected_attr <- list( ".internal" = list( "property.files" = expected_propfiles ),
                         "app" = expected_props ) 
  

  # -- assertions
  testthat::expect_equal( result$.attr, expected_attr )

  
    
})



testthat::test_that( "config.cxappSimpleConfigMultiple", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")

  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  # - test property value
  test_reference_names <- utils::head( base::replicate( 1000, 
                                                        base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) ),
                                                        simplify = TRUE ),
                                       n = 10 )

  if ( length(unique(test_reference_names)) != length(test_reference_names) )
    testthat::fail( "Could not generate 10 unique names" )

  test_reference_values <- base::replicate( 10, 
                                            base::trimws( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ) ), 
                                            simplify = TRUE )
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( sort(test_reference_names), test_reference_values, sep = " = " )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  expected_propfiles <- file.path( test_cxapp_path, "app.properties", fsep = "/")
  
  expected_props <- base::trimws(test_reference_values)
  names(expected_props) <- base::tolower(sort(test_reference_names))
  
  expected_attr <- list( ".internal" = list( "property.files" = expected_propfiles ),
                         "app" = expected_props ) 
  
  
  # -- assertions
  testthat::expect_equal( result$.attr, expected_attr )
  
})



testthat::test_that( "config.cxappSimpleConfigMultipleFirstOccurenceWd", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test property name
  test_referene_name <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 25 ), collapse = "" )
  
  
  # - test property values

  test_reference_levels <- c( "working.directory", "app.home.config", "app.home", "library.path" )
    
  test_reference_values <-   base::replicate( length(test_reference_levels), 
                                              base::trimws(paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )), 
                                              simplify = TRUE )
  
  names(test_reference_values) <- test_reference_levels
  

  # - test working directory
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_wd ) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )

  
  current_wd <- base::getwd()
  
  on.exit( {
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( test_wd )
    
  
  base::writeLines( c( "# working directory properties file", 
                       paste0( test_referene_name, "=", test_reference_values["working.directory"] ) ), 
                    con = file.path( test_wd, "app.properties") )
  

  if ( ! file.exists( file.path( test_wd, "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test working directory" )
  
  
  
  # - APP_HOME

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


  
  base::writeLines( c( "# app home config properties file", 
                       paste0( test_referene_name, "=", test_reference_values["app.home.config"] ) ), 
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/" ) )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test app home config directory" )
  
  
  
  base::writeLines( c( "# app home config properties file", 
                       paste0( test_referene_name, "=", test_reference_values["app.home"] ) ), 
                    con = file.path( test_apphome, "app.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test app home config directory" )
  
    
  
  # - test library path
    
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( file.path( test_lbpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  

  base::writeLines( c( "# library path config properties file", 
                       paste0( test_referene_name, "=", test_reference_values["library.path"] ) ), 
                    con = file.path( test_lbpath, "cxapp",  "app.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxapp", "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test library path directory" )

  
  # - test config
  
  test_cfg <- cxapp::cxapp_config()
  
  
  # -- test
  
  result <- test_cfg$option( test_referene_name, unset = NA )


  # -- expected
  
  # - expected value
  expected_value <- unname(test_reference_values[ "working.directory" ])
  
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_value )


})




testthat::test_that( "config.cxappSimpleConfigMultipleFirstOccurenceAppHomeConfig", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test property name
  test_referene_name <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 25 ), collapse = "" )
  
  
  # - test property values
  
  test_reference_levels <- c( "working.directory", "app.home.config", "app.home", "library.path" )
  
  test_reference_values <-   base::replicate( length(test_reference_levels), 
                                              base::trimws(paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )), 
                                              simplify = TRUE )
  
  names(test_reference_values) <- test_reference_levels
  
  
  # - test working directory
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_wd ) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  
  current_wd <- base::getwd()
  
  on.exit( {
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( test_wd )
  
  
  # base::writeLines( c( "# working directory properties file", 
  #                      paste0( test_referene_name, "=", test_reference_values["working.directory"] ) ), 
  #                   con = file.path( test_wd, "app.properties") )

  if ( file.exists( file.path( test_wd, "app.properties", fsep = "/" ) ) )
    testthat::fail( "Unexpected app.properties in test working directory" )
  
  
  
  # - APP_HOME
  
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
  
  
  
  base::writeLines( c( "# app home config properties file", 
                       paste0( test_referene_name, "=", test_reference_values["app.home.config"] ) ), 
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test app home config directory" )
  
  
  
  base::writeLines( c( "# app home config properties file", 
                       paste0( test_referene_name, "=", test_reference_values["app.home"] ) ), 
                    con = file.path( test_apphome, "app.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test app home config directory" )
  
  
  
  # - test library path
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( file.path( test_lbpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  base::writeLines( c( "# library path config properties file", 
                       paste0( test_referene_name, "=", test_reference_values["library.path"] ) ), 
                    con = file.path( test_lbpath, "cxapp",  "app.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxapp", "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test library path directory" )
  
  
  # - test config
  
  test_cfg <- cxapp::cxapp_config()
  
  
  # -- test
  
  result <- test_cfg$option( test_referene_name, unset = NA )
  
  
  # -- expected
  
  # - expected value
  expected_value <- unname(test_reference_values[ "app.home.config" ])
  
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_value )
  
  
})




testthat::test_that( "config.cxappSimpleConfigMultipleFirstOccurenceAppHome", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test property name
  test_referene_name <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 25 ), collapse = "" )
  
  
  # - test property values
  
  test_reference_levels <- c( "working.directory", "app.home.config", "app.home", "library.path" )
  
  test_reference_values <-   base::replicate( length(test_reference_levels), 
                                              base::trimws(paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )), 
                                              simplify = TRUE )
  
  names(test_reference_values) <- test_reference_levels
  
  
  # - test working directory
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_wd ) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  
  current_wd <- base::getwd()
  
  on.exit( {
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( test_wd )
  
  
  # base::writeLines( c( "# working directory properties file", 
  #                      paste0( test_referene_name, "=", test_reference_values["working.directory"] ) ), 
  #                   con = file.path( test_wd, "app.properties") )
  
  if ( file.exists( file.path( test_wd, "app.properties", fsep = "/" ) ) )
    testthat::fail( "Unexpected app.properties in test working directory" )
  
  
  
  # - APP_HOME
  
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
  
  
  
  # base::writeLines( c( "# app home config properties file", 
  #                      paste0( test_referene_name, "=", test_reference_values["app.home.config"] ) ), 
  #                   con = file.path( test_apphome, "config", "app.properties", fsep = "/" ) )
  
  if ( file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/" ) ) )
    testthat::fail( "Unexpected app.properties in test app home config directory" )
  
  
  
  base::writeLines( c( "# app home config properties file", 
                       paste0( test_referene_name, "=", test_reference_values["app.home"] ) ), 
                    con = file.path( test_apphome, "app.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test app home config directory" )
  
  
  
  # - test library path
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( file.path( test_lbpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  base::writeLines( c( "# library path config properties file", 
                       paste0( test_referene_name, "=", test_reference_values["library.path"] ) ), 
                    con = file.path( test_lbpath, "cxapp",  "app.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxapp", "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test library path directory" )
  
  
  # - test config
  
  test_cfg <- cxapp::cxapp_config()
  
  
  # -- test
  
  result <- test_cfg$option( test_referene_name, unset = NA )
  
  
  # -- expected
  
  # - expected value
  expected_value <- unname(test_reference_values[ "app.home" ])
  
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_value )
  
  
})




testthat::test_that( "config.cxappSimpleConfigMultipleFirstOccurenceLibraryPath", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test property name
  test_referene_name <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 25 ), collapse = "" )
  
  
  # - test property values
  
  test_reference_levels <- c( "working.directory", "app.home.config", "app.home", "library.path" )
  
  test_reference_values <-   base::replicate( length(test_reference_levels), 
                                              base::trimws(paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )), 
                                              simplify = TRUE )
  
  names(test_reference_values) <- test_reference_levels
  
  
  # - test working directory
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_wd ) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  
  current_wd <- base::getwd()
  
  on.exit( {
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( test_wd )
  
  
  # base::writeLines( c( "# working directory properties file", 
  #                      paste0( test_referene_name, "=", test_reference_values["working.directory"] ) ), 
  #                   con = file.path( test_wd, "app.properties") )
  
  if ( file.exists( file.path( test_wd, "app.properties", fsep = "/" ) ) )
    testthat::fail( "Unexpected app.properties in test working directory" )
  
  
  
  # - APP_HOME
  
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
  
  
  
  # base::writeLines( c( "# app home config properties file", 
  #                      paste0( test_referene_name, "=", test_reference_values["app.home.config"] ) ), 
  #                   con = file.path( test_apphome, "config", "app.properties", fsep = "/" ) )
  
  if ( file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/" ) ) )
    testthat::fail( "Unexpected app.properties in test app home config directory" )
  
  
  
  # base::writeLines( c( "# app home config properties file", 
  #                      paste0( test_referene_name, "=", test_reference_values["app.home"] ) ), 
  #                   con = file.path( test_apphome, "app.properties", fsep = "/" ) )
  
  if ( file.exists( file.path( test_apphome, "app.properties", fsep = "/" ) ) )
    testthat::fail( "Unexpected app.properties in test app home config directory" )
  
  
  
  # - test library path
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( file.path( test_lbpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  base::writeLines( c( "# library path config properties file", 
                       paste0( test_referene_name, "=", test_reference_values["library.path"] ) ), 
                    con = file.path( test_lbpath, "cxapp",  "app.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxapp", "app.properties", fsep = "/" ) ) )
    testthat::fail( "Could not stage app.properties in test library path directory" )
  
  
  # - test config
  
  test_cfg <- cxapp::cxapp_config()
  
  
  # -- test
  
  result <- test_cfg$option( test_referene_name, unset = NA )
 
  
  # -- expected
  
  # - expected value
  expected_value <- unname(test_reference_values[ "library.path" ])
  
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_value )
  
  
})





testthat::test_that( "config.appSingleFile", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test library path
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( file.path( test_lbpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test property values
  
  test_property_contexts <- c( "app", 
                               paste( base::sample( c( base::letters, base::letters, as.character(0:9) ), 10 ), collapse = "" ) )
  
  
  test_property_values <- list()
  
  for ( xcontext in test_property_contexts ) {
    
    context_values <- base::replicate( 10, 
                                       base::trimws( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ) ), 
                                       simplify = TRUE )
    
    names(context_values) <- sort(base::replicate( 10, 
                                              paste( base::sample( c( base::letters, base::letters, as.character(0:9) ), 15 ), collapse = "" ) , 
                                              simplify = TRUE ))
    
    test_property_values[[ xcontext ]] <- context_values
  }
  

    
  # - inject app properties file in .libPaths

  prop_lines <- sapply( names(test_property_values[["app"]]), function(x) {
    paste( base::toupper(x), test_property_values[["app"]][x], sep = " = " )
  }, USE.NAMES = FALSE )
  

  base::writeLines( c( "# test properties file",
                       prop_lines ),
                    con = file.path( test_lbpath, "cxapp", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_lbpath, "cxapp", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties file" )

  rm ( list = "prop_lines" )
  
  
  # - inject properties in file
  #   note: using random context
  
  prop_context <- names(test_property_values[ names(test_property_values) != "app" ])
  
  prop_lines <- sapply( names(test_property_values[[prop_context]]), function(x) {
    paste( base::toupper(x), test_property_values[[prop_context]][x], sep = " = " )
  }, USE.NAMES = FALSE )

  
  test_prop_file <- file.path( test_root, paste0( prop_context, ".properties" ), fsep = "/" )

  base::writeLines( c( "# test properties file",
                       prop_lines ),
                    con = test_prop_file )

  if ( ! file.exists( test_prop_file ) )
    testthat::fail( "Could not stage properties file" )



  # -- test
  result <- cxapp::cxapp_config( test_prop_file )

  

  # -- expected
  
  expected_context <- names(test_property_values[ names(test_property_values) != "app" ])
  
  expected_attr <- list( list( "property.files" = c( file.path( test_lbpath, "cxapp", "app.properties", fsep = "/" ),
                                                     file.path( test_root, paste0( expected_context, ".properties"), fsep = "/" ) ) ),
                         test_property_values[["app"]],
                         test_property_values[[expected_context]] )
  
  names(expected_attr) <- c( ".internal", "app", expected_context )
                         

  
  # -- assertions
  
  testthat::expect_equal( result$.attr, expected_attr )
                       

})





testthat::test_that( "config.cxappSingleDir", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  

  
  # - test library path
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( file.path( test_lbpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test property values
  
  test_property_contexts <- c( "app", 
                               replicate( 10, 
                                          paste( base::sample( c( base::letters, base::letters, as.character(0:9) ), 10 ), collapse = "" ), 
                                          simplify = TRUE ) )
  
  
  test_property_values <- list()
  
  for ( xcontext in test_property_contexts ) {
    
    context_values <- base::replicate( 10, 
                                       base::trimws( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ) ), 
                                       simplify = TRUE )
    
    names(context_values) <- sort(base::replicate( 10, 
                                                   paste( base::sample( c( base::letters, base::letters, as.character(0:9) ), 15 ), collapse = "" ) , 
                                                   simplify = TRUE ))
    
    test_property_values[[ xcontext ]] <- context_values
  }
  
  
  
  
  # - APP_HOME
  
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
  

  
  # - stage test config   
  
  for ( xcontext in test_property_contexts ) {
    
    prop_lines <- sapply( names(test_property_values[[xcontext]]), function(x) {
      paste( base::toupper(x), test_property_values[[xcontext]][x], sep = " = " )
    }, USE.NAMES = FALSE )
    
    
    test_prop_file <- file.path( test_apphome, "config", paste0( xcontext, ".properties" ), fsep = "/" )
    
    base::writeLines( c( "# test properties file",
                         prop_lines ),
                      con = test_prop_file )
    
    if ( ! file.exists( test_prop_file ) )
      testthat::fail( "Could not stage properties file" )    
    
  }
  

  
  # -- test
  
  result <- cxapp::cxapp_config()

  
  
  # -- expected
  
  expected_contexts <- c( "app", 
                          sort(test_property_contexts[ test_property_contexts != "app" ]) )

  expected_propfiles <- file.path( test_apphome, "config", paste0( expected_contexts, ".properties"), fsep = "/" ) 
  
  
  
  expected_attr <- list( ".internal" = list( "property.files" = expected_propfiles ) )
  
  for ( xcontext in expected_contexts )
    expected_attr[[xcontext]] <- test_property_values[[xcontext]]
  

  # -- assertions
  
  testthat::expect_equal( result$.attr, expected_attr )

  
  
})






testthat::test_that( "config.configOptionFullReferenceTyped", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  

  # - test library path
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( file.path( test_lbpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  

  
  # - test property values
  
  test_property_contexts <- c( "app", 
                               paste( base::sample( c( base::letters, base::letters, as.character(0:9) ), 10 ), collapse = "" ) )
  
  
  test_property_values <- list()
  
  for ( xcontext in test_property_contexts ) {
    
    context_values <- base::replicate( 10, 
                                       base::trimws( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ) ), 
                                       simplify = TRUE )
    
    names(context_values) <- sort(base::replicate( 10, 
                                                   paste( base::sample( c( base::letters, base::letters, as.character(0:9) ), 15 ), collapse = "" ) , 
                                                   simplify = TRUE ))
    
    test_property_values[[ xcontext ]] <- context_values
  }
  
  

  test_property_refs <- gsub( "\\.", "/", names(unlist(test_property_values)) )


    
  # - inject app properties file in .libPaths
  
  prop_lines <- sapply( names(test_property_values[["app"]]), function(x) {
    paste( base::toupper(x), test_property_values[["app"]][x], sep = " = " )
  }, USE.NAMES = FALSE )
  
  
  base::writeLines( c( "# test properties file",
                       prop_lines ),
                    con = file.path( test_lbpath, "cxapp", "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_lbpath, "cxapp", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties file" )
  
  rm ( list = "prop_lines" )
  
  
  # - inject properties in file
  #   note: using random context
  
  prop_context <- names(test_property_values[ names(test_property_values) != "app" ])
  
  prop_lines <- sapply( names(test_property_values[[prop_context]]), function(x) {
    paste( base::toupper(x), test_property_values[[prop_context]][x], sep = " = " )
  }, USE.NAMES = FALSE )
  
  
  test_prop_file <- file.path( test_root, paste0( prop_context, ".properties" ), fsep = "/" )
  
  base::writeLines( c( "# test properties file",
                       prop_lines ),
                    con = test_prop_file )
  
  if ( ! file.exists( test_prop_file ) )
    testthat::fail( "Could not stage properties file" )
  

  # - test config
  
  test_config <- cxapp::cxapp_config( test_prop_file )
  
  
  # -- test

  results <- character(0)
  
  for ( xitem in test_property_refs )
    results[[xitem]] <- test_config$option( xitem, as.type = TRUE )


  # -- expected
  
  expected_values <- unlist( test_property_values )
  names(expected_values) <- gsub( "\\.", "/", names(expected_values) )
  
  
  
  # -- assertions
  
  testthat::expect_equal( results, expected_values )

  
})




# <---------------------------------------


testthat::test_that( "config.configOptionTypeEnabled", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
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
  
  
  # test property values
  
  test_values <- c( "enable", "enabled", "grant", "permit" )

  test_names <- base::toupper( paste0( "test", test_values ) )

    



  # inject cxapp properties file in .libPaths
  test_cxapp_libpath <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_libpath ) && ! dir.create( test_cxapp_libpath, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  
  base::writeLines( c( "# test properties file", 
                       paste( test_names, test_values, sep = " = ") ),
                    con = file.path( test_cxapp_libpath, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_libpath, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties file" )
  
  
  
  

  
  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  
  expected_names <- test_names
  
  expected_values <- base::rep_len( TRUE, length(expected_names) )
  
  
  
  # -- assertions

  actual_values <- base::rep_len( NA, length(expected_names) )
  names(actual_values) <- expected_names
  
  for ( xitem in expected_names )
    actual_values[ xitem ] <- result$option( xitem, as.type = TRUE )
  
  
  testthat::expect_true( all( actual_values ) )

})



testthat::test_that( "config.configOptionTypeDisabled", {
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
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
  
  
  # test property values
  
  test_values <- c( "disable", "disabled", "revoke", "deny" )
  
  test_names <- base::toupper( paste0( "test", test_values ) )
  
  
  
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_libpath <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_libpath ) && ! dir.create( test_cxapp_libpath, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  
  base::writeLines( c( "# test properties file", 
                       paste( test_names, test_values, sep = " = ") ),
                    con = file.path( test_cxapp_libpath, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_libpath, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxapp properties file" )
  
  
  
  
  
  
  # -- test
  result <- cxapp::cxapp_config()
  
  
  # -- expected
  
  expected_names <- test_names
  
  expected_values <- base::rep_len( TRUE, length(expected_names) )
  
  
  
  # -- assertions
  
  actual_values <- base::rep_len( NA, length(expected_names) )
  names(actual_values) <- expected_names
  
  for ( xitem in expected_names )
    actual_values[ xitem ] <- result$option( xitem, as.type = TRUE )
  
  
  testthat::expect_false( any( actual_values ) )
  
})







