#
#  tests for cxapp::cxapp_config()
#  
#  Properties
#

#' @cx.testsfor cxapp::cxapp_config()




testthat::test_that( "config.optionPropertiesSingleFile", {
  
  #' @cx.tests Get value for a defined option from a single app.properties file
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - APP_HOME
  
  #   note: case insensitive matching of APP_HOME
  prev_apphome <- Sys.getenv( base::names(Sys.getenv())[ match( "APP_HOME", base::toupper(base::names(Sys.getenv())) ) ], 
                              unset = NA,
                              names = TRUE )
  
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
  
  
  # - stage cxapp in .libPaths()
  
  prev_libpath <- .libPaths()
  
  on.exit({
    .libPaths( prev_libpath )
  }, add = TRUE )
  
  
  test_libpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( file.path( test_libpath, "cxapp", fsep = "/" ) ) && ! dir.create( file.path( test_libpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxapp libpath directory" )
  
  
  .libPaths( append( test_libpath, .libPaths() ) )
  
  
  
  # - current working directory
  
  prev_wd <- base::getwd()
  
  on.exit({
    base::setwd( prev_wd )
  }, add = TRUE)
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_wd ) && ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  

  # -- test properties
  
  test_properties <- replicate( base::sample( 1:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1) ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9), "." ), base::sample( 5:40, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) )
                                                                       ), collapse = ""),
                                               simplify = TRUE )


  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )
  

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
  
  
  # - test configuration object
  test_obj <- cxapp::cxapp_config()



  # -- test
  result <- sapply( base::names(test_properties),
                    function(x) { test_obj$option( x, unset = NA ) },
                    USE.NAMES = FALSE )


  # -- expected

  # - search tree
  expected_searchtree <- test_srchtree


  # - property files

  expected_propfiles <- lapply( test_propfiles, function(x) {
    list( "path" = x,
          "sha" = digest::digest( x, algo = "sha1", file = TRUE ) )
  })


  # - properties
  expected_props <- test_properties
  base::names(expected_props) <- base::tolower(base::names(expected_props))


  # -- assertions

  # - properties
  #   standardize comparison on lower case names ... ignores case
  
  base::names(result) <- base::tolower(base::names(result))
  testthat::expect_equal( result[ base::sort(base::names(result)) ], expected_props[ base::sort(base::names(expected_props)) ] )

})





testthat::test_that( "config.optionGetFirstExistingProperty", {
  
  #' @cx.tests Get property value from first existing property in a vector of named properties
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - APP_HOME
  
  #   note: case insensitive matching of APP_HOME
  prev_apphome <- Sys.getenv( base::names(Sys.getenv())[ match( "APP_HOME", base::toupper(base::names(Sys.getenv())) ) ], 
                              unset = NA,
                              names = TRUE )
  
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
  
  
  # - stage cxapp in .libPaths()
  
  prev_libpath <- .libPaths()
  
  on.exit({
    .libPaths( prev_libpath )
  }, add = TRUE )
  
  
  test_libpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( file.path( test_libpath, "cxapp", fsep = "/" ) ) && ! dir.create( file.path( test_libpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxapp libpath directory" )
  
  
  .libPaths( append( test_libpath, .libPaths() ) )
  
  
  
  # - current working directory
  
  prev_wd <- base::getwd()
  
  on.exit({
    base::setwd( prev_wd )
  }, add = TRUE)
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_wd ) && ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  
  
  # -- test properties
  
  test_properties <- replicate( base::sample( 1:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1) ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9), "." ), base::sample( 5:40, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) )
                                               ), collapse = ""),
                                               simplify = TRUE )
  
  # note target property
  test_targetpropert_index <- 2
  
  prop_lines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )
  
  
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  base::writeLines( utils::tail( prop_lines, n = length(prop_lines) - test_targetpropert_index + 1) , con = test_propfiles )
  
  
  
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
  
  
  # - test configuration object
  test_obj <- cxapp::cxapp_config()
  
  # -- test
  result <- test_obj$option( base::names(test_properties), unset = NA ) 

  
  # -- expected
  
  # - search tree
  expected_searchtree <- test_srchtree
  
  
  # - property files
  
  expected_propfiles <- lapply( test_propfiles, function(x) {
    list( "path" = x, 
          "sha" = digest::digest( x, algo = "sha1", file = TRUE ) )
  })
  
  
  # - properties
  expected_props <- test_properties[ test_targetpropert_index ]
  
  # standardize on lower case property names
  base::names(expected_props) <- base::tolower(base::names(expected_props))
  

  # -- assertions
  
  # - properties
  
  #   standardize comparison on lower case names ... ignores case
  base::names(result) <- base::tolower(base::names(result))
  
  testthat::expect_equal( result[ base::sort(base::names(result)) ], expected_props[ base::sort(base::names(expected_props)) ] )
  
})




testthat::test_that( "config.optionGetFirstDefPropertyValue", {
  
  #' @cx.tests Get property value from first occurrence in search tree 
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - APP_HOME
  
  #   note: case insensitive matching of APP_HOME
  prev_apphome <- Sys.getenv( base::names(Sys.getenv())[ match( "APP_HOME", base::toupper(base::names(Sys.getenv())) ) ], 
                              unset = NA,
                              names = TRUE )
  
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
  
  
  # - stage cxapp in .libPaths()
  
  prev_libpath <- .libPaths()
  
  on.exit({
    .libPaths( prev_libpath )
  }, add = TRUE )
  
  
  test_libpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( file.path( test_libpath, "cxapp", fsep = "/" ) ) && ! dir.create( file.path( test_libpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxapp libpath directory" )
  
  
  .libPaths( append( test_libpath, .libPaths() ) )
  
  
  
  # - current working directory
  
  prev_wd <- base::getwd()
  
  on.exit({
    base::setwd( prev_wd )
  }, add = TRUE)
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_wd ) && ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  
  
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )  

  
  test_propfiles <- paste0( replicate( length(test_srchtree),
                                       paste( base::sample( c( base::letters, as.character(0:9) ), 15), collapse = ""),
                                       simplify = TRUE ), 
                            ".properties" )
  
    
  
  # -- test properties
  
  test_propname <- paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) ), 
                             base::sample( c( base::LETTERS, base::letters, as.character(0:9), "." ), base::sample( 5:40, 1) ), 
                             base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) ) ), collapse = "")
  
  test_propvalues <- replicate( length(test_srchtree), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1) ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_propvalues ) <- test_propfiles
  


  # - stage property files
  for ( xitem in base::names(test_propvalues) ) {

    # derive the path
    xpath <- file.path( test_srchtree[ match(xitem, base::names(test_propvalues)) ], xitem, fsep = "/")

    # register the property name
    base::writeLines( paste0( test_propname, " = ", test_propvalues[xitem] ), con = xpath )

  }


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


  # - test configuration object
  test_obj <- cxapp::cxapp_config()

  # -- test
  result <- test_obj$option( test_propname, unset = NA )


  # -- expected

  # - expected value
  expected_propvalue <- utils::head( test_propvalues, n = 1 )
  base::names(expected_propvalue) <- base::tolower(test_propname)


  # -- assertions
  testthat::expect_equal( result, expected_propvalue )


})




testthat::test_that( "config.optionUnsetNA", {
  
  #' @cx.tests Get property value when property is not defined and unset equals NA
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - APP_HOME
  
  #   note: case insensitive matching of APP_HOME
  prev_apphome <- Sys.getenv( base::names(Sys.getenv())[ match( "APP_HOME", base::toupper(base::names(Sys.getenv())) ) ], 
                              unset = NA,
                              names = TRUE )
  
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
  
  
  # - stage cxapp in .libPaths()
  
  prev_libpath <- .libPaths()
  
  on.exit({
    .libPaths( prev_libpath )
  }, add = TRUE )
  
  
  test_libpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( file.path( test_libpath, "cxapp", fsep = "/" ) ) && ! dir.create( file.path( test_libpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxapp libpath directory" )
  
  
  .libPaths( append( test_libpath, .libPaths() ) )
  
  
  
  # - current working directory
  
  prev_wd <- base::getwd()
  
  on.exit({
    base::setwd( prev_wd )
  }, add = TRUE)
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_wd ) && ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  
  
  # -- test properties
  
  test_properties <- replicate( base::sample( 1:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1) ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9), "." ), base::sample( 5:25, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) )
                                               ), collapse = ""),
                                               simplify = TRUE )
  
  prop_lines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  base::writeLines( prop_lines, con = test_propfiles )
  
  
  
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
  
  
  # - test configuration object
  test_obj <- cxapp::cxapp_config()
  
  
  # - test property name
  #   note: create a name based on the existing property names
  test_propname <- paste( base::sample( base::names(test_properties), 2, replace = TRUE ), collapse = "." )

  
  # -- test
  result <- test_obj$option( test_propname, unset = NA ) 
  

  # -- assertions

  testthat::expect_true( is.na( result ) )  

})




testthat::test_that( "config.optionUnsetNull", {
  
  #' @cx.tests Get property value when property is not defined and unset equals NULL
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - APP_HOME
  
  #   note: case insensitive matching of APP_HOME
  prev_apphome <- Sys.getenv( base::names(Sys.getenv())[ match( "APP_HOME", base::toupper(base::names(Sys.getenv())) ) ], 
                              unset = NA,
                              names = TRUE )
  
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
  
  
  # - stage cxapp in .libPaths()
  
  prev_libpath <- .libPaths()
  
  on.exit({
    .libPaths( prev_libpath )
  }, add = TRUE )
  
  
  test_libpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( file.path( test_libpath, "cxapp", fsep = "/" ) ) && ! dir.create( file.path( test_libpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxapp libpath directory" )
  
  
  .libPaths( append( test_libpath, .libPaths() ) )
  
  
  
  # - current working directory
  
  prev_wd <- base::getwd()
  
  on.exit({
    base::setwd( prev_wd )
  }, add = TRUE)
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_wd ) && ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  
  
  # -- test properties
  
  test_properties <- replicate( base::sample( 1:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1) ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9), "." ), base::sample( 5:25, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) )
                                               ), collapse = ""),
                                               simplify = TRUE )
  
  prop_lines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  base::writeLines( prop_lines, con = test_propfiles )
  
  
  
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
  
  
  # - test configuration object
  test_obj <- cxapp::cxapp_config()
  
  
  # - test property name
  #   note: create a name based on the existing property names
  test_propname <- paste( base::sample( base::names(test_properties), 2, replace = TRUE ), collapse = "." )
  
  
  # -- test
  result <- test_obj$option( test_propname, unset = NULL ) 
  
  
  # -- assertions
  
  testthat::expect_true( is.null( result ) )  
  
})








testthat::test_that( "config.optionUnsetEmptyString", {
  
  #' @cx.tests Get property value when property is not defined and unset equals an empty string
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - APP_HOME
  
  #   note: case insensitive matching of APP_HOME
  prev_apphome <- Sys.getenv( base::names(Sys.getenv())[ match( "APP_HOME", base::toupper(base::names(Sys.getenv())) ) ], 
                              unset = NA,
                              names = TRUE )
  
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
  
  
  # - stage cxapp in .libPaths()
  
  prev_libpath <- .libPaths()
  
  on.exit({
    .libPaths( prev_libpath )
  }, add = TRUE )
  
  
  test_libpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( file.path( test_libpath, "cxapp", fsep = "/" ) ) && ! dir.create( file.path( test_libpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxapp libpath directory" )
  
  
  .libPaths( append( test_libpath, .libPaths() ) )
  
  
  
  # - current working directory
  
  prev_wd <- base::getwd()
  
  on.exit({
    base::setwd( prev_wd )
  }, add = TRUE)
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_wd ) && ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  
  
  # -- test properties
  
  test_properties <- replicate( base::sample( 1:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1) ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9), "." ), base::sample( 5:25, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) )
                                               ), collapse = ""),
                                               simplify = TRUE )
  
  prop_lines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  base::writeLines( prop_lines, con = test_propfiles )
  
  
  
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
  
  
  # - test configuration object
  test_obj <- cxapp::cxapp_config()
  
  
  # - test property name
  #   note: create a name based on the existing property names
  test_propname <- paste( base::sample( base::names(test_properties), 2, replace = TRUE ), collapse = "." )
  
  
  # -- test
  result <- test_obj$option( test_propname, unset = "" ) 
  
  
  # -- assertions
  
  testthat::expect_equal( result, "" )  
  
})





testthat::test_that( "config.optionUnsetAssignedString", {
  
  #' @cx.tests Get property value when property is not defined and unset equals a specified string
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - APP_HOME
  
  #   note: case insensitive matching of APP_HOME
  prev_apphome <- Sys.getenv( base::names(Sys.getenv())[ match( "APP_HOME", base::toupper(base::names(Sys.getenv())) ) ], 
                              unset = NA,
                              names = TRUE )
  
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
  
  
  # - stage cxapp in .libPaths()
  
  prev_libpath <- .libPaths()
  
  on.exit({
    .libPaths( prev_libpath )
  }, add = TRUE )
  
  
  test_libpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( file.path( test_libpath, "cxapp", fsep = "/" ) ) && ! dir.create( file.path( test_libpath, "cxapp", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxapp libpath directory" )
  
  
  .libPaths( append( test_libpath, .libPaths() ) )
  
  
  
  # - current working directory
  
  prev_wd <- base::getwd()
  
  on.exit({
    base::setwd( prev_wd )
  }, add = TRUE)
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_wd ) && ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  
  
  # -- test properties
  
  test_properties <- replicate( base::sample( 1:20, 1), 
                                paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1) ), collapse = ""),
                                simplify = TRUE )
  
  base::names( test_properties ) <- replicate( length(test_properties), 
                                               paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9), "." ), base::sample( 5:25, 1) ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1) )
                                               ), collapse = ""),
                                               simplify = TRUE )
  
  prop_lines <- base::unlist(lapply( base::names(test_properties), function(x) {
    paste( x, test_properties[x], sep = " = ") 
  }))
  
  
  # - search tree
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )
  
  
  # - stage property files
  
  test_propfiles <- file.path( utils::head( test_srchtree, n = 1 ), "app.properties", fsep = "/" )
  
  base::writeLines( prop_lines, con = test_propfiles )
  
  
  
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
  
  
  # - test configuration object
  test_obj <- cxapp::cxapp_config()
  
  
  # - test property name
  #   note: create a name based on the existing property names
  test_propname <- paste( base::sample( base::names(test_properties), 2, replace = TRUE ), collapse = "." )
  
  
  # - test unset string value
  test_unset <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 20:60, 1), replace = TRUE ), collapse = "")
  
  
  # -- test
  result <- test_obj$option( test_propname, unset = test_unset ) 
  
  
  # -- expected
  
  expected_value <- test_unset
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_value )  
  
})












