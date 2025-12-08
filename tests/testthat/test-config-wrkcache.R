#
#  tests for cxapp::cxapp_config()
#  
#  Configuration cache
#

#' @cx.testsfor cxapp::cxapp_config()




testthat::test_that( "config.cacheDefault", {
  
  #' @cx.tests Configuration work cache is enabled by default
  
  
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
                                               paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1), replace = TRUE ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9), "." ), base::sample( 5:40, 1), replace = TRUE ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1), replace = TRUE )
                                               ), collapse = ""),
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

  if ( base::exists( ".cxapp.wrkcache.config", envir = base::.GlobalEnv ) )
    testthat::fail( "Unable clear existing config cache .cxapp.wrkcache.config" )


  
    
  # - test configuration object
  #   note: expect to initiate work cache
  test_configobj <- cxapp::cxapp_config()

  #   assert work cache was initiated with first call to cxapp::cxapp_config()
  testthat::expect_true( base::exists( ".cxapp.wrkcache.config", envir = base::.GlobalEnv ) )
  
  
  # - update test property value

  test_propupdate <- test_properties

  #   note: randomly select one property
  #   note: length of value is not same as first set of property values ... makes sure random chance does not create the same value
  test_propupdate[ sample(base::names(test_propupdate), 1) ] <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( c( 1:19, 70:90), 1), replace = TRUE ), collapse = "")

  #   note: verifies that it is the same properties being defined
  testthat::expect_equal( base::sort(base::names(test_properties)), base::sort(base::names(test_propupdate)) )
  
  
  # - update test property value
  test_proplineupdates <- base::unlist(lapply( base::names(test_propupdate), function(x) {
    paste( x, test_propupdate[x], sep = " = ") 
  }))
  
  base::writeLines( test_proplineupdates, con = test_propfiles )
  
  
  
  # - test object
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



testthat::test_that( "config.cacheDisabled", {
  
  #' @cx.tests Disabled configuration work cache forces imports of properties from property files
  
  
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
                                               paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1), replace = TRUE ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9), "." ), base::sample( 5:40, 1), replace = TRUE ), 
                                                         base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( 1:5, 1), replace = TRUE )
                                               ), collapse = ""),
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
  
  if ( base::exists( ".cxapp.wrkcache.config", envir = base::.GlobalEnv ) )
    testthat::fail( "Unable clear existing config cache .cxapp.wrkcache.config" )
  
  
  
  
  # - test configuration object
  #   note: expect to initiate work cache
  test_configobj <- cxapp::cxapp_config()
  
  #   assert work cache was initiated with first call to cxapp::cxapp_config()
  testthat::expect_true( base::exists( ".cxapp.wrkcache.config", envir = base::.GlobalEnv ) )
  
  
  # - update test property value
  
  test_propupdate <- test_properties
  
  #   note: randomly select one property
  #   note: length of value is not same as first set of property values ... makes sure random chance does not create the same value
  test_propupdate[ sample(base::names(test_propupdate), 1) ] <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample( c( 1:19, 70:90), 1), replace = TRUE ), collapse = "")
  
  #   note: verifies that it is the same properties being defined
  testthat::expect_equal( base::sort(base::names(test_properties)), base::sort(base::names(test_propupdate)) )
  
  
  # - update test property value
  test_proplineupdates <- base::unlist(lapply( base::names(test_propupdate), function(x) {
    paste( x, test_propupdate[x], sep = " = ") 
  }))
  
  base::writeLines( test_proplineupdates, con = test_propfiles )
  
  
  
  # - test object
  test_obj <- cxapp::cxapp_config( cached = FALSE )
  
  
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
  #   note: expecting updated properties ... not the cached ones
  expected_props <- test_propupdate
  base::names(expected_props) <- base::tolower(base::names(expected_props))
  
  
  # -- assertions
  
  # - properties
  #   standardize comparison on lower case names ... ignores case
  
  base::names(result) <- base::tolower(base::names(result))
  testthat::expect_equal( result[ base::sort(base::names(result)) ], expected_props[ base::sort(base::names(expected_props)) ] )
  
})
