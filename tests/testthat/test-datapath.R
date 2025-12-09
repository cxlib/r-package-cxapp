#
#
#  Tests for data path
#
#


#' @cx.testsfor cxapp::cxapp_datapath()



testthat::test_that( "datapath.noConfig", {
  
  
  #' @cx.tests Get data path with no configuration
  
  
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
  
  
  
  # - move cached config out of the way
  
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
  
  

  
  # - move cached app node out of the way
  
  prev_appnode <- NA
  
  if ( exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv) )
    prev_appnode <- base::get( ".cxapp.wrkcache.appnode", envir = .GlobalEnv )
  
  on.exit({
    if ( ! is.na(prev_appnode) )
      base::assign( ".cxapp.wrkcache.appnode", prev_appnode, envir = base::.GlobalEnv )
  }, add = TRUE )
  
  
  if ( base::exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
    rm( list = ".cxapp.wrkcache.appnode", envir = .GlobalEnv )
  
  if ( base::exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash current app node" )
  
  
  
  # - test app node
  test_appnode <- cxapp::cxapp_appnode()
  
  #   note: ensure app node application cache does not exist
  if ( dir.exists( file.path( base::tempdir(), paste0( ".application-data-", test_appnode ), fsep = "/" ) ) )
    testthat::fail( "Unexpected application data directory for app node exists")
  
  
  # -- test
  result <- cxapp::cxapp_datapath()
  
  on.exit({
    base::unlink( result, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  
  # -- expected
  
  expected_datapath <- cxapp::cxapp_standardpath( file.path( base::tempdir(), paste0( ".application-data-", test_appnode ), fsep = "/" ) )
  
  
  # -- assertions

  # - returns data path
  testthat::expect_equal( result, expected_datapath )
  
  # - creates transient data path
  testthat::expect_true( dir.exists(expected_datapath) )
  
  
})





testthat::test_that( "datapath.noConfigSubDirectories", {
  
  
  #' @cx.tests Get data path with no configuration and specified vector of directory levels
  
  
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
  
  
  
  # - move cached config out of the way
  
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
  
  
  
  
  # - move cached app node out of the way
  
  prev_appnode <- NA
  
  if ( exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv) )
    prev_appnode <- base::get( ".cxapp.wrkcache.appnode", envir = .GlobalEnv )
  
  on.exit({
    if ( ! is.na(prev_appnode) )
      base::assign( ".cxapp.wrkcache.appnode", prev_appnode, envir = base::.GlobalEnv )
  }, add = TRUE )
  
  
  if ( base::exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
    rm( list = ".cxapp.wrkcache.appnode", envir = .GlobalEnv )
  
  if ( base::exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash current app node" )
  
  
  
  # - test app node
  test_appnode <- cxapp::cxapp_appnode()
  
  #   note: ensure app node application cache does not exist
  if ( dir.exists( file.path( base::tempdir(), paste0( ".application-data-", test_appnode ), fsep = "/" ) ) )
    testthat::fail( "Unexpected application data directory for app node exists")
  
  
  # - test sub-dirs
  test_subdirs <- replicate( sample( 2:10, 1), 
                             paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), sample( 5:50, 1), replace = TRUE) , collapse = ""), 
                             simplify = TRUE)
  
  
  # -- test
  result <- do.call( cxapp::cxapp_datapath, as.list(test_subdirs) )
  
  
  on.exit({
    base::unlink( result, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  
  # -- expected
  
  lst_args <- c( base::tempdir(), 
                 paste0( ".application-data-", test_appnode ), 
                 test_subdirs, 
                 "fsep" = "/" )
  
  expected_datapath <- cxapp::cxapp_standardpath( do.call( file.path, as.list( lst_args ) ) )
  
  
  
  # -- assertions
  
  # - returns data path
  #   note: not expecting the paths to exist
  testthat::expect_equal( result, expected_datapath )
  
})




testthat::test_that( "datapath.appDataPathNotExists", {
  
  
  #' @cx.tests Get data path with configuration property referring to a path that does not exist results in an error
  
  
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
  
  
  
  # - move cached config out of the way
  
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
  
  
  # - test app data path
  test_datapath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-appdata-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_datapath ) )
    testthat::fail( "Unexpected test data path exists" )
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.DATA =", test_datapath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  

  # -- test
  testthat::expect_error( cxapp::cxapp_datapath(), regexp = "^The data path directory or its parent does not exist$" )
  
  
  # -- assertions

  testthat::expect_false( dir.exists(test_datapath) )


})



testthat::test_that( "datapath.appDataPathNoSubdirectories", {
  
  
  #' @cx.tests Get data path with configuration property referring to a path exists
  
  
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
  
  
  
  # - move cached config out of the way
  
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
  
  
  
  # - test app data path
  test_datapath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-appdata-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_datapath ) || ! dir.create( test_datapath, recursive = TRUE ) )
    testthat::fail( "Could not stage test data path" )
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.DATA =", test_datapath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # -- test
  result <- cxapp::cxapp_datapath()


  
  # -- expected
  
  expected_datapath <- test_datapath
  
  
  
  # -- assertions
  
  # - returns data path
  testthat::expect_equal( result, expected_datapath )
  
})





testthat::test_that( "datapath.appDataPathSubdirectories", {
  
  
  #' @cx.tests Get data path with configuration property referring to a path exists and a vector of subdirectory levels
  
  
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
  
  
  
  # - move cached config out of the way
  
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
  
  
  
  # - test app data path
  test_datapath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-appdata-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_datapath ) || ! dir.create( test_datapath, recursive = TRUE ) )
    testthat::fail( "Could not stage test data path" )
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.DATA =", test_datapath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test sub-dirs
  test_subdirs <- replicate( sample( 2:10, 1), 
                             paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), sample( 5:50, 1), replace = TRUE) , collapse = ""), 
                             simplify = TRUE)
  
  
  
  # -- test
  result <- do.call( cxapp::cxapp_datapath, as.list(test_subdirs) )
  
  
  
  # -- expected
  
  lst_args <- c( test_datapath, 
                 test_subdirs, 
                 "fsep" = "/" )
  
  expected_datapath <- do.call( file.path, as.list(lst_args) ) 
  
  
  
  # -- assertions
  
  # - returns data path
  #   note: not expecting the paths to exist
  testthat::expect_equal( result, expected_datapath )
  
})




