#
#  Tests for application cache
#
#
#


#' @cx.testsfor cxapp::cxapp_applicationcache()



testthat::test_that( "appcache.noConfig", {

  #' @cx.tests Default application cache with no configuration 
  

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
  if ( dir.exists( file.path( base::tempdir(), paste0( ".application-cache-", test_appnode ), fsep = "/" ) ) )
    testthat::fail( "Unexpected application cache for app node exists")


  # -- test
  result <- cxapp::cxapp_applicationcache()



  # -- expected

  # - expected cache path
  #   note: relying on that app node is not cached so reproducible
  expected_cachepath <- cxapp::cxapp_standardpath( file.path( base::tempdir(), paste0( ".application-cache-", test_appnode), fsep = "/") )



  # -- assertions

  testthat::expect_true( dir.exists(expected_cachepath) )

})





testthat::test_that( "appcache.configAppCacheNotExist", {

  #' @cx.tests Application cache with configured cache directory that does not exist results in an error
  
  
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
  
  

  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) )
    testthat::fail( "Unextpected test cache path exists" )
  

  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )


  # -- test
  testthat::expect_error( cxapp::cxapp_applicationcache(),
                          regexp = "^Cache directory does not exist" )


})





testthat::test_that( "appcache.configAppCache", {

  #' @cx.tests Application cache with configured cache directory 
  

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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  

  # -- test
  result <- cxapp::cxapp_applicationcache()


  # -- expected
  expected_cachepath <- test_cachepath


  # -- assertions
  testthat::expect_equal( result$.attr[["cache.path"]], expected_cachepath )


})





testthat::test_that( "appcache.addNothingSpecified", {


  #' @cx.tests Missing vector of files to add to an application cache results in an error
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  

  # - test cache

  test_cache <- cxapp::cxapp_applicationcache()


  # -- test

  testthat::expect_error( test_cache$add(), regexp = "^Vector of files missing$" )


})






testthat::test_that( "appcache.addNull", {

  
  #' @cx.tests Vector of files to add to an application cache equal to NULL results in an error
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # - test cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  

  # -- test

  testthat::expect_error( test_cache$add( NULL ), regexp = "^Vector of files missing$" )


})




testthat::test_that( "appcache.addNA", {

  
  #' @cx.tests Vector of files to add to an application cache equal to NA results in an error
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # - test cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # -- test

  testthat::expect_error( test_cache$add( NA ), regexp = "^Vector of files missing$" )


})




testthat::test_that( "appcache.addNothingToDo", {

  #' @cx.tests Empty vector of files to add to an application cache results in no action
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # - test cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  

  # -- test

  result <- test_cache$add( character(0) )


  # -- assertions

  testthat::expect_true( result )

  testthat::expect_length( list.files( test_cachepath, recursive = FALSE, full.names = FALSE ), 0 )


})




testthat::test_that( "appcache.addFileNotExist", {

  #' @cx.tests Add vector of files to an application cache results in an error when one or more specified files do not exist
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  

  # - test files

  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")


  test_files <- replicate( 10,
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
                           simplify = TRUE )

  test_content <- replicate( 10,
                             paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                             simplify = TRUE )


  for ( xfile in utils::head( test_files, n = length(test_files) - 1 ) ) {

    base::writeLines( test_content[ match( xfile, test_files ) ],
                      con = xfile )

    if ( ! file.exists( xfile ) )
      testthat::fail( "Could not stage test file")
  }


  if ( file.exists( utils::tail( test_files, n = 1 ) ) )
    testthat::fail( "Unexpected test file exists" )




  # - test cache

  test_cache <- cxapp::cxapp_applicationcache()


  # -- test

  testthat::expect_error( test_cache$add( test_files ), regexp = "^One or more files do not exist$" )


})







testthat::test_that( "appcache.addFileNotNamed", {

  
  #' @cx.tests Add vector of files to an application cache when specified files are not named
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  

  # - test files

  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")


  test_files <- replicate( 10,
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
                           simplify = TRUE )

  test_content <- replicate( 10,
                             paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                             simplify = TRUE )

  test_file_sha1 <- character(0)

  for ( xfile in test_files ) {

    base::writeLines( test_content[ match( xfile, test_files ) ],
                      con = xfile )

    test_file_sha1 <- append( test_file_sha1,
                              digest::digest( xfile, algo = "sha1", file = TRUE ) )

    if ( ! file.exists( xfile ) )
      testthat::fail( "Could not stage test file")
  }




  # - test cache

  test_cache <- cxapp::cxapp_applicationcache()


  # -- test

  result <- test_cache$add( test_files )



  # -- expected

  lst_cacheobjects <- character(0)

  for ( xfileref in test_files )
    lst_cacheobjects <- append( lst_cacheobjects, digest::digest( base::tolower(base::trimws(xfileref)), algo = "sha1", file = FALSE ) )


  # - expected object files

  expected_files <- file.path( test_cachepath, lst_cacheobjects, fsep = "/" )


  # - expected file sha1

  expected_file_sha1s <- test_file_sha1


  # - expected lck files (regex)

  expected_lckfile_patterns <- paste0("^", lst_cacheobjects, "\\-\\d{8}\\-\\d{4}\\.lck$" )


  # - expected content

  expected_content <- test_content
  names(expected_content) <- expected_files


  # -- assertions

  # - result returned is boolean
  testthat::expect_true( result )


  # - cached files exist
  testthat::expect_true( all(file.exists(expected_files)) )


  # - lck files for cached items

  lst_lckfiles <- cxapp::cxapp_standardpath( list.files( test_cachepath,
                                                         pattern = "^[a-z0-9]{40}\\-\\d{8}\\-\\d{4}\\.lck$",
                                                         full.names = FALSE,
                                                         recursive = FALSE ) )

  testthat::expect_length(lst_lckfiles, length(expected_files) )

  for ( xpattern in expected_lckfile_patterns )
    testthat::expect_true( any(grepl( xpattern, lst_lckfiles, ignore.case = TRUE, perl = TRUE )) )


  # - check content of the cached file
  for ( xfile in expected_files )
    testthat::expect_equal( digest::digest( xfile, algo = "sha1", file = TRUE ),
                            expected_file_sha1s[ match( xfile, expected_files ) ] )

})





testthat::test_that( "appcache.addFileNotAllNamed", {

  #' @cx.tests Add vector of files to an application cache when not all specified files are named
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  

  # - test files

  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")


  test_files <- replicate( 10,
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
                           simplify = TRUE )

  test_content <- replicate( 10,
                             paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                             simplify = TRUE )


  for ( xfile in test_files ) {

    base::writeLines( test_content[ match( xfile, test_files ) ],
                      con = xfile )

    if ( ! file.exists( xfile ) )
      testthat::fail( "Could not stage test file")
  }


  # - name entry

  test_list <- utils::head( test_files, n = length(test_files) - 1 )

  test_list_namedentry <- utils::tail( test_files, n = 1 )
  names(test_list_namedentry) <- base::basename(test_list_namedentry)

  test_list <- append( test_list, test_list_namedentry )




  # - connect cache

  test_cache <- cxapp::cxapp_applicationcache()


  # -- test

  result <- test_cache$add( test_list )



  # -- expected


  # - generate a list of cache object references

  lst_names <- base::tolower(base::trimws(names( test_list )))

  for ( xidx in which( lst_names == "" ) )
    lst_names[ xidx ] <- test_list[ xidx ]


  lst_cacheobjects <- character(0)

  for ( xfileref in lst_names )
    lst_cacheobjects <- append( lst_cacheobjects, digest::digest( base::tolower(base::trimws(xfileref)), algo = "sha1", file = FALSE ) )



  # - expected object files

  expected_files <- file.path( test_cachepath, lst_cacheobjects, fsep = "/" )


  # - expected lck files (regex)

  expected_lckfile_patterns <- paste0("^", lst_cacheobjects, "\\-\\d{8}\\-\\d{4}\\.lck$" )


  # -- assertions

  # - result returned is boolean
  testthat::expect_true( result )


  # - cached files exist
  testthat::expect_true( all(file.exists(expected_files)) )


  # - lck files for cached items

  lst_lckfiles <- cxapp::cxapp_standardpath( list.files( test_cachepath,
                                                         pattern = "^[a-z0-9]{40}\\-\\d{8}\\-\\d{4}\\.lck$",
                                                         full.names = FALSE,
                                                         recursive = FALSE ) )

  testthat::expect_length(lst_lckfiles, length(expected_files) )

  for ( xpattern in expected_lckfile_patterns )
    testthat::expect_true( any(grepl( xpattern, lst_lckfiles, ignore.case = TRUE, perl = TRUE )) )


})





testthat::test_that( "appcache.addFileAllNamed", {

  
  #' @cx.tests Add vector of files to an application cache when all specified files are named
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  

  # - test files

  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")


  test_files <- replicate( 10,
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
                           simplify = TRUE )

  test_content <- replicate( 10,
                             paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                             simplify = TRUE )


  for ( xfile in test_files ) {

    base::writeLines( test_content[ match( xfile, test_files ) ],
                      con = xfile )

    if ( ! file.exists( xfile ) )
      testthat::fail( "Could not stage test file")
  }


  # - name entry

  test_list <- test_files
  names(test_list) <- base::basename(test_list)



  # - connect cache

  test_cache <- cxapp::cxapp_applicationcache()


  # -- test

  result <- test_cache$add( test_list )



  # -- expected


  # - generate a list of cache object references

  lst_names <- base::tolower(base::trimws(names( test_list )))

  lst_cacheobjects <- character(0)

  for ( xfileref in lst_names )
    lst_cacheobjects <- append( lst_cacheobjects, digest::digest( base::tolower(base::trimws(xfileref)), algo = "sha1", file = FALSE ) )



  # - expected object files

  expected_files <- file.path( test_cachepath, lst_cacheobjects, fsep = "/" )


  # - expected lck files (regex)

  expected_lckfile_patterns <- paste0("^", lst_cacheobjects, "\\-\\d{8}\\-\\d{4}\\.lck$" )


  # -- assertions

  # - result returned is boolean
  testthat::expect_true( result )


  # - cached files exist
  testthat::expect_true( all(file.exists(expected_files)) )


  # - lck files for cached items

  lst_lckfiles <- cxapp::cxapp_standardpath( list.files( test_cachepath,
                                                         pattern = "^[a-z0-9]{40}\\-\\d{8}\\-\\d{4}\\.lck$",
                                                         full.names = FALSE,
                                                         recursive = FALSE ) )

  testthat::expect_length(lst_lckfiles, length(expected_files) )

  for ( xpattern in expected_lckfile_patterns )
    testthat::expect_true( any(grepl( xpattern, lst_lckfiles, ignore.case = TRUE, perl = TRUE )) )


})







testthat::test_that( "appcache.exists", {

  #' @cx.tests Assert that a cache entry exists if the cached entry content exists and is not expired
  #' @cx.tests Assert that a cache entry does not exist if the cached entry content exists and is expired
  #' @cx.tests Assert that a cache entry does not exist if content is stored and with no controlling metadata
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  

  # - test files

  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")

  test_file_refs <- replicate( 10,
                               base::tolower(base::trimws( cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ) )),
                               simplify = TRUE )


  # - test objects

  test_object_refs <- character(0)

  for ( xfileref in test_file_refs )
    test_object_refs <- append( test_object_refs,
                                digest::digest( xfileref, algo = "sha1", file = FALSE ) )


  names(test_object_refs) <- test_file_refs



  # - stage test objects

  #   note: assuming 10 objects
  #   note: object 1-7 not expired
  #   note: object 1-3 touched not expired
  #   note: object 8 expired
  #   note: object 9 no lck files
  #   note: object 10 no object file


  #   note: last object ref is missing
  for ( xobj in utils::head( test_object_refs, n = length(test_object_refs) - 1 ) )
    base::writeLines( paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                      con = file.path( test_cachepath, xobj, fsep  = "/" ) )


  #   note: objects not expired
  for ( xobj in test_object_refs[1:7] )
    base::writeLines( "", con = file.path( test_cachepath,
                                           paste0( xobj, format( as.POSIXct( Sys.time() + 30*24*60*60, tz = "UTC" ), format = "-%Y%m%d-%H%M" ), ".lck"),
                                           fsep  = "/" ) )


  #   note: objects touched (previously expired)
  for ( xobj in test_object_refs[1:3] )
    base::writeLines( "", con = file.path( test_cachepath,
                                           paste0( xobj, format( as.POSIXct( Sys.time() - 30*24*60*60, tz = "UTC" ), format = "-%Y%m%d-%H%M" ), ".lck"),
                                           fsep  = "/" ) )


  #   note: object expired
  base::writeLines( "", con = file.path( test_cachepath,
                                         paste0( test_object_refs[8], format( as.POSIXct( Sys.time() - 30*24*60*60, tz = "UTC" ), format = "-%Y%m%d-%H%M" ), ".lck"),
                                         fsep  = "/" ) )



  test_cacheinv <- list.files( test_cachepath, full.names = FALSE, recursive = FALSE )


  # - connect cache

  test_cache <- cxapp::cxapp_applicationcache()


  # -- test

  result <- test_cache$exists( test_file_refs )



  # -- expected

  # - result
  expected_results <- c( rep_len( TRUE, 7), rep_len( FALSE, 3) )
  names(expected_results) <- test_file_refs


  # - objects

  expected_obj <- utils::head( unname(test_object_refs), n = 7 )


  # - expected files

  expected_files <- character(0)

  for ( xobj in expected_obj )
    expected_files <- append( expected_files, test_cacheinv[ base::startsWith( test_cacheinv, xobj ) ] )



  # -- assertions

  # - result
  testthat::expect_equal( result, expected_results )

  # - files in cache
  testthat::expect_equal( sort(list.files( test_cachepath, full.names = FALSE, recursive = FALSE )), sort(expected_files) )


})






testthat::test_that( "appcache.getCloneEnabledDefault", {
  
  #' @cx.tests Retrieve a cached entry with clone enabled as default

  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  


  # - test files

  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")


  test_files <- replicate( 10,
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
                           simplify = TRUE )

  test_content <- replicate( 10,
                             paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                             simplify = TRUE )

  test_file_sha1 <- character(0)

  for ( xfile in test_files ) {

    base::writeLines( test_content[ match( xfile, test_files ) ],
                      con = xfile )

    test_file_sha1 <- append( test_file_sha1,
                              digest::digest( xfile, algo = "sha1", file = TRUE ) )

    if ( ! file.exists( xfile ) )
      testthat::fail( "Could not stage test file")
  }




  # - connect cache

  test_cache <- cxapp::cxapp_applicationcache()


  # - add files to cache
  result <- test_cache$add( test_files )


  # - random select test reference

  test_reference <- sample( test_files, 1 )


  # -- test

  result <- test_cache$get( test_reference )

  on.exit({
    base::unlink( result, force = TRUE)
  }, add = TRUE )
  


  # -- expected


  # - object reference
  expected_objref <- digest::digest( base::tolower(base::trimws(test_reference)), algo = "sha1", file = FALSE )


  # - expected parent path
  expected_parentpath <- cxapp::cxapp_standardpath( base::tempdir() )


  # - expected SHA-1
  expected_sha <- digest::digest( test_reference, algo = "sha1", file = TRUE )



  # -- assertions

  # - staged in tempdir()
  testthat::expect_equal( base::dirname(result), expected_parentpath )

  # - exists in tempdir()
  testthat::expect_true( file.exists(result ) )
  
  # - points to expected content
  testthat::expect_equal( digest::digest( result, algo = "sha1", file = TRUE ), expected_sha )


})





testthat::test_that( "appcache.getCloneDisabled", {
  
  #' @cx.tests Retrieve a cached entry with clone disabled 
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  
  test_files <- replicate( 10,
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
                           simplify = TRUE )
  
  test_content <- replicate( 10,
                             paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                             simplify = TRUE )
  
  test_file_sha1 <- character(0)
  
  for ( xfile in test_files ) {
    
    base::writeLines( test_content[ match( xfile, test_files ) ],
                      con = xfile )
    
    test_file_sha1 <- append( test_file_sha1,
                              digest::digest( xfile, algo = "sha1", file = TRUE ) )
    
    if ( ! file.exists( xfile ) )
      testthat::fail( "Could not stage test file")
  }
  
  
  
  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # - add files to cache
  result <- test_cache$add( test_files )
  
  
  # - random select test reference
  
  test_reference <- sample( test_files, 1 )
  
  
  # -- test
  
  result <- test_cache$get( test_reference, clone = FALSE )
  
  on.exit({
    base::unlink( result, force = TRUE)
  }, add = TRUE )
  
  
  
  # -- expected
  
  
  # - object reference
  expected_objref <- digest::digest( base::tolower(base::trimws(test_reference)), algo = "sha1", file = FALSE )
  
  
  # - expected path
  expected_path <- file.path( test_cachepath, expected_objref, fsep = "/" )
  
  
  # - expected SHA-1
  expected_sha <- digest::digest( test_reference, algo = "sha1", file = TRUE )
  
  
  
  # -- assertions
  
  # - staged in tempdir()
  testthat::expect_equal( result, expected_path )
  
  # - points to expected content
  testthat::expect_equal( digest::digest( result, algo = "sha1", file = TRUE ), expected_sha )
  
  
})





testthat::test_that( "appcache.drop", {
  
  #' @cx.tests Drop an entry from the cache deletes the entry content
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  

  # - test files

  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")


  test_files <- replicate( 10,
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
                           simplify = TRUE )

  test_content <- replicate( 10,
                             paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                             simplify = TRUE )

  test_file_sha1 <- character(0)

  for ( xfile in test_files ) {

    base::writeLines( test_content[ match( xfile, test_files ) ],
                      con = xfile )

    test_file_sha1 <- append( test_file_sha1,
                              digest::digest( xfile, algo = "sha1", file = TRUE ) )

    if ( ! file.exists( xfile ) )
      testthat::fail( "Could not stage test file")
  }




  # - connect cache

  test_cache <- cxapp::cxapp_applicationcache()


  # - add files to cache
  result <- test_cache$add( test_files )


  # - random select test reference

  test_reference <- sample( test_files, 1 )


  # - ensure object discoverable
  if ( ! test_cache$exists( test_reference ) )
    testthat::fail( "Unexpected could not ensure item exists in cache" )



  # -- test

  result <- test_cache$drop( test_reference )



  # -- expected


  # - object reference
  expected_objref <- digest::digest( base::tolower(base::trimws(test_reference)), algo = "sha1", file = FALSE )


  # - expected path
  expected_path <- file.path( test_cachepath, expected_objref, fsep = "/" )



  # -- assertions

  # - result
  testthat::expect_true( result )

  # - object does not exist
  testthat::expect_false( file.exists( expected_path ) )

  # - no reference to object
  lst_cacheentries <- base::basename( list.files( test_cachepath, recursive = TRUE, include.dirs = FALSE, full.names = FALSE, all.files = TRUE ) )
  testthat::expect_length( lst_cacheentries[ grepl( paste0( ".*", expected_objref, ".*" ), lst_cacheentries, ignore.case = TRUE) ], 0 )


})






testthat::test_that( "appcache.purge", {

  
  #' @cx.tests Delete all entries in the application cache 
  
  
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
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - create configuration in APP_HOME 
  
  base::writeLines( c( "# test properties file",
                       paste( "APP.CACHE.PATH =", test_cachepath )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  

  # - test files

  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")


  test_files <- replicate( 10,
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
                           simplify = TRUE )

  test_content <- replicate( 10,
                             paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ),
                             simplify = TRUE )

  test_file_sha1 <- character(0)

  for ( xfile in test_files ) {

    base::writeLines( test_content[ match( xfile, test_files ) ],
                      con = xfile )

    test_file_sha1 <- append( test_file_sha1,
                              digest::digest( xfile, algo = "sha1", file = TRUE ) )

    if ( ! file.exists( xfile ) )
      testthat::fail( "Could not stage test file")
  }




  # - connect cache

  test_cache <- cxapp::cxapp_applicationcache()


  # - add files to cache
  test_add_result <- test_cache$add( test_files )



  # -- test

  result <- test_cache$purge()



  # -- assertions

  # - result
  testthat::expect_true( dir.exists( test_cachepath ) )

  # - objects do not exist in cache
  testthat::expect_length( list.files( test_cachepath, full.names = TRUE, recursive = FALSE ), 0 )

})

