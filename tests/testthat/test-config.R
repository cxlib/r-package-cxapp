#
#  tests for cxapp::cxapp_config()
#
#

#' @cx.testsfor cxapp::cxapp_config()



testthat::test_that( "config.configSearchTreeDefaults", {

  #' @cx.tests Empty configuration with no property files results in no properties defined
  #' @cx.tests Search sequence is APP_HOME/config, APP_HOME, cxapp install directory and current working directory
    
  
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



  # -- test
  result <- cxapp::cxapp_config()


  # -- expected

  expected_searchtree <- c( file.path( test_apphome, "config", fsep = "/" ), 
                            test_apphome, 
                            file.path( test_libpath, "cxapp", fsep = "/"),
                            test_wd )




  # -- assertions

  # - search tree
  testthat::expect_equal( result$.attr[["search.tree"]], expected_searchtree )
  
  # - property files
  testthat::expect_length( result$.attr[["property.files"]], 0 )

})





testthat::test_that( "config.configSearchTreePropFiles", {
  
  #' @cx.tests Property file process sequence defined by search tree

  
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
  
  
  # - stage property files
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )
  
  for ( xpath in test_srchtree ) 
    base::writeLines( paste( paste0( "TEST.", as.character(match( xpath, test_srchtree ))), xpath, sep = "="), 
                      con = file.path( xpath, paste0( "app", as.character(match( xpath, test_srchtree )), ".properties" ) ))  
    
  
  
  
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
  
  
  
  # -- test
  result <- cxapp::cxapp_config()


  # -- expected
  
  # - search tree
  expected_searchtree <- test_srchtree
  
  # - property files
  expected_propfiles <- lapply( expected_searchtree, function(x) {
    
    prop_fname <- file.path( x, paste0( "app", as.character(match( x, expected_searchtree)) , ".properties"), fsep = "/")
    
    list( "path" = prop_fname, 
          "sha" = digest::digest( prop_fname, algo = "sha1", file = TRUE ) )
  })
  

  
  # -- assertions
  
  # - search tree
  testthat::expect_equal( result$.attr[["search.tree"]], expected_searchtree )
  
  # - property files
  testthat::expect_equal( result$.attr[["property.files"]], expected_propfiles )
  
})







testthat::test_that( "config.configSearchTreePropFileRecursionDisabled", {
  
  #' @cx.tests Property file process sequence defined by search tree with property file recursion disabled
  
  
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
  
  
  # - stage property files
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )
  
  test_propfname <- paste0( paste( sample( base::letters, 25), collapse = ""), ".properties" )
  
  for ( xpath in test_srchtree ) 
    base::writeLines( paste( paste0( "TEST.", as.character(match( xpath, test_srchtree ))), xpath, sep = "="), 
                      con = file.path( xpath, test_propfname ) )  
  
  
  
  
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
  
  
  
  # -- test
  result <- cxapp::cxapp_config( recursive = FALSE )
 
  
  
  # -- expected
  
  # - search tree
  expected_searchtree <- test_srchtree
  
  # - property files
  expected_propfiles <- lapply( utils::head( expected_searchtree, n = 1 ), function(x) {
    
    prop_fname <- file.path( x, test_propfname, fsep = "/")
    
    list( "path" = prop_fname, 
          "sha" = digest::digest( prop_fname, algo = "sha1", file = TRUE ) )
  })
  
  
  
  # -- assertions
  
  # - search tree
  testthat::expect_equal( result$.attr[["search.tree"]], expected_searchtree )
  
  # - property files
  testthat::expect_equal( result$.attr[["property.files"]], expected_propfiles )
  
})




testthat::test_that( "config.configSearchTreePropFileRecursionEnabled", {
  
  #' @cx.tests Property file process sequence defined by search tree with property file recursion enabled
  
  
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
  
  
  # - stage property files
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )
  
  test_propfname <- paste0( paste( sample( base::letters, 25), collapse = ""), ".properties" )
  
  for ( xpath in test_srchtree ) 
    base::writeLines( paste( paste0( "TEST.", as.character(match( xpath, test_srchtree ))), xpath, sep = "="), 
                      con = file.path( xpath, test_propfname ) )  
  
  
  
  
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
  
  
  
  # -- test
  result <- cxapp::cxapp_config( recursive = TRUE )
  

  
  # -- expected
  
  # - search tree
  expected_searchtree <- test_srchtree
  
  # - property files
  expected_propfiles <- lapply( expected_searchtree, function(x) {
    
    prop_fname <- file.path( x, test_propfname, fsep = "/")
    
    list( "path" = prop_fname, 
          "sha" = digest::digest( prop_fname, algo = "sha1", file = TRUE ) )
  })
  
  
  expected_props <-list()
  
  for ( xpath in expected_searchtree ) 
    expected_props[[ paste0( "test.", as.character(match(xpath, expected_searchtree)))  ]] <- xpath
    
  

  # -- assertions
  
  # - search tree
  testthat::expect_equal( result$.attr[["search.tree"]], expected_searchtree )
  
  # - property files
  testthat::expect_equal( result$.attr[["property.files"]], expected_propfiles )
  
  # - properties
  testthat::expect_equal( result$.attr[["properties"]][ base::sort(base::names(result$.attr[["properties"]])) ], expected_props[ base::sort(base::names(expected_props)) ] )
  
})
  





testthat::test_that( "config.configSearchTreeAppPropFilePrecedenceRecursionEnabled", {
  
  #' @cx.tests Property file app.properties takes precedence when other property file are present and recursion enabled
  
  
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
  
  
  # - stage property files
  
  test_srchtree <-  c( file.path( test_apphome, "config", fsep = "/" ), 
                       test_apphome, 
                       file.path( test_libpath, "cxapp", fsep = "/"),
                       test_wd )

  test_propfname <- paste0( paste( sample( base::letters, 25), collapse = ""), ".properties" )

  # note: force a natural sort order for property files
  test_propfileset <- c( paste0( "aaaaaaaaaaaaa", test_propfname),
                         paste0( "aaaaaabbbbbbb", test_propfname), 
                         "app.properties" )


  for ( xpath in test_srchtree )
    for ( xfile in test_propfileset )
      base::writeLines( paste( paste0( "TEST.", as.character(match( xpath, test_srchtree ))), file.path( xpath, xfile, fsep = "/" ), sep = "="), 
                        con = file.path( xpath, xfile, fsep = "/" ) )  
  
  

  
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
  
  
  
  # -- test
  result <- cxapp::cxapp_config( recursive = TRUE )
  

  # -- expected
  
  # - search tree
  expected_searchtree <- test_srchtree
  
  
  # - property files
  
  expected_propfileseq <- character(0)
  
  for ( xpath in expected_searchtree ) {

    expected_propfileseq <- append( expected_propfileseq, file.path( xpath, "app.properties", fsep = "/" ) )
    
    for ( xfile in base::sort(test_propfileset[ test_propfileset != "app.properties" ]) )
      expected_propfileseq <- append( expected_propfileseq, file.path( xpath, xfile, fsep = "/" ) )
  }
  
  
  expected_propfiles <- lapply( expected_propfileseq, function(x) {
    list( "path" = x, 
          "sha" = digest::digest( x, algo = "sha1", file = TRUE ) )
  })
  
  
  expected_props <-list()
  
  for ( xpath in expected_searchtree ) 
    expected_props[[ paste0( "test.", as.character(match(xpath, expected_searchtree)))  ]] <- file.path( xpath, "app.properties", fsep = "/" )
  
  
  
  # -- assertions
  
  # - search tree
  testthat::expect_equal( result$.attr[["search.tree"]], expected_searchtree )
  
  # - property files
  testthat::expect_equal( result$.attr[["property.files"]], expected_propfiles )
  
  # - properties
  testthat::expect_equal( result$.attr[["properties"]][ base::sort(base::names(result$.attr[["properties"]])) ], expected_props[ base::sort(base::names(expected_props)) ] )
  
})


