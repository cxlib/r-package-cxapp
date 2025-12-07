#
#
# Tests for utility to determine app home directory
#
#


#' @cx.testsfor cxapp::cxapp_apphome()



testthat::test_that( "apphome.noSettings", {
  
  #' @cx.tests App home directory is equal to the current working directory when no APP_HOME environment variable is defined

  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - current app_home
  
  pre_envnames <- base::names(Sys.getenv())
  
  pre_apphomes <- character(0)
  
  for ( xenv in pre_envnames[ grepl( "^app_home$", pre_envnames, ignore.case = TRUE ) ] )
    pre_apphomes[ xenv ] <- Sys.getenv( xenv, names = FALSE )
  
  on.exit({
    if ( length(pre_apphomes) > 0 )
      do.call( Sys.setenv, as.list(pre_apphomes) )
  }, add = TRUE )

  if ( length(pre_apphomes) > 0 )
    for ( xenv in base::names(pre_apphomes) )
      Sys.unsetenv( xenv )

  if ( any( "app_home" %in% base::tolower(base::names(Sys.getenv())) ) )
    testthat::fail("Could not clear existing APP_HOME case insensitive" )


  # -- working directory
  
  pre_wd <- base::getwd()
  
  on.exit({
    base::setwd( pre_wd )
  }, add = TRUE, after = FALSE )

    
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_wd) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )

  
  
  # -- test
  result <- cxapp::cxapp_apphome()
  
  
  # -- expectations
  
  expected_apphome <- test_wd
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_apphome )
  
})




testthat::test_that( "apphome.envUpperCase", {
  
  #' @cx.tests App home directory is equal to directory defined by APP_HOME environment variable in upper case
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - current app_home
  
  pre_envnames <- base::names(Sys.getenv())
  
  pre_apphomes <- character(0)
  
  for ( xenv in pre_envnames[ grepl( "^app_home$", pre_envnames, ignore.case = TRUE ) ] )
    pre_apphomes[ xenv ] <- Sys.getenv( xenv, names = FALSE )
  
  
  # note: after must be TRUE to get reset sequence right
  on.exit({
    if ( length(pre_apphomes) > 0 )
      do.call( Sys.setenv, as.list(pre_apphomes) )
  }, add = TRUE, after = TRUE )   
  
  
  if ( length(pre_apphomes) > 0 )
    for ( xenv in base::names(pre_apphomes) )
      Sys.unsetenv( xenv )
  
  if ( any( "app_home" %in% base::tolower(base::names(Sys.getenv())) ) )
    testthat::fail("Could not clear existing APP_HOME case insensitive" )
  
  
  # -- working directory
  
  pre_wd <- base::getwd()
  
  on.exit({
    base::setwd( pre_wd )
  }, add = TRUE, after = FALSE )
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_wd) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )

  
  # - APP_HOME (upper case)
  
  test_apphome <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-apphome-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_apphome) || ! dir.create( test_apphome, recursive = TRUE ) )
    testthat::fail( "Could not stage APP_HOME directory" )
  
  Sys.setenv( "APP_HOME" = test_apphome )

  # note: after must be FALSE to get reset sequence right
  on.exit({
    Sys.unsetenv("APP_HOME")
  }, add = TRUE, after = FALSE )   
  
  
  # -- test
  result <- cxapp::cxapp_apphome()
  
  
  # -- expectations
  
  expected_apphome <- test_apphome
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_apphome )
  
})




testthat::test_that( "apphome.envLowerCase", {
  
  #' @cx.tests App home directory is equal to directory defined by APP_HOME environment variable in lower case
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - current app_home
  
  pre_envnames <- base::names(Sys.getenv())
  
  pre_apphomes <- character(0)
  
  for ( xenv in pre_envnames[ grepl( "^app_home$", pre_envnames, ignore.case = TRUE ) ] )
    pre_apphomes[ xenv ] <- Sys.getenv( xenv, names = FALSE )
  
  
  # note: after must be TRUE to get reset sequence right
  on.exit({
    if ( length(pre_apphomes) > 0 )
      do.call( Sys.setenv, as.list(pre_apphomes) )
  }, add = TRUE, after = TRUE )
  
  if ( length(pre_apphomes) > 0 )
    for ( xenv in base::names(pre_apphomes) )
      Sys.unsetenv( xenv )
  
  if ( any( "app_home" %in% base::tolower(base::names(Sys.getenv())) ) )
    testthat::fail("Could not clear existing APP_HOME case insensitive" )
  
  
  # -- working directory
  
  pre_wd <- base::getwd()
  
  on.exit({
    base::setwd( pre_wd )
  }, add = TRUE, after = FALSE )
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_wd) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  
  
  # - APP_HOME (upper case)
  
  test_apphome <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-apphome-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_apphome) || ! dir.create( test_apphome, recursive = TRUE ) )
    testthat::fail( "Could not stage APP_HOME directory" )
  
  Sys.setenv( "app_home" = test_apphome )
  
  # note: after must be FALSE to get reset sequence right
  on.exit({
    Sys.unsetenv("app_home")
  }, add = TRUE, after = FALSE )   
  
  
  # -- test
  result <- cxapp::cxapp_apphome()
  
  
  # -- expectations
  
  expected_apphome <- test_apphome
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_apphome )
  
})





testthat::test_that( "apphome.envMixedCase", {
  
  #' @cx.tests App home directory is equal to directory defined by APP_HOME environment variable in mixed case
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - current app_home
  
  pre_envnames <- base::names(Sys.getenv())
  
  pre_apphomes <- character(0)
  
  for ( xenv in pre_envnames[ grepl( "^app_home$", pre_envnames, ignore.case = TRUE ) ] )
    pre_apphomes[ xenv ] <- Sys.getenv( xenv, names = FALSE )

    
  # note: after must be TRUE to get reset sequence right
  on.exit({
    if ( length(pre_apphomes) > 0 )
      do.call( Sys.setenv, as.list(pre_apphomes) )
  }, add = TRUE, after = TRUE )
  
  if ( length(pre_apphomes) > 0 )
    for ( xenv in base::names(pre_apphomes) )
      Sys.unsetenv( xenv )
  
  if ( any( "app_home" %in% base::tolower(base::names(Sys.getenv())) ) )
    testthat::fail("Could not clear existing APP_HOME case insensitive" )
  
  
  # -- working directory
  
  pre_wd <- base::getwd()
  
  on.exit({
    base::setwd( pre_wd )
  }, add = TRUE, after = FALSE )
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_wd) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  
  
  # - APP_HOME (upper case)
  
  test_apphome <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-apphome-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_apphome) || ! dir.create( test_apphome, recursive = TRUE ) )
    testthat::fail( "Could not stage APP_HOME directory" )
  
  Sys.setenv( "aPP_Home" = test_apphome )
  
  # note: after must be FALSE to get reset sequence right
  on.exit({
    Sys.unsetenv("aPP_Home")
  }, add = TRUE, after = FALSE )   
  
  
  # -- test
  result <- cxapp::cxapp_apphome()
  
  
  # -- expectations
  
  expected_apphome <- test_apphome
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_apphome )
  
})



testthat::test_that( "apphome.envMultiDef", {
  
  #' @cx.tests Resolving app home directory using APP_HOME environmental variable results in error for multiple case insensitive matches 
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - current app_home
  
  pre_envnames <- base::names(Sys.getenv())
  
  pre_apphomes <- character(0)
  
  for ( xenv in pre_envnames[ grepl( "^app_home$", pre_envnames, ignore.case = TRUE ) ] )
    pre_apphomes[ xenv ] <- Sys.getenv( xenv, names = FALSE )
  
  
  # note: after must be TRUE to get reset sequence right
  on.exit({
    if ( length(pre_apphomes) > 0 )
      do.call( Sys.setenv, as.list(pre_apphomes) )
  }, add = TRUE, after = TRUE )
  
  if ( length(pre_apphomes) > 0 )
    for ( xenv in base::names(pre_apphomes) )
      Sys.unsetenv( xenv )
  
  if ( any( "app_home" %in% base::tolower(base::names(Sys.getenv())) ) )
    testthat::fail("Could not clear existing APP_HOME case insensitive" )
  
  
  # -- working directory
  
  pre_wd <- base::getwd()
  
  on.exit({
    base::setwd( pre_wd )
  }, add = TRUE, after = FALSE )
  
  
  test_wd <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-wd-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_wd) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage working directory" )
  
  base::setwd( test_wd )
  
  
  # - APP_HOME (upper case)
  
  test_apphomes <- sapply( c( "APP_HOME", "app_home", "aPP_Home"), function(x) {
    paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample( 5:50, 1), replace = TRUE), collapse = "")
  }, USE.NAMES = TRUE )
  

  do.call( Sys.setenv, as.list(test_apphomes) )  

  # note: after must be FALSE to get reset sequence right
  on.exit({
    Sys.unsetenv( base::names(test_apphomes) ) 
  }, add = TRUE, after = FALSE )
  
  
  # -- test
  testthat::expect_error( cxapp::cxapp_apphome(), 
                          regexp = "^One or more environmental variables are names APP_HOME \\(case insentitive\\) \\[.*\\]$" )
  
  
})

