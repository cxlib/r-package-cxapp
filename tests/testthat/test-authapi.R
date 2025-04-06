#
#  Tests for cxapp::cxapp_authapi()
#
#
#


testthat::test_that( "authapi.missingAuthString", {

  # -- stage


  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - inject configuration

  base::writeLines( c( "# test configuration",
                       paste0( "CACHEPATH = ", test_cachepath ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )


  # -- test

  result <- cxapp::cxapp_authapi()


  # -- assertions

  testthat::expect_false( result )


})



testthat::test_that( "authapi.authStringNULL", {

  # -- stage


  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - inject configuration

  base::writeLines( c( "# test configuration",
                       paste0( "CACHEPATH = ", test_cachepath ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )


  # -- test

  result <- cxapp::cxapp_authapi( NULL )


  # -- assertions

  testthat::expect_false( result )


})




testthat::test_that( "authapi.authStringNA", {

  # -- stage


  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - inject configuration

  base::writeLines( c( "# test configuration",
                       paste0( "CACHEPATH = ", test_cachepath ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )


  # -- test

  result <- cxapp::cxapp_authapi( NA )


  # -- assertions

  testthat::expect_false( result )


})




testthat::test_that( "authapi.authStringNotCharacter", {

  # -- stage


  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - inject configuration

  base::writeLines( c( "# test configuration",
                       paste0( "CACHEPATH = ", test_cachepath ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )


  # -- test

  result <- cxapp::cxapp_authapi( as.numeric(123) )


  # -- assertions

  testthat::expect_false( result )


})




testthat::test_that( "authapi.authStringEmptyString", {

  # -- stage


  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - inject configuration

  base::writeLines( c( "# test configuration",
                       paste0( "CACHEPATH = ", test_cachepath ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )


  # -- test

  result <- cxapp::cxapp_authapi( "" )


  # -- assertions

  testthat::expect_false( result )


})





testthat::test_that( "authapi.authStringNotBearer", {

  # -- stage


  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - inject configuration

  base::writeLines( c( "# test configuration",
                       paste0( "CACHEPATH = ", test_cachepath ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )


  # - test authorization string

  test_authstring <- paste( paste( sample( c( base::LETTERS, base::letters ), 20, replace = TRUE ), collapse = ""),
                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 64, replace = TRUE ), collapse = "" ), sep = " " )


  # -- test

  result <- cxapp::cxapp_authapi( test_authstring )


  # -- assertions

  testthat::expect_false( result )


})




testthat::test_that( "authapi.authBearerNoVaultConfig", {

  # -- stage


  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - inject configuration

  base::writeLines( c( "# test configuration",
                       paste0( "CACHEPATH = ", test_cachepath ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )



  # - test bearer authorization string

  test_authstring <- paste( "Bearer",
                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 64, replace = TRUE ), collapse = "" ), sep = " " )


  # -- test

  result <- cxapp::cxapp_authapi( test_authstring )


  # -- assertions

  testthat::expect_false( result )


})



testthat::test_that( "authapi.authBearerNoSecretsConfig", {

  # -- stage


  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test vault path

  test_vaultpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_vaultpath ) && ! dir.create( test_vaultpath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - inject configuration

  base::writeLines( c( "# test configuration",
                       "VAULT = LOCAL",
                       paste0( "VAULT.DATA = ", test_vaultpath ),
                       paste0( "CACHEPATH = ", test_cachepath ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )



  # - test bearer authorization string

  test_authstring <- paste( "Bearer",
                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 64, replace = TRUE ), collapse = "" ), sep = " " )


  # -- test

  result <- cxapp::cxapp_authapi( test_authstring )


  # -- assertions

  testthat::expect_false( result )


})





testthat::test_that( "authapi.authBearerSecretsMissing", {


  # -- stage

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test vault path

  test_vaultpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_vaultpath ) && ! dir.create( test_vaultpath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - inject configuration

  base::writeLines( c( "# test configuration",
                       "VAULT = LOCAL",
                       paste0( "VAULT.DATA = ", test_vaultpath ),
                       paste0( "CACHEPATH = ", test_cachepath ),
                       "API.AUTH.SECRETS="
                     ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )



  # - test bearer authorization string

  test_authstring <- paste( "Bearer",
                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 64, replace = TRUE ), collapse = "" ), sep = " " )


  # -- test

  result <- cxapp::cxapp_authapi( test_authstring )


  # -- assertions

  testthat::expect_false( result )


})





testthat::test_that( "authapi.authBearerSecretsNotExist", {


  # -- stage

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test vault path

  test_vaultpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_vaultpath ) && ! dir.create( test_vaultpath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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



  # - test secrets

  test_secret_names <- replicate( 3,
                                  paste( sample( c( base::letters, as.character(0:9) ), 20, replace = TRUE ), collapse = "" ),
                                  simplify = TRUE )

  test_secrets <- file.path( "test", "api", "auth", test_secret_names, fsep = "/" )




  # - inject configuration

  base::writeLines( c( "# test configuration",
                       "VAULT = LOCAL",
                       paste0( "VAULT.DATA = ", test_vaultpath ),
                       paste0( "CACHEPATH = ", test_cachepath ),
                       paste0( "API.AUTH.SECRETS = ", paste( test_secrets, collapse = " ") ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )



  # - test bearer authorization string

  test_authstring <- paste( "Bearer",
                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 64, replace = TRUE ), collapse = "" ), sep = " " )


  # -- test

  result <- cxapp::cxapp_authapi( test_authstring )


  # -- assertions

  testthat::expect_false( result )


})






testthat::test_that( "authapi.authBearerSecretsNotJSON", {


  # -- stage

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test vault path

  test_vaultpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_vaultpath ) && ! dir.create( test_vaultpath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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


  # - test token

  test_tokens <- replicate( 10,
                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 128, replace = TRUE ), collapse = "" ),
                            simplify = TRUE )


  test_tokens_encoded <- sapply( test_tokens, function(x) {
    cxapp:::.cxapp_apitokenencode(x)
  }, USE.NAMES = TRUE )



  # - test secrets

  test_secret_names <- replicate( length(test_tokens_encoded),
                                  paste( sample( c( base::letters, as.character(0:9) ), 20, replace = TRUE ), collapse = "" ),
                                  simplify = TRUE )

  test_secrets <- paste0( "/", file.path( "test", "api", "auth", test_secret_names, fsep = "/" ) )

  for ( xfile in test_secrets ) {

    xpath <- paste0( test_vaultpath, xfile )

    if ( ! dir.exists(base::dirname(xpath)) && ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage parent for secret" )

    base::writeLines( test_tokens_encoded[ match( xfile, test_secrets ) ],
                      con = xpath )

    if ( ! file.exists(xpath) )
      testthat::fail( "Could not stage secret" )

  }




  # - inject configuration

  base::writeLines( c( "# test configuration",
                       "VAULT = LOCAL",
                       paste0( "VAULT.DATA = ", test_vaultpath ),
                       paste0( "CACHEPATH = ", test_cachepath ),
                       paste0( "API.AUTH.SECRETS = ", paste( test_secrets, collapse = " ") ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )


  # - test bearer authorization string

  test_authstring <- paste( "Bearer",
                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 64, replace = TRUE ), collapse = "" ), sep = " " )


  # -- test

  result <- cxapp::cxapp_authapi( test_authstring )


  # -- assertions

  testthat::expect_false( result )


})




testthat::test_that( "authapi.authBearerSecretsValid", {


  # -- stage

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test vault path

  test_vaultpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_vaultpath ) && ! dir.create( test_vaultpath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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


  # - test token

  test_tokens <- replicate( 10,
                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 128, replace = TRUE ), collapse = "" ),
                            simplify = TRUE )


  test_tokens_encoded <- sapply( test_tokens, function(x) {
    cxapp:::.cxapp_apitokenencode(x)
  }, USE.NAMES = TRUE )



  # - test principals

  test_principals <- replicate( length(test_tokens_encoded),
                                paste( sample( c( base::letters, as.character(0:9) ), 10, replace = TRUE ), collapse = "" ),
                                simplify = TRUE )


  # - test secrets

  test_secret_names <- replicate( length(test_tokens_encoded),
                                  paste( sample( c( base::letters, as.character(0:9) ), 20, replace = TRUE ), collapse = "" ),
                                  simplify = TRUE )

  test_secrets <- paste0( "/", file.path( "test", "api", "auth", test_secret_names, fsep = "/" ) )

  for ( xfile in test_secrets ) {

    xpath <- paste0( test_vaultpath, xfile )

    if ( ! dir.exists(base::dirname(xpath)) && ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage parent for secret" )

    def_testsecret <- list( "scope" = "user",
                            "principal" = test_principals[ match( xfile, test_secrets ) ],
                            "value" = unname(test_tokens_encoded[ match( xfile, test_secrets ) ]) )


    base::writeLines( jsonlite::toJSON(def_testsecret, pretty = FALSE, auto_unbox = TRUE ),
                      con = xpath )

    if ( ! file.exists(xpath) )
      testthat::fail( "Could not stage secret" )

  }




  # - inject configuration

  base::writeLines( c( "# test configuration",
                       "VAULT = LOCAL",
                       paste0( "VAULT.DATA = ", test_vaultpath ),
                       paste0( "CACHEPATH = ", test_cachepath ),
                       paste0( "API.AUTH.SECRETS = ", paste( test_secrets, collapse = " ") ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )



  # - test bearer authorization string
  #   randomly picking test token

  test_token_idx <- sample( c( 1:length(test_tokens)), 1 )

  test_authstring <- paste( "Bearer", test_tokens[ test_token_idx ], sep = " " )


  # -- test

  result <- cxapp::cxapp_authapi( test_authstring )



  # -- expected

  expected_scope <- "user"

  expected_principal <- test_principals[ test_token_idx ]



  # -- assertions

  # - authorization result
  testthat::expect_true( result )

  # - scope
  testthat::expect_equal( attr(result, "scope"), expected_scope )

  # - principal
  testthat::expect_equal( attr(result, "principal"), expected_principal )

})




testthat::test_that( "authapi.authBearerWildcardSecretsValid", {


  # -- stage

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - test vault path

  test_vaultpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_vaultpath ) && ! dir.create( test_vaultpath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")



  # - test cache root

  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-", tmpdir = test_root, fileext = "") )

  if ( ! dir.exists( test_cachepath ) && ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")




  # - move global in-memory cached config

  prev_config <- NA

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )


  # - move global in-memory cached application cache

  prev_appcache <- NA

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    prev_appcache <- get( ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

    if ( inherits( prev_appcache, "cxapp_applicationcache" ) )
      base::assign( ".cxapp.wrkcache.appcache", prev_appcache, envir = .GlobalEnv )


  }, add = TRUE )


  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appcache", envir = .GlobalEnv )

  if ( exists( ".cxapp.wrkcache.appcache", envir = .GlobalEnv ) )
    testthat::fail( "COuld not stash app cache")


  # - stash current APP_HOME

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


  # - test token

  test_tokens <- replicate( 10,
                            paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 128, replace = TRUE ), collapse = "" ),
                            simplify = TRUE )


  test_tokens_encoded <- sapply( test_tokens, function(x) {
    cxapp:::.cxapp_apitokenencode(x)
  }, USE.NAMES = TRUE )



  # - test principals

  test_principals <- replicate( length(test_tokens_encoded),
                                paste( sample( c( base::letters, as.character(0:9) ), 10, replace = TRUE ), collapse = "" ),
                                simplify = TRUE )


  # - test secrets

  test_secret_names <- replicate( length(test_tokens_encoded),
                                  paste( sample( c( base::letters, as.character(0:9) ), 20, replace = TRUE ), collapse = "" ),
                                  simplify = TRUE )

  test_secrets <- paste0( "/", file.path( "test", "api", "auth", test_secret_names, fsep = "/" ) )

  for ( xfile in test_secrets ) {

    xpath <- paste0( test_vaultpath, xfile )

    if ( ! dir.exists(base::dirname(xpath)) && ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage parent for secret" )

    def_testsecret <- list( "scope" = "user",
                            "principal" = test_principals[ match( xfile, test_secrets ) ],
                            "value" = unname(test_tokens_encoded[ match( xfile, test_secrets ) ] ) )


    base::writeLines( jsonlite::toJSON(def_testsecret, pretty = FALSE, auto_unbox = TRUE ),
                      con = xpath )

    if ( ! file.exists(xpath) )
      testthat::fail( "Could not stage secret" )

  }




  # - inject configuration

  base::writeLines( c( "# test configuration",
                       "VAULT = LOCAL",
                       paste0( "VAULT.DATA = ", test_vaultpath ),
                       paste0( "CACHEPATH = ", test_cachepath ),
                       paste0( "API.AUTH.SECRETS = ", paste( unique(paste0( base::dirname(test_secrets), "/*" )), collapse = " ") ) ),
                    con = file.path( test_apphome, "config", "app.properties", fsep = "/") )

  if ( ! file.exists( file.path( test_apphome, "config", "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app properties" )



  # - test bearer authorization string

  test_token_idx <- sample( c( 1:length(test_tokens)), 1 )

  test_authstring <- paste( "Bearer", test_tokens[ test_token_idx ], sep = " " )



  # -- test

  result <- cxapp::cxapp_authapi( test_authstring )



  # -- expected

  expected_scope <- "user"

  expected_principal <- test_principals[ test_token_idx ]



  # -- assertions

  # - authorization result
  testthat::expect_true( result )

  # - scope
  testthat::expect_equal( attr(result, "scope"), expected_scope )

  # - principal
  testthat::expect_equal( attr(result, "principal"), expected_principal )

})



