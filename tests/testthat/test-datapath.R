#
#  Tests for function cxapp::cxapp_datapath()
# 
#
#


testthat::test_that( "datapath.noParmsNoConfig", {

    
  # -- stage

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  
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
  
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# empty test properties file"),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # -- test
  result <- cxapp::cxapp_datapath()
  
  
  # -- assertions
  
  testthat::expect_true( base::inherits( result, "character") )
  testthat::expect_length( result, 0)
  
})




testthat::test_that( "datapath.elementsNoConfig", {

    
  # -- stage

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  
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
  
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# empty test properties file"),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # path elements
  test_xpaths <- as.list(  base::replicate( 10, 
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 10), collapse = "" ), 
                                            simplify = TRUE ) )
  
  # -- test
  result <- do.call( cxapp::cxapp_datapath, test_xpaths )

  
  # -- expected
  
  expected_path <- paste( test_xpaths, collapse = "/" )
    
  
  # -- assertions

  testthat::expect_equal( result, expected_path )  

})



testthat::test_that( "datapath.elementsDataPathNotExists", {

    
  # -- stage

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  
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
  
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  
  # data paths
  
  test_data_paths <- base::replicate( 5,
                                      cxapp::cxapp_standardpath( base::tempfile( pattern = "test-data-", tmpdir = test_root, fileext = "") ), 
                                      simplify = TRUE )
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "DATA = ", paste( test_data_paths, collapse = .Platform$path.sep ) ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # path elements
  test_xpaths <- as.list(  base::replicate( 10, 
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 10), collapse = "" ), 
                                            simplify = TRUE ) )
  
  # -- test
  result <- do.call( cxapp::cxapp_datapath, test_xpaths )
  
  
  # -- expected
  
  expected_path <- paste( utils::head( test_data_paths, n = 1), paste( test_xpaths, collapse = "/" ), sep = "/" )
  

  # -- assertions
  
  testthat::expect_equal( result, expected_path )  
  
})


testthat::test_that( "datapath.elementsDataPathLastExists", {
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
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
  
  
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  
  # data paths
  
  test_data_paths <- base::replicate( 5,
                                      cxapp::cxapp_standardpath( base::tempfile( pattern = "test-data-", tmpdir = test_root, fileext = "") ), 
                                      simplify = TRUE )
  
  if ( ! dir.exists( utils::tail( test_data_paths, n = 1) ) && ! dir.create( utils::tail( test_data_paths, n = 1), recursive = TRUE ) )
    testthat::fail("Could not create test data directory")
  
  
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "DATA = ", paste( test_data_paths, collapse = .Platform$path.sep ) ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # path elements
  test_xpaths <- as.list(  base::replicate( 10, 
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 10), collapse = "" ), 
                                            simplify = TRUE ) )
  
  # -- test
  result <- do.call( cxapp::cxapp_datapath, test_xpaths )
  
  
  # -- expected
  
  expected_path <- paste( utils::tail( test_data_paths, n = 1), paste( test_xpaths, collapse = "/" ), sep = "/" )
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_path )  
  
})

