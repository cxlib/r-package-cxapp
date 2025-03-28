#
#  Tests for application cache
#
#
#


testthat::test_that( "appcache.noConfig", {

  
  # -- stage 

  # - get a standardized reference to tempdir
  test_tmpdir <- cxapp::cxapp_standardpath( base::tempdir() )
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - identify existing cache directories 
  
  tmp_paths <- list.dirs( test_tmpdir, full.names = FALSE, recursive = FALSE )
  
  prev_cachepaths <- tmp_paths[ grepl( "^\\.application-cache-", tmp_paths, ignore.case = TRUE, perl = TRUE ) ]
    
  on.exit( {

    # cache paths that exist when exiting    
    exists_tmppaths <- list.dirs( test_tmpdir, full.names = FALSE, recursive = FALSE )
    exists_cachepaths <- exists_tmppaths[ grepl( "^\\.application-cache-", exists_tmppaths, ignore.case = TRUE, perl = TRUE ) ]
    
    # identify new cache paths 
    new_cachepaths <- exists_cachepaths[ ! exists_cachepaths %in% prev_cachepaths ]
    
    # remove new cache paths
    unlink( cxapp::cxapp_standardpath( file.path( test_tmpdir, new_cachepaths, fsep = "/" ) ), recursive = TRUE, force = TRUE )
    
  }, add = TRUE )
  
  
  # -- test
  result <- cxapp::cxapp_applicationcache()
  

  
  # -- expected

  lst_tmppaths <-list.dirs( test_tmpdir, full.names = FALSE, recursive = FALSE ) 
  lst_cachepaths <- lst_tmppaths[ grepl( "^\\.application-cache-", lst_tmppaths, ignore.case = TRUE, perl = TRUE ) ]
  
  expected_cachepath <- file.path( test_tmpdir, lst_cachepaths[ ! lst_cachepaths %in% prev_cachepaths ], fsep = "/" ) 
  
  
  
  # -- assertions
  
  testthat::expect_true( dir.exists(expected_cachepath) )

})





testthat::test_that( "appcache.configAppCacheNotExist", {
  
  
  # -- stage 

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")

  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  

  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  

  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_cachepath ) )
    testthat::fail( "Unextpected test cache path exists" )

  

  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  


  
  # -- test
  
  testthat::expect_error( cxapp::cxapp_applicationcache(), 
                          regexp = "^Cache directory does not exist" )
  

})





testthat::test_that( "appcache.configAppCache", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  
  # -- test
  
  result <- cxapp::cxapp_applicationcache()
  
  
  # -- expected
  
  expected_cachepath <- test_cachepath
  
  
  # -- assertions
  testthat::expect_equal( result$.attr[["cache.path"]], expected_cachepath )
  
  
})





testthat::test_that( "appcache.addNothingSpecified", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # -- test
  
  testthat::expect_error( test_cache$add(), regexp = "^Vector of files missing$" )

  
})






testthat::test_that( "appcache.addNull", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # -- test
  
  testthat::expect_error( test_cache$add( NULL ), regexp = "^Vector of files missing$" )

  
})




testthat::test_that( "appcache.addNA", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # -- test
  
  testthat::expect_error( test_cache$add( NA ), regexp = "^Vector of files missing$" )
  
  
})




testthat::test_that( "appcache.addNothingToDo", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # -- test
  
  result <- test_cache$add( character(0) )
  
  
  # -- assertions
  
  testthat::expect_true( result )
  
  testthat::expect_length( list.files( test_cachepath, recursive = FALSE, full.names = FALSE ), 0 )
  
  
})





testthat::test_that( "appcache.addFileNotExist", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  

  test_files <- replicate( 10, 
                           cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
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
  
    
  
  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # -- test
  
  testthat::expect_error( test_cache$add( test_files ), regexp = "^One or more files do not exist$" )

  
})







testthat::test_that( "appcache.addFileNotNamed", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  
  test_files <- replicate( 10, 
                           cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
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
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  
  test_files <- replicate( 10, 
                           cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
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
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  
  test_files <- replicate( 10, 
                           cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
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






testthat::test_that( "appcache.configExpire", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       "# expire in 5 min",
                       "APPCACHE.EXPIRE = 5",
                       paste( "CACHEPATH =", test_cachepath ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  #   note: use single file for this scenario
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  

  test_file <- cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") )
  
  test_content <- paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" )


  base::writeLines( test_content, con = test_file )
  
  if ( ! file.exists( test_file ) )
    testthat::fail( "Could not stage test file")
  

  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()
  
  
  # - capture time of test
  
  test_time <- as.POSIXct( Sys.time(), tz = "UTC" )
  

  # -- test
  
  result <- test_cache$add( test_file )
  
  
  
  # -- expected
  
  lst_cacheobjects <- digest::digest( base::tolower(base::trimws(test_file)), algo = "sha1", file = FALSE )  
  

  
  # - expected object files
  
  expected_files <- file.path( test_cachepath, lst_cacheobjects, fsep = "/" )
  
  

  # - expected lck files (regex)
  
  expected_lckfile_patterns <- paste0("^", lst_cacheobjects, "\\-\\d{8}\\-\\d{4}\\.lck$" )
  
  

  # - test reference time to use in duration calcs
  #   note: poor mans floor of date/time to minute
  
  expected_reftime <- as.POSIXct( format( test_time, format = "%Y%m%d-%H%M" ), tz = "UTC", format = "%Y%m%d-%H%M" )
  
  
  
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
  

  # - verify cache lock is 5 min ... or so
  #   expecting something like 60 second diff

  obj_lck <- as.POSIXct( gsub( "^[a-f0-9]{40}-(\\d{8})-(\\d{4})\\.lck$", "\\1-\\2", lst_lckfiles ), tz = "UTC", format = "%Y%m%d-%H%M" )
  
  # read this ... difference to obj_lck from current_time in minutes
  expire_diff <- as.numeric( base::difftime( obj_lck, expected_reftime, units = "mins" ) )
  

  # note: we floored our reference time 
  # note: if reference time is floored and we pass the minute mark in processing .. the lck time is floored to the "next" minute so diff in 5 or 6
  testthat::expect_true( expire_diff %in% c( 5, 6 ) )
  
  
})










testthat::test_that( "appcache.exists", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files

  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")

  test_file_refs <- replicate( 10, 
                               base::tolower(base::trimws( cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ) )),
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






testthat::test_that( "appcache.get", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  
  test_files <- replicate( 10, 
                           cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
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
  

  # -- expected
  
  
  # - object reference
  expected_objref <- digest::digest( base::tolower(base::trimws(test_reference)), algo = "sha1", file = FALSE )
  

  # - expected path
  expected_path <- file.path( test_cachepath, expected_objref, fsep = "/" )


  # - expected SHA-1
  expected_sha1 <- digest::digest( test_reference, algo = "sha1", file = TRUE )


  
  # -- assertions

  # - returned path
  testthat::expect_equal( result, expected_path )

  # - points to expected content
  testthat::expect_equal( digest::digest( result, algo = "sha1", file = TRUE ), expected_sha1 )


})






testthat::test_that( "appcache.drop", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  
  test_files <- replicate( 10, 
                           cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
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
  
  
  
})







testthat::test_that( "appcache.touch", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       "# expire in 30 days or 30*24*60 minutes",
                       "APPCACHE.EXPIRE = 43200",
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  test_file_refs <- replicate( 10, 
                               base::tolower(base::trimws( cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ) )),
                               simplify = TRUE )
  
  
  # - test objects
  
  test_object_refs <- character(0)
  
  for ( xfileref in test_file_refs )
    test_object_refs <- append( test_object_refs, 
                                digest::digest( xfileref, algo = "sha1", file = FALSE ) )
  
  
  names(test_object_refs) <- test_file_refs
  
  
  
  # - stage test objects
  
  for ( xobj in test_object_refs ) {

    # object
    base::writeLines( paste(sample( c( base::LETTERS, base::letters, as.character(0:9) ), 120, replace = TRUE), collapse = "" ), 
                      con = file.path( test_cachepath, xobj, fsep  = "/" ) )

    # lck file
    base::writeLines( "", con = file.path( test_cachepath, 
                                           paste0( xobj, format( as.POSIXct( Sys.time() + 5*60*60, tz = "UTC" ), format = "-%Y%m%d-%H%M" ), ".lck"), 
                                           fsep  = "/" ) )
    
  }
  
  

  
  # - random select test reference
  
  test_reference <- sample( test_file_refs, 1 )
  
  
  # - connect cache
  
  test_cache <- cxapp::cxapp_applicationcache()

  
  # - ensure object discoverable
  if ( ! test_cache$exists( test_reference ) )
    testthat::fail( "Unexpected could not ensure item exists in cache" )
  
  

  # - inventory test cache area
  test_cacheinv <- list.files( test_cachepath, full.names = FALSE, recursive = FALSE )

  
  # -- test
  
  result <- test_cache$touch( test_reference )
  
  
  
  # -- expected

  # - object reference
  expected_objref <- digest::digest( base::tolower(base::trimws(test_reference)), algo = "sha1", file = FALSE )
  
  
  # - expected path
  expected_path <- file.path( test_cachepath, expected_objref, fsep = "/" )
  


  # -- assertions

  # - object does exists
  testthat::expect_true( file.exists( expected_path ) )

  
  # - use new lock file as surrogate
  result_cacheinv <- list.files( test_cachepath, full.names = FALSE, recursive = FALSE )
  new_lckfile <- result_cacheinv[ ! result_cacheinv %in% test_cacheinv ]

  testthat::expect_true( grepl( paste0( "^", expected_objref, "\\-\\d{8}-\\d{4}\\.lck" ), new_lckfile, perl = TRUE ) )

})





testthat::test_that( "appcache.purge", {
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move global cached config out of the way
  
  prev_config <- NA
  
  if ( exists( ".cxappglobalconfig", envir = .GlobalEnv) )
    prev_config <- base::get( ".cxappglobalconfig", envir = .GlobalEnv )
  
  on.exit({
    if ( inherits( prev_config, "cxapp_config") )
      assign(".cxappglobalconfig", prev_config, envir = .GlobalEnv )
  }, add = TRUE)
  
  rm( list = ".cxappglobalconfig", envir = .GlobalEnv )
  
  
  
  # - update .libPaths
  
  test_lbpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-libary-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_lbpath ) || ! dir.create( test_lbpath, recursive = TRUE ) )
    testthat::fail("Could not stage test library path")
  
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, .libPaths() ) )
  
  
  
  # - test cache path
  
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-cache-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_cachepath ) || ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail("Could not stage test cache path")
  
  
  
  # - inject app properties file in .libPaths
  test_cxapp_path <- file.path( test_lbpath, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( "CACHEPATH =", test_cachepath )),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test files
  
  test_srcpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-sources-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_srcpath ) || ! dir.create( test_srcpath, recursive = TRUE ) )
    testthat::fail("Could not stage test source path")
  
  
  test_files <- replicate( 10, 
                           cxlib::cxlib_standardpath( base::tempfile( pattern = "source-file-", tmpdir = test_srcpath, fileext = ".txt") ),
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

