#
#  Tests for cxapp::cxapp_log()
#  
#
#

testthat::test_that( "log.noConfig", {

  # -- stage
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  

  # -- test   
  result <- testthat::capture_output_lines( cxapp::cxapp_log( test_msgs ), print = FALSE )

  
  # -- expected
  
  expected_msgs <- test_msgs
  
  
  # -- assertions

  actual_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", result )   

  testthat::expect_equal( actual_msgs, expected_msgs )
    

})




testthat::test_that( "log.configLogPathNotExist", {
  

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
  
  
  # log directory
  
  test_log_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_log_path ) || file.exists( test_log_path ) )
    testthat::fail( "Unexpected test log exists" )

  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "LOG.PATH = ", test_log_path ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  

  # messages 

  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  

  # -- test   
  testthat::expect_error( cxapp::cxapp_log( test_msgs, echo = FALSE ), 
                          regexp = paste( "^Log directory", test_log_path , "does not exist$" ) )
  
  
  
})





testthat::test_that( "log.configLogPathExistDefaultLog", {
  
  
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
  
  
  # log directory
  
  test_log_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_log_path ) && ! dir.create( test_log_path, recursive = TRUE ) )
    testthat::fail("Could not create test log directory")
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "LOG.PATH = ", test_log_path ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  # -- test   
  result <- cxapp::cxapp_log( test_msgs, echo = FALSE )
  
  
  # -- expected
  
  expected_msgs <- test_msgs

  expected_log_file <- file.path( test_log_path, "app.log", fsep = "/" )
  expected_log_msgs <- c( "Log file created", 
                          test_msgs )
  
  # -- assertions
  
  
  # returned messages
  actual_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", result, perl = TRUE )   
  testthat::expect_equal( actual_msgs, expected_log_msgs )

  
  # log file
  testthat::expect_true( base::file.exists( expected_log_file ) )
  
  
  # log file messages
  actual_log_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", base::readLines( expected_log_file ), perl = TRUE )   
  testthat::expect_equal( actual_log_msgs, expected_log_msgs )
  
  
})






testthat::test_that( "log.configLogPathExistDefaultLogAttr", {
  
  
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
  
  
  # log directory
  
  test_log_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_log_path ) && ! dir.create( test_log_path, recursive = TRUE ) )
    testthat::fail("Could not create test log directory")
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "LOG.PATH = ", test_log_path ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  # - attributes
  test_attr <- base::trimws( base::replicate( 5, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  
  # -- test   
  result <- cxapp::cxapp_log( test_msgs, attr = test_attr, echo = FALSE )
  
  
  # -- expected
  
  expected_attr <- paste0( "[", paste( test_attr, collapse = ";" ), "]" )

  expected_msgs <- paste( test_msgs, expected_attr )
 
  expected_log_file <- file.path( test_log_path, "app.log", fsep = "/" )
  expected_log_msgs <- c( "Log file created", 
                          expected_msgs )
  
  # -- assertions
  
  
  # returned messages
  actual_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", result, perl = TRUE )   
  testthat::expect_equal( actual_msgs, expected_log_msgs )
  
  
  # log file
  testthat::expect_true( base::file.exists( expected_log_file ) )
  
  
  # log file messages
  actual_log_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", base::readLines( expected_log_file ), perl = TRUE )   
  testthat::expect_equal( actual_log_msgs, expected_log_msgs )
  
  
})






testthat::test_that( "log.configLogPathExistDefaultLogRotateYear", {
  
  
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
  
  
  # log directory
  
  test_log_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_log_path ) && ! dir.create( test_log_path, recursive = TRUE ) )
    testthat::fail("Could not create test log directory")
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "LOG.PATH = ", test_log_path ),
                       "LOG.ROTATION = year" ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  # -- test   
  result <- cxapp::cxapp_log( test_msgs, echo = FALSE )
  
  
  # -- expected
  
  expected_msgs <- test_msgs
  
  expected_log_file <- file.path( test_log_path, 
                                  paste0( "app-", 
                                          base::format( base::as.POSIXlt(base::Sys.time(), tz = "UTC"), format = "%Y" ),
                                          ".log"),
                                  fsep = "/" )
                                  
  expected_log_msgs <- c( "Log file created", 
                          test_msgs )
  
  # -- assertions
  
  
  # returned messages
  actual_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", result, perl = TRUE )   
  testthat::expect_equal( actual_msgs, expected_log_msgs )
  
  
  # log file
  testthat::expect_true( base::file.exists( expected_log_file ) )
  
  
  # log file messages
  actual_log_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", base::readLines( expected_log_file ), perl = TRUE )   
  testthat::expect_equal( actual_log_msgs, expected_log_msgs )

})



testthat::test_that( "log.configLogPathExistDefaultLogRotateMonth", {
  
  
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
  
  
  # log directory
  
  test_log_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_log_path ) && ! dir.create( test_log_path, recursive = TRUE ) )
    testthat::fail("Could not create test log directory")
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "LOG.PATH = ", test_log_path ),
                       "LOG.ROTATION = month" ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  # -- test   
  result <- cxapp::cxapp_log( test_msgs, echo = FALSE )
  
  
  # -- expected
  
  expected_msgs <- test_msgs
  
  expected_log_file <- file.path( test_log_path, 
                                  paste0( "app-", 
                                          base::format( base::as.POSIXlt(base::Sys.time(), tz = "UTC"), format = "%Y%m" ),
                                          ".log"),
                                  fsep = "/" )
  
  expected_log_msgs <- c( "Log file created", 
                          test_msgs )
  
  # -- assertions
  
  
  # returned messages
  actual_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", result, perl = TRUE )   
  testthat::expect_equal( actual_msgs, expected_log_msgs )
  
  
  # log file
  testthat::expect_true( base::file.exists( expected_log_file ) )
  
  
  # log file messages
  actual_log_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", base::readLines( expected_log_file ), perl = TRUE )   
  testthat::expect_equal( actual_log_msgs, expected_log_msgs )
  
})



testthat::test_that( "log.configLogPathExistDefaultLogRotateDay", {
  
  
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
  
  
  # log directory
  
  test_log_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_log_path ) && ! dir.create( test_log_path, recursive = TRUE ) )
    testthat::fail("Could not create test log directory")
  
  
  # inject cxapp properties file in .libPaths
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "LOG.PATH = ", test_log_path ),
                       "LOG.ROTATION = day" ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  # -- test   
  result <- cxapp::cxapp_log( test_msgs, echo = FALSE )
  
  
  # -- expected
  
  expected_msgs <- test_msgs
  
  expected_log_file <- file.path( test_log_path, 
                                  paste0( "app-", 
                                          base::format( base::as.POSIXlt(base::Sys.time(), tz = "UTC"), format = "%Y%m%d" ),
                                          ".log"),
                                  fsep = "/" )
  
  expected_log_msgs <- c( "Log file created", 
                          test_msgs )
  
  # -- assertions
  
  
  # returned messages
  actual_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", result, perl = TRUE )   
  testthat::expect_equal( actual_msgs, expected_log_msgs )
  
  
  # log file
  testthat::expect_true( base::file.exists( expected_log_file ) )
  
  
  # log file messages
  actual_log_msgs <- gsub( "^\\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s+(.*)$", "\\1", base::readLines( expected_log_file ), perl = TRUE )   
  testthat::expect_equal( actual_log_msgs, expected_log_msgs )
  
})



testthat::test_that( "log.configLogPathExistDefaultLogRotateInvalid", {
  
  
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
  
  

  # log directory
  
  test_log_path <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_log_path ) && ! dir.create( test_log_path, recursive = TRUE ) )
    testthat::fail("Could not create test log directory")
  
  
  # inject cxapp properties file in .libPaths
  
  test_invalid_rotate_ref <- "sometinginvalid"
  
  test_cxapp_path <- file.path( test_root, "cxapp", fsep = "/" )
  
  if ( ! dir.exists( test_cxapp_path ) && ! dir.create( test_cxapp_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxapp in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "LOG.PATH = ", test_log_path ),
                       paste0( "LOG.ROTATION = ", test_invalid_rotate_ref ) ),
                    con = file.path( test_cxapp_path, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxapp_path, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  # messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  # -- test   
  testthat::expect_error( cxapp::cxapp_log( test_msgs, echo = FALSE ), regexp = paste( "^Log rotation", test_invalid_rotate_ref, "not known$" ) )
  
  
})
