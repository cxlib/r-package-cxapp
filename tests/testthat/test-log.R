#
#  Tests for cxapp::cxapp_log()
#  
#
#

#' @cx.testsfor cxapp::cxapp_log()


testthat::test_that( "log.noConfig", {

  #' @cx.tests Log messages written to console when log directory path property not defined

  
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
  
  

  # - test messages  

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
  
  
  #' @cx.tests Add messages to log results in an error when log directory path property defined and log directory does not exist

  
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
  
  
  # - test log directory
  test_logpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_logpath ) || file.exists( test_logpath ) )
    testthat::fail( "Unexpected test log exists" )
  
  

  # - test app properties
  
  base::writeLines( c( "# test properties file",
                       paste0( "APP.LOG.PATH = ", test_logpath ) ),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  

  # - test messages 

  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  

  # -- test   
  testthat::expect_error( cxapp::cxapp_log( test_msgs, echo = FALSE ), 
                          regexp = paste( "^Log directory", test_logpath , "does not exist$" ) )
  
  
  
})





testthat::test_that( "log.defaultLogFileExt", {
  
  #' @cx.tests Add messages to log where log file has default log file extension
  
  
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
  
  
  # - test log directory
  test_logpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_logpath ) || file.exists( test_logpath ) )
    testthat::fail( "Unexpected test log exists" )
  
  if ( ! dir.exists( test_logpath ) && ! dir.create( test_logpath, recursive = TRUE ) )
    testthat::fail("Could not stage test log directory")
  
  
  
  # - test app properties
  
  base::writeLines( c( "# test properties file",
                       paste0( "APP.LOG.PATH = ", test_logpath ) ),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  

  # -- test   
  result <- cxapp::cxapp_log( test_msgs, echo = FALSE )
  
  
  # -- expected
  
  expected_msgs <- test_msgs

  expected_log_file <- file.path( test_logpath, "app.log", fsep = "/" )
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






testthat::test_that( "log.messageAttr", {
  
  
  #' @cx.tests Add messages to log with attributes
  
  
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
  
  
  # - test log directory
  test_logpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_logpath ) || file.exists( test_logpath ) )
    testthat::fail( "Unexpected test log exists" )
  
  if ( ! dir.exists( test_logpath ) && ! dir.create( test_logpath, recursive = TRUE ) )
    testthat::fail("Could not stage test log directory")
  
  
  
  # - test app properties
  
  base::writeLines( c( "# test properties file",
                       paste0( "APP.LOG.PATH = ", test_logpath ) ),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  
  # - test attributes
  test_attr <- base::trimws( base::replicate( 5, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  
  # -- test   
  result <- cxapp::cxapp_log( test_msgs, attr = test_attr, echo = FALSE )
  
  
  # -- expected
  
  expected_attr <- paste0( "[", paste( test_attr, collapse = ";" ), "]" )

  expected_msgs <- paste( test_msgs, expected_attr )
 
  expected_log_file <- file.path( test_logpath, "app.log", fsep = "/" )
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






testthat::test_that( "log.configLogRotateYear", {
  

  #' @cx.tests Add messages to log where the log file is rotated yearly
  
  
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
  
  
  # - test log directory
  test_logpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_logpath ) || file.exists( test_logpath ) )
    testthat::fail( "Unexpected test log exists" )
  
  if ( ! dir.exists( test_logpath ) && ! dir.create( test_logpath, recursive = TRUE ) )
    testthat::fail("Could not stage test log directory")
  
  
  
  # - test app properties
  
  base::writeLines( c( "# test properties file",
                       paste0( "APP.LOG.PATH = ", test_logpath ),
                       "APP.LOG.ROTATION = year" ),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  
  
  
  # -- test   
  result <- cxapp::cxapp_log( test_msgs, echo = FALSE )
  
  
  # -- expected
  
  expected_msgs <- test_msgs
  
  expected_log_file <- file.path( test_logpath, 
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




testthat::test_that( "log.configLogRotateMonth", {
  
  
  #' @cx.tests Add messages to log where the log file is rotated monthly
  
  
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
  
  
  # - test log directory
  test_logpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_logpath ) || file.exists( test_logpath ) )
    testthat::fail( "Unexpected test log exists" )
  
  if ( ! dir.exists( test_logpath ) && ! dir.create( test_logpath, recursive = TRUE ) )
    testthat::fail("Could not stage test log directory")
  
  
  
  # - test app properties
  
  base::writeLines( c( "# test properties file",
                       paste0( "APP.LOG.PATH = ", test_logpath ),
                       "APP.LOG.ROTATION = month" ),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  
  
  
  # -- test   
  result <- cxapp::cxapp_log( test_msgs, echo = FALSE )
  
  
  # -- expected
  
  expected_msgs <- test_msgs
  
  expected_log_file <- file.path( test_logpath, 
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




testthat::test_that( "log.configLogRotateDay", {
  
  
  #' @cx.tests Add messages to log where the log file is rotated daily
  
  
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
  
  
  # - test log directory
  test_logpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_logpath ) || file.exists( test_logpath ) )
    testthat::fail( "Unexpected test log exists" )
  
  if ( ! dir.exists( test_logpath ) && ! dir.create( test_logpath, recursive = TRUE ) )
    testthat::fail("Could not stage test log directory")
  
  
  
  # - test app properties
  
  base::writeLines( c( "# test properties file",
                       paste0( "APP.LOG.PATH = ", test_logpath ),
                       "APP.LOG.ROTATION = day" ),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  
  
  
  # -- test   
  result <- cxapp::cxapp_log( test_msgs, echo = FALSE )
  
  
  # -- expected
  
  expected_msgs <- test_msgs
  
  expected_log_file <- file.path( test_logpath, 
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




testthat::test_that( "log.configLogRotateInvalid", {
  
  #' @cx.tests Add messages to log where the log file rotation is an invalid rotation period results in an error
  
  
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
  
  
  # - test log directory
  test_logpath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-log-path-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_logpath ) || file.exists( test_logpath ) )
    testthat::fail( "Unexpected test log exists" )
  
  if ( ! dir.exists( test_logpath ) && ! dir.create( test_logpath, recursive = TRUE ) )
    testthat::fail("Could not stage test log directory")
  
  
  
  # - test app properties
  
  test_invalid_rotate_ref <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), sample( 25:50, 1 ), replace = TRUE), collapse = "" )
  
  base::writeLines( c( "# test properties file",
                       paste0( "APP.LOG.PATH = ", test_logpath ),
                       paste0( "APP.LOG.ROTATION =", test_invalid_rotate_ref )),
                    con = file.path( test_apphome, "app.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_apphome, "app.properties", fsep = "/") ) )
    testthat::fail( "Could not stage app.properties" )
  
  
  
  # - test messages 
  
  test_msgs <- base::trimws( base::replicate( 10, 
                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40), collapse = "" ), 
                                              simplify = TRUE ) )
  
  
  
  
  
  
  
  
  # -- test   
  testthat::expect_error( cxapp::cxapp_log( test_msgs, echo = FALSE ), regexp = paste( "^Log rotation", test_invalid_rotate_ref, "not known$" ) )
  
  
})

