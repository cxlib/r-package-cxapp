#' Simple function to log error messages
#' 
#' @param x bject or vector of messages to log
#' @param echo Enable/Disable output of log messages to console
#' 
#' @returns Invisible vector of log messages
#' 
#' @description
#' The `cxxapp_logerr` function logs messages in specified order prefixed with the current 
#' date and time. 
#' 
#' If `x` is not a vector, the object is expected to inherit `try-error` 
#' (see \link[base]{try}).
#' 
#' The log file parent directory path is defined by the `LOG.PATH` cxapp property. If
#' `LOG.PATH` is not defined log messages are written to the console. 
#' 
#' The `LOG.NAME` option specifies the basis for the log and error log file names.
#' Any log file name parts associated with log rotation is appended to the log file base name 
#' separated by an underscore (`_`). Error log file names end in the file extension `err`.
#' 
#' If `LOG.NAME` is not defined, the log file name is `app.err`.
#' 
#' The `LOG.ROTATION` cxapp property defines the log file rotation. Valid rotations
#' are `YEAR`, `MONTH` and `DAY`. The log rotation follows the format four digit
#' year and two digit month and day.  
#' 
#' Log messages follow the convention `[%Y-%m-%d %H:%M:%S] <message>  [record]`
#' where `record` is a pseudo-random string that identifies lines that are submitted 
#' in the same call to `cxapp_logerr()`.
#' 
#' if executing in `interactive()` mode, error log messages are written to the console.
#' 
#' 
#' @export


cxapp_logerr <- function( x, echo = base::interactive() ) {
  
  
  # -- futility 
  if ( missing(x) || is.null(x) || all(is.na(x)) || ! inherits( x, c( "character", "try-error") ) )
    return(character(0))
  
  if ( ! inherits( x, "try-error") && 
       ( (length(x) == 0) || all( base::trimws(x) == "" ) ) )
    return(character(0))
  

  # -- record code
  code_str <- paste0( " [", 
                      digest::digest( paste( c( format( base::as.POSIXlt( base::Sys.time(), tz = "UTC"), format = "%Y-%m-%d %H:%M:%OS6"), 
                                                as.character(Sys.info())), collapse = ":" ), algo = "crc32" ), 
                      "]")
  
  

  
  # -- init log message 
  
  init_msg <- paste( format( base::as.POSIXlt( base::Sys.time(), tz = "UTC"), format = "[%Y-%m-%d %H:%M:%S]"), 
                     "Log file created",
                     code_str )
  
  
  
  # -- format messages

  # - message lines  
  
  msg_lines <- character(0)

  if ( inherits( x, "try-error") )
    msg_lines <- base::trimws( unlist( strsplit( as.character(x), "\n", fixed = TRUE )))
  
    
  if ( ! inherits( x, "try-error") )
    msg_lines <- base::trimws(x) 
  

  
  # - message record
  
  str_datetime <- format( base::as.POSIXlt( base::Sys.time(), tz = "UTC"), format = "[%Y-%m-%d %H:%M:%S]")
  
  msgs <- base::trimws( paste( str_datetime, msg_lines, code_str, sep = " ") )
  
  
  
  
  # -- get configuration
  cfg <- cxapp::.cxappconfig()
  
  
  # -- echo mode ... send to console  
  if ( echo || is.na( cfg$option( "LOG.PATH", unset = NA ) ) ) 
    base::cat( msgs , sep = "\n")
  
  
  # -- no log configuration  
  if ( is.na( cfg$option( "LOG.PATH", unset = NA ) ) ) 
    return(invisible(msgs))
  
  
  
  # -- log file
  
  log_dir <- cxapp::cxapp_standardpath( cfg$option( "LOG.PATH", as.type = FALSE ) )
  
  if ( ! dir.exists( log_dir ) )
    stop( "Log directory ", log_dir, " does not exist" )
  
  
  log_file_name <- cfg$option( "LOG.NAME", unset = "app", as.type = FALSE )
  
  #    note: known log rotations
  log_rotations <- c( "year" = "%Y", "month" = "%Y%m", "day" = "%Y%m%d" )
  
  
  if ( ! is.na( cfg$option( "LOG.ROTATION", unset = NA, as.type = FALSE ) ) ) {
    
    if ( ! base::tolower(cfg$option( "LOG.ROTATION", as.type = FALSE )) %in% names(log_rotations) )
      stop( "Log rotation ", cfg$option( "LOG.ROTATION", as.type = FALSE ), " not known")
    
    log_file_name <- append( log_file_name, 
                             base::format( base::as.POSIXlt(base::Sys.time(), tz = "UTC"), format = log_rotations[ base::tolower(cfg$option( "LOG.ROTATION", as.type = FALSE )) ] ) )
  }                             
  
  log_file_path <- file.path( log_dir, paste0( paste( log_file_name, collapse = "-"), ".err" ), fsep = "/" )
  
  
  # initiate log if it does not exist
  if ( ! file.exists(log_file_path) )
    msgs <- append( init_msg, msgs)
  
  
  # -- append log file
  base::cat( msgs, sep = "\n", file = log_file_path, append = TRUE )
  
  
  # -- return  
  return(invisible(msgs))
  
}