#' Simple function to log messages
#' 
#' @param x Vector of messages to log
#' @param attr A vector of attribute values to append to log message
#' @param echo Enable/Disable output of log messages to console
#' 
#' @returns Invisible vector of log messages
#' 
#' @description
#' The `cxapp_log` function logs messages in specified order prefixed with the current 
#' date and time. 
#' 
#' The log file parent directory path is defined by the `LOG.PATH` cxapp property. If
#' `LOG.PATH` is not defined log messages are written to the console. 
#' 
#' The `LOG.NAME` option specifies the basis for the log file name. Any log file 
#' name parts associated with log rotation is appended to the log file base name 
#' separated by an underscore (`_`). Log file names end in the file extension `log`.
#' 
#' If `LOG.NAME` is not defined, the log file name is `app.log`.
#' 
#' The `LOG.ROTATION` cxapp property defines the log file rotation. Valid rotations
#' are `YEAR`, `MONTH` and `DAY`. The log rotation follows the format four digit
#' year and two digit month and day.  
#' 
#' Log messages follow the convention `[%Y-%m-%d %H:%M:%S] <message>` 
#' 
#' Specified attribute values are appended to the log message between square 
#' brackets delimited by semi-colon.
#' 
#' if executing in `interactive()` mode, log messages are written to the console.
#' 
#' 
#' @export


cxapp_log <- function( x, attr = NULL, echo = base::interactive() ) {
  
  
  # -- futility 
  if ( missing(x) || is.null(x) || all(is.na(x)) )
    return()

  
  # -- attributes
  
  attr_str <- ""
  
  if ( ! is.null(attr) )
    attr_str <- paste0( "[", paste(unlist(as.character(attr), use.names = FALSE), collapse = ";"), "]" )
  
  
  
  # -- init log message 
  init_msg <- format( base::as.POSIXlt( base::Sys.time(), tz = "UTC"), format = "[%Y-%m-%d %H:%M:%S] Log file created")


  # -- format messages
  msgs <- base::trimws( paste( format( base::as.POSIXlt( base::Sys.time(), tz = "UTC"), format = "[%Y-%m-%d %H:%M:%S]"), x, attr_str, sep = " ") )

  
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

  log_file_path <- file.path( log_dir, paste0( paste( log_file_name, collapse = "-"), ".log" ), fsep = "/" )
      

  # initiate log if it does not exist
  if ( ! file.exists(log_file_path) )
    msgs <- append( init_msg, msgs)
      

  # -- append log file
  base::cat( msgs, sep = "\n", file = log_file_path, append = TRUE )


  # -- return  
  return(invisible(msgs))
  
}