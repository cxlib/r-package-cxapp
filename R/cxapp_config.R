#' Utility class to represent app configurations
#' 
#' @field .attr Internal configuration data store
#' 
#' @method initialize initialize
#' @method get get
#' @method show show
#' 
#' @description
#' A utility class to represent app configurations defined in property files and
#' environmental variables.
#' 
#' Property files are named `<context>.properties` where the configuration option
#' is referred to by `context/<property>`. 
#' 
#' Property file syntax and conventions are specified in the help reference for 
#' function \link[cxapp]{cxapp_propertiesread}.
#' 
#' Class initialization takes a vector of paths as an argument. If the path
#' specified ends in `.properties`, it is assumed a properties file. Otherwise,
#' the entry is assumed a directory containing property files. The vector `x` 
#' is processed in specified order and files within a directory in natural sort 
#' order. 
#' 
#' A property file name contains the characters a-z and digits 0-9 and the file 
#' extension `properties`. The property file name excluding the file extension 
#' is used as the context to look up a named property value.
#' 
#' The `option()` method returns the value of an option if it exists or the value
#' of `unset` if the option does not exist. An option is referred to by the string
#' `<context>/<property>`. If the context is specified, the context is assumed
#' to be `cxapp`.
#' 
#' If the option is not defined as part of a property file, the `option()` method
#' searches for an environment variable `<context>_<property>`. Any periods in
#' property name part is converted to underscores.  
#' 
#' An option value that contains the prefix `[env] <name>` or starts with the 
#' character `$<name>` is interpreted as a reference to an environmental 
#' variable with specified name. If the specified environmental variable is not
#' defined, the value of `unset` is returned. The environmental variable name is
#' case sensitive with leading and trailing spaces removed.
#' 
#' An option value that contains the prefix `[vault] <name>` is interpreted as
#' a reference to a vault secret with specified name. If a vault service is not 
#' configured or available or the vault hs not defined the specified secret, the
#' value of `unset` is returned. The secret name is case sensitive with leading
#' and trailing spaces removed.
#' 
#' 
#' 
#' @exportClass cxapp_config
#' @export cxapp_config


cxapp_config <- methods::setRefClass( "cxapp_config", 
                                      fields = list( ".attr" = "list" ) )


cxapp_config$methods( "initialize" = function( x ) {
  "Initialize"
  
  
  # -- init .attr
  .self$.attr <- list( ".internal" = list( "property.files" = character(0) ) 
                    )
  
  
  # -- property files to load
  prop_files <- character(0)
  
  
  # -- add first occurrence of cxapp.properties found in .libPaths()
  srch_paths <- base::file.path( cxapp:::.cxapp_standardpath(.libPaths()), 
                                 "cxapp",
                                 "cxapp.properties", 
                                 fsep = "/" )
  
  if ( any(file.exists( srch_paths )) )
    prop_files[ "cxapp.properties" ] <- utils::head( srch_paths[ base::file.exists( srch_paths) ], n = 1 ) 
  
  
  if ( ! missing(x) && ! is.null(x) )
    for ( xitem in x[ ! is.na(x) ] ) {

      # note: first occurrence of a file is retain ... all others ignored
      
      # specified as a file 
      if ( base::grepl( ".properties$", xitem, perl = TRUE, ignore.case = TRUE ) &&
           file.exists( xitem ) ) {
        
        if ( ! base::basename(xitem) %in% names(prop_files) )
          prop_files[ base::basename(xitem) ] <- xitem
        
        next()
      }

      # xitem is not a directory
      if ( ! dir.exists( xitem ) )
        next()
      

      # treat xitem as a directory
      for ( xfile in list.files( xitem, pattern = ".properties$", recursive = FALSE, include.dirs = FALSE, full.names = TRUE ) )
        if ( ! base::basename(xfile) %in% names(prop_files) )
          prop_files[ base::basename(xfile) ] <- xfile

    }
  
  
  # <- register discovered property files 
  .self$.attr[[".internal"]][["property.files"]] <- unname(prop_files)
   
  
  # -- process property files
  
  for ( xpath in .self$.attr[[".internal"]][["property.files"]] ) {

    props <- cxapp::cxapp_propertiesread( xpath )
    names(props) <- base::tolower(names(props))

    xcontext <- base::basename( tools::file_path_sans_ext( xpath ) )
    
    if ( ! grepl( "^[a-z0-9]+$", xcontext, perl = TRUE, ignore.case = TRUE ) )
      stop( "Invalid property file name" )
    
        
    .self$.attr[[ xcontext ]] <-  props
    
    base::rm( props )
  }

    
  
})



cxapp_config$methods( "option" = function( x, unset = NA, as.type = TRUE ) {
  "Get property value"
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits(x, "character") )
    stop( "The specified option is missing" )

  if ( length(x) > 1 )
    stop( "More than one option specified. Expecting one.")

  if ( ! base::grepl( "^([a-z0-9\\._\\-]+/)?[a-z0-9\\._]+$", x, ignore.case = TRUE, perl = TRUE ) )
    stop( "Option reference is invalid" )
  
  
  # -- generate a standard set of references
  #    note: if context not specified, assume cxapp
  
  opt_std <- base::tolower( ifelse( grepl( "/", x), x, paste("cxapp", x, sep = "/") ) )
  
  opt_ref <- c( "property" = base::gsub( "/", ".", opt_std ),
                "env" = base::gsub( "[\\-\\./]", "_", opt_std ) )

 
  # -- initialize value
  xvalue <- NA
  
  
  # -- search properties
  xopts <- base::unlist( .self$.attr[ names(.self$.attr) != ".internal" ] )
  
  if ( base::tolower(opt_ref["property"]) %in% base::tolower(names(xopts)) )
    xvalue <- base::trimws( unname(xopts[ opt_ref["property"] ]) )
  

  # -- search environmental variables  
  #    note: search is case in-sensitive
  if ( is.na( xvalue ) ) {
    
    opt_env_names <- names(Sys.getenv())

    if ( base::tolower(opt_ref["env"]) %in% base::tolower(opt_env_names) ) 
      xvalue <- base::trimws( Sys.getenv( utils::head( opt_env_names[ base::tolower(opt_env_names) %in% base::tolower(opt_ref["env"]) ], n = 1 ), unset = NA ) )
    
  }

  
  if ( is.na(xvalue) )
    return(unset)


  # -- env variable re-directs
  
  #    note: the value has the prefix "[env]"
  if ( base::startsWith( base::trimws(base::toupper(xvalue)), "[ENV]" ) ) {
    
    # note: start position 6 is length of [env] + 1
    xref_name <- base::trimws( base::substring( base::trimws(xvalue), 6 ) )
    
    xvalue <- base::Sys.getenv( xref_name, unset = unset )
  }
  
  
  #    note: the value starts with the character "$"
  if ( base::startsWith( base::trimws(base::toupper(xvalue)), "$" ) ) {
    
    # note: start position 2 is character after $
    xref_name <- base::trimws( base::substring( base::trimws(xvalue), 2 ) )
    
    xvalue <- base::Sys.getenv( xref_name, unset = unset )
  }
  

  
  # -- vault secret re-directs
  
  if ( base::startsWith( base::trimws(base::toupper(xvalue)), "[VAULT]" ) ) {

    # note: start position 8 is length of [vault] + 1
    xref_name <- base::trimws( base::substring( base::trimws(xvalue), 8 ) )
    
    # connect to a vault    
    vaultsvc <- cxapp::cxapp_vault()
    
    xvalue <- vaultsvc$secret( xref_name, unset = unset )
  }
  
    
  
  if ( ! as.type )
    return(xvalue)
      

  # -- paths
  #    note: if property name includes the term PATH
  #    note: value is treated as valus of delimited list of paths
  
  if ( grepl( "path", gsub( ".*/(.*)", "\\1", opt_std ), ignore.case = TRUE ) )
    return( base::trimws(base::unlist(base::strsplit( xvalue, .Platform$path.sep, fixed = TRUE))) )
  
  
  # -- enabled switch
  #    note: value is a single word
  #    note: if the value is equal to enable, enabled, grant or permit
  #    note: enabled switch is TRUE
  
  if ( grepl( "^(enable|enabled|grant|permit)$", xvalue, ignore.case = TRUE, perl = TRUE ) )
    return( TRUE )

    
  # -- disabled switch
  #    note: value is a single word
  #    note: if the value is equal to disable, disabled, revoke or deny
  #    note: disabled switch is TRUE
  
  if ( grepl( "^(disable|disabled|revoke|deny)$", xvalue, ignore.case = TRUE, perl = TRUE ) )
    return( FALSE )
  
  
  # -- or it is simply a value
  
  return(base::trimws(xvalue))

})




cxapp_config$methods( "show" = function( x ) {
  "Display list of properties"
  
  xlst <- character(0)
  
  # -- add list of property files to list
  
  xlst <- append( xlst, c( "Property files",
                           paste( base::rep_len( "-", 60), collapse = "") ) )
  
  if ( ! ".internal" %in% names(.self$.attr) || 
       ! "property.files" %in% names(.self$.attr[[".internal"]]) || 
       ( length(.self$.attr[[".internal"]][["property.files"]]) == 0 ) )
    xlst <- append( xlst, "(None)" )
  
  
  if ( ".internal" %in% names(.self$.attr) && 
       "property.files" %in% names(.self$.attr[[".internal"]]) &&
       ( length(.self$.attr[[".internal"]][["property.files"]]) > 0 ) )
    
    xlst <- append( xlst, .self$.attr[[".internal"]][["property.files"]] )
  
  
  # -- display list
  cat( c( base::rep_len(" ", 2),
          xlst, 
          base::rep_len(" ", 2) ), 
       sep = "\n" )
  
})