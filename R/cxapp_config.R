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
#' One or more properties are defined in property files. A property file name
#' contains the characters a-z and digits 0-9 and the file extension 
#' `properties`. 
#' 
#' Property files are searched in the following sequence of directories (search
#' tree).
#' 
#' \itemize{
#'   \item Directory `$APP_HOME/config`, if the `APP_HOME` environmental variable
#'         is defined
#'   \item Directory `$APP_HOME`, if the `APP_HOME` environmental variable is
#'         defined
#'   \item The cxapp package install directory in the library tree 
#'         (\link[base]{.libPaths})
#'   \item Current working directory (\link[base]{getwd})
#' }
#' 
#' If `APP_HOME` environmental variable is a list of paths, each path is added 
#' to search tree in the sequence specified.
#' 
#' The class initialization first searches for and imports properties from the
#' file `app.properties` in the search tree and then process each additional 
#' property file in natural sort order. If a property file exists in multiple
#' search locations and `recursive = FALSE`, the first occurrence is used and
#' the remaining property file locations are ignored.
#' 
#' Property file syntax and conventions, including property naming conventions, 
#' are specified in the help reference for function
#' \link[cxapp]{cxapp_propertiesread}.
#' 
#' Configuration property is referred to by the `<property>` name, case
#' insensitive. If `<property>` is defined in more than one property file, the 
#' `<property>` the value will refer to the first occurrence of the property 
#' definition as determined by the property file search sequence. 
#' 
#' If class initialization `cached = TRUE` (default), the in-memory cached 
#' configuration will be used instead of searching for and importing all 
#' property files. The configuration properties are cached in the R object
#' `.cxapp.wrkcache.config` in the R session global environment 
#' \link[base]{.GlobalEnv}.
#' 
#' Note that `cached = FALSE` will update the existing cache after the property 
#' files are imported following the convention that static configuration 
#' (property files) takes precedence over dynamic configuration.
#' 
#' All cached configuration properties are reset (not amended) every time 
#' property files are imported regardless of the value of `cached`.
#' 
#' The `option()` method returns the value of property `x`, if it exists. If `x`
#' is a character vector of property names, the value of the first existing 
#' property option from the specified sequence of names is returned. 
#' 
#' The property value returned is a named entry with the matching property name
#' in lower case.
#' 
#' If none of the specified properties exist as a configuration property and 
#' `search.envars = TRUE` (default), the `option()` method searches for an
#' environment variable for each entry of `x`, first in lower case followed by
#' upper case, and returns first occurrence. The character period `.` in a 
#' property name is replaced with a single underscore `_` in the environment 
#' variable name. If the environmental variable names do not correspond to a 
#' property name, append the environmental variable names to the end of `x`
#' (note translation of special characters and case matching).
#' 
#' If the property is not defined or do not resolve to an environmental variable, 
#' the value of `unset` is returned.
#' 
#' An option value that contains the prefix `[env] <name>` or starts with the 
#' character `$`, as in `$<name>`, is interpreted as a reference to an environmental 
#' variable with specified name. If the specified environmental variable is not
#' defined, the value of `unset` is returned. The environmental variable name is
#' case sensitive with leading and trailing spaces removed.
#' 
#' An option value that contains the prefix `[vault] <name>` is interpreted as
#' a reference to a vault secret with specified name. The value of `unset` is 
#' returned if a vault service is not configured, the vault service is not 
#' available or the vault returns \emph{not found} for the specified secret. 
#' The secret name is case sensitive with leading and trailing spaces removed.
#' 
#' 
#' @exportClass cxapp_config
#' @export cxapp_config


cxapp_config <- methods::setRefClass( "cxapp_config", 
                                      fields = list( ".attr" = "list" ) )


cxapp_config$methods( "initialize" = function( cached = TRUE, recursive = TRUE ) {
  "Initialize"

  
  # -- init .attr
  .self$.attr <- list( "search.tree" = character(0),
                       "property.files" = list(),
                       "properties" = list() ) 
  

  # -- some sense checks    

  if ( ! inherits( cached, "logical" ) )
    stop( "The cached switch is invalid" )


                    
  # -- cached
  if ( cached && base::exists( ".cxapp.wrkcache.config", envir = base::.GlobalEnv )) {

    # - simple integrity checks    
    if ( ! inherits( base::get( ".cxapp.wrkcache.config", envir = base::.GlobalEnv ), "list" ) ||
         ! all( base::names(.self$.attr) %in% base::names(base::get( ".cxapp.wrkcache.config", envir = base::.GlobalEnv )) ) )
      stop( "Cached configuration invalid or corrupted" )

    # - restore configuration from cached copy
    .self$.attr <- base::get( ".cxapp.wrkcache.config", envir = base::.GlobalEnv )
    
    return()
  }

    
  
  
  
  # -- property files to load

  #   directory search tree for *.properties
  srch_lst <- character(0)

  
  # - APP_HOME environment
  
  if ( "APP_HOME" %in% base::toupper(names(Sys.getenv())) ) {

    # case insensitive matching     
    env_names <- base::names(Sys.getenv())

    if ( length(env_names[ base::toupper(env_names) %in% "APP_HOME" ]) != 1 )
      stop( "Multiple instances of APP_HOME environmental variable identified when case ignored" )
    
    app_home_env <- Sys.getenv( env_names[ match( "APP_HOME", base::toupper(env_names) ) ] )
    
    srch_lst <- append( srch_lst, 
                         sapply( base::unlist( base::strsplit( app_home_env, .Platform$path.sep, fixed = TRUE), use.names = FALSE ), 
                                 function(x) {
                                   cxapp::cxapp_standardpath( c( file.path( x, "config", fsep = "/"), x ) ) 
                                 } ) )

  }  # end of if-statement for APP_HOME
  
  
  # - cxapp install directory in library tree
  
  if ( "cxapp" %in% list.dirs( .libPaths(), recursive = FALSE, full.names = FALSE ) ) {

    # note: list.dirs( .libPaths(), ... ) does not preserve the order of .libPaths()
    lb_paths <- base::unlist( lapply( .libPaths(), function(x) {
      cxapp::cxapp_standardpath( list.dirs( x, recursive = FALSE, full.names = TRUE ) )
    }), use.names = FALSE )
    

    srch_lst <- append( srch_lst, 
                        utils::head( lb_paths[ base::basename(lb_paths) == "cxapp" ], n = 1 ) )
    
  }
  

  # - current working directory
  srch_lst <- append( srch_lst, cxapp::cxapp_standardpath( base::getwd() ) )
  
  
  # - add search tree to internal references
  .self$.attr[["search.tree"]] <- srch_lst[ dir.exists(srch_lst) ]
  

  
  # - identify property files to load
  
  prop_files <- character(0)

  
  for ( xpath in .self$.attr[["search.tree"]] ) {

    xpath_propfiles <- base::sort(list.files( xpath, pattern = "\\.properties$", recursive = FALSE, full.names = TRUE ))

    
    # deal with app.properties precedence
    # using unique() later to clean out duplicate paths
    if ( "app.properties" %in% base::basename(base::tolower(xpath_propfiles)) &&
         ( recursive || ( length(app_propfiles) == 0 ) ) )
      prop_files <- append( prop_files, file.path( xpath, "app.properties", fsep = "/" ) )

    
    for ( xfile in xpath_propfiles ) 
      if ( recursive || ! base::basename(xfile) %in% base::basename(prop_files) )
        prop_files <- append( prop_files, xfile ) 

  }  #  end of for-statement across each directory in search tree  
    
  
  # - remove duplicates
  #   note: paths are not resolved to real paths in order to permit configuration linking
  prop_files <- base::unique(prop_files)
    



  # -- import properties
  
  for ( xfile in prop_files ) {
    
    # - import properties from file 
    props <- cxapp::cxapp_propertiesread( xfile )
    base::names(props) <- base::tolower(base::trimws(base::names(props)))

    # - append missing properties
    .self$.attr[["properties"]] <- append( .self$.attr[["properties"]], 
                                           as.list( props[ ! base::names(props) %in% base::names(.self$.attr[["properties"]]) ] ) )
   
    # - add property file to list of imported 
    .self$.attr[["property.files"]][[ length(.self$.attr[["property.files"]]) + 1 ]]  <- list( "path" = xfile, 
                                                                                               "sha" = digest::digest( xfile, algo = "sha1", file = TRUE ) )
    
  }  #  end of for-statement to import each identified property file
  
  
  
  # -- update cache
  #    note: we always do this .. makes cached = TRUE work
  base::assign( ".cxapp.wrkcache.config", .self$.attr, envir = base::.GlobalEnv )

})



cxapp_config$methods( "option" = function( x, unset = NA, search.envars = TRUE ) {
  "Get property value"
  
  if ( missing(x) || ! inherits( x, c( "character", "numeric" ) ) )
    stop( "The specified option is missing" )

  if ( any( ! grepl( "^[a-z0-9][a-z0-9_\\.]{0,98}[a-z0-9]$", as.character(x), ignore.case = TRUE, perl = TRUE )) )
    stop( "One or more option references are invalid" )
  
  
  if ( ! inherits( search.envars, "logical" ) )
    stop( "Option to search environmental variables is an invalid value" )

  
  # -- generate a standard set of references
  opt_refs <- lapply( base::tolower(base::trimws(as.character(x))), function(x) {
    c( "property" = x, 
       "env" = base::gsub( "\\.", "_", x ) )
  })
  
  
 
  # -- initialize option standardized name and value 
  opt_std <- NA
  xvalue <- NA
  
  
  # -- search defined properties
  
  for ( xopt in opt_refs ) 
    if ( xopt["property"] %in% base::names(.self$.attr[["properties"]]) ) {
      opt_std <- xopt["property"]
      xvalue <- .self$.attr[["properties"]][[ xopt["property"] ]]
      break()
    }

  
  # -- search environmental variables
  if ( is.na(xvalue) && search.envars )
    for ( xopt in opt_refs ) 
      if ( xopt["env"] %in% base::tolower(base::names(Sys.getenv())) ) {
        opt_std <- xopt["property"]
        xvalue <- Sys.getenv( xopt["env"], unset = Sys.getenv( base::toupper(xopt["env"]), unset = NA ) )
        break()
      }
  


  # -- return for property not found
  if ( is.na(xvalue) )
    return(unset)


  # -- env variable re-directs
  #    note: variable name is identified by value prefix [ENV] or $
  #    note; ignore case
  
  if ( any( base::startsWith( base::trimws(base::toupper(xvalue)), c( "[ENV]", "$" )) ) ) {

    # - determine environmental variable name 
    #   note: variable name is string after [env] or $
    xref_name <- base::trimws(gsub( "^(\\[env\\]|\\$)(.*)$", "\\2",  base::trimws(xvalue), ignore.case = TRUE, perl = TRUE ))    

    # - if named environmental variable does not exist
    if ( ! xref_name %in% base::names(Sys.getenv()) )
      return(unset)
    
    # - resolve value    
    xvalue <- base::Sys.getenv( xref_name, unset = unset )
  }
  

  # -- vault secret re-directs
  #    note: vault secret identified by value prefix [VAULT]
  #    note; ignore case
  
  if ( base::startsWith( base::trimws(base::toupper(xvalue)), "[VAULT]" ) ) {

    # - determine secret name
    #   note: 
    xref_name <- base::trimws( base::substring( base::trimws(xvalue), base::nahar( "[VAULT]") ) ) 

    # connect to a vault
    vaultsvc <- cxapp::cxapp_vault()

    xvalue <- vaultsvc$secret( xref_name, unset = unset )
  }


  # -- name return value
  base::names(xvalue) <- opt_std
  

  # -- return value
  return(base::trimws(xvalue))

})




cxapp_config$methods( "show" = function( x ) {
  "Display list of properties"
  
  lst_info <- character(0)

  
  # -- add search tree to list
  lst_info <- append( lst_info, c( "Search tree",
                                   paste( base::rep_len( "-", 60), collapse = "") ) )
  
  if ( ! "search.tree" %in% names(.self$.attr) || 
       ( length(.self$.attr[["search.tree"]]) == 0 ) )
    lst_info <- append( lst_info, "(None)" )
  
    
  if ( "search.tree" %in% names(.self$.attr) || 
       ( length(.self$.attr[["search.tree"]]) > 0 ) )
    lst_info <- append( lst_info, .self$.attr[["search.tree"]] )
  
  
  # -- add list of property files to list
  
  lst_info <- append( lst_info, c( base::rep_len(" ", 2), 
                                   "Property files",
                                   paste( base::rep_len( "-", 60), collapse = "") ) )
  
  if ( ! "property.files" %in% names(.self$.attr) || 
       ( length(.self$.attr[["property.files"]]) == 0 ) )
    lst_info <- append( lst_info, "(None)" )
  
  
  if ( "property.files" %in% names(.self$.attr) &&
       ( length(.self$.attr[["property.files"]]) > 0 ) )
    lst_info <- append( lst_info, 
                        base::unlist(lapply( .self$.attr[["property.files"]], function(x) { x[["path"]] } ), use.names = FALSE))
                        
  
  # -- add list of properties
  
  lst_info <- append( lst_info, c( base::rep_len(" ", 2), 
                                   "Defined properties",
                                   paste( base::rep_len( "-", 60), collapse = "") ) )
  
  
  if ( ! "properties" %in% names(.self$.attr) || 
       ( length(.self$.attr[["properties"]]) == 0 ) )
    lst_info <- append( lst_info, "(None)" )
  
  
  if ( "properties" %in% names(.self$.attr) &&
       ( length(.self$.attr[["properties"]]) > 0 ) )
    lst_info <- append( lst_info, base::sort(base::names(.self$.attr[["properties"]])) )
  
  
  
  # -- display list
  cat( c( base::rep_len(" ", 2),
          lst_info, 
          base::rep_len(" ", 2) ), 
       sep = "\n" )
  
})