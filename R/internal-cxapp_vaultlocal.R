#' Internal reference class representing a vault on the local file system 
#'
#' @method initialize initialize
#' @method list list
#' @method secret secret
#' @method show show
#'
#' @description
#' An internal utility class representing a simple local file system vault to
#' store secrets.
#' 
#' The vault is hierarchical and uses a path notation with forward slash as 
#' separator and leading slash representing the root of the vault.
#' 
#' The levels of the hierarchy and the secret name consists of the characters
#' a-z, 0-9 and punctuation dash (-), underscore '_' and period '.'.
#' 
#' The vault storage directory is defined by the cxapp property `VAULT.DATA`.
#' 
#' \strong{\emph{It is on the role of the developer and implementer to ensure 
#' that the secrets stored in the vault and their use are appropriately protected.}}  
#' 
#' 
#' The `list()` provides a means to list the secrets stored within the vault.
#' 
#' The `secret()` method retrieves a specified secret from the vault. If the
#' secret is not defined, the value of `unset` is returned. A secret is stored and
#' returned as a single text value.
#' 
#' @keywords internal


.cxapp_vaultlocal <- methods::setRefClass( "cxapp_vault_local", 
                                            fields = list( ".attr" = "character" ) )


.cxapp_vaultlocal$methods( "initialize" = function() {
  "Initialize vault"

  # -- initialize self
  #    note: initialize with temporary directory that does not exist
  .self$.attr <- c( "path" = cxapp:::.cxapp_standardpath( base::tempfile( pattern = "vault-", tmpdir = base::tempdir(), fileext = "") ), 
                    "cache" = cxapp:::.cxapp_standardpath( base::tempfile( pattern = "vault-cache-", tmpdir = base::tempdir(), fileext = "") ) )
  
  
  # -- configuration
  cfg <- cxapp::cxapp_config()
  
  if ( base::toupper( cfg$option( "vault", unset = "unknown" ) != "LOCAL" ) )
    stop( "Vault configuration is not local" )
  
  
  # -- vault store 
  vault_root <- cfg$option( "vault.data", unset = NA )
  
  if ( is.na(vault_root) || ! dir.exists(vault_root) )
    stop( "The vault root directory is not defined or does not exist" )
    
  
  .self$.attr["path"] <- vault_root
})



.cxapp_vaultlocal$methods( "list" = function() {
  "List secrets"
  
  # -- hierarchical vault
  lst_files <- base::list.files( .self$.attr["path"], recursive = TRUE, include.dirs = FALSE, full.names = FALSE, all.files = TRUE )
  
  if ( length(lst_files) == 0 )
    return(character(0))
  
  # -- ensure returned paths are absolute references
  lst <- paste0( "/", lst_files )
  
  return( base::sort( lst ) )
})
  


.cxapp_vaultlocal$methods( "secret" = function( x, unset = NA ) {
  "Get secret"
  
  # -- obviously unset
  if ( missing(x) || is.null(x) || any(is.na(x)) ) 
    return(invisible(unset))
  
  
  if ( length(x) != 1 )
    stop( "Only obtaining one secret at a time is supported" )
  
  
  if ( ! grepl( "^(/[a-z0-9\\-\\._]+)+$", x, perl = TRUE, ignore.case = TRUE ) )
    stop( "Secret name is invalid" )
  

  # -- secret stored  
  secrets_file <- base::file.path( .self$.attr["path"], x, fsep = "/" )

  # -- secret not defined
  if ( ! file.exists( secrets_file ) )
    return(invisible(unset))
  

  return(invisible( utils::head( base::readLines( secrets_file ), n = 1 ) ))
})


.cxapp_vaultlocal$methods( "show" = function() {
  "Default display"
  
  cat( "Local secret vault", sep = "\n")
})

