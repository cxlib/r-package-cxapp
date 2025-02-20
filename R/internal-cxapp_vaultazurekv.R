#' (Experimental) Internal reference class representing a connection to Azure Key Vault 
#'
#' @method initialize initialize
#' @method secret secret
#' @method show show
#'
#' @description
#' An internal utility class representing a connection to an Azure Key Vault
#' 
#' The Azure Key Vault is not hierarchical but can use a path notation with 
#' forward slash as separator to represent a crude hierarchy. The forward 
#' slashes are translated to underscores mimicing a hierarchical reference 
#' structure.
#' 
#' The levels of the hierarchy and the secret name consists of the characters
#' a-z, 0-9 and punctuation dash (-), underscore '_' and period '.'.
#' 
#' The Azure Key Vault relies on multiple cxapp properties.
#' 
#' The Azure Key Vault service is enabled by setting the cxapp property `VAULT` to
#' the value `AZUREKV`.
#' 
#' The cxapp properrt `AZUREKV.URL` defines the Azure Key Vault connection URL. 
#' 
#' The following properties are used to connect and retrieve a temporary access 
#' token. All are required.
#' 
#' `AZUREKV.OAUTH.URL` is the URL for Microsoft OAuth service.
#' 
#' `AZUREKV.OAUTH.CLIENTID` is the OAuth client ID.
#' 
#' `AZUREKV.OAUTH.CLIENTSECRET` is the client secret.
#' 
#' `AZUREKV.OAUTH.SCOPE` is the authentication scope. 
#' 
#' 
#' \strong{\emph{It is on the role of the developer and implementer to ensure 
#' that the secrets stored in the vault and their use are appropriately protected.}}  
#' 
#' The `secret()` method retrieves a specified secret from the vault. If the
#' secret is not defined or a connection failed, the value of `unset` is returned. 
#' A secret is stored and returned as a single text value.
#' 
#'
#' 
#' @keywords internal

.cxapp_vaultazurekv <- methods::setRefClass( ".cxapp_vaultazurekv", 
                                             fields = list( ".attr" = "character" ) )




.cxapp_vaultazurekv$methods( "initialize" = function() {
  "Initialize vault"
  
  
  # -- initialize self
  #    note: initialize with dummy tokens
  .self$.attr <- c( "url" = NA, 
                    "api-version" = "2016-10-01",
                    "token" = paste( base::replicate( 5, 
                                                      paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 40 ), collapse = ""), 
                                                      simplify = TRUE ), 
                                     collapse = "" )
  )
  
  
  # -- configuration
  cfg <- cxapp::cxapp_config()
  
  if ( base::toupper( cfg$option( "vault", unset = "unknown" ) != "AZUREKV" ) )
    stop( "Vault configuration is not Azure Key Vault" )
  
  
  .self$.attr["url"] <- cfg$option( "AZUREKV.URL", unset = NA )
  
  if ( is.na( .self$.attr["url"] ) )
    stop( "Azure Key Vault URL not configured" )
  
  
  
  
  oauth_config <- character(0)
  
  for ( cfg_property in c( "AZUREKV.OAUTH.URL", "AZUREKV.OAUTH.CLIENTID", "AZUREKV.OAUTH.CLIENTSECRET", "AZUREKV.OAUTH.SCOPE" ) ) {
    
    if ( is.na(cfg$option( cfg_property, unset = NA )) )
      stop( "Required Azure Key Vault property ", cfg_property, " not defined" )

    oauth_config[ cfg_property ] <- cfg$option( cfg_property, unset = NA )
    
  }

  
  # -- get temporary access token 
  
  rslt <- httr2::request( oauth_config[ "AZUREKV.OAUTH.URL" ] ) |>
    httr2::req_method( "POST") |>
    httr2::req_body_form( "grant_type" = "client_credentials", 
                          "client_id" = oauth_config[ "AZUREKV.OAUTH.CLIENTID" ],
                          "client_secret" = oauth_config[ "AZUREKV.OAUTH.CLIENTSECRET" ], 
                          "scope" = oauth_config[ "AZUREKV.OAUTH.SCOPE" ]   ) |>
    httr2::req_perform()
  
  
  if ( rslt$status_code != 200 )
    stop( "Azure Key Vault authentication failed" )
  
  
  lst <- httr2::resp_body_json(rslt)
  
  if ( ! "access_token" %in% names(lst) )
    stop( "Azure Key Vault access token not available" )
  
  
  # -- register access token
  .self$.attr["token"] <- lst[["access_token"]]
  
})




.cxapp_vaultazurekv$methods( "secret" = function( x, unset = NA ) {
  "Get secret"
  
  
  # -- convert to flat hierarchy 
  xsecret <- base::gsub( "/", "_", x )
  
  # -- remove leading underscore
  if ( base::startsWith( xsecret, "_" ) )
    xsecret <- base::substring( xsecret, 2 )
  
  
  secret_url <- paste0( .self$.attr["url"], "/secrets/", xsecret, "?api-version=", .self$.attr["api-version"] )
  
  rslt_secret <- httr2::request( secret_url ) |>
    httr2::req_method("GET") |>
    httr2::req_auth_bearer_token( .self$.attr["token"] ) |>
    httr2::req_perform()
  
  
  if ( rslt_secret$status_code != 200 )
    return(invisible(unset))
  
  
  vault_secret <- httr2::resp_body_json( rslt_secret )
  
  
  if ( ! "value" %in% names(vault_secret) )
    return(invisible(unset))
  
  
  return(invisible(vault_secret$value))
  
})



.cxapp_vaultazurekv$methods( "show" = function() {
  "Default display"
  
  cat( paste( "Azure Key Vault", .self$.attr["url"] ), sep = "\n")
})
