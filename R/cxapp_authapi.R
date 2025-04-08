#' Utility function to authenticate an  API authorization request
#'
#' @param x Authorization string
#'
#' @return `TRUE` if authorization is valid. `FALSE`, otherwise
#'
#' @description
#'
#' The function currently only supports bearer method of authorization. If the
#' authorization is not a bearer method, the result is surely `FALSE`.
#'
#' The function also only supports bearer tokens registered as a secret in a
#' supported key vault (see \link[cxapp]{cxapp_vault}).
#'
#' The space delimited list of named secrets is defined using the configuration option
#' `API.AUTH.SECRETS` in the app.properties file (see \link[cxapp]{cxapp_config}).
#' If no secrets are defined, the authorization result is surely `FALSE`.
#'
#' The function searches the defined list of secrets for a match to the encoded
#' bearer string, excluding the \emph{bearer} prefix. The search assumes the secret
#' is a JSON data structure with a named element `value` retaining the encoded
#' string.
#'
#' If a secret ends in an asterisk `*`, the secret name is assumed to be a prefix
#' and all secrets with the prefix is searched.
#'
#' The use of the hierarchical reference in secret names is supported. See the
#' respective vault configuration.
#'
#' If the encoded bearer string matches the secret value, the function returns
#' `TRUE`.
#'
#' The result attribute `scope`, if defined, denotes if the authorization applies
#' to a service or particular user.
#'
#' The result attribute `principal`, if defined, names the service or user
#' associated with the authorization.
#'
#'
#' @export

cxapp_authapi <- function( x ) {

  # -- nothing or invalid input specified
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || ! inherits(x, "character") || (base::nchar(base::trimws(x)) == 0) )
    return(FALSE)


  # - connect configuration object
  cfg <- cxapp::.cxappconfig()


  # -- bearer token
  if (  base::startsWith( base::toupper(base::trimws(x)), "BEARER" ) ) {

    # - extract and encode token
    #   note: from here, only encoded tokens are used
    token <- try( cxapp:::.cxapp_apitokenencode( gsub( "^BEARER\\s+(.*)", "\\1", base::trimws(x), ignore.case = TRUE, perl = TRUE ) ), silent = FALSE )

    #   note: encoded token has standard length 128 (SHA-512)
    if ( inherits(token, "try-error") || ( base::nchar(token) != 128 ) )
      return(invisible(FALSE))

    # - connect application cache
    appcache <- cxapp::.cxappcache()

    # - cache reference
    cache_ref <- paste0( ".cache/.token/..", token, ".." )

    # - token is cached ?

    if ( appcache$exists( cache_ref ) ) {

      # - token is cached
      token_info <- try( jsonlite::fromJSON( appcache$get( cache_ref ) ), silent = FALSE )

      if ( inherits( token_info, "try-error") )
        return(invisible(FALSE))


      #   resolve pass with attributes

      auth_result <- TRUE

      for ( xattr in c( "scope", "principal") )
        if ( xattr %in% names(token_info) )
          attr( auth_result, xattr) <- unname(token_info[[xattr]])

      return(invisible(auth_result))


      # end of if-statement block when token is cached
    } else {

      # - token is not cached ... look into vaults

      # - connect to vault
      vlt <- try( cxapp::cxapp_vault(), silent = TRUE )

      if ( inherits( vlt, "try-error") )
        return(invisible(FALSE))


      # - secrets in vault
      #   note: NA means no list of secrets to check against
      cfg_secrets_opt <- try( cfg$option( "app/api.auth.secrets", unset = NA ), silent = FALSE )

      if ( inherits( cfg_secrets_opt, "try-error") || is.na(cfg_secrets_opt) )
        return(invisible(FALSE))

      cfg_secrets <- unlist( strsplit( gsub( "\\s{2,}", " ", cfg_secrets_opt, perl = TRUE), " ", fixed = TRUE ), use.names = FALSE )


      # - retrieve list of secrets in vault
      #   note: do this here ... read once instead of every time in loop
      #   note: standardize secret reference to use / instead of -
      lst_secrets <- character(0)

      if ( any(base::endsWith( cfg_secrets, "*" )) )
        lst_secrets <- base::gsub( "-", "/", vlt$list() )

      # - identify secrets to look for
      lookup_secrets <- character(0)

      for ( xitem in cfg_secrets ) {

        # - wildcard match
        if ( base::endsWith( xitem, "*" ) ) {

          # note: standardize secret reference to use / instead of -
          secret_prefix <- base::gsub( "-", "/", base::substr( base::trimws(xitem), 1, base::nchar(base::trimws(xitem)) - 1 ) )

          lookup_secrets <- append( lookup_secrets,
                                    lst_secrets[ grepl( paste0( "^", secret_prefix), lst_secrets, ignore.case = FALSE, perl = TRUE ) ] )

          next()
        }  # end if-statement for wildcard match


        # - explicit secret name
        lookup_secrets <- append( lookup_secrets, xitem )

      } # end of for-statement to resolve secrets to search


      # - process secrets
      for ( xsecret in lookup_secrets ) {

        vlt_value <- try( vlt$secret( xsecret ), silent = TRUE )

        if ( inherits( vlt_value, "try-error" ) )
          next()


        secret_def <- try( jsonlite::fromJSON( vlt_value ), silent = TRUE )

        if ( inherits( secret_def, "try-error" ) )
          next()

        if ( "value" %in% names(secret_def) &&
             ( base::trimws(secret_def[["value"]]) == base::trimws(token) ) ) {

          # - cache secret

          #   save def as temporary file

          def_file <- cxapp::cxapp_standardpath( base::tempfile( pattern = "cache-entry-", tmpdir = base::tempdir(), fileext = "" ) )
          names(def_file) <- cache_ref

          entries <- names(secret_def)
          entries_to_save <- entries[ base::toupper(entries) != "VALUE" ]

          base::writeLines( jsonlite::toJSON( secret_def[ entries_to_save ] ),
                            con = def_file )

          #   add to cache
          appcache$add( def_file )

          if ( file.exists(def_file) && ! file.remove( def_file ) )
            stop( "Could not clear staged secret" )



          #   resolve pass with attributes

          auth_result <- TRUE

          for ( xattr in c( "scope", "principal") )
            if ( xattr %in% names(secret_def) )
              attr( auth_result, xattr) <- unname(secret_def[[xattr]])


          return(invisible(auth_result))
        }

      } # end for-statement for lookup secrets

    } # end if-statement when token is not cached

  }  # end if-statement for bearer token


  # -- default resolve is FALSE ... i.e. unauthorized
  return(invisible(FALSE))
}