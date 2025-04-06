#' Internal utility to encode an API token
#'
#' @param x Token as a string
#'
#' @return Character vector of length 1 containing the encoded token
#'
#' @description
#' The utility function is a one-way encoding of a token string. The encoding
#' does not use a salt but rellies on a hashing strategy with information loss.
#'
#'
#' @keywords internal

.cxapp_apitokenencode <- function( x ) {


  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || ! inherits(x, "character") )
    stop( "The input token is missing or invalid" )


  # -- initial hash of token
  entry_hash <- digest::digest( x, algo = "sha512", file = FALSE )

  # -- split hash into 8-character blocks
  #    note: SHA-512 as a character string is 128 characters
  #    note: 128 characters contains 16 x 8-character blocks

  blocks <- character(0)

  for ( xi in 0:15 )
    blocks <- append( blocks, base::substr( entry_hash, 8*xi + 1, 8*(xi+1) ) )


  # -- sequence
  block_str <- paste( blocks[ c( 8, 13, 1, 7, 16, 10, 3, 15, 14, 4, 5, 2 ) ], collapse = "" )


  # -- hash loop

  hash_value <- block_str

  for ( xalgo in c( "sha256", "md5", "sha512" ) )
    hash_value <- digest::digest( hash_value, algo = xalgo, file = FALSE )

  if ( hash_value == block_str )
    stop( "Hash loop failed" )


  return(invisible(hash_value))

}
