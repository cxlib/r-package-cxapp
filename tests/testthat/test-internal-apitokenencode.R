#
#  Tests for internal function cxapp:::.cxapp_apitokenencode()
#
#


testthat::test_that( "apitokenencode.collisions", {

  # -- stage

  # - iterations
  test_iterations <- 100000

  # - test tokens
  test_tokens <- utils::head( unique( replicate( 2*test_iterations,
                                                 paste( base::sample( c( base::letters, base::letters, as.character(0:9) ), 128, replace = TRUE), collapse = ""),
                                                 simplify = TRUE) ), n = test_iterations )

  if ( length(test_tokens) != test_iterations )
    testthat::fail( "Could not stage expected number of tokens" )


  # -- test

  results <- sapply( test_tokens, function(z) {
    cxapp:::.cxapp_apitokenencode( z )
  }, USE.NAMES = TRUE )


  # -- expected

  expected_token_length <- 128

  expected_numtokens <- test_iterations


  # -- assertions

  # - expecting encoded token length is 128 characters
  testthat::expect_equal( unique(nchar(results)), expected_token_length )


  # - ensure results contains one unique encoded value for each token
  testthat::expect_length( unique(results), expected_numtokens )


  # - ensure encoded does not result in corresponding clear text token value
  testthat::expect_false( any( unname(results) == names(results)) )


})
