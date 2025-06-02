#
#  Tests for cxapp::cxapp_appnode()
#  
#
#



testthat::test_that( "appnode.rsessionbased", {

  # -- stage
  
  # - move global in-memory cached appnode
  
  prev_node <- NA
  
  if ( exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
    prev_node <- get( ".cxapp.wrkcache.appnode", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appnode", envir = .GlobalEnv )
    
    if ( ! is.na(prev_node) )
      base::assign( ".cxapp.wrkcache.appnode", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appnode", envir = .GlobalEnv )  
  
  
  
  
  # - iterations
  test_iterations <- 20000
  

  # -- test

  result <- replicate( test_iterations, 
                       cxapp::cxapp_appnode(), 
                       simplify = TRUE )
  

  # -- expected
  
  expected_numcodes <- test_iterations
  
  
  # -- assertions
  
  # - number of returned values
  testthat::expect_length( result, expected_numcodes )
  
  
  # - number of unique values
  testthat::expect_length( base::unique(result), 1 )
  

})




testthat::test_that( "appnode.uniqueness", {
  
  # -- stage

  # - move global in-memory cached appnode
  
  prev_node <- NA
  
  if ( exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
    prev_node <- get( ".cxapp.wrkcache.appnode", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appnode", envir = .GlobalEnv )
    
    if ( ! is.na(prev_node) )
      base::assign( ".cxapp.wrkcache.appnode", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.appnode", envir = .GlobalEnv )  
  
  
  
  
  # - iterations
  test_iterations <- 20000
  
  
  # -- test
  
  result <- character(0)
  
  for ( xi in 1:test_iterations ) { 
  
    # - append appnode value
    result <- append( result, 
                      cxapp::cxapp_appnode() )
  
    # - remove cache to generate new value
    if ( exists( ".cxapp.wrkcache.appnode", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.appnode", envir = .GlobalEnv )  
    
  }
  
  
  # -- expected
  
  expected_numcodes <- test_iterations
  
  
  # -- assertions
  
  # - number of returned values
  testthat::expect_length( result, expected_numcodes )
  
  
  # - sufficient uniqueness
  testthat::expect_true( length(base::unique(result)) > base::floor( expected_numcodes / 2 ) )
  
  
})


  