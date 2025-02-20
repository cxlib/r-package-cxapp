#
#  tests for utility cxapp::.cxapp_standardpath()
#
#
#


testthat::test_that( "standardpath.noParms", {
  
  # -- test 
  result <- cxapp:::.cxapp_standardpath() 
  
  # -- assertions
  testthat::expect_length( result, 0 )
  testthat::expect_true( inherits( result, "character" ) )

})



testthat::test_that( "standardpath.paramNull", {
  
  # -- test 
  result <- cxapp:::.cxapp_standardpath( NULL ) 
  
  # -- assertions
  testthat::expect_length( result, 0 )
  testthat::expect_true( inherits( result, "character" ) )
  
})




testthat::test_that( "standardpath.paramNA", {
  
  # -- test 
  result <- cxapp:::.cxapp_standardpath( NA ) 

  # -- assertions
  testthat::expect_true( is.na(result) )

})



testthat::test_that( "standardpath.paramSinglePathFsepSlash", {

  # -- stage
  test_xpath <- "some/path/for/x"
    
  # -- test 
  result <- cxapp:::.cxapp_standardpath( test_xpath ) 

  # -- assertions
  testthat::expect_equal( result, test_xpath )
  
})



testthat::test_that( "standardpath.paramSinglePathFsepBackSlash", {
  
  # -- stage
  test_xpath <- "some\\path\\for\\x"
  
  # -- test 
  result <- cxapp:::.cxapp_standardpath( test_xpath ) 

  # -- expected
  expected_xpath <- gsub( "\\\\", "/", test_xpath )
  
  # -- assertions
  testthat::expect_equal( result, expected_xpath )
  
})


testthat::test_that( "standardpath.paramMultiplePaths", {
  
  # -- stage
  test_xpaths <- c( "some\\path\\for\\x", 
                    "c:\\some\\path\\for\\x",
                    "some/path/for/x",
                    "\\root\to/some/path/here" )
  
  # -- test 
  result <- cxapp:::.cxapp_standardpath( test_xpaths ) 
  
  # -- expected
  expected_xpaths <- gsub( "\\\\", "/", test_xpaths )
  
  # -- assertions
  testthat::expect_equal( result, expected_xpaths )
  
})


