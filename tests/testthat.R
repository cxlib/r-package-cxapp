# note: not loading library cxapp and testthat as package should be able to be
#       used without library() or require()
# library(cxapp)
# library(testthat)

testthat::test_check("cxapp")