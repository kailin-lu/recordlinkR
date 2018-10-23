library(testthat)
library(recordlinkR)

# test_check("recordlinkR")
# 
# test_that("lowerAndStrip lower cases names", {
#   expect_equal(lowerAndStrip('KAILIN', 'kailin', remove.punctuation = FALSE))
#   expect_equal(lowerAndStrip('abcEFG ', 'abcefg ', remove.punctuation = FALSE))
#   expect_equal(lowerAndStrip(' ABCefg ', ' abcefg ', remove.punctuation = FALSE))
# })
# 
# test_that("lowerAndStrip strips punctuation", {
#   expect_equal(lowerAndStrip('KAILIN', 'kailin', remove.punctuation = TRUE))
#   expect_equal(lowerAndStrip('abcEFG ', 'abcefg ', remove.punctuation = TRUE))
#   expect_equal(lowerAndStrip(' ABCefg ', ' abcefg ', remove.punctuation = TRUE))
# })