context('Data Preprocessing')
library(recordlinkR)


test_that('strings are lower cased', {
  test.lower <- data.frame(first = c('MARY', 'AMY', 'pETER', 'Max'))
  test.lower.1 <- data.frame(first=c('maX', 'Abc', 'DEX'))
  
  expect_equal(cleanNames(test.lower)$first, c('mary', 'amy', 'peter', 'max'))
  expect_equal(cleanNames(test.lower.1)$first, c('max', 'abc', 'dex'))
})