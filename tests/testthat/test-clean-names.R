context('Data Preprocessing')
library(recordlinkR)


test.lower <- data.frame(first = c('MARY', 'AMY', 'pETER', 'Max'), stringsAsFactors=F)

test_that('Strings are lower cased', {
  expect_equal(cleanNames(test.lower), data.frame(first=c('mary', 'amy', 'peter', 'max'), stringsAsFactors=F))
})


test.punc <- data.frame(first = c('ma_-ry', 'a@#my', 'peter!@#', 'm,ax'))
test.punc.1 <- data.frame(first=c('**max', 'abc!&%', 'dex'))

test_that('Punctuation is removed', {
  expect_equal(cleanNames(test.punc), data.frame(first=c('mary', 'amy', 'peter', 'max'), stringsAsFactors=F))
  expect_equal(cleanNames(test.punc.1), data.frame(first=c('max', 'abc', 'dex'), stringsAsFactors=F))
})
