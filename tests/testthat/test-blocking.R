context('Blocking')

test_that('Encoder methods are valid', {
  expect_error(block(dfA=NA, dfB=NA, cols.encoder=list('A'=c(1), 'B'=c(1)), encoder.block.method = NULL))
  expect_error(block(dfA=NA, dfB=NA, cols.encoder=list('A'=c(1), 'B'=c(1)), encoder.block.method = 'Not Valid'))
})

test_that('Lengths of variables in A and B are the same', {
  expect_error(block(dfA=NA, dfB=NA, cols.encoder=list('A'=c(1,2), 'B'=c(1))))
  expect_error(block(dfA=NA, dfB=NA, cols.encoder=list('A'=c(1), 'B'=c(1,2))))
})

test_that('If blocking on numeric, then a numeric range is provided', {
  expect_error(block(dfA=NA, dfB=NA, cols.numeric=list('A'=c(1), 'B'=c(1))))
})