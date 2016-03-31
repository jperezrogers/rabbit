context("module::validateDatatype")
source("../../R/module-validateDatatype-method.R")

# test that an error is thrown if datatype is NULL
test_that("an error is thrown if datatype is NULL",{
  msg <- "The datatype of a Task cannot be NULL"
  expect_error(validateDatatype(),msg)
  expect_error(validateDatatype(NULL),msg)
})

# test that an error is thrown if datatype is not a character
test_that("an error is thrown if datatype is not a character",{
  msg <- "The datatype of a Task must be a character vector"
  expect_error(validateDatatype(NA),msg)
  expect_error(validateDatatype(123),msg)
  expect_error(validateDatatype(as.factor("microarray")),msg)
})

# test that an error is thrown if datatype is not one or more of microarray, count, or rpkm
test_that("an error is thrown if datatype is not one or more of microarray, count, or rpkm",{
  msg <- "The datatype of a Task must be one or more of 'microarray', 'count', 'rpkm'"
  expect_error(validateDatatype("not"),msg)
  expect_error(validateDatatype(c("microarray","not")),msg)
  expect_error(validateDatatype(c("microarray","count","rpkm","not")),msg)
})