context("module::validateLabel")
source("../../R/module-validateLabel-method.R")

# test that an error is thrown when label is NULL
test_that("an error is thrown when label is NULL",{
  msg <- "The label of a Task cannot be NULL"
  expect_error(validateLabel(),msg)
  expect_error(validateLabel(NULL),msg)
})

# test that an error is thrown when label is not of length 1
test_that("an error is thrown when label is not of length 1",{
  msg <- "The label of a Task must be a character vector of length 1"
  expect_error(validateLabel(c(1,2,3)),msg)
  expect_error(validateLabel(c("label1","label2")),msg)
  expect_error(validateLabel(list("a"=c("label1"),"b"=c("label2"))),msg)
})

# test that an error is thrown when label is not a character
test_that("an error is thrown when label is not a character",{
  msg <- "The label of a Task must be of class character"
  expect_error(validateLabel(NA),msg)
  expect_error(validateLabel(123),msg)
  expect_error(validateLabel(TRUE),msg)
  expect_error(validateLabel(list()),msg)
  expect_error(validateLabel(matrix()),msg)
  expect_error(validateLabel(data.frame()),msg)
  expect_error(validateLabel(as.factor("label")),msg)
})

# test that the function executes silently when label is a character
test_that("the function executes silently when label is a character",{
  expect_silent(validateLabel("label"))
})