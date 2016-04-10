context("m1::validateMethodM1")
source("../../R/m1-validateMethod-method.R")

# test that an error is thrown if method is NULL
test_that("an error is thrown if method is NULL",{
  msg <- "method must be defined"
  expect_error(validateMethodM1(),msg)
  expect_error(validateMethodM1(method=NULL),msg)
})

# test_that an error is thrown if method is not a function
test_that("an error is thrown if method is not a function",{
  msg <- "method must be a function"
  expect_error(validateMethodM1(method=NA),msg)
  expect_error(validateMethodM1(method="abc"),msg)
  expect_error(validateMethodM1(method=123),msg)
  expect_error(validateMethodM1(method=as.factor("abc")),msg)
  expect_error(validateMethodM1(method=list()),msg)
  expect_error(validateMethodM1(method=data.frame()),msg)
})

# test that an error is thrown if method does not have input parameter x
test_that("an error is thrown if method does not have input parameter x",{
  msg <- "method must have input parameter 'x'"
  method <- function(){invisible()}
  expect_error(validateMethodM1(method=method),msg)
  method <- function(y){invisible()}
  expect_error(validateMethodM1(method=method),msg)
  method <- function(y,z){invisible()}
  expect_error(validateMethodM1(method=method),msg)
})

# test that a warning is issued if x is provided with a default value
test_that("a warning is issued if x is provided with a default value",{
  msg <- "'x' should not have a default value. The default provided will be disregarded when invoking the method"
  method <- function(x="abc"){invisible()}
  expect_warning(validateMethodM1(method=method),msg)
  method <- function(x="abc",y=123){invisible()}
  expect_warning(validateMethodM1(method=method),msg)
})

# test that an error is thrown if additional parameters do not have default values
test_that("an error is thrown if additional parameters do not have default values",{
  msg <- "All parameters other than 'x' in 'method' must have default values"
  method <- function(x,y){invisible()}
  expect_error(validateMethodM1(method=method),msg)
  method <- function(x,y=NULL){invisible()}
  expect_error(validateMethodM1(method=method),msg)
  method <- function(x,y="abc",z){invisible()}
  expect_error(validateMethodM1(method=method),msg)
})

# test that the function executes silently if the input is correctly specified
test_that("the function executes silently if the input is correctly specified",{
  method <- function(x){invisible()}
  expect_silent(validateMethodM1(method=method))
  method <- function(x,y="abc"){invisible()}
  expect_silent(validateMethodM1(method=method))
})

