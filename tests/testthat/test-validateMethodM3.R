context("m3::validateMethodM3")
source("../../R/M3-validateMethod-method.R")

# test that an error is thrown if method is NULL
test_that("an error is thrown if method is NULL",{
  msg <- "method must be defined"
  expect_error(validateMethodM3(),msg)
  expect_error(validateMethodM3(method=NULL),msg)
})

# test_that an error is thrown if method is not a function
test_that("an error is thrown if method is not a function",{
  msg <- "method must be a function"
  expect_error(validateMethodM3(method=NA),msg)
  expect_error(validateMethodM3(method="abc"),msg)
  expect_error(validateMethodM3(method=123),msg)
  expect_error(validateMethodM3(method=as.factor("abc")),msg)
  expect_error(validateMethodM3(method=list()),msg)
  expect_error(validateMethodM3(method=data.frame()),msg)
})

# test that an error is thrown if method does not have input parameters x and rank
test_that("an error is thrown if method does not have input parameters x and rank",{
  msg <- "method must have input parameters 'x' and 'rank'"
  method <- function(){invisible()}
  expect_error(validateMethodM3(method=method),msg)
  method <- function(x){invisible()}
  expect_error(validateMethodM3(method=method),msg)
  method <- function(x,z){invisible()}
  expect_error(validateMethodM3(method=method),msg)
  method <- function(rank){invisible()}
  expect_error(validateMethodM3(method=method),msg)
  method <- function(rank,z){invisible()}
  expect_error(validateMethodM3(method=method),msg)
})

# test that a warning is issued if x is provided with a default value
test_that("a warning is issued if x is provided with a default value",{
  msg <- "'x' should not have a default value. The default provided will be disregarded when invoking the method"
  method <- function(x="abc",rank){invisible()}
  expect_warning(validateMethodM3(method=method),msg)
  method <- function(x="abc",rank=123){invisible()}
  expect_warning(validateMethodM3(method=method),msg)
  method <- function(x="abc",rank=123,z="efg"){invisible()}
  expect_warning(validateMethodM3(method=method),msg)
})

# test that a warning is issued if rank is provided with a default value
test_that("a warning is issued if rank is provided with a default value",{
  msg <- "'rank' should not have a default value. The default provided will be disregarded when invoking the method"
  method <- function(x,rank=123){invisible()}
  expect_warning(validateMethodM3(method=method),msg)
  method <- function(x,rank=123,z="efg"){invisible()}
  expect_warning(validateMethodM3(method=method),msg)
})

# test that an error is thrown if additional parameters do not have default values
test_that("an error is thrown if additional parameters do not have default values",{
  msg <- "All parameters other than 'x' and 'rank' in 'method' must have default values"
  method <- function(x,rank,z){invisible()}
  expect_error(validateMethodM3(method=method),msg)
  method <- function(x,rank,z=NULL){invisible()}
  expect_error(validateMethodM3(method=method),msg)
})

# test that the function executes silently if the input is correctly specified
test_that("the function executes silently if the input is correctly specified",{
  method <- function(x,rank){invisible()}
  expect_silent(validateMethodM3(method=method))
  method <- function(x,rank,z="abc"){invisible()}
  expect_silent(validateMethodM3(method=method))
})

