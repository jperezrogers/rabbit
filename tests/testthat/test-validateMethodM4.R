context("m4::validateMethodM4")
source("../../R/m4-validateMethod-method.R")

# test that an error is thrown if method is NULL
test_that("an error is thrown if method is NULL",{
  msg <- "method must be defined"
  expect_error(validateMethodM4(),msg)
  expect_error(validateMethodM4(method=NULL),msg)
})

# test_that an error is thrown if method is not a function
test_that("an error is thrown if method is not a function",{
  msg <- "method must be a function"
  expect_error(validateMethodM4(method=NA),msg)
  expect_error(validateMethodM4(method="abc"),msg)
  expect_error(validateMethodM4(method=123),msg)
  expect_error(validateMethodM4(method=as.factor("abc")),msg)
  expect_error(validateMethodM4(method=list()),msg)
  expect_error(validateMethodM4(method=data.frame()),msg)
})

# test that an error is thrown if method does not have input parameters x and y
test_that("an error is thrown if method does not have input parameters x and y",{
  msg <- "method must have input parameters 'x' and 'y'"
  method <- function(){invisible()}
  expect_error(validateMethodM4(method=method),msg)
  method <- function(x){invisible()}
  expect_error(validateMethodM4(method=method),msg)
  method <- function(x,data){invisible()}
  expect_error(validateMethodM4(method=method),msg)
  method <- function(x,data,z){invisible()}
  expect_error(validateMethodM4(method=method),msg)
  method <- function(y){invisible()}
  expect_error(validateMethodM4(method=method),msg)
  method <- function(y,data){invisible()}
  expect_error(validateMethodM4(method=method),msg)
  method <- function(y,data,z){invisible()}
  expect_error(validateMethodM4(method=method),msg)
})

# test that a warning is issued if x is provided with a default value
test_that("a warning is issued if x is provided with a default value",{
  msg <- "'x' should not have a default value. The default provided will be disregarded when invoking the method"
  method <- function(x="abc",y){invisible()}
  expect_warning(validateMethodM4(method=method),msg)
  method <- function(x="abc",y=123){invisible()}
  expect_warning(validateMethodM4(method=method),msg)
  method <- function(x="abc",y=123,data){invisible()}
  expect_warning(validateMethodM4(method=method),msg)
  method <- function(x="abc",y=123,data="efg"){invisible()}
  expect_warning(validateMethodM4(method=method),msg)
})

# test that a warning is issued if y is provided with a default value
test_that("a warning is issued if y is provided with a default value",{
  msg <- "'y' should not have a default value. The default provided will be disregarded when invoking the method"
  method <- function(x,y=123){invisible()}
  expect_warning(validateMethodM4(method=method),msg)
  method <- function(x,y=123,data){invisible()}
  expect_warning(validateMethodM4(method=method),msg)
  method <- function(x,y=123,data="efg"){invisible()}
  expect_warning(validateMethodM4(method=method),msg)
})

# test that a warning is issued if data is provided with a default value
test_that("a warning is issued if data is provided with a default value",{
  msg <- "'data' should not have a default value. The default provided will be disregarded when invoking the method"
  method <- function(x,y,data="efg"){invisible()}
  expect_warning(validateMethodM4(method=method),msg)
  method <- function(x,y,data="efg",z=1){invisible()}
  expect_warning(validateMethodM4(method=method),msg)
})

# test that an error is thrown if additional parameters do not have default values
test_that("an error is thrown if additional parameters do not have default values",{
  msg <- "All parameters other than 'x' and 'y', and 'data' in 'method' must have default values"
  method <- function(x,y,z){invisible()}
  expect_error(validateMethodM4(method=method),msg)
  method <- function(x,y,z=NULL){invisible()}
  expect_error(validateMethodM4(method=method),msg)
  method <- function(x,y,data,z){invisible()}
  expect_error(validateMethodM4(method=method),msg)
  method <- function(x,y,data,z=NULL){invisible()}
  expect_error(validateMethodM4(method=method),msg)
})

# test that the function executes silently if the input is correctly specified
test_that("the function executes silently if the input is correctly specified",{
  method <- function(x,y){invisible()}
  expect_silent(validateMethodM4(method=method))
  method <- function(x,y,data){invisible()}
  expect_silent(validateMethodM4(method=method))
  method <- function(x,y,data=NULL){invisible()}
  expect_silent(validateMethodM4(method=method))
  method <- function(x,y,data,z="abc"){invisible()}
  expect_silent(validateMethodM4(method=method))
})

