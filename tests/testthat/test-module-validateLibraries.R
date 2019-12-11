context("module::validateLibraries")
# source("../../R/module-validateLibraries-method.R")

# test that the function executes silently if libraries is null
test_that("the function executes silently if libraries is null",{
  expect_silent(validateLibraries())
  expect_silent(validateLibraries(NULL))
})

# test that the function executes silently if libraries is appropriately called
test_that("the function executes silently if libraries is appropriately called",{
  expect_silent(validateLibraries("base"))
  expect_silent(validateLibraries(c("base","base")))
})

# test that an error is thrown when libraries is not a character
test_that("an error is thrown when libraries is not a character",{
  msg <- "libraries must be a character vector"
  expect_error(validateLibraries(NA),msg)
  expect_error(validateLibraries(123),msg)
  expect_error(validateLibraries(as.factor("base")),msg)
})

# test that an error is thrown when libraries are not installed
test_that("an error is thrown when libraries are not installed",{
  msg <- "The following libraries are not installed. Please install them: myLibrary"
  expect_error(validateLibraries("myLibrary"),msg)
  expect_error(validateLibraries(c("myLibrary","base")),msg)
  expect_error(validateLibraries(c("base","myLibrary")),msg)
  msg <- "The following libraries are not installed. Please install them: myLibrary, myOtherLibrary"
  expect_error(validateLibraries(c("myLibrary","myOtherLibrary")),msg)
})