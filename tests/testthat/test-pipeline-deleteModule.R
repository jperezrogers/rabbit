context("pipeline::deleteModule")

# source("../../R/pipeline-class.R")

# set up test data
pipeline <- Pipeline$new(label="pipeline")
pipeline$addModule(type="M1",label="newM1")
pipeline$addModule(type="M2",label="newM2")

# test that an error is thrown if label is null
test_that("an error is thrown if label is null",{
  msg <- "parameter 'label' cannot be NULL"
  expect_error(pipeline$deleteModule(),msg)
  expect_error(pipeline$deleteModule(label=NULL),msg)
  expect_equal(pipeline$getOrder(),c("newM1","newM2","classification"))
})

# test that an error is thrown if label is not of class character
test_that("an error is thrown if label is not of class character",{
  msg <- "parameter 'label' must be of class character"
  expect_error(pipeline$deleteModule(label=NA),msg)
  expect_error(pipeline$deleteModule(label=123),msg)
  expect_error(pipeline$deleteModule(label=as.factor("abc")),msg)
  expect_error(pipeline$deleteModule(label=list()),msg)
  expect_error(pipeline$deleteModule(label=data.frame()),msg)
  expect_equal(pipeline$getOrder(),c("newM1","newM2","classification"))
})

# test that an error is thrown if label is not of length 1
test_that("an error is thrown if label is not of length 1",{
  msg <- "parameter 'label' must be of length 1"
  expect_error(pipeline$deleteModule(label=c("newM1","newM2")),msg)
  expect_equal(pipeline$getOrder(),c("newM1","newM2","classification"))
})

# test that an error is thrown if label is not a valid module label
test_that("an error is thrown if label is not a valid module label",{
  msg <- "'' is not a valid module label"
  expect_error(pipeline$deleteModule(label=""),msg)
  msg <- "'abc' is not a valid module label"
  expect_error(pipeline$deleteModule(label="abc"),msg)
  expect_equal(pipeline$getOrder(),c("newM1","newM2","classification"))
})

# test that an error is thrown if the user tries to delete the classification module
test_that("an error is thrown if the user tries to delete the classification module",{
  msg <- "the 'classification' module cannot be deleted"
  expect_error(pipeline$deleteModule(label="classification"),msg)
})

# test that the named module is deleted if label is correctly specified
test_that("the named module is deleted if label is correctly specified",{
  pipeline$deleteModule(label="newM1")
  expect_equal(names(pipeline$modules),c("newM2","classification"))
  expect_equal(pipeline$getOrder(),c("newM2","classification"))
  pipeline$deleteModule(label="newM2")
  expect_equal(names(pipeline$modules),c("classification"))
  expect_equal(pipeline$getOrder(),c("classification"))
})
