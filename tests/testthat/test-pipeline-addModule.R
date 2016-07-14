context("pipeline::addModule")

source("../../R/pipeline-class.R")

# set up test data
pipeline <- Pipeline$new(label="pipeline")

# test that an error is thrown if type is NULL
test_that("an error is thrown if type is NULL",{
  msg <- "parameter 'type' cannot be NULL"
  expect_error(pipeline$addModule(),msg)
  expect_error(pipeline$addModule(type=NULL),msg)
})

# test that an error is thrown if type is not one of M1, M2, or M3
test_that("an error is thrown if type is not one of M1, M2, or M3",{
  msg <- "parameter 'type' must be one of 'M1', 'M2', or 'M3'"
  expect_error(pipeline$addModule(type=NA),msg)
  expect_error(pipeline$addModule(type=123),msg)
  expect_error(pipeline$addModule(type="abc"),msg)
  expect_error(pipeline$addModule(type=c("M1","M2")),msg)
  expect_error(pipeline$addModule(type=list()),msg)
})

# test that an error is thrown if label is NULL
test_that("an error is thrown if label is NULL",{
  msg <- "parameter 'label' cannot be NULL"
  expect_error(pipeline$addModule(type="M1"),msg)
  expect_error(pipeline$addModule(type="M1",label=NULL),msg)
})

# test that an error is thrown if label is not a character
test_that("an error is thrown if label is not a character",{
  msg <- "parameter 'label' must be of class character"
  expect_error(pipeline$addModule(type="M1",label=NA),msg)
  expect_error(pipeline$addModule(type="M1",label=123),msg)
  expect_error(pipeline$addModule(type="M1",label=list()),msg)
  expect_error(pipeline$addModule(type="M1",label=data.frame),msg)
})

# test that an error is thrown if label is not of length 1
test_that("an error is thrown if label is not of length 1",{
  msg <- "parameter 'label' must be of length 1"
  expect_error(pipeline$addModule(type="M1",label=c("M1","M2")),msg)
})

# test that a module of type M1 is added when type=M1
test_that("a module of type M1 is added when type=M1",{
  pipeline <- Pipeline$new(label="pipeline")
  pipeline$addModule(type="M1",label="newM1")
  expect_equal(names(pipeline$modules),c("newM1","classification"))
  expect_equal(pipeline$getOrder(),c("newM1","classification"))
})

# test that a module of type M2 is added when type=M2
test_that("a module of type M2 is added when type=M2",{
  pipeline <- Pipeline$new(label="pipeline")
  pipeline$addModule(type="M2",label="newM2")
  expect_equal(names(pipeline$modules),c("newM2","classification"))
  expect_equal(pipeline$getOrder(),c("newM2","classification"))
})

# test that a module of type M3 is added when type=M3
test_that("a module of type M3 is added when type=M3",{
  pipeline <- Pipeline$new(label="pipeline")
  pipeline$addModule(type="M3",label="newM3")
  expect_equal(names(pipeline$modules),c("newM3","classification"))
  expect_equal(pipeline$getOrder(),c("newM3","classification"))
})

# test that an error is thrown if the user tried to add another module with a repeated label
test_that("an error is thrown if the user tried to add another module with a repeated label",{
  pipeline <- Pipeline$new(label="pipeline")
  pipeline$addModule(type="M1",label="newM1")
  msg <- "A module with label newM1 already exists in this pipeline"
  expect_error(pipeline$addModule(type="M1",label="newM1"),msg)
})

# test that a new module is added to the order member in the correct slot when there are 2+ modules
test_that("a new module is added to the order member in the correct slot when there are 2+ modules",{
  pipeline <- Pipeline$new(label="pipeline")
  pipeline$addModule(type="M1",label="newM1")
  pipeline$addModule(type="M2",label="newM2")
  expect_equal(pipeline$getOrder(),c("newM1","newM2","classification"))
})
