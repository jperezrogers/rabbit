context("pipeline::orderModules")

source("../../R/pipeline-class.R")

# set up test data
pipeline <- Pipeline$new(label="pipeline")
pipeline$addModule(type="M1",label="newM1")
pipeline$addModule(type="M2",label="newM2")

# test that the function is silent when x is null
test_that("the function is silent when x is null",{
  expect_silent(pipeline$orderModules())
  expect_silent(pipeline$orderModules(x=NULL))
})

# test that an error is thrown if x is provided but is not a character
test_that("an error is thrown if x is provided but is not a character",{
  msg <- "parameter 'x' must be a character vector"
  expect_error(pipeline$orderModules(x=NA),msg)
  expect_error(pipeline$orderModules(x=123),msg)
  expect_error(pipeline$orderModules(x=as.factor("abc")),msg)
  expect_error(pipeline$orderModules(x=list()),msg)
  expect_error(pipeline$orderModules(x=data.frame()),msg)
})

# test that an error is thrown if invalid module labels are provided
test_that("an error is thrown if invalid module labels are provided",{
  msg <- "The following are not valid module labels: abc"
  expect_error(pipeline$orderModules(x="abc"),msg)
  msg <- "The following are not valid module labels: abc, def"
  expect_error(pipeline$orderModules(x=c("abc","def")),msg)
  msg <- "The following are not valid module labels: abc"
  expect_error(pipeline$orderModules(x=c("abc","newM1")),msg)
  msg <- "The following are not valid module labels: abc"
  expect_error(pipeline$orderModules(x=c("abc","newM1","newM2")),msg)
  msg <- "The following are not valid module labels: abc, def"
  expect_error(pipeline$orderModules(x=c("abc","newM1","newM2","def")),msg)
})

# test that an error is thrown if not all module labels in the pipeline are provided
test_that("an error is thrown if not all module labels in the pipeline are provided",{
  msg <- "parameter 'x' must include all module labels. The following are missing: newM2, classification"
  expect_error(pipeline$orderModules(x="newM1"),msg)
  msg <- "parameter 'x' must include all module labels. The following are missing: classification"
  expect_error(pipeline$orderModules(x=c("newM1","newM2")),msg)
})

# test that an error is thrown if the classification module is not the last module in x
test_that("an error is thrown if the classification module is not the last module in x",{
  msg <- "The last element of parameter 'x' must be 'classification'"
  expect_error(pipeline$orderModules(x=c("classification","newM1","newM2")),msg)
  expect_error(pipeline$orderModules(x=c("newM1","classification","newM2")),msg)
})

# test that order is updated if x is correctly specified
test_that("order is updated if x is correctly specified",{
  expect_equal(pipeline$getOrder(),c("newM1","newM2","classification"))
  pipeline$orderModules(x=c("newM2","newM1","classification"))
  expect_equal(pipeline$getOrder(),c("newM2","newM1","classification"))
  pipeline$orderModules(x=c("newM1","newM2","classification"))
  expect_equal(pipeline$getOrder(),c("newM1","newM2","classification"))
})
