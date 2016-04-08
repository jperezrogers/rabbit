context("module::validateControl")
source("../../R/module-validateControl-method.R")
source("../../R/module-validateParameters-method.R")

# set up test data
parameters <- data.frame(
 parameter = c("x","y"),
 class = c("logical","numeric"),
 label = c("myX","myY"),
 stringsAsFactors=FALSE
 )

# test that the function is silent when parameters and control are both NULL
test_that("the function is silent when parameters and control are both NULL",{
  expect_silent(validateControl())
  expect_silent(validateControl(parameters=NULL,control=NULL))
  expect_silent(validateControl(parameters=NA,control=NULL))
  expect_silent(validateControl(parameters=data.frame(),control=NULL))
})

# test that an error is thrown when parameters is null and control is not null
test_that("an error is thrown when parameters is null and control is not null",{
  msg <- "parameters cannot be NULL if control is defined"
  expect_error(validateControl(control=NA),msg)
  expect_error(validateControl(control=list()),msg)
})

# test that an error is thrown when parameters is not valid
test_that("an error is thrown when parameters is not valid",{
  msg <- "The parameters argument must be of class data.frame"
  expect_error(validateControl(parameters=NA,control=list()),msg)
})

# test that an error is thrown when control is not a list
test_that("an error is thrown when control is not a list",{
  msg <- "control must be a list"
  expect_error(validateControl(parameters=parameters,control=NA),msg)
  expect_error(validateControl(parameters=parameters,control=data.frame()),msg)
})

# test that an error is thrown when control is not a list of length greater than zero
test_that("an error is thrown when control is not a list of length greater than zero",{
  msg <- "control must be a list of length > 0"
  expect_error(validateControl(parameters=parameters,control=list()),msg)
})

# test that an error is thrown when control is not a named list
test_that("an error is thrown when control is not a named list",{
  msg <- "control must be a named list"
  expect_error(validateControl(parameters=parameters,control=list(1)),msg)
  expect_error(validateControl(parameters=parameters,control=list(1,2)),msg)
  expect_error(validateControl(parameters=parameters,control=list("a","b")),msg)
})

# test that an error is thrown when not all names in control are unique
test_that("an error is thrown when not all names in control are unique",{
  msg <- "All named elements in control must be unique"
  expect_error(validateControl(parameters=parameters,control=list(x=1,x=2)),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=1,x=2,y=3)),msg)
})

# test that the names in control match the values in the parameter slot of parameters
test_that("the names in control match the values in the parameter slot of parameters",{
  msg <- "Not all named elements in control are in the parameter slot of parameters"
  expect_error(validateControl(parameters=parameters,control=list(x=1,2)),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=1,y=2,z=3)),msg)
})

# test that all named elements in control are not null
test_that("all named elements in control are not null",{
  msg <- "Named elements in control cannot be NULL"
  expect_error(validateControl(parameters=parameters,control=list(a=c())))
  expect_error(validateControl(parameters=parameters,control=list(a=c(),b=c("a"))))
})

# test that an error is thrown if not values in control do not match the class provided in parameters
test_that("an error is thrown if not values in control do not match the class provided in parameters",{
  msg <- "Not all of the values provided for parameter 'x' in control are of class 'logical'"
  expect_error(validateControl(parameters=parameters,control=list(x=1)),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=1,y=1)),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=1,y="a")),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=c(1,TRUE))),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=c(TRUE,1))),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=c(1,TRUE),y=1)),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=c(1,TRUE),y="a")),msg)
  msg <- "Not all of the values provided for parameter 'y' in control are of class 'numeric'"
  expect_error(validateControl(parameters=parameters,control=list(x=TRUE,y="a")),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=TRUE,y=TRUE)),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=TRUE,y=c("a",1))),msg)
  expect_error(validateControl(parameters=parameters,control=list(x=TRUE,y=c(1,"a"))),msg)
})
