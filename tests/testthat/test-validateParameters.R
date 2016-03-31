context("module::validateParameters")
source("../../R/module-validateParameters-method.R")

# test that nothing happens when parameters is NULL
test_that("nothing happens when parameters is NULL",{
  parameters <- NULL
  expect_silent(validateParameters(parameters))
})

# test that an error is thrown when parameters is not a data.frame
test_that("an error is thrown when parameters is not a data.frame",{
  msg <- "The parameters argument must be of class data.frame"
  parameters <- matrix()
  expect_error(validateParameters(parameters),msg)
})

# test that an error is thrown when parameters is an empty data.frame
test_that("an error is thrown when parameters is an empty data.frame",{
  msg <- "The parameters argument must only contain named columns 'parameter', 'class', and 'label'"
  parameters <- data.frame()
  expect_error(validateParameters(parameters),msg)
})

c("parameter","class","label")
# test that an error is thrown when parameters does not have exactly columns parameter, class and label
test_that("an error is thrown when parameters does not have exactly columns parameter, class and label",{
  msg <- "The parameters argument must only contain named columns 'parameter', 'class', and 'label'"
  parameters <- data.frame("col"=c("a","b"))
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c("a","b"))
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","b"))
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","b"),"not"=c("a","b"))
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","b"),"label"=c("a","b"),"col"=c("a","b"))
  expect_error(validateParameters(parameters),msg)
})

# test that an error is thrown when not all values in the parameter column are characters 
test_that("an error is thrown when not all values in the parameter column are unique",{
  msg <- "All 'parameter' values must be of class character. Try setting stringsAsFactors=F when defining your data.frame."
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","b"),"label"=c("a","b"))
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c(1,1),"class"=c("a","b"),"label"=c("a","b"))
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c(TRUE,FALSE),"class"=c("a","b"),"label"=c("a","b"))
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c(1,1),"class"=c("a","b"),"label"=c("a","b"),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
})

# test that an error is thrown when not all values in the parameter column are unique
test_that("an error is thrown when not all values in the parameter column are unique",{
  msg <- "All 'parameter' values must be unique"
  parameters <- data.frame("parameter"=c("a","a"),"class"=c("a","b"),"label"=c("a","b"),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
})

# test that an error is thrown when not all values in the label column are characters 
test_that("an error is thrown when not all values in the label column are characters",{
  msg <- "All 'label' values must be of class character"
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","b"),"label"=as.factor(c("a","b")),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","b"),"label"=c(1,1),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","b"),"label"=c(TRUE,FALSE),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
})

# test that an error is thrown when not all labels are unique
test_that("an error is thrown when not all labels are unique",{
  msg <- "All parameters must have a unique label"
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","b"),"label"=c("a","a"),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
})

# test that an error is thrown when not all values in the class column are characters 
test_that("an error is thrown when not all values in the class column are characters",{
  msg <- "All 'class' values must be of class character"
  parameters <- data.frame("parameter"=c("a","b"),"class"=as.factor(c("a","b")),"label"=c("a","b"),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c("a","b"),"class"=c(1,1),"label"=c("a","b"),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c("a","b"),"class"=c(TRUE,FALSE),"label"=c("a","b"),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
})

# test that an error is thrown when not all classes exist
test_that("an error is thrown when not all classes exist",{
  msg <- "a is not a valid class"
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","b"),"label"=c("a","b"),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("a","factor"),"label"=c("a","b"),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
  msg <- "b is not a valid class"
  parameters <- data.frame("parameter"=c("a","b"),"class"=c("factor","b"),"label"=c("a","b"),stringsAsFactors=F)
  expect_error(validateParameters(parameters),msg)
})
