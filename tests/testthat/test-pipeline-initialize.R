context("pipeline::initialize")

source("../../R/pipeline-class.R")

# set up test data

# test that an error is thrown if parameter 'label' is null
test_that("an error is thrown if parameter 'label' is null",{
  msg <- "parameter 'label' must not be NULL"
  expect_error(Pipeline$new(),msg)
  expect_error(Pipeline$new(label=NULL),msg)
  expect_error(Pipeline$new(cv="cv",nfolds=10,p=0.80),msg)
})

# test that an error is thrown if parameter 'label' is not a character
test_that("an error is thrown if parameter 'label' is not a character",{
  msg <- "parameter 'label' must be of class character"
  expect_error(Pipeline$new(label=NA),msg)
  expect_error(Pipeline$new(label=123),msg)
  expect_error(Pipeline$new(label=as.factor("mylabel")),msg)
  expect_error(Pipeline$new(label=list()),msg)
})

# test that parameter 'label' is correctly assigned if it is correctly specified
test_that("parameter 'label' is correctly assigned if it is correctly specified",{
  expect_silent(Pipeline$new(label="mynewlabel"))
  pipeline <- Pipeline$new(label="mynewlabel")
  expect_equal(pipeline$label,"mynewlabel")
  pipeline <- Pipeline$new(label="my_new_label")
  expect_equal(pipeline$label,"my_new_label")
  pipeline <- Pipeline$new(label="my\tnew\tlabel")
  expect_equal(pipeline$label,"my\tnew\tlabel")
})

# test that an error is thrown if parameter 'cv' is not correctly specified
test_that("an error is thrown if parameter 'cv' is not correctly specified",{
  msg <- "parameter 'cv' must be one of 'cv', 'loocv', 'lgocv', or 'boot'"
  expect_error(Pipeline$new(label="pipeline",cv=NULL),msg)
  expect_error(Pipeline$new(label="pipeline",cv=NA),msg)
  expect_error(Pipeline$new(label="pipeline",cv="tenfold"),msg)
  expect_error(Pipeline$new(label="pipeline",cv=c("cv","loocv")),msg)
  expect_error(Pipeline$new(label="pipeline",cv=list()),msg)
  expect_error(Pipeline$new(label="pipeline",cv=data.frame()),msg)
})

# test that parameter 'cv' is correctly assigned if it is correctly specified
test_that("parameter 'cv' is correctly assigned if it is correctly specified",{
  pipeline <- Pipeline$new(label="pipeline")
  expect_equal(pipeline$cv,"cv")
  pipeline <- Pipeline$new(label="pipeline",cv="cv")
  expect_equal(pipeline$cv,"cv")
  pipeline <- Pipeline$new(label="pipeline",cv="loocv")
  expect_equal(pipeline$cv,"loocv")
  pipeline <- Pipeline$new(label="pipeline",cv="lgocv")
  expect_equal(pipeline$cv,"lgocv")
  pipeline <- Pipeline$new(label="pipeline",cv="boot")
  expect_equal(pipeline$cv,"boot")
})

# test that an error is thrown if parameter 'nfolds' is not numeric
test_that("an error is thrown if parameter 'nfolds' is not numeric",{
  msg <- "parameter 'nfolds' must be of class numeric"
  expect_error(Pipeline$new(label="pipeline",nfolds=NULL),msg)
  expect_error(Pipeline$new(label="pipeline",nfolds=NA),msg)
  expect_error(Pipeline$new(label="pipeline",nfolds="10"),msg)
  expect_error(Pipeline$new(label="pipeline",nfolds=as.factor(10)),msg)
})

# test that an error is thrown if parameter 'nfolds' is not an integer
test_that("an error is thrown if parameter 'nfolds' is not an integer",{
  msg <- "parameter 'nfolds' must be an integer >1"
  expect_error(Pipeline$new(label="pipeline",nfolds=1),msg)
  expect_error(Pipeline$new(label="pipeline",nfolds=1.2),msg)
  expect_error(Pipeline$new(label="pipeline",nfolds=0),msg)
  expect_error(Pipeline$new(label="pipeline",nfolds=-10),msg)
})

# test that parameter 'nfolds' is correctly assigned if it is correctly specified
test_that("parameter 'nfolds' is correctly assigned if it is correctly specified",{
  pipeline <- Pipeline$new(label="pipeline")
  expect_equal(pipeline$nfolds,10)
  pipeline <- Pipeline$new(label="pipeline",nfolds=10)
  expect_equal(pipeline$nfolds,10)
})

# test that an error is thrown if parameter 'p' is not numeric
test_that("an error is thrown if parameter 'p' is not numeric",{
  msg <- "parameter 'p' must be a numeric value between 0 and 1"
  expect_error(Pipeline$new(label="pipeline",cv="lgocv",p=NULL),msg)
  expect_error(Pipeline$new(label="pipeline",cv="lgocv",p=NA),msg)
  expect_error(Pipeline$new(label="pipeline",cv="boot",p=NULL),msg)
  expect_error(Pipeline$new(label="pipeline",cv="boot",p=NA),msg)
  expect_error(Pipeline$new(label="pipeline",cv="boot",p="10"),msg)
  expect_error(Pipeline$new(label="pipeline",cv="boot",p=list()),msg)
})

# test that an error is thrown if parameter 'p' is not in the range [0,1]
test_that("an error is thrown if parameter 'p' is not in the range [0,1]",{
  msg <- "parameter 'p' must be a numeric value between 0 and 1"
  expect_error(Pipeline$new(label="pipeline",cv="lgocv",p=70),msg)  
  expect_error(Pipeline$new(label="pipeline",cv="boot",p=70),msg)
})

# test that parameter 'p' is correctly assigned if it is correctly specified
test_that("parameter 'p' is correctly assigned if it is correctly specified",{
  expect_silent(Pipeline$new(label="pipeline",cv="lgocv",p=0.8))
  pipeline <- Pipeline$new(label="pipeline",cv="lgocv",p=0.8)
  expect_equal(pipeline$p,0.80)
  expect_silent(Pipeline$new(label="pipeline",cv="boot",p=0.8))
  pipeline <- Pipeline$new(label="pipeline",cv="boot",p=0.8)
  expect_equal(pipeline$p,0.80)
  expect_silent(Pipeline$new(label="pipeline",cv="cv",p=NULL))
  expect_silent(Pipeline$new(label="pipeline",cv="loocv",p=NULL))
  pipeline <- Pipeline$new(label="pipeline",cv="cv",p=NULL)
  expect_null(pipeline$p)
  pipeline <- Pipeline$new(label="pipeline",cv="cv",p=100)
  expect_equal(pipeline$p,100)
})

# test that the modules slot is a list upon initiation
test_that("the modules slot is a list upon initiation",{
  pipeline <- Pipeline$new(label="pipeline",cv="lgocv",p=0.8)
  expect_true(is.list(pipeline$modules))
})

# test that the classification module is automatically added to the module slot
test_that("the classification module is automatically added to the module slot",{
  pipeline <- Pipeline$new(label="pipeline",cv="lgocv",p=0.8)
  expect_equal(names(pipeline$modules),"classification")
})

# test that the classification module is empty by default
test_that("the classification module is empty by default",{
  pipeline <- Pipeline$new(label="pipeline",cv="lgocv",p=0.8)
  expect_equal(length(pipeline$modules$classification$tasks),0)
})

# test that the order member contains only classification by default
test_that("the order member contains only classification by default",{
  pipeline <- Pipeline$new(label="pipeline",cv="lgocv",p=0.8)
  expect_equal(pipeline$getOrder(),c("classification"))
})
