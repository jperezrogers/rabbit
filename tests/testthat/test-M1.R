context("m1-class")

# source("../../R/m1-class.R")

# set up test data

# test that an error is thrown during initialization when label is not provided correctly
test_that("an error is thrown during initialization when label is not provided correctly",{
  msg <- "'label' must be provided"
  expect_error(M1$new(),msg)
  expect_error(M1$new(label=NULL),msg)
  msg <- "'label' must be of class character"
  expect_error(M1$new(label=NA),msg)
  expect_error(M1$new(label=123),msg)
  expect_error(M1$new(label=list()),msg)
  expect_error(M1$new(label=data.frame()),msg)
})

# test that M1 initializes correctly given the right label
test_that("M1 initializes correctly given the right label",{
  mod <- M1$new("mymod")
  expect_equal(mod$label,"mymod")
  expect_equal(mod$tasks,list())
})

# test that each validate function is called when adding a new Task
test_that("each validate function is called when adding a new Task",{
  mod <- M1$new("mymod")
  msg <- "The label of a Task cannot be NULL"
  expect_error(mod$addTask(),msg)
  msg <- "method must be defined"
  expect_error(mod$addTask(label="task"),msg)
})

# test that new tasks are added correctly
test_that("new tasks are added correctly",{
  mod <- M1$new("mymod")
  mod$addTask(label="mytask",datatype="microarray",method=function(x){return(x)})
  expect_equal(length(mod$tasks),1)
  expect_equal(mod$tasks[["mytask"]]$datatype,"microarray")
  expect_equal(mod$tasks[["mytask"]]$method,function(x){return(x)})
})

# test that an error is thrown if delete task is incorrectly specified
test_that("an error is thrown if delete task is incorrectly specified",{
  mod <- M1$new("mymod")
  mod$addTask(label="mytask",datatype="microarray",method=function(x){return(x)})
  expect_error(mod$deleteTask(),"argument 'label' cannot be NULL")
  expect_error(mod$deleteTask(label=NULL),"argument 'label' cannot be NULL")
  expect_error(mod$deleteTask(123),"argument 'label' must be of class character")
  expect_error(mod$deleteTask(list()),"argument 'label' must be of class character")
  expect_error(mod$deleteTask(as.factor("mytask")),"argument 'label' must be of class character")
  expect_error(mod$deleteTask("yourtask"),"yourtask is not a valid task label")
  expect_error(mod$deleteTask(c("mytask","yourtask")),"yourtask is not a valid task label")
  expect_error(mod$deleteTask(c("othertask","yourtask")),"othertask, yourtask are not valid task labels")
  expect_error(mod$deleteTask(c("mytask","othertask","yourtask")),"othertask, yourtask are not valid task labels")
})

# test that a task is deleted when delete task is correctly specified
test_that("a task is deleted when delete task is correctly specified",{
  mod <- M1$new("mymod")
  mod$addTask(label="myFirstTask",datatype="microarray",method=function(x){return(x)})
  mod$addTask(label="mySecondTask",datatype="microarray",method=function(x){return(x)})
  expect_equal(names(mod$tasks),c("myFirstTask","mySecondTask"))
  mod$deleteTask("myFirstTask")
  expect_equal(names(mod$tasks),"mySecondTask")
  mod$deleteTask("mySecondTask")
  expect_equal(mod$tasks,list())
  mod$addTask(label="myFirstTask",datatype="microarray",method=function(x){return(x)})
  mod$addTask(label="mySecondTask",datatype="microarray",method=function(x){return(x)})
  mod$deleteTask(c("myFirstTask","mySecondTask"))
  expect_equal(mod$tasks,list())
})
















