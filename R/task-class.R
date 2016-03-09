

# load the required libraries
library(R6)

# define the Task class
Task <- R6Class("Task",
  
  public = list(
    # public members
    label = NA,
    method = NA,
    parameters = NA,
    libraries = NA,
    control = NA,
    datatype = NA,
    testdata = NA,
    moduletype = NA,
    
    # public methods
    initialize = function(label,method,parameters=NULL,libraries=NULL,control=NULL,datatype,testdata=NULL,moduletype){
      self$label = label
      self$method = method
      self$parameters = parameters
      self$libraries = libraries
      self$control = control
      self$datatype = datatype
      self$testdata = testdata
      self$moduletype = moduletype
      private$validateParameters(self$parameters)
    }
  ),
  
  private = list(
    
    # function to convert the class of an object given the object and class
    convertClass = function(dat,class){
      return(do.call(paste0("as.",class),args=list(x=dat)))
    },
    
    # function to validate the input of the parameters member
    validateParameters = function(parameters){
      if(!is.null(parameters)){
        
        # check that parameters is a data.frame
        if(!is.data.frame(parameters)){
          stop("The 'parameters' argument must be of class data.frame")
        }
        
        # check if parameters have colnames = c("parameter", "class", "label") and optionally c("default")
        ref <- c("parameter","class","label","default")
        if(!(setequal(ref[1:3],colnames(parameters))|setequal(ref,colnames(parameters)))){
          stop("The 'parameters' argument must only contain named columns 'parameters', 'class', 'label', and optionally 'default'")
        }
        
        # check that every 'parameter' is a character
        if(!all(is.character(parameters$parameter))){
          stop("Not all values provided in parameters$parameter are of class character")
        }
        
        # check if every parameter has a name, if not, assign default to be name of parameter  
        if(!all(is.character(parameters$label))){
          warning("Not all values provided in parameters$label are of class character. Using values from parameters$parameter for all non-character elements instead")
          idx <- which(!is.character(parameters$label))
          parameters$label[idx] <- parameters$parameter[idx]
        }
        
        # check if default is defined, if so, convert the default values to the desired class
        if("default"%in%colnames(parameters)){
          for(i in 1:nrow(parameters)){
            parameters$default[i] <- private$convertClass(parameters$default[i],parameters$class[i])
          }
        }
      }
    }
  )
)



label <- "example_task"

# need to check that all parameters here other than first two have defaults defined or in parameters var.=
method <- function(x,y,alternative="two.sided",var.equal=FALSE){
  require(limma)
  mod <- model.matrix(~y)
  fit <- lmFit(x,mod)
  fit2 <- eBayes(fit)$p.value
  p <- fit2[,2]
  return(head(p))
}

parameters <- data.frame(
  parameter = c("alternative","var.equal"),
  class = c("character","logical"),
  label = c("alternative","var.equal"),
  default = c("two.sided",FALSE) # user can optionally define defaults here
,stringsAsFactors=FALSE)

libraries <- c("limma")

control <- list(
  var.equal = c(TRUE,FALSE)
)

datatype <- c("microarray")

testdata = NULL

moduletype = "M1"

mytask <- Task$new(label=label,method=method,parameters=parameters,control=control,datatype=datatype,moduletype=moduletype,libraries=libraries)







func1 <- function(emat,pheno){
  mod <- model.matrix(~CANCER+SMK,data=pheno)
  fit <- lmFit(emat,mod)
  fit2 <- eBayes(fit)
  return(fit2$p.value[,1])
}

func2 <- function(emat,pheno){
  t.test(emat[1,]~CANCER,data=pheno)
}



f <- function(x,y,data=NULL,...){
  p <- apply(x,1,function(z){t.test(y~z)$p.value})
  return(x[p<0.05,])
}

config <- list(
  method = f,
  label = "Welch t-test",
  params = data.frame(parameter = "data",
                      class = "data.frame",
                      label = "data"),
  control = NULL
)




















library(R6)

Task <- R6Class("Task",
  public = list(
    initialize = function(...){
      self$method <- m
      self$params <- list(...)
    },
    run = function(){
      return(self$method(self$params))
    },
    method = NULL,
    params = list()
  ),
  private = list(
    # can have private member functions & fields
    total = 3
  ),
  lock_class=TRUE
)