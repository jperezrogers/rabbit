#' Class providing the Pipeline object
#' 
#' Here is a more detailed sentence
#' about the Pipeline class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords biomarker classification
#' @return An object of class \code{Pipeline}
#' @format \code{\link{R6Class}} object
#' 
#' @usage Pipeline$new(label, order = NULL, cv = "cv", nfolds = 10, p = NULL)
#' 
#' @field label The name of the Module
#' @field order An ordered character vector of \code{Module} names. \code{order} determined the order in which \code{Modules} will be run in the pipeline
#' @field cv The resampling method to be used for cross-validation. One of \code{"cv"}, \code{"loocv"}, \code{"lgocv"}, \code{"boot"}, or \code{"boot632"}
#' @field nfolds The number of folds for k-fold cross-validation or the number of resamplings for \code{"lgocv"}, \code{"boot"}, or \code{"boot632"}
#' @field p The percentage of samples in the internal training set for \code{"lgocv"}
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{addModule(type,label)}}{Add a Module to the Pipeline}
#'    \item{\code{orderModules(x)}}{Change the order in which Modules are executed}
#'    \item{\code{run(x,y,data=NULL,ranks=NULL,verbose=FALSE,...)}}{Execute the pipeline}
#' }
#' 
#' @examples
#' 

Pipeline <- R6Class("Pipeline",
  
  public = list(
  
    #================#
    # public members #
    #================#
    
    label = NA, # the name of the pipeline,
    modules = list(), # a list of modules in the pipeline
    cv = NA, # resampling method
    nfolds = NA, # number of folds or resampling iterations
    p = NA, # percentage of samples in the training set
    
    #================#
    # public methods #
    #================#
    
    initialize = function(label=NULL,cv="cv",nfolds=10,p=0.80){
      
      # maybe pipelines should come stock with an M4 module that cannot be moved
      
      # check that label is provided and in the right format, if so, set 
      # self$label
      if(is.null(label)){
        stop("parameter 'label' must not be NULL")
      } else if(!is.character(label)){
        stop("parameter 'label' must be of class character")
      } else {
        self$label <- label
      }
      
      # check that cv is valid
      msg <- "parameter 'cv' must be one of 'cv', 'loocv', 'lgocv', or 'boot'"
      if(!is.character(cv)){
        stop(msg)
      } else if(length(cv)!=1){
        stop(msg)
      } else if(!cv%in%c("cv","loocv","lgocv","boot")){
        stop(msg)
      } else {
        self$cv <- cv
      }
      
      # check that nfolds is an integer
      is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
      if(!is.numeric(nfolds)){
        stop("parameter 'nfolds' must be of class numeric")
      } else if(!is.wholenumber(nfolds)|nfolds<=1){
        stop("parameter 'nfolds' must be an integer >1")
      } else {
        self$nfolds <- nfolds
      }
      
      # check that p is a numeric between 0 and 1
      if(self$cv%in%c("lgocv","boot")){
        if(!is.numeric(p)){
          stop("parameter 'p' must be a numeric value between 0 and 1")
        } else if(p>=1|p<=0){
          stop("parameter 'p' must be a numeric value between 0 and 1")
        }
      }
      self$p <- p
      
      # set the module list to be empty
      self$modules <- list()
      
      # add an M4 module to the pipeline
      self$modules[["classification"]] <- M4$new(label="classification")
      
      # add the classification module to the order member
      private$order <- c("classification")
      
    },
    
    addModule = function(type=NULL,label=NULL){
      
      # check that type is not NULL
      if(is.null(type)){
        stop("parameter 'type' cannot be NULL")
      }
      
      # check that type is one of the available module types
      if(!is.character(type)){
        stop("parameter 'type' must be one of 'M1', 'M2', or 'M3'")
      } else if(length(type)!=1){
        stop("parameter 'type' must be one of 'M1', 'M2', or 'M3'")
      } else if(!type%in%c("M1","M2","M3")){
        stop("parameter 'type' must be one of 'M1', 'M2', or 'M3'")
      }
      
      # check that label is not NULL
      if(is.null(label)){
        stop("parameter 'label' cannot be NULL")
      }
      
      # check that label is a character
      if(!is.character(label)){
        stop("parameter 'label' must be of class character")
      }
      
      # check that label is of length 1
      if(length(label)!=1){
        stop("parameter 'label' must be of length 1")
      }
      
      # check that the label does not match the label of an existing module
      if(label%in%names(self$modules)){
        stop(paste0("A module with label ",label," already exists in this pipeline"))
      }
      
      # if all of the above pass, add a new module to the pipeline 
      if(type=="M1"){
        self$modules[[label]] <- M1$new(label=label)
      } else if(type=="M2"){
        self$modules[[label]] <- M2$new(label=label)
      } else if(type=="M3"){
        self$modules[[label]] <- M3$new(label=label)
      }
      
      # add the module to the order member
      if(length(private$order)==1){
        private$order <- c(label,private$order)
      } else {
        private$order <- append(private$order,label,length(private$order)-1)
      }
      
      # reorder the modules member to reflect the order
      self$modules <- self$modules[match(private$order,names(self$modules))]
      
    },
    
    # function to remove a module from a pipeline
    deleteModule = function(label=NULL){
      
      # check that label is not null
      if(is.null(label)){
        stop("parameter 'label' cannot be NULL")
      }
      
      # check that label is a character
      if(!is.character(label)){
        stop("parameter 'label' must be of class character")
      }
      
      # check that label is of length 1
      if(length(label)!=1){
        stop("parameter 'label' must be of length 1")
      }
      
      # check that label is a valid module label
      if(!label%in%names(self$modules)){
        stop(paste0("'",label,"' is not a valid module label"))
      }

      # check if label = "classification"
      if(label=="classification"){
        stop("The 'classification' module cannot be deleted from a pipeline")
      }
      
      # delete the module
      self$modules[[label]] <- NULL
      private$order <- private$order[-which(private$order==label)]
      
    },
    
    orderModules = function(x=NULL){
      
      # if x is null, do nothing
      if(is.null(x)){
        return(invisible())
      }
      
      # check that x is a character vector
      if(!is.character(x)){
        stop("parameter 'x' must be a character vector")
      }
      
      # check that all provided labels are actual module labels, if not, issue an error message
      if(!all(x%in%names(self$modules))){
        
        not.valid <- x[!x%in%names(self$modules)]
        stop(paste0("The following are not valid module labels: ",paste(not.valid,collapse=", ")))
      
      # check if x is the same length as the number of modules, if not, issue an error message
      } else if(length(x)!=length(self$modules)){
        
        not.valid <- names(self$modules)[!names(self$modules)%in%x]
        stop(paste0("parameter 'x' must include all module labels. The following are missing: ",paste(not.valid,collapse=", ")))
        
      # check if the last module label in x is 'classification', if not, issue an error message
      } else if(x[length(x)]!="classification"){
        
        stop("The last element of parameter 'x' must be 'classification'")
      
      # if all of the above pass, reorder the modules based on the order of x  
      } else {
        
        private$order <- x
        
      }
    },
    
    # function to return the private order member
    getOrder = function(verbose=FALSE){
      
      if(verbose){
        cat(paste0(self$label,"\n"))
        sapply(1:length(self$modules),function(i){
          cat(paste0("\t",i,". ",private$order[i],"\n"))
        })  
      }
      
      return(private$order)
      
    },
    #paste0(getwd(),"/",self$label,"_",round(as.numeric(Sys.time())))
    run = function(x,y,data=NULL,ranks=NULL,outputdir=getwd(),iter=NULL,seed=NULL,force=FALSE,verbose=FALSE){
      # seed and iter need to be specified together
      
      # step 1: validate input parameters
      
      # step 2: validate pipeline config (i.e. cv, p, etc.)
      # > validate that all modules have tasks
      
      # step 3: create model index (and assign to model.index private member)
      private$model.index <- .indexModels(self)
      
      # step 4: create parameter key (and assign to parameter.key private member)
      private$parameter.key <- .generateParameterKey(self)
      
      # step 5: create label key (and assign to label.key private member)
      private$label.key <- .generateLabelKey()
      
      # step 6: split data into internal training/test sets based on cv
      data.partition <- .partitionData(y,cv=self$cv,nfolds=self$nfolds,p=self$p)
      
      # step 7: create the output directory structure that will store the results
      .createOutputDirectoryStructure(data.partition,outputdir,private$model.index,force)
      
      # step 8: run each model
      if(verbose){
        cat("Building & evaluating models:\n")
      }
      
      for(i in 1:nrow(private$model.index)){
        if(verbose){
          cat(paste0("Model ",i,"\n"))
        }
      }
      
      invisible()
    },
    
    #=========================#
    # public 'hidden' methods #
    #=========================#
    
    .getPrivate = function(what){
      return(private[[what]])
    },
    
    .setPrivate = function(what,value){
      private[[what]] <- value
    },
    
    .buildModelsSingleIter = function(x,y,partition,iter,model.index,data=NULL,rank=NULL,verbose=FALSE,exitOnError=FALSE,returnTraceback=TRUE){
      
      # set up the internal training set
      train.x <- x[,partition[[iter]]]
      train.y <- y[partition[[iter]]]
      
      # if data is not null, subset it to only the training set samples
      if(!is.null(data)){
        train.data <- data[partition[[iter]],]
      }
      
      # set up the internal test set
      test.x <- x[,-partition[[iter]]]
      test.y <- y[-partition[[iter]]]
      
      # starting model evaluation
      if(verbose){cat("Evaluating Models\n")}
      
      # for each model
      for(model.n in 1:nrow(pipeline$.getPrivate(what="model.index"))){
        
        # print out the model number
        if(verbose){cat("\tModel",model.n)}
        
        # beginning model computation, no errors have occured
        error.occured <- FALSE
        
        # set a generic placeholder for x and rank
        generic.x <- train.x
        generic.rank <- rank
      
        # loop through the modules & tasks for the first model
        for(module.name in colnames(pipeline$.getPrivate(what="model.index"))){
          
          i <- pipeline$.getPrivate(what="model.index")[model.n,module.name]
          task.label <- pipeline$.getPrivate(what="label.key")[[module.name]][[i]]
          method <- pipeline$modules[[module.name]]$tasks[[task.label]]$method
          params <- pipeline$.getPrivate(what="parameter.key")[[module.name]][[i]]
          
          # load the required libraries if they're not already loaded
          libs <- pipeline$modules[[module.name]]$tasks[[task.label]]$libraries
          if(!is.null(libs)){
            suppressPackageStartupMessages(library(libs,character.only=T))  
          }
          
          # extract the module class
          module.class <- pipeline$modules[[module.name]]$getClass
          
          #=====================================================================
          # Depending on the module class, we have to assign values to certain
          # input parameters that will be passed to the task's method function. 
          #=====================================================================
          
          if(module.class=="M1"){
            
            params[["x"]] <- generic.x
            
          } else if(module.class=="M2"){
            
            params[["x"]] <- generic.x
            params[["y"]] <- train.y
            if(!is.null(data)){
              params[["data"]] <- train.data
            }
            
          } else if(module.class=="M3"){
            
            params[["x"]] <- generic.x
            params[["rank"]] <- generic.rank
            
          } else if(module.class=="M4"){
            
            params[["x"]] <- generic.x
            params[["y"]] <- train.y
            if("data"%in%names(params)){
              params[["data"]] <- train.data
            }
            
            # subset the test data to contain only the features in generic.x
            params[["testdata"]] <- test.x[row.names(generic.x),]
            
          }
          
          #=====================================================================
          # Now that the parameters are set, we can run the task. In the event
          # that there's an error, the user may not want to abort and entire
          # pipeline run. If exitOnError=F, in the event that an error occurs,
          # the pipline will continue executing, however it will not report
          # results for the model that caused the error. For debugging purposes,
          # the user can also set traceback=T which will print the error message
          # to STDOUT.
          #=====================================================================
          
          # if the user wants the traceback when an error occurs, use try(), 
          # else use tryCatch which will 'handle' the error
          if(returnTraceback){
            o <- try(do.call(method,params))  
          } else {
            o <- tryCatch(do.call(method,params),error = function(c){return(NULL)})
          }
          
          # check if an error occured and determine whether to proceed
          if(class(o)=="try-error"|is.null(o)){
            error.occured <- TRUE
            if(verbose){cat(paste0("\tAn error occured in the '",task.label,"' task within the '",module.name,"' module\n",sep=""))}
            if(exitOnError){
              stop("An error has occured. Try using traceback() or set traceback=T or exitOnError=F")
            } else {
              break()
            }
          }
          
          #=====================================================================
          # If no errors occured running the task, proceed to update the generic
          # variables for use in the next task. Or, if the module is of class
          # M4, wite out the final feature names and prediction scores to files
          #=====================================================================
          
          # if no errors occured, continute with processing
          if(module.class=="M1"){
            
            generic.x <- o
            
          } else if(module.class=="M2"){
            
            generic.x <- o[["x"]]
            generic.rank <- o[["rank"]]
            
          } else if(module.class=="M3"){
            
            generic.x <- o
            
          } else if(module.class=="M4"){
            
            # if module.class is M4 (the classification module), write out the 
            # feature names and predictions
            feature.names <- row.names(generic.x)
            predictions <- o
            
            # for now, write them out to flat files
            prefix <- paste0(outputdir,"/cv_loop_",cv.n,"/model_",model.n)
            write.table(feature.names,paste(prefix,"feature_names.txt",sep="/"),
                        sep="\t",col.names=F,row.names=F,quote=F)
            write.table(predictions,paste(prefix,"predictions.txt",sep="/"),
                        sep="\t",col.names=F,row.names=T,quote=F)
            
          }

          # do some checks that the output is appropriate
          
        }
        
        # update the model print statement if complete
        if(!error.occured){
          if(verbose){cat("...complete\n")}
        }
        
      }
    },
    
    .runTask = function(method,params){
      out <- try(do.call(method,params))
      
      return(out)
    }
  ),
  
  private = list(
    
    #=================#
    # private members #
    #=================#
    
    order = NA, # the order in which the modules will be executed
    model.index = NA, # the index matrix of models and tasks
    parameter.key = NA, # key matching modules, tasks, and parameters
    label.key = NA # key matching task names to indices
    
    #=================#
    # private methods #
    #=================#

    
  ),
  
  active = list(
    
  ),
  
  lock_class=FALSE
  
)
























