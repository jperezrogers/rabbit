# Class providing the Pipeline object
#
# Here is a more detailed sentence
# about the Pipeline class
#
# @docType class
# @keywords biomarker classification
# @return An object of class \code{Pipeline}
# @format \code{\link{R6Class}} object
#
# @field label The name of the Module
# @field order An ordered character vector of \code{Module} names. \code{order} determined the order in which \code{Modules} will be run in the pipeline
# @field cv The resampling method to be used for cross-validation. One of \code{"cv"}, \code{"loocv"}, \code{"lgocv"}, \code{"boot"}
# @field nfolds The number of folds for k-fold cross-validation or the number of resamplings for \code{"lgocv"}, \code{"boot"}, or \code{"boot632"}
# @field p The percentage of samples in the internal training set for \code{"lgocv"}
#
# @section Methods:
# \describe{
#    \item{\code{addModule(type,label)}}{Add a Module to the Pipeline}
#    \item{\code{orderModules(x)}}{Change the order in which Modules are executed}
#    \item{\code{run(x,y,data=NULL,ranks=NULL,verbose=FALSE,...)}}{Execute the pipeline}
# }
#
# @keywords internal

Pipeline <- R6::R6Class("Pipeline",

  public = list(

#===============================================================================
# Public Members
#===============================================================================

    # label = NA, # the name of the pipeline,
    modules = list(), # a list of modules in the pipeline
    # cv = NA, # resampling method
    # nfolds = NA, # number of folds or resampling iterations
    # p = NA, # percentage of samples in the training set

#===============================================================================
# Public Methods
#===============================================================================

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # initialization
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    initialize = function(label=NULL,cv="lgocv",nfolds=10,p=0.80){

      # check that label is provided and in the right format, if so, set
      # private$label
      if(is.null(label)){
        stop("parameter 'label' must not be NULL")
      } else if(!is.character(label)){
        stop("parameter 'label' must be of class character")
      } else {
        private$label <- label
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
        private$cv <- cv
      }

      # check that nfolds is an integer
      is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
      if(!is.numeric(nfolds)){
        stop("parameter 'nfolds' must be of class numeric")
      } else if(!is.wholenumber(nfolds)){
        stop("parameter 'nfolds' must be an integer >=1")
      } else {
        private$nfolds <- nfolds
      }

      # check that p is a numeric between 0 and 1
      if(private$cv%in%c("lgocv","boot")){
        if(!is.numeric(p)){
          stop("parameter 'p' must be a numeric value between 0 and 1")
        } else if(p>=1|p<=0){
          stop("parameter 'p' must be a numeric value between 0 and 1")
        }
      }
      private$p <- p

      # set the module list to be empty
      self$modules <- list()

      # add an M4 module to the pipeline
      self$modules[["classification"]] <- M4$new(label="classification")

      # add the classification module to the order member
      private$order <- c("classification")

    },

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # addModule
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # deleteModule
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
        stop("the 'classification' module cannot be deleted")
      }

      # delete the module
      self$modules[[label]] <- NULL
      private$order <- private$order[-which(private$order==label)]

    },

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # orderModules
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

      # check if duplicate module names were provided
      } else if(length(unique(x))!=length(x)){

        stop("duplicate module labels are not permitted")

      # check if the last module label in x is 'classification', if not, issue an error message
      } else if(x[length(x)]!="classification"){

        stop("the last element of parameter 'x' must be 'classification'")

      # if all of the above pass, reorder the modules based on the order of x
      } else {

        private$order <- x

      }
    },

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # getOrder
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    # function to return the private order member
    getOrder = function(verbose=FALSE){

      if(verbose){
        cat(paste0(private$label,"\n"))
        sapply(1:length(self$modules),function(i){
          cat(paste0("\t",i,". ",private$order[i],"\n"))
        })
      }

      return(private$order)

    },

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # generic placeholder for a run function (for now)
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    run = function(x,y,data=NULL,rank=NULL,outputdir=getwd(),iter=NULL,seed=NULL,
                   force=FALSE,verbose=FALSE,exitOnError=FALSE,returnTraceback=TRUE,
                  inputPartition=NULL){
      
      if(is.null(inputPartition)&&(private$nfolds==1)){
        stop(paste0('If nfolds is equal to 1, then an input partition must be given in inputPartition'))
      }
      # set the seed
      if(!is.null(seed)){
        set.seed(seed)  
      }
      
      # step #: validate pipeline config

      # step #: validate that all modules have tasks
      task.count <- countTasks(self)
      if(any(task.count==0)){
        stop(paste0("all modules must have at least one task: ",paste(names(which(task.count==0)),collapse=", ")))
      }

      # step #: create model index (and assign to model.index private member)
      private$model.index <- indexModels(self)

      # step #: create parameter key (and assign to parameter.key private member)
      private$parameter.key <- generateParameterKey(self)

      # step #: create label key (and assign to label.key private member)
      private$label.key <- generateLabelKey(self)

      # step #: validate input parameters
      self$.validateUserInputs(x,y,data,rank,iter,seed)

      # step 6: split data into internal training/test sets based on cv
      if (is.null(inputPartition)){
        data.partition <- partitionData(y,cv=private$cv,nfolds=private$nfolds,p=private$p)
      } else{
        data.partition <- inputPartition
      }

      # step 7: create the output directory structure that will store the results
      createOutputDirectoryStructure(data.partition,outputdir,private$model.index,force,iter)

      # step 8: run each model
      if(is.null(iter)){
        i.iter <- 1
        i.max <- private$nfolds
      } else {
        i.iter <- iter
        i.max <- iter
      }

      if(verbose){cat("Evaluating Models\n\n")}
      for(i in i.iter:i.max){
        if(!is.null(seed)){
          set.seed(seed)  
        }
        self$.buildModelsSingleIter(x=x,y=y,data=data,rank=rank,outputdir=outputdir,
                                    partition=data.partition,iter=i,
                                    model.index=private$model.index,
                                    verbose,exitOnError,returnTraceback)
      }

    },

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # getModelSpecs function
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    getModelSpecs = function(){
      
      # create model index
      model.index <- indexModels(self)
      
      # create a detailed label key
      parameter.key <- generateParameterKey(self)
      label.key <- generateLabelKey(self)
      label.key.detailed <- list()
      for(n in 1:length(names(parameter.key))){
        module.name <- names(parameter.key)[n]
        module <- parameter.key[[module.name]]
        module.tasks <- c()
        if(length(self$modules[[module.name]]$tasks)>0){
          for(i in 1:length(module)){
            task.label <- label.key[[module.name]][[i]]
            additional.params.idx <- which(!names(module[[i]])%in%c("x","y","data","rank","testdata"))
            additional.params <- module[[i]][additional.params.idx]
            if(length(additional.params)>0){
              strings <- c()
              for(j in 1:length(additional.params)){
                string <- paste(names(additional.params)[j],"=",additional.params[[j]])
                strings <- c(strings,string)
              }
              extra.params <- paste0("(",paste0(strings,collapse=", "),")")
              task.label <- paste(task.label,extra.params)
            }
            module.tasks <- append(module.tasks,task.label)
          }
        }
        label.key.detailed[[module.name]] <- module.tasks
      }
      
      # create the specs matrix
      col.idx <- seq(1:ncol(model.index))
      specs <- t(sapply(1:nrow(model.index),function(z){
        sapply(col.idx,function(x){unlist(label.key.detailed[[x]][model.index[z,x]])})
      }))
      colnames(specs) <- colnames(model.index)
      rownames(specs) <- rownames(model.index)
      
      # return specs
      return(specs)
      
    },

#===============================================================================
# Public 'Hidden' Methods
#===============================================================================

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # .getPrivate
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    .getPrivate = function(what){
      return(private[[what]])
    },

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # .setPrivate
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    .setPrivate = function(what,value){
      private[[what]] <- value
    },

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # .validateUserInputs
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    .validateUserInputs = function(x=NULL,y=NULL,data=NULL,rank=NULL,iter=NULL,seed=NULL){

      # check that x is not null
      if(is.null(x)){
        stop("parameter 'x' cannot be NULL")
      }

      # check that x is a numerical matrix
      if(!is.matrix(x)){
        stop("parameter 'x' must be of class 'matrix'")
      }
      
      # check that x has unique column and row names
      if(is.null(colnames(x))|is.null(rownames(x))){
        stop("parameter 'x' must have row and column names")
      } else if(length(unique(colnames(x)))!=length(colnames(x))){
        stop("colnames of parameter 'x' must all be unique")
      } else if(length(unique(rownames(x)))!=length(rownames(x))){
        stop("rownames of parameter 'x' must all be unique")
      }

      # check that y is not null
      if(is.null(y)){
        stop("parameter 'y' cannot be NULL")
      }

      # check that y is equal to ncol(x)
      if(length(y)!=ncol(x)){
        stop("parameter 'y' must have length equal to ncol(x)")
      }

      # check that y is a two level factor with levels 0 and 1
      if(!is.factor(y)){
        stop("parameter 'y' must be a two-level factor with levels '0' and '1'")
      } else if(!all(y%in%c(0,1))){
        stop("parameter 'y' must be a two-level factor with levels '0' and '1'")
      }
      
      # extract the parameters used in all tasks in the pipeline
      params <- unlist(lapply(unlist(private$parameter.key,recursive=F),function(x) names(unlist(x))))

      # check if any tasks contain the 'data' input parameter
      if(is.null(data)&"data"%in%params){
        stop("parameter 'data' cannot be NULL. One or more tasks require this parameter to be defined.")
      } else if(!is.null(data)&"data"%in%params){
        if(!all(rownames(data)==colnames(x))){
          stop("rownames(data) must be identical to colnames(x)")
        }
      }

      # check if any tasks contain the 'rank' input parameter
      mods <- names(self$modules)
      classes <- sapply(private$order,function(x){self$modules[[x]]$getClass})
      if(is.null(rank)&"rank"%in%params){
        # if rank is an input parameter for one of the tasks and M2 (which output's rank) is not defined,
        # or, if M3 (which takes in rank) is before M2 in the pipeline, throw and error.
        if(!"M2"%in%classes|(min(which(classes=="M2"))>min(which(classes=="M3")))){
          stop("parameter 'rank' cannot be NULL. One or more tasks require this parameter to be defined.")
        }
      } else if(!is.null(rank)&"rank"%in%params){
        if(length(rank)!=nrow(x)){
          stop("parameter 'rank' must have length equal to nrow(x)")
        }
      }

      # check that iter is a valid integer
      if(!is.null(iter)){
        if(!(iter%in%c(1:private$nfolds))){
          stop(paste0("parameter 'iter' must be an integer between 1 and ",private$nfolds))
        }
      }

      # if iter is defined, seed must also be defined
      if(!is.null(iter)){
        if(is.null(seed)){
          stop("parameter 'seed' must be defined when 'iter' is not NULL")
        } else if(!is.numeric(seed)){
          stop("parameter 'seed' must be of class 'numeric'")
        }
      }

    },

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # .validatePipelineConfig
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    .validatePipelineConfig = function(){
      invisible(self)
    },

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # .buildModelSingleIter
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    .buildModelsSingleIter = function(x,y,data=NULL,rank=NULL,outputdir,partition,iter,model.index,verbose=FALSE,exitOnError=FALSE,returnTraceback=TRUE){

      # set up the internal training set
      train.x <- x[,partition[[iter]]]
      train.y <- y[partition[[iter]]]

      # if data is not null, subset it to only the training set samples
      if(!is.null(data)){
        train.data <- data[partition[[iter]],]
      } else {
        train.data <- NULL
      }

      # set up the internal test set
      test.x <- x[,-partition[[iter]]]
      test.y <- y[-partition[[iter]]]
      
      # set up the list of generics
      nmodules <- ncol(private$model.index)
      generics <- list()
      for(i in 1:nmodules){
        generics[[paste0("L",i)]] <- list()
      }
      
      # set the generics for the first module in the pipeline
      generics[[1]] <- list(x=train.x,y=train.y,data=train.data,rank=rank,testdata=test.x)
      
      # define minimally changing module
      min.change <- 1

      # starting model evaluation
      if(verbose){cat("Iteration",iter,"\n")}

      # for each model
      for(model.n in 1:nrow(private$model.index)){

        # print out the model number
        if(verbose){cat("\tModel",model.n)}

        # beginning model computation, no errors have occured
        error.occured <- FALSE
        
        # set the minimum change between current module and former module
        if(model.n==1){
          min.change <- 1
        } else {
          current.index <- private$model.index[model.n,]
          former.index <- private$model.index[model.n-1,]
          min.change <- as.numeric(which(current.index!=former.index)[1])
        }
        
        # generics counter
        generics.counter <- min.change

        # loop through the modules & tasks for the first model
        for(module.name in colnames(private$model.index)[min.change:ncol(private$model.index)]){
          
          i <- private$model.index[model.n,module.name]
          task.label <- private$label.key[[module.name]][[i]]
          
          method <- self$modules[[module.name]]$tasks[[task.label]]$method
          params <- private$parameter.key[[module.name]][[i]]

          # load the required libraries if they're not already loaded
          libs <- self$modules[[module.name]]$tasks[[task.label]]$libraries
          if(!is.null(libs)){
            sapply(libs,function(x){suppressPackageStartupMessages(library(x,character.only=T))})
          }

          # extract the module class
          module.class <- self$modules[[module.name]]$getClass

          #=====================================================================
          # Depending on the module class, we have to assign values to certain
          # input parameters that will be passed to the task's method function.
          #=====================================================================

          params <- updateParametersFromGenerics(params,generics[[generics.counter]],module.class)
          params$model.n=model.n
          params$iter=iter
          params$outputdir=outputdir

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
            if(verbose){
              cat(paste0("\tAn error occured in the '",task.label,
                         "' task within the '",module.name,"' module\n",sep=""))
              }
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

          generics[[generics.counter+1]] <- updateGenerics(generics[[generics.counter]],o,module.class)

          # bump the generics counter for the next module
          generics.counter <- generics.counter + 1

          if(module.class=="M4"){

            # for now, write results out to flat files
            feature.names <- row.names(generics[[generics.counter]]$x)

            # check the output of the classification task for correctness
            if(!is.list(o)){
              warning(paste0("returned object from task '",task.label,"' is not a list. Setting scores and classifications to 'NA'"))
              scores <- rep(NA,ncol(params$testdata))
              classifications <- rep(NA,ncol(params$testdata))
            } else {
              if(!"score"%in%names(o)){
                warning(paste0("'scores' element missing from '",task.label,"' output. Setting scores to 'NA'"))
                scores <- rep(NA,ncol(params$testdata))
              } else {
                scores <- o$score
              }
              if(!"class"%in%names(o)){
                warning(paste0("'class' element missing from '",task.label,"' output. Setting class to 'NA'"))
                classifications <- rep(NA,ncol(params$testdata))
              } else {
                classifications <- o$class
              }
            }

            prefix <- paste0(outputdir,"/cv_loop_",iter,"/model_",model.n)
            outfile <- data.frame(Sample=colnames(params$testdata),Score=scores,Classification=classifications, Response=test.y)
            write.table(feature.names,paste(prefix,"feature_names.txt",sep="/"),
                        sep="\t",col.names=F,row.names=F,quote=F)
            write.table(outfile,paste(prefix,"predictions.txt",sep="/"),
                        sep="\t",col.names=T,row.names=F,quote=F)

          }

          # do some checks that the output is appropriate

        }

        # update the model print statement if complete
        if(!error.occured){
          if(verbose){cat("...complete\n")}
        }

      }
    }

  ),

  private = list(

#===============================================================================
# Private Members
#===============================================================================

    label = NA, # the name of the pipeline,
    cv = NA, # resampling method
    nfolds = NA, # number of folds or resampling iterations
    p = NA, # percentage of samples in the training set
    order = NA, # the order in which the modules will be executed
    model.index = NA, # the index matrix of models and tasks
    parameter.key = NA, # key matching modules, tasks, and parameters
    label.key = NA # key matching task names to indices

#===============================================================================
# Private Methods
#===============================================================================


  ),

  active = list(

  ),

  lock_class=FALSE

)
























