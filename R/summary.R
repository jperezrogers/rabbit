#' Report a summary of an object of class Pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param x an object of class Pipeline
#' @param level one of 'overview', 'structure', or 'active' (see details below)
#' 
#' @value The form of the value returned depends on the class of its argument.
#' 
#' @examples
#' 
#' pipeline <- newPipeline(label="example pipeline",cv="cv",nfolds=10,p=0.80)
#' summary(pipeline)

summary.Pipeline <- function(x,level="overview",pretty=TRUE){
  
  # check that level is one of 'overview', 'structure', or 'active'
  if(!level%in%c("overview","structure","active")){
    stop("parameter 'level' must be one of 'all', 'structure', or 'active'")
  }
  
  # extract the pipeline overview
  if(level=="overview"){
    
    overview <- list()
    overview[["label"]] <- x$label
    model.index <- .indexModels(x)
    totmod <- ifelse(is.null(model.index),0,nrow(model.index))
    overview[["total models"]] <- totmod
    overview[["cross-validation method (cv)"]] <- x$cv
    overview[["training set fraction (p)"]] <- x$p
    
    if(pretty){
      cat("Pipeline Overview\n")
      for(name in names(overview)){
        cat(paste0("   ",name,": ",overview[[name]],"\n"))
      }
    }
    
    returned <- overview
    
  }
  
  # extract the pipeline structure component
  if(level=="structure"){
    
    structure <- list()
    for(module in names(pipeline$modules)){
      structure[[module]] <- names(pipeline$modules[[module]]$tasks)
    }
    
    if(pretty){
      cat("Pipeline Structure\n")
      for(i in 1:length(names(structure))){
        module <- names(structure)[i]
        cat(paste0("\n   Module ",i,": ",module,"\n"))
        for(j in 1:length(structure[[module]])){
          task <- structure[[module]][j]
          cat(paste0("      `-- Task ",j,": ",task,"\n"))
        }
      }
    }
    
    returned <- overview
    
  }
  

  
  # TODO: extract the active component
  active <- data.frame()
  # if(level=="active"){
  #   
  # }
  
  if(!pretty){
    return(returned)
  }

}