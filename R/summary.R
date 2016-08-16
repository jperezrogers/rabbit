#' Report a summary of an object of class Pipeline
#'
#' A more detailed description will eventually go here...
#'
#' @param object an object of class Pipeline
#' @param ... additional parameters to be passed to summary. See Details below.
#'
#' @examples
#' \dontrun{
#' pipeline <- newPipeline(label="example pipeline",cv="lgocv",nfolds=10,p=0.80)
#' summary(pipeline)
#' }
#' @export

summary.Pipeline <- function(object,...){

  x <- object
  args <- list(...)
  level <- NULL
  pretty <- NULL
  extra.args <- c()

  # check if level is provided via named variable
  if("level"%in%names(args)){
    level <- args$level
    args$level <- NULL
  }

  # check if pretty is provided via named variable
  if("pretty"%in%names(args)){
    pretty <- args$pretty
    args$pretty <- NULL
  }

  # check if any other variables are provided via named variables
  if(length(args)>0){
    if(!is.null(names(args))){
      extra.args <- names(args)[which(names(args)!="")]
      args[extra.args] <- NULL
    }
  }

  # if there are remaining args and level is null, assign first arg to 'level'
  if(is.null(level)){
    if(length(args)>0){
      level <- args[[1]]
      args[[1]] <- NULL
    } else {
      level <- "overview"
    }
  }

  # if there are remaining args and pretty is null, assign next arg to 'pretty'
  if(is.null(pretty)){
    if(length(args)>0){
      pretty <- args[[1]]
      args[[1]] <- NULL
    } else {
      pretty <- TRUE
    }
  }

  # if there are remaining args, add them to the extra args vector
  if(length(args)>0){
    extra.args <- append(extra.args,as.vector(unlist(args)))
  }

  # if there are other variables provided, issue a warning
  if(length(extra.args>0)){
    warning(paste0("unused arguments: ",paste0(extra.args,collapse=", ")))
  }

  # check that level is one of 'overview', 'structure', or 'active'
  if(!level%in%c("overview","structure","detailed","active")){
    stop("parameter 'level' must be one of 'overview', 'structure', 'detailed', or 'active'")
  }

  # check that pretty is a logical variable
  if(!is.logical(pretty)){
    stop("parameter 'pretty' must be either TRUE or FALSE")
  }

  # extract the pipeline overview
  if(level=="overview"){

    overview <- list()
    overview[["label"]] <- x$label
    model.index <- indexModels(x)
    totmod <- ifelse(is.null(model.index),0,nrow(model.index))
    overview[["total models"]] <- totmod
    overview[["cross-validation method (cv)"]] <- x$cv
    if(x$cv%in%c("cv","lgocv","boot")){
      overview[["number of folds (nfolds)"]] <- x$nfolds
    }
    if(x$cv%in%c("lgocv","boot")){
      overview[["training set fraction (p)"]] <- x$p
    }

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
    for(module in x$.getPrivate(what="order")){
      task.labels <- names(x$modules[[module]]$tasks)
      if(is.null(task.labels)){
        n <- NA
      } else {
        n <- task.labels
      }
    structure[[module]] <- n
    }

    if(pretty){
      cat("Pipeline Structure\n")
      for(i in 1:length(names(structure))){
        module <- names(structure)[i]
        cat(paste0("\n   Module ",i,": ",module,"\n"))
        if(all(is.na(structure[[module]]))){
          cat(paste0("      < No tasks >\n"))
        } else {
          for(j in 1:length(structure[[module]])){
            task <- structure[[module]][j]
            cat(paste0("      `-- Task ",j,": ",task,"\n"))
          }
        }
      }
    }

    returned <- structure

  }

  # extract the detailed pipeline structure component
  if(level=="detailed"){

    if(pretty){

      cat("Pipeline Structure - Detailed\n")

      parameter.key <- generateParameterKey(x)
      label.key <- generateLabelKey(x)

      for(n in 1:length(names(parameter.key))){

        module.name <- names(parameter.key)[n]
        module <- parameter.key[[module.name]]

        cat(paste0("\n   Module ",n,": ",module.name,"\n"))

        if(length(x$modules[[module.name]]$tasks)==0){

          cat(paste0("      < No tasks >\n"))

        } else {

          for(i in 1:length(module)){

            task.label <- label.key[[module.name]][[i]]
            cat(paste0("      `-- Task ",i,": ",task.label))
            additional.params.idx <- which(!names(module[[i]])%in%c("x","y","data","rank","testdata"))
            additional.params <- module[[i]][additional.params.idx]
            if(length(additional.params)>0){
              cat(" (")
              strings <- c()
              for(j in 1:length(additional.params)){
                string <- paste(names(additional.params)[j],"=",additional.params[[j]])
                strings <- c(strings,string)
              }
              cat(paste0(strings,collapse=", "))
              cat(")")
            }

            cat("\n")

          }

        }

      }

    }

    returned <- NULL

  }

  # extract the pipeline active component
  if(level=="active"){

    if(pretty){
      cat("Pipeline Task Activity\n")
    }

    mod.col <- c()
    task.col <- c()
    active.col <- c()

    mods <- x$.getPrivate(what="order")
    for(i in 1:length(mods)){

      activity.status <- x$modules[[mods[i]]]$getActive()

      if(pretty){
        cat(paste0("\n   Module ",i,": ",mods[i],"\n"))
        cat(paste0("      Active\tTask\n"))
      }

      for(j in 1:length(activity.status)){

        mod.col <- c(mod.col,mods[[i]])
        task.col <- c(task.col,names(activity.status)[j])
        active.col <- c(active.col,activity.status[[j]])

        if(pretty){
          cat(paste0("       ",activity.status[[j]],"\t",names(activity.status)[j],"\n"))
        }

      }
    }

    returned <- data.frame(Module=mod.col,Task=task.col,Active=active.col)

  }

  if(!pretty){
    return(returned)
  }

}
