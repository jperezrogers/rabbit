#' Function to generate a key matching Task object labels to a model index
#' 
#' More detailed explaination here
#' 
#' @param pipeline An object of class \code{Pipeline}
#' 
#' @return A named list of lists where the name of each parent corresponds to a module in \code{pipeline} and each child corresponds to a task within that module
#' 
#' @examples
#' .generateLabelKey(pipeline)

.generateLabelKey = function(pipeline){
  
  # count the tasks
  task.count <- .countTasks(pipeline)
  
  # set up the label key
  label.key <- list()
  for(label in names(task.count)){
    label.key[[label]] <- as.list(rep(NA,task.count[label]))
  }
  
  # loop through each module & task, adding all task labels to the key
  for(module in pipeline$modules){
    counter <- 0
    for(task in module$tasks){
      o <- expand.grid(task$control,stringsAsFactors=F)
      if(nrow(o)==0){
        counter <- counter + 1
        label.key[[module$label]][[counter]] <- task$label
      } else {
        for(i in 1:nrow(o)){
          counter <- counter + 1
          label.key[[module$label]][[counter]] <- task$label
        }
      }
    }
  }
  
  # return label.key
  return(label.key)
  
}