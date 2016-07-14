#' Function to compute and order all pairwise combinations of elements in a list
#' 
#' Author: Joe Perez-Rogers, adapted from Joshua Campbell
#' 
#' @param params a list of vectors. Elements within a vector should be unique. Elements between vectors need not be unique nor do the vectors need to be the same length
#' 
#' @return a \code{data.frame} with \code{nrow} equal to the product of the lengths of the vectors in \code{param} and \code{ncol} equal to the length of \code{param}. Each value in the data frame corresponds to the index of an element in \code{param} (the first column corresponds to elements in the first vector, etc.)
#' 
#' @examples
#' params <- list(c(1:5),c(1:7),c(1:3))
#' .calculateIndex(params)

.calculateIndex = function(params){
  
  # count total number of tasks per module
  param.max = as.numeric(lapply(params,length))
  
  # calculate the total number of models
  total = prod(param.max)
  
  # calculate the total number of modules
  num.params = length(params)
  
  # set up a index matrix
  index <- matrix(0,nrow=total,ncol=num.params)
  
  # compute the index
  for(i in 1:num.params){
    if(i == 1){
      e = 1	
    } else {
      e = prod(param.max[1:(i-1)])
    }
    num = total/prod(param.max[1:i])
    index[,i] = rep(rep(1:param.max[i],each=e),num)
  }
  
  # sort the index by sequential columns
  index <- index[do.call(order,args=split(index,rep(1:ncol(index),each=nrow(index)))),]
  
  # return the index
  return(index)
  
}