# Function to compute and order all pairwise combinations of elements in a list

calculateIndex = function(params){
  
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