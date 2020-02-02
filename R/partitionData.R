# A function to partition a dataset based on a cross-validation scheme
# 
# More detailed explaination here
# 
# @importFrom caret createFolds createDataPartition createResample
# 
# @param y A two-level response variable vector
# @param cv The cross-validation scheme. One of 'cv', 'loocv', 'lgocv', or 'boot'
# @param nfolds The number of folds or iterations
# @param p The proportion of samples in the training set
# 
# @return a list of indices, one for each partition of the data
# 
# @examples
# \dontrun{
# y <- as.factor(sample(c(1,0),20))
# partitionData(y,cv="cv",nfolds=10)
# partitionData(y,cv="boot",nfolds=10)
# partitionData(y,cv="lgocv",nfolds=10,p=0.80)
# }
# @keywords internal

partitionData = function(y,cv="cv",nfolds=10,p=0.80){
  
  # set up the sample indices
  index <- 1:length(y)
  if(cv=="cv"){
    data.partition <- caret::createFolds(y=index,k=nfolds,list=TRUE,returnTrain=TRUE)
  } else if(cv=="loocv"){
    data.partition <- as.list(index)
    names(data.partition) <- paste0("Fold",index)
  } else if(cv=="lgocv"){
    data.partition <- caret::createDataPartition(y=index,times=nfolds,p=p,list=TRUE)
  } else if(cv=="boot"){
    data.partition <- caret::createResample(y=index,times=nfolds)
  }
  # return the data partition
  return(data.partition)
  
}