#' Train a weighted voting classifier
#'
#' A more detailed description will eventually go here...
#'
#' @param model a list as generated by wv.model()
#' @param data matrix with samples in columns and genes in rows
#' @param type one of "raw" or "prob"
#'
#' @examples
#' \dontrun{
#' library(multtest)
#' data(golub)
#' mod <- wv.model(golub,golub.cl,correction=T)
#' wv.predict(mod,golub,type="raw")
#' }
#' @export

wv.predict <- function (model, data, type="raw") {
  # Implementation of weighted voting code from Golub et al., Science, 1999.
  # Adam Gower, 2008
  
  # INPUT
  # model: a list with two elements, named weights and means, that contains the 
  #   weights and means for each probeset as generated by wv.model()
  # data: matrix or data frame that contains all probesets for weighted voting by 
  #   rows and test samples by columns
  # NOTE: the row names of data must be named in the same manner as the weights 
  #   and means in the model!
  #
  # OUTPUT
  # depends on the value of type. If type="raw", prediction scores are output, 
  # else, binary predictions
  # scores: a vector of weighted voting scores for each sample; negative = class 
  #   0, positive = class 1
  # predictions: a vector of class predictions (0 or 1) for each sample as 
  #   determined from the sign of the scores vector
  
  if(!type%in%c("raw","prob")){
    stop("parameter 'type' must be one of 'raw' or 'prob'")
  }
  
  indices <- match(names(model$means), rownames(data))
  votes <- model$weights * (data[indices,,drop=F]-model$means)
  V <- sapply(c(-1,1), function(s){
    apply(votes, 2, function(x){sum(x[which(sign(x)==s)])})
  })
  
  if(is.null(dim(V))){
    scores <- V[1]+V[2]
  } else {
    scores <- V[,1] + V[,2]
  }
  predictions <- as.numeric(scores > 0)
  if(is.null(dim(V))){
    strengths <- abs(scores) / (V[2] - V[1])
  } else {
    strengths <- abs(scores) / (V[,2] - V[,1])
  }
  
  if(type=="raw"){
    output <- predictions
  } else if(type=="prob"){
    output <- scores
  }
  
  return(output)
}