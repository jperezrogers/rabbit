#' Train a weighted voting classifier
#'
#' A more detailed description will eventually go here...
#'
#' @param data matrix with samples in columns and genes in rows
#' @param classlabel two-level factor of binary outcomes (0 or 1)
#' @param correction should the Broad Institute's correction method be used?
#'
#' @examples
#' \dontrun{
#' library(multtest)
#' data(golub)
#' mod <- wv.model(golub,golub.cl,correction=T)
#' }
#' @export

wv.model <- function(data, classlabel, correction = TRUE){
# Implementation of weighted voting code from Golub et al., Science, 1999.
# Adam Gower, 2008

# INPUT
# data: matrix or data frame that contains all probesets for weighted voting by 
#   rows and training samples by columns.
# classlabel: vector of binary outputs (0 or 1).
# correction: flag to use Broad Institute's correction method (see below).
#
# OUTPUT
# A list with two elements:
# weights: a vector of weights for each probeset
# means: a vector of means for each probeset across all samples
# The elements of both vectors are named according to the probeset names, i.e., 
#   the row names of the data matrix 
  
  y <- lapply(1:2, function (i) {data[,classlabel==names(table(classlabel))[i],drop=F]})
  mu <- sapply(y, apply, 1, mean, na.rm=T, simplify=F)
  sigma <- sapply(y, apply, 1, stats::sd, na.rm=T, simplify=F)
  if (correction) {sigma <- mapply(pmax, sigma, lapply(mu, `*`, 0.2),SIMPLIFY=F)}
  a <- (mu[[2]] - mu[[1]]) / (sigma[[1]] + sigma[[2]])
  g <- (mu[[1]] + mu[[2]]) / 2
  return(list(weights=a, means=g))
}

