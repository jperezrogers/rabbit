#' Run a pipeline
#' 
#' A more detailed description will eventually go here...
#' 
#' @param pipeline an object of class \code{Pipeline}
#' @param x a numerical matrix of gene expression values with samples in columns and genes in rows.
#' @param y a two-level factor or numeric vector of length \code{ncol(x)} containing the outcome for each sample.
#' @param data a \code{data.frame} with samples in rows and phenotype data in columns. \code{nrow(data)} must equal \code{ncol(x)}.
#' @param rank a numeric vector of length \code{nrow(x)} that can be passed to the \link[base]{sort} function.
#' @param outputdir directory where the results should be stored.
#' @param iter an integer indicating the iteration to compute. See Details below.
#' @param seed the numerical seed to set. Required when iter is not NULL. See Details below.
#' @param force logical. Should existing results in \code{outputdir} be overwritten?
#' @param verbose logical. Should intermediate text be printed to the screen?
#' @param exitOnError should the pipeline run be aborted due to an error (\code{TRUE}) or should the pipeline simply skip the problematic model (\code{FALSE})
#' @param returnTraceback in the event of an error, should the error traceback be returned?
#' 
#' @details
#' More details to come...
#' 
#' @examples
#' \dontrun{
#' run(pipeline,x,y)
#' }
#' @export

run <- function(pipeline,x,y,data=NULL,rank=NULL,outputdir=getwd(),iter=NULL,seed=NULL,
                force=FALSE,verbose=FALSE,exitOnError=FALSE,returnTraceback=TRUE,inputPartition=NULL){
  
  pipeline$run(x,y,data,rank,outputdir,iter,seed,force,verbose,exitOnError,returnTraceback,inputPartition)
  
}