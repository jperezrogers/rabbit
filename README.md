# rabbit

An R package for creating computational pipelines for biomarker discovery.

# Task class

Example Usage:

```r
label <- "example_task"

method <- function(x,y,alternative="two.sided",var.equal=FALSE){
  require(limma)
  mod <- model.matrix(~y)
  fit <- lmFit(x,mod)
  fit2 <- eBayes(fit)$p.value
  p <- fit2[,2]
  return(head(p))
}

parameters <- data.frame(
  parameter = c("alternative","var.equal"),
  class = c("character","logical"),
  label = c("alternative","var.equal"),
  default = c("two.sided",FALSE)
,stringsAsFactors=FALSE)

libraries <- c("limma")

control <- list(
  var.equal = c(TRUE,FALSE)
)

datatype <- c("microarray")

testdata = NULL

moduletype = "M1"

mytask <- Task$new(
  label=label,
  method=method,
  parameters=parameters,
  control=control,
  datatype=datatype,
  moduletype=moduletype,
  libraries=libraries
)
```
