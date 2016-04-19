# rabbit

An R package for creating computational pipelines for biomarker discovery.

Example Usage For Function:

```r

#=======================================================================
# set up test data
#=======================================================================

infile <- readRDS(file="../../cbm/biomarker_discovery_pipeline/data/GSE4115_10female_10male.rds")
x <- exprs(infile)
y <- infile$sex
data <- pData(infile)
data$Age = sample(45:56,20,replace=T)
data$Height = sample(58:72,20,replace=T)

#=======================================================================
# create an empty pipeline object
#=======================================================================

pipeline <- Pipeline$new(label="example pipeline")

#=======================================================================
# set up feature filter module & associated tasks
#=======================================================================

# initialize the module
pipeline$addModule(type="M1",label="feature filter")

# add a task
pipeline$modules$`feature filter`$addTask(
  label = "variance filter",
  method = function(x){
    variance <- apply(x,1,var)
    x <- x[variance>median(variance),]
    return(x)
  },
  datatype = "microarray" 
)

# add another task
pipeline$modules$`feature filter`$addTask(
  label = "mean expression",
  method = function(x){
    mean.expr <- apply(x,1,mean)
    p <- apply(x,1,function(i){
      t.test(i,mu=mean.expr,alternative="greater")$p.value
    })
    x <- x[p<0.05,]
    return(x)
  },
  datatype = "microarray" 
)

#=======================================================================
# set up feature selection module & associated tasks
#=======================================================================

# initialize the module
pipeline$addModule(type="M2",label="feature selection")

# add a task (linear model correcting for 1 covariate)
pipeline$modules$`feature selection`$addTask(
  label = "linear model correcting for 1 covariate",
  method = function(x,y,data,cutoff=0.05){
    mod <- model.matrix(~as.character(y)+Age,data=data)
    fit <- lmFit(x,mod)
    fit2 <- eBayes(fit)$p.value
    x <- x[fit2[,2]<cutoff,]
    rank <-fit2[,2][fit2[,2]<cutoff]
    return(list(x=x,rank=rank))
  },
  datatype = "microarray",
  parameters = data.frame(
    parameter = "cutoff",
    class = "numeric",
    label = "cutoff",
    stringsAsFactors = F
  ),
  control = data.frame(
    cutoff = c(0.05,0.005),
    stringsAsFactors = F
  ),
  libraries = "limma"
)

# add another task (linear model correcting for 2 covariates)
pipeline$modules$`feature selection`$addTask(
  label = "linear model correcting for 2 covariates",
  method = function(x,y,data,cutoff=0.05){
    mod <- model.matrix(~as.character(y)+Age+Height,data=data)
    fit <- lmFit(x,mod)
    fit2 <- eBayes(fit)$p.value
    x <- x[fit2[,2]<cutoff,]
    rank <-fit2[,2][fit2[,2]<cutoff]
    return(list(x=x,rank=rank))
  },
  datatype = "microarray",
  parameters = data.frame(
    parameter = "cutoff",
    class = "numeric",
    label = "cutoff",
    stringsAsFactors = F
  ),
  control = data.frame(
    cutoff = c(0.05,0.005),
    stringsAsFactors = F
  ),
  libraries = "limma"
)


#=======================================================================
# set up biomarker size module & associated tasks
#=======================================================================

pipeline$addModule(type="M3",label="biomarker size selection")

pipeline$modules$`biomarker size selection`$addTask(
  label = "size",
  method = function(x,rank,size=5){
    idx <- sort(rank,decreasing=F,index.return=T)$ix
    x <- x[idx,]
    x <- x[1:size,]
    return(x)
  },
  datatype = "microarray",
  parameters = data.frame(
    parameter = "size",
    class = "numeric",
    label = "size",
    stringsAsFactors = F
  ),
  control = data.frame(
    size = seq(5,100,by=5),
    stringsAsFactors = F
  )
)

#=======================================================================
# set up classification module & associated tasks
#=======================================================================

```
