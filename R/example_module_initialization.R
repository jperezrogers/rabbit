
main <- function(){
#=======================================================================
# set up example data
#=======================================================================

# read in the data and make up some dummy variables
infile <- readRDS(file="../../cbm/biomarker_discovery_pipeline/data/GSE4115_10female_10male.rds")
x <- exprs(infile)
y <- factor(infile$sex)
data <- pData(infile)
data$Age = sample(45:56,20,replace=T)
data$Height = sample(58:72,20,replace=T)

# split the data into training and test sets
# groups <- sample(1:20,10,replace=F)
# train.x <- x[,groups]
# train.y <- y[groups]
# test.x <- x[,-groups]
# test.y <- y[groups]

#=======================================================================
# create an empty pipeline object
#=======================================================================

pipeline <- newPipeline(label="example pipeline",cv="cv",nfolds=5)

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
    mean.expr <- mean(apply(x,1,mean))
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
  method = function(x,y,data,cutoff=0.05,tmp="abc"){
    mod <- model.matrix(~as.character(y)+Age,data=data)
    fit <- lmFit(x,mod)
    fit2 <- eBayes(fit)$p.value
    x <- x[fit2[,2]<cutoff,]
    rank <-fit2[,2][fit2[,2]<cutoff]
    return(list(x=x,rank=rank))
  },
  datatype = "microarray",
  parameters = data.frame(
    parameter = c("cutoff"),
    class = c("numeric"),
    label = c("cutoff"),
    stringsAsFactors = F
  ),
  # control = list(
  #   cutoff = c(0.05,0.005)
  # ),
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
  # control = list(
  #   cutoff = c(0.05,0.005)
  # ),
  libraries = "limma"
)


#=======================================================================
# set up biomarker size module & associated tasks
#=======================================================================

pipeline$addModule(type="M3",label="biomarker size selection")

pipeline$modules$`biomarker size selection`$addTask(
  label = "size selection",
  method = function(x,rank,size=5){
    idx <- sort(rank,decreasing=F,index.return=T)$ix
    x <- x[idx,]
    if(size>nrow(x)){
      warning(paste0("biomarker size: ",size," requested, ",nrow(x)," available"))
    } else {
      x <- x[1:size,]  
    }
    return(x)
  },
  datatype = "microarray",
  parameters = data.frame(
    parameter = "size",
    class = "numeric",
    label = "size",
    stringsAsFactors = F
  ),
  control = list(
    size = c(5,25)
    # size = seq(5,100,by=5)
  )
)

#=======================================================================
# set up classification module & associated tasks
#=======================================================================

pipeline$modules$classification$addTask(
  label = "lda",
  method = function(x,y,testdata){ # might also need to take in x2, y2 for the test set
    mod <- caret::train(x=t(x),y=y,method="lda")
    preds <- predict(mod,newdata=t(testdata),type="prob")
    return(preds[,1])
  },
  datatype = "microarray"
)

pipeline$modules$classification$addTask(
  label = "knn",
  method = function(x,y,testdata){ # might also need to take in x2, y2 for the test set
    mod <- caret::train(x=t(x),y=as.factor(y),method="knn",
                        trControl=trainControl(method = "none"),
                        tuneGrid=data.frame(k=2))
    preds <- predict(mod,newdata=t(testdata),type="prob")
    return(preds[,1])
  },
  datatype = "microarray"
)

outputdir <- "/Users/Joe/Desktop/Pipeline_Output"
for(i in 1:pipeline$nfolds){
  cat("CV Loop ",i,"\n")
  pipeline$run(x,y,data,outputdir=outputdir,iter=i,force=T,verbose=T)  
}







# set up the private members of the pipeline
pipeline$.setPrivate(what="model.index",value=.indexModels(pipeline))
pipeline$.setPrivate(what="parameter.key",value=.generateParameterKey(pipeline))
pipeline$.setPrivate(what="label.key",value=.generateLabelKey(pipeline))
data.partition <- .partitionData(y,cv=pipeline$cv,nfolds=pipeline$nfolds,p=pipeline$p)

# create the output directory structure
outputdir <- "/Users/Joe/Desktop/Pipeline_Output"
.createOutputDirectoryStructure(data.partition,outputdir,pipeline$.getPrivate(what="model.index"),force=T)

# deconstructing the .buildModelsSingleIter() function
partition <- data.partition
iter <- 1
model.index <- pipeline$.getPrivate(what="model.index")
data=data
rank=NULL
verbose=TRUE

model.n <- 1
cv.n <- 1

exitOnError=FALSE
returnTraceback=TRUE

pipeline$.buildModelsSingleIter(x,y,data.partition,iter,model.index,data=data,rank=NULL,verbose=TRUE,exitOnError=FALSE,returnTraceback=TRUE)
  
}








