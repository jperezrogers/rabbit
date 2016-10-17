# rabbit

An R package for creating computational pipelines for biomarker discovery.

Example Usage For Function:

## Quick Start

### Installing rabbit
Install the most recent version of the rabbit package directly from Github. A development version of the package is also available and can be downloaded by setting `ref="development"` though this version may be unstable.

```r
library(devtools)
install_github("jperezrogers/rabbit", ref="master")
```

### Load Example Data
For illustrative purposes, we'll use the famous leukemia dataset from Golub et al. (1999) contained within the multtest package. This dataset contains gene expression values of 3051 genes across 38 leukemia patients. 27 patients were diagnosed with acute lymphoblastic leukemia (ALL) and 11 were diagnosed with acute myeloid leukemia (AML). Within the outcome vector `golub.cl`, ALL patients are coded as `0` while AML patients are coded as `1`.

```r
library(multtest)
data(golub)
```

### Load Stock Pipeline
The  rabbit package provides a stock biomarker discovery pipeline developed based on the results from the MAQC-II study published by Shi et al. (2010). The stock pipeline is stored in the `stockPipeline` variable and contains 840 pre-build models, each with a unique combination of methods designed to select an optimal set of predictors and classification algorithm. For a more detailed description of the stock pipeline.

```r
library(rabbit)
data(stockPipeline)
```

The pipeline requires that each sample and feature (gene, in this case) have a unique name. Here, we add mock names to the `golub` data matrix.

```r
colnames(golub) <- paste("Sample", 1:ncol(golub), sep="_")
rownames(golub) <- paste("Gene", 1:nrow(golub), sep="_")
```

### Run Stock Pipeline
```r
run(stockPipeline, x=golub, y=as.factor(golub.cl), outputdir=getwd(), seed=1234, verbose=TRUE, force=TRUE)
```
