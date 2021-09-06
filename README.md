# _MatrixQCvis_ - Interactive exploration of data quality

## Overview

The _MatrixQCvis_ package provides shiny-based 
interactive visualization of data quality metrics at the per-sample and 
per-feature level. It is broadly applicable to quantitative omics data types 
that come in matrix-like format (features x samples). It enables the detection 
of low-quality samples, drifts, outliers and batch effects in data sets.
Visualizations include amongst others bar- and violin plots of the (count/intensity) 
values, mean vs standard deviation plots, MA plots, empirical cumulative 
distribution function (ECDF) plots, visualizations of the distances 
between samples, and multiple 
types of dimension reduction plots. 

Furthermore, _MatrixQCvis_ allows for 
differential expression analysis based on the _limma_ (moderated t-tests) and 
_proDA_ (Wald tests) packages. _MatrixQCvis_ builds upon the popular 
Bioconductor _SummarizedExperiment_ S4 class and enables thus the facile 
integration into existing workflows. The package 
is especially tailored towards metabolomics and proteomics mass spectrometry 
data, but also allows to assess the data quality of other data types that 
can be represented in a _SummarizedExperiment_ object.

## Installation

To install _MatrixQCvis_ from GitHub, install the package via `devtools`:
```r 
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
library(devtools)
install_github("tnaake/MatrixQCvis")
```

Alternatively, the _MatrixQCvis_ package can also be installed via the 
Bioconductor project. 

For the [release](http://bioconductor.org/packages/release/bioc/html/MatrixQCvis.html) 
version enter:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("MatrixQCvis")
```

For the [development](http://bioconductor.org/packages/release/bioc/html/MatrixQCvis.html) 
version enter:
```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# the following initializes usage of Bioc devel
BiocManager::install(version = "devel")

BiocManager::install("MatrixQCvis")
```

## Quick start

_MatrixQCvis_ is based on the _SummarizedExperiment_ class. The shiny 
application can be started with passing a _SummarizedExperiment_ object 
(in the following denoted as _se_) or without such an object. The 
second function call will load an interface to load a 
_SummarizedExperiment_ object stored in a RDS file. 

```r
library("MatrixQCvis")

## initialize the application with passing a SummarizedExperiment object
shinyQC(se)

## initialize the application without passing a SummarizedExperiment object
shinyQC()
```
