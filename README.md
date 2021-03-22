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
types of dimension reduction plots. Furthermore, _MatrixQCvis_ allows for 
differential expression analysis based on the _limma_ (moderated t-tests) and 
_proDA_ (Wald tests) packages. _MatrixQCvis_ builds upon the popular 
Bioconductor _SummarizedExperiment_ S4 class and enables thus the facile 
integration into existing workflows. The package 
is especially tailored towards metabolomics and proteomics mass spectrometry 
data, but also allows to assess the data quality of other data types that 
can be represented in a _SummarizedExperiment_ object.

## Installation

Currently, _MatrixQCvis_ is available via `devtools`:
```r 
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
library(devtools)
install_github("tnaake/MatrixQCvis")
```

_MatrixQCvis_ was submitted to Bioconductor and is currently under review.

## Quick start

```r
library(MatrixQCvis)
shinyQC()
```
