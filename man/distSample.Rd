% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_values.R
\name{distSample}
\alias{distSample}
\title{Create a heatmap using distance information between samples}
\usage{
distSample(d, se, label = "name", title = "raw", ...)
}
\arguments{
\item{d}{\code{matrix} containing distances, obtained from \code{distShiny}}

\item{se}{\code{SummarizedExperiment}}

\item{label}{\code{character}, refers to a column in \code{colData(se)}}

\item{title}{\code{character}}

\item{...}{further arguments passed to \code{ComplexHeatmap::Heatmap}}
}
\value{
\code{Heatmap} object from \code{ComplexHeatmap}
}
\description{
The function \code{distSample} creates a heatmap from a distance matrix 
created by the function \code{distShiny}. The heatmap is annotated by the 
column specified by the \code{label} column in \code{colData(se)}.
}
\details{
Internal use in \code{shinyQC}
}
\examples{
## create se
a <- matrix(seq_len(100), nrow = 10, ncol = 10,
            dimnames = list(seq_len(10), paste("sample", seq_len(10))))
a[c(1, 5, 8), seq_len(5)] <- NA
set.seed(1)
a <- a + rnorm(100)
a_i <- imputeAssay(a, method = "MinDet")
cD <- data.frame(name = colnames(a_i),
    type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a_i))
se <- SummarizedExperiment::SummarizedExperiment(assay = a_i, rowData = rD,
    colData = cD)

dist <- distShiny(a_i)
distSample(dist, se, label = "type", title = "imputed", 
    show_row_names = TRUE)

}
