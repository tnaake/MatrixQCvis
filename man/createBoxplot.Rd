% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_values.R
\name{createBoxplot}
\alias{createBoxplot}
\title{Create a boxplot of (count/intensity) values per sample}
\usage{
createBoxplot(
  se,
  orderCategory = colnames(colData(se)),
  title = "",
  log = TRUE,
  violin = FALSE
)
}
\arguments{
\item{se}{\code{SummarizedExperiment} containing the (count/intensity) values 
in the \code{assay} slot}

\item{orderCategory}{\code{character}, one of \code{colnames(colData(se))}}

\item{title}{\code{character} or \code{numeric} of \code{length(1)}}

\item{log}{\code{logical}, if \code{TRUE} (count/intensity) values are 
displayed as log values}

\item{violin}{\code{logical}, if \code{FALSE} a boxplot is created, if 
\code{TRUE} a violin plot is created}
}
\value{
\code{gg} object from \code{ggplot2}
}
\description{
The function \code{create_boxplot} creates a boxplot per sample for the 
intensity/count values.
}
\details{
Internal usage in \code{shinyQC}.
}
\examples{
## create se
a <- matrix(seq_len(100), nrow = 10, ncol = 10, 
    dimnames = list(seq_len(10), paste("sample", seq_len(10))))
a[c(1, 5, 8), seq_len(5)] <- NA
set.seed(1)
a <- a + rnorm(100)
cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
    rowData = rD, colData = cD)

createBoxplot(se, orderCategory = "name", title = "", log = TRUE, 
    violin = FALSE)

}
