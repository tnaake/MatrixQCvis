% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_values.R
\name{hoeffDPlot}
\alias{hoeffDPlot}
\title{Create a plot from a list of Hoeffding's D values}
\usage{
hoeffDPlot(df, lines = TRUE)
}
\arguments{
\item{df}{\code{data.frame} containing one or multiple columns containing the 
Hoeffding's D statistics}

\item{lines}{\code{logical}, should points belonging to the same sample be 
connected}
}
\value{
\code{gg} object from \code{ggplot2}
}
\description{
The function \code{hoeffDPlot} creates via \code{ggplot} a violin plot per 
factor, a jitter plot of the data points and (optionally) connects the points
via lines. \code{hoeffDPlot} uses the \code{plotly} package to make the 
figure interactive.
}
\details{
The function \code{hoeffDPlot} will create the violin plot and jitter plot 
according to the specified order given by the colnames of \code{df}. 
\code{hoeffDPlot} will thus internally refactor the \code{colnames} of the 
supplied \code{data.frame} according to the order of the \code{colnames}.
}
\examples{
## create se
set.seed(1)
a <- matrix(rnorm(10000), nrow = 1000, ncol = 10, 
            dimnames = list(seq_len(1000), paste("sample", seq_len(10))))
a[c(1, 5, 8), seq_len(5)] <- NA
cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
    rowData = rD, colData = cD)

tbl <- MAvalues(se, log = FALSE, group = "all")
hd_r <- hoeffDValues(tbl, "raw")

## normalized values
se_n <- se
assay(se_n) <- normalizeAssay(a, "sum")
tbl_n <- MAvalues(se_n, log = FALSE, group = "all")
hd_n <- hoeffDValues(tbl_n, "normalized")

df <- data.frame(raw = hd_r, normalized = hd_n)
hoeffDPlot(df, lines = TRUE)
hoeffDPlot(df, lines = FALSE)

}
