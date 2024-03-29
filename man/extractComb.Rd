% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_measuredvalues_missingvalues.R
\name{extractComb}
\alias{extractComb}
\title{Obtain the features that are present in a specified set}
\usage{
extractComb(se, combination, measured = TRUE, category = "type")
}
\arguments{
\item{se}{\code{SummarizedExperiment}}

\item{combination}{\code{character}, refers to factors in \code{category}}

\item{measured}{\code{logical}}

\item{category}{\code{character}, corresponding to a column name in 
\code{colData(se)}}
}
\value{
\code{character}
}
\description{
The function \code{extractComb} extracts the features that match a
\code{combination} depending if the features was measured or missing. 
The function will return the sets that match the \code{combination}, 
thus, the function might be useful when answering questions about which 
features are measured/missing under certain combinations (e.g. sample 
types or experimental conditions).
}
\details{
The function \code{extractComb} uses the \code{make_comb_mat} function from 
\code{ComplexHeatmap} package.

Presence is defined by a feature being measured in at least one sample of a 
set.

Absence is defined by a feature with only missing values (i.e. no measured 
values) of a set.
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
se <- SummarizedExperiment::SummarizedExperiment(assay = a, rowData = rD, colData = cD)

extractComb(se, combination = "2", measured = TRUE, category = "type") 

}
