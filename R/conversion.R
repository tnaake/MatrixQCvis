#' @name biocrates
#' 
#' @title Convert Biocrates xlsx output to `SummarizedExperiment` object
#' 
#' @description 
#' The function `biocrates` will create a `SummarizedExperiment` from a
#' Biocrates xlsx file. 
#' The function `biocrates` takes as input the path to a .xlsx file 
#' (Biocrates output) and additional parameters given to the `read.xlsx` 
#' function from the `xlsx` package (e.g. specifying the sheet name by
#' `sheetName` or the sheet index by `sheetIndex`).
#' 
#' @details 
#' The column "Sample Identification" has to contain unique identifiers
#' (no duplications).
#' 
#' @param file `character`
#' @param sheetName `character`, given to `read.xslx`
#' @param ... additional parameters given to `read.xslx`
#' 
#' @examples
#' file <- "path/to/biocrates/object"
#' 
#' \dontrun{
#' biocrates(file = file, sheetName = NULL, ...)
#' }
#' 
#' @usage biocrates(file, sheetName = NULL, ...)
#' @return 
#' `SummarizedExperiment` object
#' 
#' @export
#' 
#' @importFrom xlsx read.xlsx
#' @importFrom dplyr select
#' @importFrom SummarizedExperiment SummarizedExperiment
file <- "Projects/20210112_Hagen_plasma_serum_metabolites/2020-12-11_Results_Plate1_not_normalized.xlsx"
biocrates <- function(file, sheetName = NULL, ...) {
    
    xls <- read.xlsx(file, header = TRUE, ...)
    
    ## colnames is in the first row, assign and remove the first row
    colnames(xls) <- xls[1, ]
    xls <- xls[-1, ]
    
    ## find the columns that contain the metabolites, row 1 contains class,
    ## row 2 contains LOD (is NA for columns not containing the metabolites)
    inds_met <- !is.na(xls[2, ])
    ## set the first TRUE value to FALSE since it contains the label of the row
    inds_met[which(inds_met)[1]] <- FALSE
    
    ## create rowData 
    rD <- data.frame(name = colnames(xls)[inds_met], 
                class = as.character(xls[1, inds_met]))
    rownames(rD) <- rD[["feature"]]
    
    ## find the rows that contain the samples
    inds_name <- !is.na(xls[, 1])
    
    ## create colData
    ## rename column "Sample Identification" to "name" and move to the beginning
    ## of cD
    cD <- xls[inds_name, 1:(min(which(inds_met))-1)]
    cD <- data.frame(name = cD[, "Sample Identification"], cD)
    cD <- select(cD, -c("Sample.Identification"))
    rownames(cD) <- cD[["name"]]
    
    ## create assay, set values of 0 to NA
    a <- xls[inds_name, inds_met]
    a <- as.matrix(a)
    mode(a) <- "numeric"
    a[a == 0] <- NA
    rownames(a) <- cD[["name"]]

    ## create SummarizedExperiment
    se <- SummarizedExperiment(assays = t(a), rowData = rD, colData = cD)
    
    return(se)
}
