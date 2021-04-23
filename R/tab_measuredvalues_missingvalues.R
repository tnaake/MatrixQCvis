#' @name samples_memi
#'
#' @title Create tibble containing number of measured/missing features 
#' of samples
#'
#' @description `samples_memi` returns a `tibble` with 
#' the number of measured/missing
#' features of samples. The function will take as input a 
#' `SummarizedExperiment` object and will access its `assay()` slot
#'
#' @param se `SummarizedExperiment` object
#' 
#' @return `tibble`
#' 
#' @examples 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' sample <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' featData <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = featData, colData = sample)
#' 
#' ## create the data.frame with information on number of measured/missing
#' ## values
#' samples_memi(se)
#' 
#' @importFrom rlang .data
#' 
#' @export
samples_memi <- function(se) {
    
    a <- SummarizedExperiment::assay(se)
    
    ## count per column the missing/measured values depending on the 
    a <- as.data.frame(a) 
    a_l <- tidyr::pivot_longer(data = a, cols = seq_len(ncol(a)))
    a_l <- dplyr::group_by(.data = a_l, .data$name)
    a_l <- dplyr::summarise(.data = a_l, measured = sum(!is.na(.data$value)), 
        missing = sum(is.na(.data$value)))
    return(a_l)
}

#' @name barplot_samples_memi
#'
#' @title Barplot of number of measured/missing features of samples
#'
#' @description `barplot_samples_memi` plots the number of measured/missing
#' features of samples as a barplot. The function will take as input the 
#' returned `tibble` of `samples_memi`. 
#'
#' @param tbl `tibble` object
#' @param measured `logical`, should the number of measured or missing values
#' be plotted
#' 
#' @return `gg` object from `ggplot2`
#' 
#' @examples 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' sample <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' featData <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = featData, colData = sample)
#'
#' ## create the data.frame with information on number of measured/missing
#' ## values
#' tbl <- samples_memi(se) 
#'
#' ## plot number of measured values
#' barplot_samples_memi(tbl, measured = TRUE)
#'
#' ## plot number of missing values
#' barplot_samples_memi(tbl, measured = FALSE)
#'
#' @importFrom ggplot2 ggplot geom_bar aes_string ylab theme_bw xlab theme
#' @importFrom ggplot2 element_text
#' @importFrom plotly ggplotly
#' 
#' @export
barplot_samples_memi <- function(tbl, measured = TRUE) {
    
    ## create a barplot with the number of measured/missing features per sample
    g <- ggplot2::ggplot(tbl) 
    if (measured) 
        g <- g + 
            ggplot2::geom_bar(ggplot2::aes_string(x = "name", y = "measured"), 
                                                        stat = "identity") +
            ggplot2::ylab("number of measured features")
    if (!measured) 
        g <- g + 
            ggplot2::geom_bar(ggplot2::aes_string(x = "name", y = "missing"), 
                                                        stat = "identity") +
            ggplot2::ylab("number of missing features")
    g <- g + ggplot2::theme_bw() + ggplot2::xlab("sample") + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    plotly::ggplotly(g)
} 

#' @name hist_feature
#'
#' @title Histogram for measured value per feature
#'
#' @description 
#' The function `hist_compound` creates a histogram with the number
#' of measured values per feature
#'
#' @param x `matrix` containing intensities. Missing values are encoded 
#' as `NA`.
#' @param measured `logical`, should the measured values 
#' (`measured = TRUE`) or missing values (`measured = FALSE`) be taken
#' @param ... additional parameters passed to `geom_histogram`, e.g. 
#' `binwidth`.
#'
#' @return `gg` object from `ggplot2`
#' 
#' @examples
#' x <- matrix(c(c(1, 1, 1), c(1, NA, 1), c(1, NA, 1), 
#'     c(1, 1, 1), c(NA, 1, 1), c(NA, 1, 1)), byrow = FALSE, nrow = 3)
#' colnames(x) <- c("A_1", "A_2", "A_3", "B_1", "B_2", "B_3")
#' hist_feature(x, binwidth = 1)
#' 
#' @export
hist_feature <- function(x, measured = TRUE, ...) { 
    
    ## retrieve the measured values per compound and store the values in a 
    ## tibble
    if (measured) {
        val <- !is.na(x)   
        title <- "Measured values"
        x_lab <- "number of measured features"
    } else { ## missing
        val <- is.na(x)
        title <- "Missing values"
        x_lab <- "number of missing features"
    }
    val <- rowSums(val)
    val <- tibble::tibble(values = val)
    
    ## plotting
    p <- ggplot2::ggplot(val, aes_string(x = "values")) + 
        ggplot2::geom_histogram(...) + 
        ggplot2::xlab(x_lab) + 
        ggplot2::ylab("number of samples") + 
        ggplot2::ggtitle(title) + 
        ggplot2::theme_bw()
    plotly::ggplotly(p)
}

## measurement statuses

## number of missing values per samples (hy histograms for different sample 
## types to contain consistent distribution and reasonable superiority of 
## QC samples in terms of detection rate)

#' @name measured_category
#' 
#' @title Obtain the number of measured intensities per sample type
#' 
#' @description 
#' The function `measured_category` creates a `tibble` with
#' the number of measured values per feature. 0 means that there were only 
#' missing values (`NA`) for the feature and sample type. 
#' `measured_category` will return a `tibble` where columns are the 
#' unique sample types and rows are the features as in `assay(se)`.
#' 
#' @details 
#' `measured_category` is a helper function. 
#' 
#' @param se `SummarizedExperiment`
#' @param measured `logical`, should the measured values 
#' (`measured = TRUE`) or missing values (`measured = FALSE`) be taken
#' @param category `character`, corresponds to a column name in `colData(se)`
#' 
#' @return `tibble`
#' 
#' @examples 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' measured_category(se, measured = TRUE, category = "type")
#' 
#' @export
measured_category <- function(se, measured = TRUE, category = "type") {
    
    ## retrieve assay and colData from se
    a <- SummarizedExperiment::assay(se)
    cD <- SummarizedExperiment::colData(se)
    
    ## get the sample category levels and create a vector with the unique
    ## sample category levels
    samp <- cD[[category]]
    samp_u <- unique(samp)
    
    ## create the data.frame to store the values, the `data.frame` has the
    ## dimensions: nrow(a)/number of features as in a and number of 
    ## unique sample types
    tbl_type <- matrix(NA, nrow = nrow(a), ncol = length(samp_u), 
                                    dimnames = list(rownames(a), samp_u)) %>% 
        tibble::as_tibble(rownames = "feature")
    
    ## iterate through the columns and write to the respective column if 
    ## the feature was measured in (at least) one sample of this type
    for (i in seq_along(samp_u)) {
        col_inds <- samp == samp_u[i]
        a_i <- a[, col_inds, drop = FALSE]
        if (measured) {
            a_i_val <- rowSums(!is.na(a_i))    
        } else { ## missing
            a_i_val <- rowSums(is.na(a_i))
        }
        tbl_type[, samp_u[i]] <- a_i_val
    }
    
    return(tbl_type)
}

## compounds per class
#' @name hist_feature_category
#'
#' @title Histogram of features per sample type
#'
#' @description The function `hist_feature_category` creates histogram
#' plots for each sample type in `se`.
#'
#' @param se `SummarizedExperiment`, the assay slot contains the intensity 
#' values per sample. Missing values are encoded as `NA`.
#' @param measured `logical`, should the measured values 
#' (`measured = TRUE`) or missing values (`measured = FALSE`) be taken 
#' @param category `character`, corresponding to a column in `colData(se)`
#' @param... additional parameters passed to `geom_histogram`, e.g. `binwidth`.
#' 
#' @return `gg` object from `ggplot2`
#'
#' @examples
#' 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' hist_feature_category(se, measured = TRUE, category = "type")
#' 
#' @export
hist_feature_category <- function(se, measured = TRUE, 
    category = "type", ...) {

    ## create data frame with columns as unique sample type
    tbl_type <- measured_category(se, measured = measured, 
        category = category)

    ## create a tibble in long format and prepare for plotting (exclude column
    ## feature)
    tbl_type_l <- tidyr::pivot_longer(tbl_type, cols = 2:ncol(tbl_type))
    
    if (measured) {
        title <- "Measured values"
        x_lab <- "number of measured features"
    } else {
        title <- "Missing values"
        x_lab <- "number of missing features"
    }

    p <- ggplot2::ggplot(tbl_type_l, ggplot2::aes_string(x = "value")) + 
        ggplot2::geom_histogram(ggplot2::aes_string(fill = "name"), ...) +
        ggplot2::facet_grid(. ~ name) + ggplot2::ggtitle(title) + 
        ggplot2::xlab(x_lab) + ggplot2::ylab("number of samples") +
        ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")
    plotly::ggplotly(p)
}

## frequencies of missing values per compound with respect to class, 
## sample type, study groups to assess differences in occurences
#' @name upset_category
#'
#' @title UpSet plot to display measures values across sample types
#'
#' @description 
#' The function `upset_category` displays the frequency of measured values per
#' feature with respect to class/sample type to assess difference in
#' occurences. Internally, the measured values per sample are obtained via
#' the `measured_category` function: this function will 
#'
#' @param se `SummarizedExperiment`,
#'  containing the intensity values in `assay(se)`, missing values are
#' encoded by `NA`
#' @param category `character`, corresponding to a column in `colData(se)`
#' @param ... additional parameters passed to `measured_category`
#'
#' @return 
#' `UpSet` plot
#' 
#' @examples 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' upset_category(se, category = "type")
#' 
#' @importFrom UpSetR upset
#' @importFrom dplyr select
#' 
#' @export
upset_category <- function(se, category = "type", ...) {
    
    ## create data frame with columns as unique sample type
    tbl_type <- measured_category(se, category = category, ...)
    tbl_type <- dplyr::select(tbl_type, -"feature")
    
    tbl_type_binary <- ifelse(tbl_type > 0, 1, 0) 
    tbl_type_binary <- as.data.frame(tbl_type_binary)
    UpSetR::upset(tbl_type_binary, order.by = "freq", nsets = ncol(tbl_type))
}

#' @name extractComb
#'
#' @title Obtain the features that are present in a specified set
#'
#' @description
#' The function `extractComb` extracts the features that are 
#'
#' @param se `SummarizedExperiment`
#' @param combination `character`
#' @param measured `logical`
#' @param category `character`, corresponding to a column name in `colData(se)`
#'
#' @return `character`
#' 
#' @examples
#' 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, rowData = rD, colData = cD)
#' 
#' extractComb(se, combination = "2", measured = TRUE, category = "type") 
#' 
#' @importFrom ComplexHeatmap make_comb_mat comb_name extract_comb
#' @importFrom dplyr pull select
#' 
#' @export
extractComb <- function(se, combination, measured = TRUE, category = "type") {
    
    ## obtain the number of measured samples per type and create a binary matrix
    tbl <- measured_category(se = se, measured = measured, category = category)
    feat <- dplyr::pull(tbl, "feature")
    tbl_nf <- dplyr::select(tbl, -"feature")
    
    ## create the binary matrix
    tbl_bin <- ifelse(tbl_nf > 0, 1, 0)
    
    ## create the combination matrix object
    comb_mat <- ComplexHeatmap::make_comb_mat(tbl_bin, mode = "distinct")
    
    ## create a character vector that contains the combination in a binary 
    ## format from the combination argument (character vector with types)
    combination_num <- ifelse(colnames(tbl_bin) %in% combination, 1, 0)
    combination_num <- paste(combination_num, collapse = "")
    
    ## return the names of the features that match the combination
    if (combination_num %in% ComplexHeatmap::comb_name(comb_mat)) {
        res <- ComplexHeatmap::extract_comb(comb_mat, combination_num)
        res <- as.character(feat[res])
    } else {
        res <- "no features for this combination"
    }
    
    return(res)
}

