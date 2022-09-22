#' @name samplesMeasuredMissing
#'
#' @title Create tibble containing number of measured/missing features 
#' of samples
#'
#' @description \code{samplesMeasuredMissing} returns a \code{tbl} with 
#' the number of measured/missing
#' features of samples. The function will take as input a 
#' \code{SummarizedExperiment} object and will access its \code{assay()} slot
#'
#' @param se \code{SummarizedExperiment} object
#' 
#' @return \code{tbl} with number of measured/missing features per sample
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
#' samplesMeasuredMissing(se)
#' 
#' @importFrom SummarizedExperiment assay
#' 
#' @export
samplesMeasuredMissing <- function(se) {
    
    a <- SummarizedExperiment::assay(se)
    sum_na <- colSums(is.na(a))
    
    ## count per column the missing/measured values
    tibble::tibble(name = colnames(a),
        measured = nrow(a) - sum_na, missing = sum_na)
}

#' @name barplotSamplesMeasuredMissing
#'
#' @title Barplot of number of measured/missing features of samples
#'
#' @description \code{barplotSamplesMeasuredMissing} plots the number of 
#' measured/missing features of samples as a barplot. The function will 
#' take as input the returned \code{tbl} of \code{samplesMeasuredMissing}. 
#'
#' @param tbl \code{tbl} object
#' @param measured \code{logical}, should the number of measured or missing 
#' values be plotted
#' 
#' @return \code{gg} object from \code{ggplot2}
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
#' ## create the data.frame with information on number of measured/missing
#' ## values
#' tbl <- samplesMeasuredMissing(se) 
#'
#' ## plot number of measured values
#' barplotSamplesMeasuredMissing(tbl, measured = TRUE)
#'
#' ## plot number of missing values
#' barplotSamplesMeasuredMissing(tbl, measured = FALSE)
#'
#' @importFrom ggplot2 ggplot geom_bar aes_string ylab theme_classic xlab theme
#' @importFrom ggplot2 element_text
#' @importFrom plotly ggplotly
#' 
#' @export
barplotSamplesMeasuredMissing <- function(tbl, measured = TRUE) {
    
    ## create a barplot with the number of measured/missing features per sample
    g <- ggplot2::ggplot(tbl) 
    if (measured) 
        g <- g + ggplot2::geom_bar(
            ggplot2::aes_string(x = "name", y = "measured"), stat = "identity") +
            ggplot2::ylab("number of measured features")
    if (!measured) 
        g <- g + ggplot2::geom_bar(
            ggplot2::aes_string(x = "name", y = "missing"), stat = "identity") +
            ggplot2::ylab("number of missing features")
    g <- g + ggplot2::theme_classic() + ggplot2::xlab("sample") + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    plotly::ggplotly(g)
} 

#' @name histFeature
#'
#' @title Histogram for measured value per feature
#'
#' @description 
#' The function \code{histFeature} creates a histogram with the number
#' of measured/missing values per feature.
#'
#' @param x \code{matrix} containing intensities. Missing values are encoded 
#' as \code{NA}.
#' @param measured \code{logical}, should the measured values 
#' (\code{measured = TRUE}) or missing values (\code{measured = FALSE}) be taken
#' @param ... additional parameters passed to \code{geom_histogram}, e.g. 
#' \code{binwidth}.
#'
#' @return \code{plotly} object from \code{ggplotly}
#' 
#' @examples
#' x <- matrix(c(c(1, 1, 1), c(1, NA, 1), c(1, NA, 1), 
#'     c(1, 1, 1), c(NA, 1, 1), c(NA, 1, 1)), byrow = FALSE, nrow = 3)
#' colnames(x) <- c("A_1", "A_2", "A_3", "B_1", "B_2", "B_3")
#' histFeature(x, binwidth = 1)
#' 
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot geom_histogram xlab ylab ggtitle theme_classic
#' @importFrom plotly ggplotly
#' @export
histFeature <- function(x, measured = TRUE, ...) { 
    
    ## retrieve the measured values per compound and store the values in a 
    ## tibble
    if (measured) {
        val <- !is.na(x)   
        title <- "Measured values"
        x_lab <- "number of measured values per feature"
    } else { ## missing
        val <- is.na(x)
        title <- "Missing values"
        x_lab <- "number of missing values per feature"
    }
    val <- rowSums(val)
    val <- tibble::tibble(values = val)
    
    ## plotting
    g <- ggplot2::ggplot(val, aes_string(x = "values")) + 
        ggplot2::geom_histogram(...) + 
        ggplot2::xlab(x_lab) + 
        ggplot2::ylab("number of features") + 
        ggplot2::ggtitle(title) + 
        ggplot2::theme_classic()
    plotly::ggplotly(g)
    
    
}


#' @name measuredCategory
#' 
#' @title Obtain the number of measured intensities per sample type
#' 
#' @description 
#' The function \code{measuredCategory} creates a \code{tbl} with
#' the number of measured values per feature. 0 means that there were only 
#' missing values (\code{NA}) for the feature and sample type. 
#' \code{measuredCategory} will return a \code{tbl} where columns are the 
#' unique sample types and rows are the features as in \code{assay(se)}.
#' 
#' @details 
#' \code{measuredCategory} is a helper function. 
#' 
#' @param se \code{SummarizedExperiment}
#' @param measured \code{logical}, should the measured values 
#' (\code{measured = TRUE}) or missing values (\code{measured = FALSE}) 
#' be taken
#' @param category \code{character}, corresponds to a column name in 
#' \code{colData(se)}
#' 
#' @return \code{matrix} with number of measured/missing features per 
#' \code{category} type
#' 
#' @examples
#' ## create se
#' set.seed(1)
#' a <- matrix(rnorm(100), nrow = 10, ncol = 10,
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a,
#'     rowData = rD, colData = cD)
#'
#' measuredCategory(se, measured = TRUE, category = "type")
#'
#' @importFrom SummarizedExperiment assay
#' @importFrom tibble as_tibble
#'
#' @export
measuredCategory <- function(se, measured = TRUE, category = "type") {
    
    ## access the colData slot and add the rownames as a new column to cD
    ## (will add the column "rowname")
    cD <- se@colData
    category <- match.arg(category, choices = colnames(cD))
    colnames(cD) <- make.names(colnames(cD))
    category <- make.names(category)
    
    ## access the assay slot
    a <- SummarizedExperiment::assay(se)
    
    ## get the sample category levels and create a vector with the unique
    ## sample category levels
    samp <- cD[[category]]
    samp_u <- unique(samp)

    ## create the data.frame to store the values, the data.frame has the
    ## dimensions: nrow(a)/number of features as in a and number of 
    ## unique sample types
    mat_samp <- matrix(NA, nrow = nrow(a), ncol = length(samp_u),
        dimnames = list(rownames(a), samp_u))

    ## iterate through the columns and write to the respective column if 
    ## the feature was measured in (at least) one sample of this type
    for (i in samp_u) {
        a_samp_i <- a[, samp == i, drop = FALSE]
        if (measured) {
            mat_samp[, i] <- rowSums(!is.na(a_samp_i))    
        } else { ## missing
            mat_samp[, i] <- rowSums(is.na(a_samp_i))
        }
    }
    #data.frame(feature = rownames(mat_samp), mat_samp)
    mat_samp
}

## compounds per class
#' @name histFeatureCategory
#'
#' @title Histogram of features per sample type
#'
#' @description The function \code{histFeatureCategory} creates histogram
#' plots for each sample type in \code{se}.
#'
#' @param se \code{SummarizedExperiment}, the assay slot contains the intensity 
#' values per sample. Missing values are encoded as \code{NA}.
#' @param measured \code{logical}, should the measured values 
#' (\code{measured = TRUE}) or missing values (\code{measured = FALSE}) be taken 
#' @param category \code{character}, corresponding to a column in 
#' \code{colData(se)}
#' @param... additional parameters passed to \code{geom_histogram}, e.g. 
#' \code{binwidth}.
#' 
#' @return \code{plotly} object from \code{ggplotly}
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
#' histFeatureCategory(se, measured = TRUE, category = "type")
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes_string geom_histogram facet_grid ggtitle
#' @importFrom ggplot2 xlab ylab theme_classic theme
#' @importFrom plotly ggplotly
#'
#' @export
histFeatureCategory <- function(se, measured = TRUE, 
    category = "type", ...) {

    ## create data frame with columns as unique sample type
    mat_type <- measuredCategory(se, measured = measured, 
        category = category)

    ## create a tibble in long format and prepare for plotting (exclude column
    ## feature)
    tbl_type_l <- mat_type |> 
        as.data.frame() |> 
        tidyr::pivot_longer(cols = 1:ncol(mat_type))
    
    if (measured) {
        title <- "Measured values"
        x_lab <- "number of measured values per feature"
    } else {
        title <- "Missing values"
        x_lab <- "number of missing values per feature"
    }

    p <- ggplot2::ggplot(tbl_type_l, ggplot2::aes_string(x = "value")) + 
        ggplot2::geom_histogram(ggplot2::aes_string(fill = "name"), ...) +
        ggplot2::facet_grid(. ~ name) + ggplot2::ggtitle(title) + 
        ggplot2::xlab(x_lab) + ggplot2::ylab("number of features") +
        ggplot2::theme_classic() + ggplot2::theme(legend.position = "none")
    plotly::ggplotly(p)
}

## frequencies of missing values per compound with respect to class, 
## sample type, study groups to assess differences in occurences
#' @name upsetCategory
#'
#' @title UpSet plot to display measures values across sample types
#'
#' @description 
#' The function \code{upsetCategory} displays the frequency of measured values 
#' per feature with respect to class/sample type to assess difference in
#' occurrences. Internally, the measured values per sample are obtained via
#' the \code{measuredCategory} function: this function will access the number
#' of measured/missing values per category and feature. From this, a binary 
#' \code{tbl} will be created specifying if the feature is present/missing, 
#' which will be given to the \code{upset} function from the \code{UpSetR} 
#' package.
#'
#' @details 
#' Presence is defined by a feature being measured in at least one 
#' sample of a set.
#' 
#' Absence is defined by a feature with only missing values (i.e. 
#' no measured values) of a set.
#' 
#' @param se \code{SummarizedExperiment},
#'  containing the intensity values in \code{assay(se)}, missing values are
#' encoded by \code{NA}
#' @param category \code{character}, corresponding to a column in 
#' \code{colData(se)}
#' @param measured \code{logical}, should the measured values 
#' (\code{measured = TRUE}) 
#' or missing values (\code{measured = FALSE}) be taken 
#'
#' @return \code{upset} plot
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
#' upsetCategory(se, category = "type")
#' 
#' @importFrom UpSetR upset
#' 
#' @export
upsetCategory <- function(se, category = colnames(colData(se)), measured = TRUE) {
    
    category <- match.arg(category)
    ## create data frame with columns as unique sample type
    mat_type <- measuredCategory(se, category = category, measured = measured) 
    
    if (measured) 
        ## Presence is defined by a feature being measured in at least one 
        ## sample of a set
        mat_bin <- ifelse(mat_type > 0, 1, 0) 
    else {
        ## Absence is defined by a feature with only missing values (i.e. 
        ## no measured values) of a set. 
        cD <- se@colData
        category_tab <- table(cD[[category]])
        category_tab <- category_tab[colnames(mat_type)]
        mat_bin <- ifelse(mat_type == as.numeric(category_tab), 1, 0)
    }
    if (sum(colSums(mat_bin) > 0) > 1) {
        mat_bin <- as.data.frame(mat_bin)
        UpSetR::upset(mat_bin, order.by = "freq", nsets = ncol(mat_type))
    } else {
        NULL
    }
}

#' @name extractComb
#'
#' @title Obtain the features that are present in a specified set
#'
#' @description
#' The function \code{extractComb} extracts the features that match a
#' \code{combination} depending if the features was measured or missing. 
#' The function will return the sets that match the \code{combination}, 
#' thus, the function might be useful when answering questions about which 
#' features are measured/missing under certain combinations (e.g. sample 
#' types or experimental conditions).
#' 
#' @details 
#' The function \code{extractComb} uses the \code{make_comb_mat} function from 
#' \code{ComplexHeatmap} package.
#' 
#' Presence is defined by a feature being measured in at least one sample of a 
#' set.
#' 
#' Absence is defined by a feature with only missing values (i.e. no measured 
#' values) of a set. 
#'
#' @param se \code{SummarizedExperiment}
#' @param combination \code{character}, refers to factors in \code{category}
#' @param measured \code{logical}
#' @param category \code{character}, corresponding to a column name in 
#' \code{colData(se)}
#'
#' @return \code{character}
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
#' 
#' @export
extractComb <- function(se, combination, measured = TRUE, category = "type") {
    
    ## obtain the number of measured samples per type and create a binary matrix
    mat_type <- measuredCategory(se = se, measured = measured, category = category)
    feat <- rownames(mat_type)
    
    ## create the binary matrix
    if (measured) {
        ## Presence is defined by a feature being measured in at least one 
        ## sample of a set
        mat_bin <- ifelse(mat_type > 0, 1, 0) 
    } else {
        ## Absence is defined by a feature with only missing values (i.e. 
        ## no measured values) of a set. 
        cD <- se@colData
        category_tab <- table(cD[[category]])
        category_tab <- category_tab[colnames(mat_type)]
        mat_bin <- ifelse(mat_type == as.numeric(category_tab), 1, 0)
    }
    
    ## create the combination matrix object
    comb_mat <- ComplexHeatmap::make_comb_mat(mat_bin, mode = "distinct")
    
    ## create a character vector that contains the combination in a binary 
    ## format from the combination argument (character vector with types)
    combination_num <- ifelse(colnames(mat_bin) %in% combination, 1, 0)
    combination_num <- paste(combination_num, collapse = "")
    
    ## return the names of the features that match the combination
    if (combination_num %in% ComplexHeatmap::comb_name(comb_mat)) {
        res <- ComplexHeatmap::extract_comb(comb_mat, combination_num)
    } else {
        res <- "no features for this combination"
    }
    
    res
}

