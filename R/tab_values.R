#' @name createBoxplot
#'
#' @title Create a boxplot of (count/intensity) values per sample
#'
#' @description
#' The function \code{create_boxplot} creates a boxplot per sample for the 
#' intensity/count values.
#' 
#' @details
#' Internal usage in \code{shinyQC}.
#'
#' @param se \code{SummarizedExperiment} containing the (count/intensity) values 
#' in the \code{assay} slot
#' @param orderCategory \code{character}, one of \code{colnames(colData(se))}
#' @param title \code{character} or \code{numeric} of \code{length(1)}
#' @param log2 \code{logical}, if \code{TRUE} (count/intensity) values are 
#' displayed as log2 values
#' @param violin \code{logical}, if \code{FALSE} a boxplot is created, if 
#' \code{TRUE} a violin plot is created
#'
#' @examples
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'     dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' createBoxplot(se, orderCategory = "name", title = "", log2 = TRUE, 
#'     violin = FALSE)
#' 
#' @return \code{gg} object from \code{ggplot2}
#' 
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_longer 
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom SummarizedExperiment assay colData
#' @importFrom ggplot2 ggplot aes_string geom_boxplot geom_violin
#' @importFrom ggplot2 scale_x_discrete theme element_text ggtitle xlab
#' 
#' @export
createBoxplot <- function(se, orderCategory = colnames(colData(se)), 
    title = "", log2 = TRUE, violin = FALSE) {

    
    ## match arguments for order
    orderCategory <- match.arg(orderCategory)

    ## access the assay slot
    a <- SummarizedExperiment::assay(se)
    
    ## access the colData slot and add the rownames as a new column to cD
    ## (will add the column "x5at1t1g161asy")
    cD <- SummarizedExperiment::colData(se) |> as.data.frame()
    if (!all(colnames(a) == rownames(cD)))
        stop("colnames(assay(se)) do not match rownames(colData(se))")
    cD <- tibble::rownames_to_column(cD, var = "x5at1t1g161asy")

    ## pivot_longer will create the columns name (containing the colnames of a)
    ## and value (containing the actual values)
    a <- tibble::as_tibble(a) 
    a_l <- tidyr::pivot_longer(data = a, cols = seq_len(ncol(a)))

    ## take log2 values if log2 = TRUE
    if (log2) a_l$value <- log2(a_l$value)
    
    ## add another column that gives the order of plotting (will be factor)
    ## order alphabetically: combine the levels of the orderCategory and add 
    ## the name (to secure that the levels are unique)
    ## add another column for the x_values
    a_l <- dplyr::left_join(x = a_l, y = cD, 
        by = c("name" = "x5at1t1g161asy"), copy = TRUE)
    a_o <- paste(a_l[[orderCategory]], a_l[["name"]])
    a_o_u <- unique(a_o)
    a_l$x_ggplot_vals <- factor(x = a_o, levels = sort(a_o_u))
    
    ## do the actual plotting
    g <- ggplot2::ggplot(a_l, 
        ggplot2::aes_string(y = "value", x = "x_ggplot_vals"))

    if (!violin) { 
        g <- g + ggplot2::geom_boxplot() 
    } else { ## violin == TRUE
        g <- g + ggplot2::geom_violin()
    }
    
    g <- g + 
        ggplot2::scale_x_discrete(labels = unique(a_l[["name"]])[order(a_o_u)])
    g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) + 
        ggplot2::ggtitle(title) + ggplot2::xlab("samples")
}


#' @name driftPlot
#' 
#' @title Plot the trend line for aggregated values
#' 
#' @description 
#' The function \code{driftPlot} aggregates the (count/intensity) values from the 
#' \code{assay()} slot of a \code{SummarizedExperiment} by the \code{median} or 
#' \code{sum} of the (count/intensity) values. \code{driftPlot} then visualizes 
#' these aggregated values and adds a trend line (using either LOESS or a 
#' linear model) from (a subset of) the aggregated values. The subset is 
#' specified by the arguments \code{category} and \code{level}.
#' 
#' @details 
#' The x-values are sorted according to the \code{orderCategory} argument: The 
#' levels of the corresponding column in \code{colData(se)} are pasted with the 
#' sample names (in the column \code{name}) and factorized.
#' Internal usage in \code{shinyQC}.
#' 
#' @param se \code{SummarizedExperiment}
#' @param aggregation \code{character}, type of aggregation of (count/intensity) 
#' values
#' @param category \code{character}, column of \code{colData(se)}
#' @param orderCategory \code{character}, column of \code{colData(se)}
#' @param level \code{character}, from which samples should the LOESS curve be
#' calculated, either \code{"all"} or one of the levels of the selected columns
#' of \code{colData(se)} (\code{"category"}) 
#' @param method \code{character}, either \code{"loess"} or \code{"lm"}
#' 
#' @return \code{gg} object from \code{ggplot2}
#' 
#' @examples 
#' #' ## create se
#' set.seed(1)
#' a <- matrix(rnorm(1000), nrow = 10, ncol = 100, 
#'     dimnames = list(1:10, paste("sample", 1:100)))
#' a[c(1, 5, 8), 1:5] <- NA
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 50), rep("2", 50)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' driftPlot(se, aggregation = "sum", category = "type", 
#'     orderCategory = "type", level = "1", method = "loess")
#' 
#' @importFrom dplyr across left_join summarise starts_with
#' @importFrom tidyr pivot_longer 
#' @importFrom stats median
#' @importFrom SummarizedExperiment assay colData
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth theme_classic
#' @importFrom ggplot2 scale_color_manual scale_x_discrete xlab ylab theme
#' @importFrom ggplot2 element_text
#' @importFrom plotly ggplotly style 
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom rlang .data
#' 
#' @export
driftPlot <- function(se, aggregation = c("median", "sum"), 
        category = colnames(colData(se)), orderCategory = colnames(colData(se)), 
        level = c("all", unique(colData(se)[, category])),
        method = c("loess", "lm")) {
    
    aggregation <- match.arg(aggregation)
    category <- match.arg(category)
    category <- make.names(category)
    orderCategory <- match.arg(orderCategory)
    orderCategory <- make.names(orderCategory)
    level <- match.arg(level)
    method <- match.arg(method)
    
    ## access the assay slot
    a <- SummarizedExperiment::assay(se)
    
    ## access the colData slot and add the rownames as a new column to cD
    ## (will add the column "x5at1t1g161asy")
    cD <- SummarizedExperiment::colData(se) |> as.data.frame()
    colnames(cD) <- make.names(colnames(cD))
    if (!all(colnames(a) == rownames(cD)))
        stop("colnames(a) do not match rownames(colData(se))")
    cD <- tibble::rownames_to_column(cD, var = "x5at1t1g161asy")
    
    a <- tibble::as_tibble(a) 
    a_l <- tidyr::pivot_longer(data = a, cols = seq_len(ncol(a)))

    if (aggregation == "median") FUN <- median
    if (aggregation == "sum") FUN <- sum

    ## aggregate the values across the samples
    a_l <- dplyr::group_by(.data = a_l, .data$name)
    a_l <- dplyr::summarise(a_l, 
        dplyr::across(dplyr::starts_with("value"), FUN, na.rm = TRUE))

    ## join with cD
    tb <- dplyr::left_join(a_l, cD, 
        by = c("name" = "x5at1t1g161asy"), copy = TRUE)
    df <- as.data.frame(tb)

    df <- data.frame(df, col_ggplot_points = "all")
    df$name <- factor(df$name, sort(df$name))

    df_c <- df[, category]

    ## add another column that gives the order of plotting (will be factor)
    ## order alphabetically: combine the levels of the orderCategory and add 
    ## the name (to secure that the levels are unique)
    ## add another column for the x_values
    df_o <- df[, orderCategory]
    df_o_n <- paste(df_o, df$name)
    df$x_ggplot_vals <- factor(df_o_n, sort(df_o_n))

    ## create a separate data.frame which only contains the level or "all"
    if (level == "all") {
        df_subset <- df  
    } else {
        df_subset <- df[df_c == level, ]
    }
    df_subset$col_ggplot_points <- "subset"

    ## create another column which converts factors into numeric (needed for 
    ## geom_smooth)
    df_subset$x_ggplot_vals_num <- as.numeric(df_subset$x_ggplot_vals)

    g <- ggplot2::ggplot(df,
            ggplot2::aes_string(x = "x_ggplot_vals", y = "value", 
            col = "col_ggplot_points")) + 
        suppressWarnings(
            ggplot2::geom_point(ggplot2::aes_string(text = "name"))) + 
        ggplot2::geom_point(data = df_subset, 
            ggplot2::aes_string(x = "x_ggplot_vals", y = "value", 
            col = "col_ggplot_points")) + 
        ggplot2::geom_smooth(data = df_subset, 
            ggplot2::aes_string(x = "x_ggplot_vals_num", y = "value"), 
            method = method) +
        ggplot2::theme_classic() + 
        ggplot2::scale_color_manual(values = c("#000000", "#2A6CEF")) +
        ggplot2::scale_x_discrete(labels = df$name[order(df$x_ggplot_vals)]) +
        ggplot2::xlab("samples") + 
        ggplot2::ylab(paste(aggregation, "of values")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), 
            legend.position = "none")
    g <- plotly::ggplotly(g, tooltip = c("text"))
    g |> plotly::style(hoveron = "fills", traces = 2)
}


#' @name cv 
#' 
#' @title Calculate coefficient of variation
#' 
#' @description 
#' The function \code{cv} calculates the coefficient of variation from columns of 
#' a matrix. The coefficients of variation are calculated according to the 
#' formula \code{sd(y) / mean(y) * 100} with \code{y} the column values. 
#' 
#' @details
#' The function returned a named \code{list} (the name is specified by the 
#' \code{name} argument) containing the coefficient of variation of the 
#' columns of \code{x}.
#' 
#' @param x \code{matrix}
#' @param name \code{character}, the name of the returned list
#' 
#' @return \code{list}
#' 
#' @examples
#' x <- matrix(1:10, ncol = 2)
#' cv(x)
#' 
#' @importFrom stats sd
#' 
#' @export
cv <- function(x, name = "raw") {
    
    sd_v <- apply(x, 2, stats::sd, na.rm = TRUE)
    mean_v <- apply(x, 2, mean, na.rm = TRUE)
    cv_v <- sd_v / mean_v * 100
    
    ## create a named list and return the list
    cv_v <- list(cv_v)
    names(cv_v) <- name
    return(cv_v)
}

#' @name plotCV
#' 
#' @title Plot CV values
#' 
#' @description 
#' The function \code{plotCV} displays the coefficient of variation values of 
#' set of values supplied in a \code{data.frame} object. The function will 
#' create a plot using the \code{ggplot2} package and will print the values 
#' in the different columns in different colors.
#' 
#' @details 
#' Internal usage in \code{shinyQC}.
#' 
#' @param df \code{data.frame} containing one or multiple columns containing the 
#' coefficients of variation
#' 
#' @examples
#' x1 <- matrix(1:10, ncol = 2)
#' x2 <- matrix(11:20, ncol = 2)
#' x3 <- matrix(21:30, ncol = 2)
#' x4 <- matrix(31:40, ncol = 2)
#' 
#' ## calculate cv values
#' cv1 <- cv(x1, "x1")
#' cv2 <- cv(x2, "x2")
#' cv3 <- cv(x3, "x3")
#' cv4 <- cv(x4, "x4")
#' 
#' df <- data.frame(cv1, cv2, cv3, cv4)
#' plotCV(df)
#' 
#' @return \code{gg} object from \code{ggplot2}
#' 
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line xlab ylab
#' @importFrom ggplot2 theme_bw theme element_text
#' @export
plotCV <- function(df) {
    
    ## add a sample column
    if (!is.data.frame(df)) stop("df is not a data.frame")
    tbl <- tibble::tibble(sample = rownames(df), df)
    tbl$sample <- factor(x = tbl$sample, levels = tbl$sample)
    tbl <- tidyr::pivot_longer(tbl, cols = 2:ncol(tbl))
    
    ggplot2::ggplot(tbl, ggplot2::aes_string(x = "sample", y = "value")) + 
        ggplot2::geom_point(ggplot2::aes_string(color = "name")) + 
        ggplot2::geom_line(ggplot2::aes_string(group = "name", 
            color = "name")) + 
        ggplot2::xlab("sample") + ggplot2::ylab("coefficient of variation") + 
        ggplot2::theme_bw() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
}

#' @name ECDF
#'
#' @title Create ECDF plot of a sample against a reference
#'
#' @description
#' The function \code{ECDF} creates a plot of the empirical cumulative 
#' distribution function of a specified sample and an outgroup (reference). 
#' The reference is specified by the \code{group} argument. The row-wise 
#' (feature) mean values of the reference are calculated after excluding 
#' the specified \code{sample}.
#'
#' @details 
#' Internal use in \code{shinyQC}. 
#' 
#' The function \code{ECDF} uses the \code{ks.test} function from \code{stats} 
#' to perform a two-sample Kolmogorov-Smirnov test. The Kolmogorov-Smirnov 
#' test is run with the alternative \code{"two.sided"}
#' (null hypothesis is that the true distribution function of the 
#' \code{sample} is equal to the hypothesized distribution function of the 
#' \code{group}).
#' 
#' The \code{exact} argument in \code{ks.test} is set to \code{NULL}, meaning 
#' that an exact p-value is computed if the product of the sample sizes is 
#' less than 10000 of \code{sample} and \code{group}. Otherwise, asymptotic 
#' distributions are used whose approximations might be inaccurate in low 
#' sample sizes.
#' 
#' @param se \code{SummarizedExperiment} object
#' @param sample \code{character}, name of the sample to compare against the 
#' group
#' @param group \code{character}, either \code{"all"} or one of 
#' \code{colnames(colData(se))}
#' 
#' @examples
#' ## create se
#' set.seed(1)
#' a <- matrix(rnorm(1000), nrow = 100, ncol = 10, 
#'     dimnames = list(1:100, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = rD, colData = cD)
#' 
#' ECDF(se, sample = "sample 1", group = "all")
#' 
#' @importFrom SummarizedExperiment assay colData
#' @importFrom stats ks.test
#' @importFrom ggplot2 ggplot aes_string stat_ecdf theme_bw xlab ylab
#' @importFrom ggplot2 ggtitle theme element_blank
#' @importFrom tibble rownames_to_column
#' 
#' @return \code{gg} object from \code{ggplot2}
#' 
#' @export
ECDF <- function(se, sample = colnames(se), 
    group = c("all", colnames(colData(se)))) {

    ## match arguments
    sample <- match.arg(sample)
    group <- match.arg(group)
    group <- make.names(group)
    
    ## access the assay slot
    a <- SummarizedExperiment::assay(se)
    
    ## access the colData slot and add the rownames as a new column to cD
    ## (will add the column "x5at1t1g161asy")
    cD <- SummarizedExperiment::colData(se) |> as.data.frame()
    colnames(cD) <- make.names(colnames(cD))
    if (!all(colnames(a) == rownames(cD)))
        stop("colnames(a) do not match rownames(colData(se))")
    cD <- tibble::rownames_to_column(cD, var = "x5at1t1g161asy")
    
    df <- data.frame(value = a[, sample], type = sample)
    
    ## calculate sample-wise the values
    ## get indices 
    inds_l <- colnames(a) == sample
    inds_nl <- !inds_l
    
    ## truncate the indices based on the group (only use the group for the 
    ## comparison and the "outgroup")
    if (group != "all") {
        g <- cD[cD[, "x5at1t1g161asy"] == sample, group]
        inds_nl <- inds_nl & cD[, group] == g
    }
    
    rM <- rowMeans(a[, inds_nl], na.rm = TRUE)
    df_group <- data.frame(value = rM, type = "group")
    
    ## remove from df that contains the sample values and from df_group the 
    ## features that have in any of df/df_group NA values
    inds <- !(is.na(df_group$value) |  is.na(df$value))
    df <- df[inds, ]
    df_group <- df_group[inds, ]
    
    ## calculate KS statistic of data
    value_s <- df$value
    value_g <- df_group$value
    ks_test <- stats::ks.test(x = value_s, y = value_g, exact = NULL, 
        alternative = "two.sided")
    
    ## bind together
    df <- rbind(df, df_group)

    ggplot2::ggplot(df, 
        ggplot2::aes_string(x = "value", color = "type", group = "type")) + 
        ggplot2::stat_ecdf(size=1) + ggplot2::theme_bw() + 
        ggplot2::xlab(sample) + ggplot2::ylab("ECDF") +
        ggplot2::ggtitle(sprintf("K-S Test: D: %s, p-value: %s", 
            signif(ks_test$statistic, 3), 
            signif(ks_test$p.value, 3))) + 
        ggplot2::theme(legend.title = ggplot2::element_blank(), 
            legend.position ="top")
}

#' @name distShiny
#'
#' @title Create distance matrix from numerical matrix
#'
#' @description
#' The function \code{distShiny} takes as an input a numerical \code{matrix} or
#' \code{data.frame} and returns the distances between the rows and columns based
#' on the defined \code{method} (e.g. euclidean distance). 
#' 
#' @details 
#' Internal use in \code{shinyQC}.
#'
#' @param x \code{matrix} or \code{data.frame} with samples in columns and 
#' features in rows
#' @param method \code{character}, method for distance calculation
#'
#' @examples
#' x <- matrix(1:100, nrow = 10, ncol = 10, 
#'         dimnames = list(1:10, paste("sample", 1:10)))
#' distShiny(x = x)
#' 
#' @return \code{matrix}
#' 
#' @importFrom stats dist
#'
#' @export
distShiny <- function(x, method = "euclidean") {
    ## calculate the distance matrix
    d <- stats::dist(t(x), method = method, diag = TRUE)
    data.matrix(d)
}

#' @name distSample
#'
#' @title  Create a heatmap using distance information between samples
#' 
#' @description 
#' The function \code{distSample} creates a heatmap from a distance matrix 
#' created by the function \code{distShiny}. The heatmap is annotated by the 
#' column specified by the \code{label} column in \code{colData(se)}.
#' 
#' @details 
#' Internal use in \code{shinyQC}
#'
#' @param d \code{matrix} containing distances, obtained from \code{distShiny}
#' @param se \code{SummarizedExperiment}
#' @param label \code{character}, refers to a column in \code{colData(se)}
#' @param title \code{character}
#' @param ... further arguments passed to \code{ComplexHeatmap::Heatmap}
#'
#' @examples
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10,
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' a_i <- imputeAssay(a, method = "MinDet")
#' cD <- data.frame(name = colnames(a_i),
#'     type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a_i))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a_i, rowData = rD,
#'     colData = cD)
#' 
#' dist <- distShiny(a_i)
#' distSample(dist, se, label = "type", title = "imputed", 
#'     show_row_names = TRUE)
#'
#' @return \code{plotly}
#'
#' @importFrom SummarizedExperiment colData
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap
#' @importFrom grDevices hcl.colors
#' @importFrom stats setNames as.dist
#'
#' @export
distSample <- function(d, se, label = "name", title = "raw", ...) {

    
    ## create the annotation, use here the colData, get the column label in 
    ## colData
    val <- colData(se)[[label]]
    val_u <- unique(val)
    df <- data.frame(x = val)
    colnames(df) <- label
    l <- list(stats::setNames(grDevices::hcl.colors(n = length(val_u)), val_u))
    names(l) <- label
    
    ## set default arguments for ComplexHeatmap::Heatmap
    top_annotation <- ComplexHeatmap::HeatmapAnnotation(df = df, col = l)
    show_column_names <- FALSE
    args_default <- list(matrix = d, 
        clustering_distance_rows = function(x) stats::as.dist(x),
        clustering_distance_columns = function(x) stats::as.dist(x),
        top_annotation = top_annotation,
        name = "distances", column_title = title,
        show_column_names = show_column_names)

    ## write the ellipsis to args
    args <- list(...)
    args_default[names(args)] <- args

    ## call the ComplexHeatmap::Heatmap function
    do.call(what = ComplexHeatmap::Heatmap, args = args_default)

    # @importFrom heatmaply heatmaply
    ## define the row annotation
    ##row_side_col <- list(SummarizedExperiment::colData(se)[[label]])
    ##names(row_side_col) <- label
    ## ## do the actual plotting
    ## heatmaply::heatmaply(d, trace = "none", plot_method = "plotly", 
    ##     row_dend_left = TRUE, main = title, dendrogram = "both",
    ##     row_side_colors = row_side_col, scale = "none", 
    ##     showticklabels = c(FALSE, FALSE), show_dendrogram = c(TRUE, FALSE))
}

#' @name sumDistSample
#'
#' @title Plot the sum of distances to other samples
#'
#' @description 
#' The function \code{sumDistSample} creates a plot showing the sum of distance 
#' of a sample to other samples. 
#'
#' @param d \code{matrix} containing distances, obtained from \code{distShiny}
#' @param title \code{character} specifying the title to be added to the plot
#' 
#' @examples
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' dist <- distShiny(a)
#' 
#' sumDistSample(dist, title = "raw")
#' 
#' @return \code{gg} object from \code{ggplot2}
#' 
#' @importFrom ggplot2 ggplot aes_string geom_point geom_segment ggtitle
#' @importFrom ggplot2 xlab ylab theme_bw theme element_blank
#' @importFrom tibble tibble
#' @importFrom plotly ggplotly
#'
#' @export
sumDistSample <- function(d, title = "raw") {
    d_sum <- rowSums(d) 
    tbl <- tibble::tibble(name = names(d_sum), distance = d_sum)
    g <- ggplot2::ggplot(tbl, ggplot2::aes_string(x = "distance", y = "name")) + 
        ggplot2::geom_point(size = 0.5) + 
        ggplot2::geom_segment(ggplot2::aes_string(xend = 0, yend = "name"), 
            size = 0.1) + 
        ggplot2::ggtitle(title) +
        ggplot2::xlab("sum of distances") + ggplot2::ylab("") + 
        ggplot2::theme_bw() + 
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                        panel.grid.minor.y = ggplot2::element_blank())
    
    g <- plotly::ggplotly(g, tooltip = c("x", "y"))
    
    return(g)
}

#' @name MAvalues
#'
#' @title Create values (M and A) for MA plot
#'
#' @description 
#' The function \code{MAvalues} will create MA values as input for the function 
#' \code{MAplot} and \code{hoeffDValues}. 
#' \code{M} and \code{A} are specified relative to specified samples which 
#' is determined by the \code{group} argument. In case of \code{group == "all"}, 
#' all samples (expect the specified one) are taken for the reference 
#' calculation. In case of \code{group != "all"} will use the samples belonging 
#' to the same group given in \code{colnames(colData(se))} expect the 
#' specified one. 
#'
#' @param se \code{SummarizedExperiment}
#' @param log2 \code{logical}, specifies if values re \code{log2} 
#' transformed prior to calculating M and A values. If the values are already 
#' transformed, \code{log2} should be set to \code{FALSE}.
#' @param group \code{character}, either \code{"all"} or one of 
#' \code{colnames(colData(se))}
#'
#' @return \code{tbl} with columns \code{Feature}, \code{name} (sample name), 
#' \code{A}, \code{M} and additional columns of \code{colData(se)}
#' 
#' @examples
#' ## create se
#' set.seed(1)
#' a <- matrix(rnorm(10000), nrow = 1000, ncol = 10, 
#'             dimnames = list(1:1000, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = rD, colData = cD)
#' 
#' MAvalues(se, log2 = FALSE, group = "all")
#' 
#' @importFrom dplyr pull left_join
#' @importFrom SummarizedExperiment assay colData
#' @importFrom tibble as_tibble add_column tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' 
#' @export
MAvalues <- function(se, log2 = TRUE, group = c("all", colnames(colData(se)))) {

    ## check arguments group
    group <- match.arg(group)

    ## access the assay slot
    a <- SummarizedExperiment::assay(se)
    
    ## access the colData slot and add the rownames as a new column to cD
    ## (will add the column "x5at1t1g161asy")
    cD <- SummarizedExperiment::colData(se) |> as.data.frame()
    if (!all(colnames(a) == rownames(cD)))
        stop("colnames(assay(se)) do not match rownames(colData(se))")
    cD <- tibble::rownames_to_column(cD, var = "x5at1t1g161asy")

    if (ncol(a) < 2)
        stop("MAplot needs more than one samples")

    ## take logarithm of assay values if the values are not transformed yet
    if (log2) a <- log2(a)

    A <- M <- matrix(NA, nrow = nrow(a), ncol = ncol(a),
        dimnames = list(rownames(a), colnames(a)))
    
    ## calculate sample-wise the values (iterate through the columns)
    for (i in colnames(a)) {

        ## get indices
        inds_l <- colnames(a) == i
        inds_nl <- !inds_l

        ## truncate the indices based on the group (only use the group for the 
        ## comparison and the "outgroup")
        if (group != "all") {
            g <- cD[cD[, "x5at1t1g161asy"] == i, group]
            inds_nl <- inds_nl & cD[, group] == g
        }

        rM <- rowMeans(a[, inds_nl], na.rm = TRUE)
        M[, inds_l] <- a[, inds_l] - rM
        A[, inds_l] <- 0.5 * (a[, inds_l] + rM)
    }

    ## combine the data and add additional information from 
    A_l <- tibble::as_tibble(A) 
    A_l <- tibble::add_column(Feature = rownames(A), .data = A_l, .before = 1)  
    A_l <- tidyr::pivot_longer(A_l, cols = 2:ncol(A_l), values_to = "A")
    
    M_l <- tibble::as_tibble(M)
    M_l <- tibble::add_column(Feature = rownames(M), M_l, .before = 1)
    M_l <- tidyr::pivot_longer(M_l, cols = 2:ncol(M_l), values_to = "M")

    tbl <- tibble::tibble(A_l, M = dplyr::pull(M_l, "M")) 
    tbl <- dplyr::left_join(x = tbl, y = cD, by = c("name" = "x5at1t1g161asy"))

    return(tbl)
}

#' @name hoeffDValues
#' 
#' @title Create values of Hoeffding's D statistics from M and A values
#' 
#' @description 
#' The function creates and returns Hoeffding's D statistics values 
#' from MA values.   
#'
#' @details 
#' The function uses the function \code{hoeffd} from the \code{Hmisc} package to 
#' calculate the values.
#'
#' @param tbl \code{tibble}, as obtained from the function \code{MAvalues}
#' @param name \code{character}, name of the returned list
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
#' tbl <- MAvalues(se)
#' hoeffDValues(tbl, "raw")
#' 
#' ## normalized values
#' se_n <- se
#' assay(se_n) <- normalizeAssay(a, "sum")
#' tbl_n <- MAvalues(se_n, group = "all")
#' hoeffDValues(tbl_n, "normalized")
#'
#' ## transformed values
#' se_t <- se
#' assay(se_t) <- transformAssay(a, "log2")
#' tbl_t <- MAvalues(se_t, group = "all")
#' hoeffDValues(tbl_t, "transformed")
#'
#' @importFrom Hmisc hoeffd
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate group_by summarise pull
#'
#' @return named list with Hoeffding's D values per sample
#' 
#' @export
hoeffDValues <- function(tbl, name = "raw") {
    
    ## create a wide tibble with the samples as columns and features as rows 
    ## for A and M values
    A <- tidyr::pivot_wider(tbl, id_cols = "Feature", values_from = "A", 
        names_from = "name")
    M <- tidyr::pivot_wider(tbl, id_cols = "Feature", values_from = "M", 
        names_from = "name")
    
    ## create the D statistic between M and A values, 
    ## only return these comparisons (not A vs. A and M vs. M, i.e. index 
    ## by [2, 1])
    hd_l <- dplyr::mutate(tbl, name = factor(name, levels = unique(name)))
    hd_l <- dplyr::group_by(hd_l, name)
    hd_l <- dplyr::summarise(hd_l, hd_l = Hmisc::hoeffd(A, M)$D[2, 1]) 
    hd_l <- dplyr::pull(hd_l, hd_l)
    
    ## assign the names (do not use the first column since it contains the 
    ## Feature names)
    names(hd_l) <- colnames(A)[-1]
    
    ## create a list containing the named numeric and return the named list of
    ## these values
    l <- list(hd_l)
    names(l) <- name
    
    return(l)
}

#' @name hoeffDPlot
#' 
#' @title Create a plot from a list of Hoeffding's D values
#' 
#' @description 
#' The function \code{hoeffDPlot} creates via \code{ggplot} a violin plot per 
#' factor, a jitter plot of the data points and (optionally) connects the points
#' via lines. \code{hoeffDPlot} uses the \code{plotly} package to make the 
#' figure interactive.
#' 
#' @details 
#' The function \code{hoeffDPlot} will create the violin plot and jitter plot 
#' according to the specified order given by the colnames of \code{df}. 
#' \code{hoeffDPlot} will thus internally refactor the \code{colnames} of the 
#' supplied \code{data.frame} according to the order of the \code{colnames}. 
#' 
#' @param df \code{data.frame} containing one or multiple columns containing the 
#' Hoeffding's D statistics
#' @param lines \code{logical}, should points belonging to the same sample be 
#' connected
#'
#' @examples
#' ## create se
#' set.seed(1)
#' a <- matrix(rnorm(10000), nrow = 1000, ncol = 10, 
#'             dimnames = list(1:1000, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' tbl <- MAvalues(se, log2 = FALSE, group = "all")
#' hd_r <- hoeffDValues(tbl, "raw")
#' 
#' ## normalized values
#' se_n <- se
#' assay(se_n) <- normalizeAssay(a, "sum")
#' tbl_n <- MAvalues(se_n, log2 = FALSE, group = "all")
#' hd_n <- hoeffDValues(tbl_n, "normalized")
#' 
#' df <- data.frame(raw = hd_r, normalized = hd_n)
#' hoeffDPlot(df, lines = TRUE)
#' hoeffDPlot(df, lines = FALSE)
#' 
#' @return 
#' \code{gg} object from \code{ggplot2}
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_violin geom_point aes_string geom_line
#' @importFrom ggplot2 ylab theme_bw theme
#' @importFrom plotly ggplotly
#' 
#' @export
hoeffDPlot <- function(df, lines = TRUE) {
    
    ## set lines to FALSE if only one list is supplied
    if (ncol(df) == 1) lines <- FALSE
    
    ## obtain the supplied order
    cols <- colnames(df)
    
    ## add the sample names as a column and create a long format of the df
    df <- data.frame(sample = rownames(df), df)
    df <- tidyr::pivot_longer(df, cols = 2:ncol(df), 
        names_to = "processing_step")
    
    ## refactor the names according to the supplied order (cols)
    names_f <- factor(dplyr::pull(df,"processing_step"), cols)
    df <- dplyr::mutate(df, processing_step = names_f)

    df <- dplyr::mutate(df, x = as.numeric(names_f))

    df$x_jitter <- jitter(df$x)
    # ## do the actual plotting
    g <- ggplot2::ggplot(df) + 
        ggplot2::geom_violin(
            ggplot2::aes_string(x = "processing_step", y = "value"), 
            na.rm = TRUE) + 
        suppressWarnings(
            ggplot2::geom_point(
                ggplot2::aes_string(x = "x_jitter", y = "value", 
                    color = "processing_step", text = "sample")))
    if (lines) g <- g + ggplot2::geom_line(
        ggplot2::aes_string(x = "x_jitter", y = "value", group = "sample"))
    g <- g + ggplot2::ylab("Hoeffding's D statistic") + 
        ggplot2::xlab("processing step") + ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    plotly::ggplotly(g, tooltip = c("text", "y"))
}

#' @name MAplot
#'
#' @title Create a MA plot
#'
#' @description 
#' The function creates a 2D histogram of M and A values.
#' 
#' @details 
#' \code{MAplot} returns a 2D hex histogram instead of a classical scatterplot 
#' due to computational reasons and better visualization of overlaying points.
#' The argument \code{plot} specifies the sample (refering to 
#' \code{colData(se)$name}) to be plotted. If \code{plot = "all"}, MA values 
#' for all samples will be plotted (samples will be plotted in facets). 
#' If the number of features (\code{tbl$Features}) is below 1000, points will be 
#' plotted (via \code{geom_points}), otherwise hexagons will be plotted
#' (via \code{geom_hex}).
#'
#' @param tbl \code{tibble} containing the M and A values, as obtained from the 
#' \code{MAvalues} function
#' @param group \code{character}, one of \code{colnames(colData(se))} 
#' (\code{se} used in \code{MAvalues}) or \code{"all"}
#' @param plot \code{character}, one of \code{colData(se)$name} (\code{se} 
#' used in \code{MAvalues}) or \code{"all"}
#'
#' @return 
#' \code{gg} object from \code{ggplot2}
#'
#' @examples
#' ## create se
#' set.seed(1)
#' a <- matrix(rnorm(10000), nrow = 1000, ncol = 10,
#'             dimnames = list(1:1000, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a,
#'     rowData = rD, colData = cD)
#'
#' tbl <- MAvalues(se, log2 = FALSE, group = "all")
#' MAplot(tbl, group = "all", plot = "all")
#'
#' @importFrom dplyr pull filter
#' @importFrom ggplot2 ggplot aes_string geom_hex geom_point facet_wrap
#' @importFrom ggplot2 theme coord_fixed xlim ylim theme_bw
#' @importFrom stats formula
#' 
#' @export
MAplot <- function(tbl, group = c("all", colnames(tbl)), 
    plot = c("all", unique(tbl[["name"]]))) {
    
    group <- match.arg(group)
    
    if (!all(plot %in% c("all", unique(tbl[["name"]])))) {
        stop("plot not in 'all' or 'unique(tbl$name)'")
    }
    
    ## get the number of features (this will govern if points will be plotted
    ## or hexagons)
    n <- dplyr::pull(tbl, "Feature")
    n <- unique(n)
    n <- length(n)

    A <- dplyr::pull(tbl, "A")
    M <- dplyr::pull(tbl, "M")
    x_lim <- c(min(A, na.rm = TRUE), max(A, na.rm = TRUE))
    x_lim <- ifelse(is.infinite(x_lim), NA, x_lim)
    y_lim <- c(min(M, na.rm = TRUE), max(M, na.rm = TRUE))
    y_lim <- ifelse(is.infinite(y_lim), NA, y_lim)
    
    if (!("all" %in% plot)) {
        tbl <- dplyr::filter(tbl, tbl[["name"]] %in% plot)
    }
    
    ## create a formula depending on the group argument for facet_wrap 
    if (group == "all" | group == "name") {
        fm <- stats::formula(paste("~", quote(name))) 
    } else {
        fm <- stats::formula(paste("~", group, "+", quote(name)))
    }
    
    ## do the actual plotting
    g <- ggplot2::ggplot(tbl, ggplot2::aes_string(x = "A", y = "M")) 
    if (n >= 1000) {
        g <- g + ggplot2::geom_hex()
    } else {
        g <- g + ggplot2::geom_point()
    }
    
    if (group != "name") {
        g <- g + ggplot2::facet_wrap(fm) + ggplot2::theme(aspect.ratio = 1)
    } else {
        g <- g + ggplot2::coord_fixed()
    }
    
    if (!any(is.na(x_lim))) g <- g + ggplot2::xlim(x_lim)
    if (!any(is.na(y_lim))) g <- g + ggplot2::ylim(y_lim)
    g + ggplot2::theme_bw()
}

#' @name createDfFeature
#' 
#' @title Create data frame of (count/intensity) values for a selected feature
#' along data processing steps
#' 
#' @description 
#' The function \code{createDfFeature} takes as input a list of matrices and 
#' returns the row \code{Feature} of each matrix as a column of a 
#' \code{data.frame}. The function \code{createDfFeature} provides the input 
#' for the function \code{featurePlot}. 
#' 
#' @details 
#' Internal usage in \code{shinyQC}
#' 
#' @param l \code{list} containing matrices at different processing steps
#' @param feature \code{character}, element of \code{rownames} of the matrices 
#' in \code{l}
#' 
#' @return \code{data.frame}
#' 
#' @examples 
#' set.seed(1)
#' x1 <- matrix(rnorm(100), ncol = 10, nrow = 10, 
#'     dimnames = list(paste("feature", 1:10), paste("sample", 1:10)))
#' x2 <- x1 + 5
#' x3 <- x2 + 10
#' 
#' l <- list(x1 = x1, x2 = x2, x3 = x3)
#' createDfFeature(l, "feature 1")
#' 
#' @export
createDfFeature <- function(l, feature) {
    l_slice <- lapply(seq_along(l), function(x) as.numeric(l[[x]][feature, ]))
    names(l_slice) <- names(l)
    df <- data.frame(l_slice)
    rownames(df) <- colnames(l[[1]])
    return(df)
}

#' @name featurePlot
#'
#' @title Create a plot of (count/intensity) values over the samples
#'
#' @description
#' The function \code{featurePlot} creates a plot of (count/intensity) values 
#' for different data processing steps (referring to columns in the 
#' \code{data.frame}) over the different samples (referring to rows in 
#' the \code{data.frame}).
#' 
#' @details
#' Internal usage in \code{shinyQC}.
#' 
#' @param df \code{data.frame}
#' 
#' @return \code{gg} object from \code{ggplot2}
#' 
#' @examples 
#' set.seed(1)
#' x1 <- matrix(rnorm(100), ncol = 10, nrow = 10, 
#'     dimnames = list(paste("feature", 1:10), paste("sample", 1:10)))
#' x2 <- x1 + 5
#' x3 <- x2 + 10
#' l <- list(x1 = x1, x2 = x2, x3 = x3)
#' df <- createDfFeature(l, "feature 1")
#' featurePlot(df)
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line xlab ylab
#' @importFrom ggplot2 theme_bw theme element_text
#' 
#' @export
featurePlot <- function(df) {
    
    ## add a sample column
    if (!is.data.frame(df)) stop("df is not a data.frame")
    tbl <- tibble::tibble(sample = rownames(df), df)
    tbl$sample <- factor(x = tbl$sample, levels = tbl$sample)
    tbl <- tidyr::pivot_longer(tbl, cols = 2:ncol(tbl))
    
    ggplot2::ggplot(tbl, ggplot2::aes_string(x = "sample", y = "value")) + 
        ggplot2::geom_point(ggplot2::aes_string(color = "name")) + 
        ggplot2::geom_line(ggplot2::aes_string(group = "name", 
            color = "name")) + 
        ggplot2::xlab("sample") + ggplot2::ylab("value") + 
        ggplot2::theme_bw() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
}

#' @name cvFeaturePlot
#' 
#' @title Plot of feature-wise coefficient of variation values
#' 
#' @description 
#' The function \code{cvFeaturePlot} returns a \code{plotly} plot of coefficient 
#' of variation values. It will create a violin plot and superseded points
#' of coefficient of variation values per list entry of \code{l}.
#' 
#' @details 
#' \code{lines = TRUE} will connect the points belonging to the same feature 
#' with a line. If there are less than two features, the violin plot will not be
#' plotted. The violin plots will be ordered according to the order in \code{l}
#'  
#' @param l \code{list} containing matrices
#' @param lines \code{logical}
#' 
#' @return \code{plotly}
#' 
#' @examples
#' x1 <- matrix(1:100, ncol = 10, nrow = 10, 
#'     dimnames = list(paste("feature", 1:10), paste("sample", 1:10)))
#' x2 <- x1 + 5
#' x3 <- x2 + 10
#' l <- list(x1 = x1, x2 = x2, x3 = x3)
#' cvFeaturePlot(l, lines = FALSE)
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_violin aes_string geom_point geom_line
#' @importFrom ggplot2 ylab xlab theme_bw theme
#' @importFrom plotly ggplotly
#' @export
cvFeaturePlot <- function(l, lines = FALSE) {
    
    names_l <- names(l)
    l_cv <- lapply(seq_along(l), function(x) cv(t(l[[x]]), name = names_l[x]))
    
    ## create df containing cv values
    df <- data.frame(feature = rownames(l[[1]]), l_cv)
    df <- tidyr::pivot_longer(df, cols = 2:ncol(df))
    df$name <- factor(df$name, levels = names(l))
    df <- df |> 
        dplyr::mutate(x = as.numeric(as.factor(df$name)))
    
    df$x_jitter <- jitter(df$x)

    # ## do the actual plotting
    g <- ggplot2::ggplot(df) 
    if (nrow(df) > 2) g <- g + 
        ggplot2::geom_violin(ggplot2::aes_string(x = "name", y = "value"), 
            na.rm = TRUE) 
    g <- g + suppressWarnings(
        ggplot2::geom_point(
            ggplot2::aes_string(x = "x_jitter", y = "value", color = "name",
                                                            text = "feature")))
    if (lines) g <- g + ggplot2::geom_line(
        ggplot2::aes_string(x = "x_jitter", y = "value", group = "feature"))
    g <- g + ggplot2::ylab("coefficient of variation") + 
        ggplot2::xlab("processing step") +
        ggplot2::theme_bw() + ggplot2::theme(legend.position = "none") 
    plotly::ggplotly(g, tooltip = c("text", "y"))
}

## normalization allows general-purpose adjustment for differences among your 
## sample; data transformation and scaling are two different approaches to make 
## individual features more comparable. You can use one or combine them to 
## achieve better results. 
## Sample Normalization: sample-specific normalization (weight, volumne),
## normalization by sum/median/reference sample or feature/pooled sample 
## from group, quantile normalization
## data transformation: log transformation, cube root transformation
## data scaling: mean scaling (mean-centered only), auto scaling 
## (mean-centered and divided by sd), pareto scaling (mean-centered and divided
## by square root of sd), range scaling (mean-centered and diveded by range of 
## each variable)


#' @name normalizeAssay
#'
#' @title Normalize a data sets (reduce technical sample effects)
#'
#' @description
#' The function \code{normalizeAssay} performs normalization by sum of the 
#' (count/intensity) values per sample or quantile division per sample
#' or by quantile normalization (adjusting the distributions that they become
#' identical in statistical distributions). The divisor for quantile division
#' (e.g., the 75% quantile per sample) can be specified by the \code{probs} 
#' argument. Quantile normalization is performed by using the 
#' \code{normalizeQuantiles} function from \code{limma}.
#' 
#' @details
#' Internal usage in \code{shinyQC}. If \code{method} is set to \code{"none"}, 
#' the object \code{x} is returned as is (pass-through).
#' 
#' If \code{probs} is NULL, \code{probs} is internally set to "name" if 
#' \code{method = "quantile division"}.
#' 
#' @param a \code{matrix} with samples in columns and features in rows
#' @param method \code{character}, one of \code{"none"}, \code{"sum"}, 
#' \code{"quantile division"}, \code{"quantile"}
#' @param probs \code{numeric}, ranging between \code{[0, 1)}. \code{probs} 
#' is used as the divisor for quantile division in 
#' \code{method = "quantile division"}
#'
#' @examples
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'         dimnames = list(1:10, paste("sample", 1:10)))
#' normalizeAssay(a, "sum")
#' 
#' @return \code{matrix}
#'
#' @importFrom limma normalizeQuantiles
#' @importFrom stats quantile
#'  
#' @export
normalizeAssay <- function(a, 
    method = c("none", "sum", "quantile division", "quantile"), probs = 0.75) {
    
    if (!is.matrix(a)) stop ("a is not a matrix")
    method <- match.arg(method)
    
    a_n <- a

    if (method == "sum") {
        a_n <- apply(a_n, 2, function(x) x / sum(x, na.rm = TRUE))
    }
    if (method == "quantile division") {
        if (is.null(probs)) probs <- 0.75
        a_n <- apply(a_n, 2,
                function(x) x / stats::quantile(x, probs = probs, na.rm = TRUE))
    }
    if (method == "quantile") {
        a_n <- limma::normalizeQuantiles(a_n, ties = TRUE)
    }
    
    rownames(a_n) <- rownames(a)
    colnames(a_n) <- colnames(a)
    return(a_n)
}

#' @name batchCorrectionAssay
#'
#' @title Remove batch effects from (count/intensity) values of a 
#' \code{SummarizedExperiment}
#'
#' @description
#' The function \code{batchCorrectionAssay} removes the batch effect of 
#' (count/intensity) values of a \code{SummarizedExperiment}. 
#' It uses either the \code{removeBatchEffect} function 
#' or no batch effect correction method (pass-through, 
#' \code{none}).
#'
#' @details 
#' The column \code{batchColumn} in \code{colData(se)} contains the information 
#' on the batch identity. Internal use in \code{shinyQC}.
#' 
#' If \code{batchColumn} is NULL, \code{batchColumn} is internally set to the 
#' name of the first column in \code{colData(se)} if 
#' \code{method = "removeBatchEffect (limma)"}.
#' 
#' @param se \code{SummarizedExperiment}
#' @param method \code{character}, one of \code{"none"} or 
#' \code{"removeBatchEffect"}
#' @param batchColumn \code{character}, one of \code{colnames(colData(se))}
#'
#' @examples
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), 
#'     type = c(rep("1", 5), rep("2", 5)), batch = rep(c(1, 2), 5))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' batchCorrectionAssay(se, method = "removeBatchEffect (limma)", 
#'                             batchColumn = "batch")
#' 
#' @return \code{matrix}
#' 
#' @importFrom limma removeBatchEffect
#' @importFrom SummarizedExperiment assay colData
#' 
#' @export
batchCorrectionAssay <- function(se, 
        method = c("none", "removeBatchEffect (limma)"), 
        batchColumn = colnames(colData(se))) {
    
    method <- match.arg(method)
    a <- SummarizedExperiment::assay(se)
    a_b <- as.matrix(a)
    
    if (method == "removeBatchEffect (limma)") {
        
        cD <- SummarizedExperiment::colData(se)
        if (is.null(batchColumn)) {
            batchColumn <- colnames(cD)[1]
        }
        
        if (!batchColumn %in% colnames(cD)) {
            stop("batchColumn not in colnames(colData(se))")
        }
        
        batch <- cD[, batchColumn]
        a_b <- limma::removeBatchEffect(a_b, batch = batch)
    }
    
    rownames(a_b) <- rownames(a)
    colnames(a_b) <- colnames(a)
    
    return(a_b)
}

#' @name transformAssay
#'
#' @title Transform the (count/intensity) values of a \code{data.frame}, 
#' \code{tbl} or \code{matrix} 
#' 
#' @description
#' The function \code{transformAssay} transforms the (count/intensity) values 
#' of a  \code{matrix}. It uses either \code{log}, \code{log2}, variance 
#' stabilizing normalisation (\code{vsn}) or no transformation method 
#' (pass-through, \code{none}). The object
#' \code{x} has the samples in the columns and the features in the rows.
#'
#' @details 
#' Internal use in \code{shinyQC}.
#' 
#' @param a \code{matrix} with samples in columns and features in rows
#' @param method \code{character}, one of \code{"none"}, \code{"log"}, 
#' \code{"log2"} or \code{"vsn"}
#'
#' @examples
#' a <- matrix(1:1000, nrow = 100, ncol = 10, 
#'         dimnames = list(1:100, paste("sample", 1:10)))
#' transformAssay(a, "none")
#' transformAssay(a, "log")
#' transformAssay(a, "log2")
#' transformAssay(a, "vsn")
#'
#' @return \code{matrix}
#' 
#' @importFrom vsn vsn2
#' 
#' @export
transformAssay <- function(a, method = c("none", "log", "log2", "vsn")) {
    
    if (!is.matrix(a)) stop("a is not a matrix")
    method <- match.arg(method)

    a_t <- a

    if (method == "log") {
        a_t <- log(a_t)
    }
    if (method == "log2") {
        a_t <- log2(a_t)
    }
    if (method == "vsn") {
        a_t <- vsn2(a_t)
        a_t <- a_t@hx
    }
    
    rownames(a_t) <- rownames(a) 
    colnames(a_t) <- colnames(a)
    return(a_t)
}

#' @name imputeAssay
#' 
#' @title Impute missing values in a \code{matrix}
#' 
#' @description 
#' The function \code{impute} imputes missing values based on one of the 
#' following principles: Bayesian missing value imputation (\code{BPCA}), 
#' k-nearest neighbor averaging (\code{kNN}), Malimum likelihood-based 
#' imputation method using the EM algorithm (\code{MLE}), replacement by 
#' the smallest non-missing value
#' in the data (\code{Min}), replacement by the minimal value observed as
#' the q-th quantile (\code{MinDet}, default \code{q = 0.01}), and replacement by
#' random draws from a Gaussian distribution centred to a minimal value 
#' (\code{MinProb}).
#'
#' @details
#' \code{BPCA} wrapper for \code{pcaMethods::pca} with \code{methods = "bpca"}. \code{BPCA} is a
#' missing at random (MAR) imputation method. 
#' 
#' \code{kNN} wrapper for \code{impute::impute.knn} with \code{k = 10}, \code{rowmax = 0.5}, 
#' \code{colmax = 0.5}, \code{maxp = 1500}. \code{kNN} is a MAR imputation method.
#' 
#' \code{MLE} wrapper for \code{imputeLCMD::impute.MAR} with \code{method = "MLE"}, 
#' \code{model.selector = 1}/\code{imputeLCMD::impute.wrapper.MLE}. \code{MLE} is a MAR
#' imputation method.
#' 
#' \code{Min} imputes the missing values by the observed minimal value of \code{x}. 
#' \code{Min} is a missing not at random (MNAR) imputation method.
#' 
#' \code{MinDet} is a wrapper for \code{imputeLCMD::impute.MinDet} with \code{q = 0.01}. 
#' \code{MinDet} performs the imputation using a 
#' deterministic minimal value approach. The missing entries are
#' replaced with a minimal value, estimated from the \code{q}-th quantile from each
#' sample. \code{MinDet} is a MNAR imputation method.
#' 
#' \code{MinProb} is a wrapper for \code{imputeLCMD::impute.MinProb} with \code{q = 0.01} and 
#' \code{tune.sigma = 1}. \code{MinProb} performs the imputation based on random draws 
#' from a Gaussion distribution with the mean set to the minimal value of a 
#' sample. \code{MinProb} is a MNAR imputation method.
#' 
#' @param a \code{matrix} with samples in columns and features in rows
#' @param method \code{character}, one of \code{"BPCA"}, \code{"kNN"}, \code{"MLE"}, \code{"Min"}, 
#' \code{"MinDet"}, or \code{"MinProb"}
#'
#' @examples
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'     dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' 
#' imputeAssay(a, method = "kNN")
#' imputeAssay(a, method = "Min")
#' imputeAssay(a, method = "MinDet")
#' imputeAssay(a, method = "MinProb")
#' 
#' @return \code{matrix}
#' 
#' @importFrom imputeLCMD impute.MinDet impute.MinProb
#' @importFrom impute impute.knn
#' @importFrom pcaMethods pca completeObs
#' 
#' @export
imputeAssay <- function(a,
    method = c("BPCA", "kNN", "MLE", "Min", "MinDet", "MinProb")) {

    if (!is.matrix(a)) stop("a is not a matrix")
    method <- match.arg(method)

    ## convert the data.frame into matrix
    a_i <- as.matrix(a)

    if (method == "BPCA") {
        n_samp <- ncol(a_i)
        ## expects a matrix with features in cols, samples in rows
        res <- pcaMethods::pca(t(a_i), method = "bpca", nPcs = (n_samp - 1),
                                                            verbose = FALSE)
        a_i <- pcaMethods::completeObs(res)
        a_i <- t(a_i)
    }

    if (method == "kNN") {
        ## expects a matrix with features in rows, samples in columns
        a_i <- impute::impute.knn(data = a_i)$data
    }

    if (method == "MLE") {
        ## expects a matrix with features in rows, samples in columns
        a_i <- imputeLCMD::impute.wrapper.MLE(dataSet.mvs = a_i)
    }
        
    if (method == "Min") {
        min_val <- min(a_i, na.rm = TRUE)
        a_i[is.na(a_i)] <- min_val
    }
        
    if (method == "MinDet") {
        ## expects a matrix with features in rows, samples in columns
        a_i <- imputeLCMD::impute.MinDet(dataSet.mvs = a_i, q = 0.01)
    }
        
    if (method == "MinProb") {
        ## expects a matrix with features in rows, samples in columns
        a_i <- imputeLCMD::impute.MinProb(dataSet.mvs = a_i, q = 0.01, 
            tune.sigma = 1)
    }
        
    return(a_i)
}


