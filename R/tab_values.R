#' @name create_boxplot
#'
#' @title Create a boxplot of (count/intensity) values per sample
#'
#' @description
#' The function `create_boxplot` creates 
#' 
#' @details 
#' Internal usage in `shinyQC`.
#'
#' @param x `matrix` or `data.frame` containing the (count/intensity) values, 
#' samples are in columns and features in rows
#' @param title `character` or `numeric` of `length(1)`
#' @param log2 `logical`, if `TRUE` (count/intensity) values are displayed as 
#' log2 values
#' @param violin `logical`, if `FALSE` a boxplot is created, if `TRUE` a 
#' violin plot is created
#'
#' @examples
#' x <- matrix(1:100, ncol = 10, dimnames = list(1:10, paste("sample", 1:10)))
#' create_boxplot(x, title = "", log2 = TRUE, violin = FALSE)
#' 
#' @return `gg`
#' 
#' @importFrom rlang .data
#' @import dplyr
#' 
#' @export
create_boxplot <- function(x, title = "", log2 = TRUE, violin = FALSE) {

    ## pivot_longer will create the columns name (containing the colnames of x)
    ## and value (containing the actual values)
    x_l <- x %>% as_tibble() %>% pivot_longer(cols = seq_len(ncol(x)))

    ## take log2 values if log2 = TRUE
    if (log2) x_l$value <- log2(x_l$value)
    
    ## keep colnames in the supplied order
    x_l$name <- factor(x = x_l$name, levels = colnames(x))

    ## do the actual plotting
    g <- ggplot(x_l, aes_string(y = "value", x = "name"))

    if (!violin) { 
        g <- g + geom_boxplot() 
    } else { ## violin == TRUE
        g <- g + geom_violin()
    }

    g + theme(axis.text.x = element_text(angle = 90)) + ggtitle(title)

}


#' @name driftPlot
#' 
#' @title Plot the trend line for aggregated values
#' 
#' @description 
#' The function `driftPlot` aggregates the (count/intensity) values from the 
#' `assay()` slot of a `SummarizedExperiment` by the `median` or `sum` of the
#' (count/intensity) values. `driftPlot` then visualizes these aggregated values 
#' and adds a trend line (using either LOESS or a linear model) from 
#' (a subset of) the aggregated values. The subset is specified by the 
#' arguments `category` and `level`
#' 
#' @details 
#' The x-values are sorted according to the `orderCategory` argument: The 
#' levels of the corresponding column in `colData(se)` are pasted with the 
#' sample names (in the column `name`) and factorized.
#' Internal usage in `shinyQC`.
#' 
#' @param se `SummarizedExperiment`
#' @param aggregation `character`, type of aggregation of (count/intensity) 
#' values
#' @param category `character`, column of `colData(se)`
#' @param orderCategory `character`, column of `colData(se)`
#' @param level `character`, from which samples should the LOESS curve be
#' calculated, either `"all"` or one of the levels of the selected columns
#' of `colData(se)` (`"category"`) 
#' @param method `character`, either `"loess"` or `"lm"`
#' 
#' @return `gg` object from `ggplot2`
#' 
#' @examples 
#' #' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'     dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' sample <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' featData <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = featData, colData = sample)
#' 
#' driftPlot(se, aggregation = "sum", category = "type", 
#'     orderCategory = "name", level = "1", method = "loess")
#' 
#' @importFrom stats median
#' @importFrom rlang .data
#' 
#' @export
driftPlot <- function(se, aggregation = c("median", "sum"), 
        category = colnames(colData(se)), orderCategory = colnames(colData(se)), 
        level = c("all", unique(colData(se)[, category])),
        method = c("loess", "lm")) {
    
    aggregation <- match.arg(aggregation)
    category <- match.arg(category)
    orderCategory <- match.arg(orderCategory)
    level <- match.arg(level)
    method <- match.arg(method)
    
    a <- assay(se)
    cD <- colData(se)
    
    a_l <- a %>% as_tibble() %>% pivot_longer(cols = seq_len(ncol(a)))
    
    if (aggregation == "median") FUN <- median
    if (aggregation == "sum") FUN <- sum
    
    ## aggregate the values across the samples
    a_l <- a_l %>% 
        group_by(.data$name) %>% 
        summarise(across(starts_with("value"), FUN, na.rm = TRUE))
    
    ## join with cD
    tb <- left_join(a_l, cD, by = "name", copy = TRUE)
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
    
    ggplot(df,
            aes_string(x = "x_ggplot_vals", y = "value", 
            col = "col_ggplot_points")) + 
        geom_point() + 
        geom_point(data = df_subset, 
            aes_string(x = "x_ggplot_vals", y = "value", 
            col = "col_ggplot_points")) + 
        geom_smooth(data = df_subset, 
            aes_string(x = "x_ggplot_vals_num", y = "value"), method = method) +
        theme_classic() + 
        scale_color_manual(values = c("#000000", "#2A6CEF")) +
        scale_x_discrete(labels = df$name[order(df$x_ggplot_vals)]) +
        xlab("samples") + ylab(paste(aggregation, "of values")) +
        theme(axis.text.x = element_text(angle = 90), legend.position = "none")
}


#' @name cv 
#' 
#' @title Calculate coefficient of variation
#' 
#' @description 
#' The function `cv` calculates the coefficient of variation from columns of 
#' a matrix. The coefficients of variation are calculated according to the 
#' formula `sd(y) / mean(y) * 100` with `y` the column values. 
#' 
#' @details
#' The function returned a named `list` (the name is specified by the `name`
#' argument) containing the coefficient of variation of the columns of `x`.
#' 
#' @param x `matrix`
#' @param name `character`, the name of the returned list
#' 
#' @return `list`
#' 
#' @examples
#' x <- matrix(1:10, ncol = 2)
#' cv(x)
#' 
#' @export
cv <- function(x, name = "raw") {
    
    sd_v <- apply(x, 2, sd, na.rm = TRUE)
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
#' The function `plotCV` displays the coefficient of variation values of 
#' set of values supplied in a `data.frame` object. The function will create
#' a plot using the `ggplot2` package and will print the values in the 
#' different columns in different colors.
#' 
#' @details 
#' Internal usage in `shinyQC`.
#' 
#' @param df `data.frame` containing one or multiple columns containing the 
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
#' @return `gg`
#' 
#' @export
plotCV <- function(df) {
    
    ## add a sample column
    if (!is.data.frame(df)) stop("df is not a data.frame")
    tbl <- tibble(sample = rownames(df), df)
    tbl$sample <- factor(x = tbl$sample, levels = tbl$sample)
    tbl <- pivot_longer(tbl, cols = 2:ncol(tbl))
    
    ggplot(tbl, aes_string(x = "sample", y = "value")) + 
        geom_point(aes_string(color = "name")) + 
        geom_line(aes_string(group = "name", color = "name")) + 
        xlab("sample") + ylab("coefficient of variation") + 
        theme_bw() + theme(axis.text.x = element_text(angle = 90))
}

#' @name ECDF
#'
#' @title Create ECDF plot of a sample against a reference
#'
#' @description
#' The function `ECDF` creates a plot of the empirical cumulative distribution
#' function of a specified sample and an outgroup. 
#'
#' @details 
#' Internal use in `shinyQC`.
#' 
#' @param se `SummarizedExperiment` object
#' @param sample `character`, name of the sample to compare against the group
#' @param group `character`, either `"all"` or one of `colnames(colData(se))`
#' 
#' @examples
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'     dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' sample <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' featData <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = featData, colData = sample)
#' 
#' ECDF(se, "sample 1", group = "all")
#' 
#' @importFrom SummarizedExperiment assay colData
#' 
#' @return `gg` object from `ggplot2`
#' 
#' @export
ECDF <- function(se, sample = colnames(se), 
    group = c("all", colnames(colData(se)))) {
    
    ## match arguments
    sample <- match.arg(sample)
    group <- match.arg(group)
    
    ## obtain assay and colData from SummarizedExperiment object for 
    ## further use 
    a <- assay(se)
    cd <- colData(se)
    
    df <- data.frame(value = a[, sample], type = sample)
    
    ## calculate sample-wise the values
    ## get indices 
    inds_l <- colnames(a) == sample
    inds_nl <- !inds_l
    
    ## truncate the indices based on the group (only use the group for the 
    ## comparison and the "outgroup")
    if (group != "all") {
        g <- cd[cd[, "name"] == sample, group]
        inds_nl <- inds_nl & cd[, group] == g
    }
    
    rM <- rowMeans(a[, inds_nl], na.rm = TRUE)
    df_group <- data.frame(value = rM, type = "group")
    
    inds <- !(is.na(df_group$value) |  is.na(df$value))
    df <- df[inds, ]
    df_group <- df_group[inds, ]
    
    ## create ECDF of data (taken from https://rpubs.com/mharris/KSplot)
    value_s <- df$value
    value_g <- df_group$value
    # cdf1 <- ecdf(value_s) 
    # cdf2 <- ecdf(value_g) 
    # 
    # ## find min and max statistics to draw line between points of 
    # ## greatest distance
    # minMax <- seq(min(value_s, value_g, na.rm = TRUE), 
    #     max(value_s, value_g, na.rm = TRUE), 
    #     length.out = nrow(df))
    # 
    # inds <- which.max( abs(cdf1(minMax) - cdf2(minMax)) )
    # x0 <- minMax[inds]
    # y1 <- cdf1(x0)
    # y2 <- cdf2(x0)
    # # 
    # # cdf1(5e08)
    # ggplot(df, aes(x = value)) + stat_ecdf() + geom_vline(xintercept = x0) + 
    #     xlim(0, 1e10) + geom_hline(yintercept = y1)
    # 
    #x0 <- minMax[
    ## which(abs(cdf1(minMax) - cdf2(minMax)) == 
    ## max(abs(cdf1(minMax) - cdf2(minMax))))[1]] 
    
    #y0 <- cdf1(x0)
    #y1 <- cdf2(x0)
    
    ks_test <- ks.test(value_s, value_g, exact = TRUE)
    
    ## bind together
    df <- rbind(df, df_group)

    ggplot(df, aes_string(x = "value", color = "type", group = "type")) + # group = group, color = group))+
        stat_ecdf(size=1) + theme_bw() + xlab(sample) + ylab("ECDF") +
        #geom_segment(aes(x = x0, y = y1, xend = x0, yend = y2),
        #             linetype = "dashed", color = "red") +
        #geom_point(aes(x = x0, y= y1), color = "red", size = 5) +
        #geom_point(aes(x = x0, y= y2), color = "red", size = 5) +
        ggtitle(sprintf("K-S Test: D: %s, p-value: %s", 
            signif(ks_test$statistic, 3), 
            signif(ks_test$p.value, 3))) + 
        theme(legend.title=element_blank(), legend.position ="top")
}

#' @name distShiny
#'
#' @title Create distance matrix from numerical matrix
#'
#' @description
#' The function `distShiny` takes as an input a numerical `matrix` or
#' `data.frame` and returns the distances between the rows and columns based
#' on the defined `method` (e.g. euclidean distance). 
#' 
#' @details 
#' Internal use in `shinyQC`.
#'
#' @param x `matrix` or `data.frame`
#' @param method `character`, method for distance calculation
#'
#' @examples
#' x <- matrix(1:100, nrow = 10, ncol = 10, 
#'         dimnames = list(1:10, paste("sample", 1:10)))
#' distShiny(x = x)
#' 
#' @return `matrix`
#'
#' @export
distShiny <- function(x, method = "euclidean") {
    ## calculate the distance matrix
    d <- dist(t(x), method = method, diag = TRUE)
    data.matrix(d)
}

#' @name distSample
#'
#' @title  Create a heatmap using distance information between samples
#' 
#' @description 
#' The function `distSample` creates a heatmap from a distance matrix created
#' by the function `distShiny`. The heatmap is annotated by the column specified
#' by the `label` column in `colData(se)`.
#' 
#' @details 
#' Internal use in `shinyQC`
#'
#' @param d `matrix` containing distances, obtained from `distShiny`
#' @param se `SummarizedExperiment`
#' @param label `character`, refers to a column 
#' @param title `character`
#'
#' @examples
#' library(dplyr)
#' library(SummarizedExperiment)
#' 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' a_i <- a %>% impute(., method = "MinDet")
#' sample <- data.frame(name = colnames(a_i), 
#'     type = c(rep("1", 5), rep("2", 5)))
#' featData <- data.frame(spectra = rownames(a_i))
#' se <- SummarizedExperiment(assay = a_i, rowData = featData, colData = sample)
#' 
#' dist <- distShiny(a_i)
#' distSample(dist, se, "type")
#'
#' @return `ComplexHeatmap`
#'
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap
#' 
#' @export
distSample <- function(d, se, label = "name", title = "raw") {
    
    ## create the annotation, use here the colData, get the column label in 
    ## colData
    val <- colData(se)[[label]]
    val_u <- unique(val)
    df <- data.frame(x = val)
    colnames(df) <- label
    l <- list(setNames(hcl.colors(n = length(val_u)), val_u))
    names(l) <- label
    ha <- HeatmapAnnotation(df = df, col = l)
    
    ## do the actual plotting
    Heatmap(d, name = "distances",
        top_annotation = ha, column_title = title,
        show_row_names = TRUE, show_column_names = FALSE)
}

#' @name sumDistSample
#'
#' @title Plot the sum of distances to other samples
#'
#' @description 
#' The function `sumDistSample` creates a plot showing the sum of distance of a 
#' sample to other samples. 
#'
#' @param d `matrix` containing distances, obtained from `distShiny`
#' @param title `character` specifying the title to be added to the plot
#' 
#' @examples
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' dist <- distShiny(a)
#' 
#' sumDistSample(dist, title = "raw")
#' 
#' @return `gg` object from `ggplot`
#' 
#' @import ggplot2
#' 
#' @export
sumDistSample <- function(d, title = "raw") {
    d_sum <- rowSums(d) 
    tbl <- tibble(name = names(d_sum), distance = d_sum)
    g <- ggplot(tbl, aes_string(x = "distance", y = "name")) + geom_point() + 
        geom_segment(aes_string(xend = 0, yend = "name")) + ggtitle(title) +
        xlab("sum of distances") + ylab("") + theme_bw()
    ggplotly(g, tooltip = c("x", "y"))
}

#' @name MAvalues
#'
#' @title Create values (M and A) for MA plot
#'
#' @description 
#' THe function `MAvalues` will create MA values as input for the function 
#' `MAplot`. `M` and `A` are specified relative to specified samples which 
#' is determined by the `group` argument. In case of `group == "all"`, all 
#' samples (expect the specified one) are taken for the reference calculation.
#' In case of `group != "all"` will use the samples belonging to the same group
#' given in `colnames(colData(se))` expect the specified one. 
#'
#' @param se `SummarizedExperiment`
#' @param log2 `logical`
#' @param group `character`, either `"all"` or one of `colnames(colData(se))`
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
#' se <- SummarizedExperiment(assay = a, rowData = featData, colData = sample)
#' 
#' MAvalues(se, group = "all")
#' 
#' @import dplyr
#' @importFrom SummarizedExperiment assay colData
#' @import tidyr
#' @importFrom tibble add_column
#' @importFrom Matrix rowMeans
#' 
#' @export
MAvalues <- function(se, log2 = TRUE, group = c("all", colnames(colData(se)))) {
    
    ## check arguments group
    group <- match.arg(group)
    
    ## retrieve the assay and colData entries
    a <- assay(se)
    cd <- colData(se) %>% as.data.frame()
    
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
            g <- cd[cd[, "name"] == i, group]
            inds_nl <- inds_nl & cd[, group] == g
        }
        
        rM <- rowMeans(a[, inds_nl], na.rm = TRUE)
        M[, inds_l] <- a[, inds_l] - rM
        A[, inds_l] <- 0.5 * (a[, inds_l] + rM)
    }
    
    ## combine the data and add additional information from 
    A_l <- A %>% as_tibble() 
    A_l <- add_column(Feature = rownames(A), .data = A_l, .before = 1)  
    A_l <- pivot_longer(A_l, cols = 2:ncol(A_l), values_to = "A")
    
    M_l <- M %>% as_tibble()
    M_l <- add_column(Feature = rownames(M), M_l, .before = 1)
    M_l <- pivot_longer(M_l, cols = 2:ncol(M_l), values_to = "M")
    
    tbl <- tibble(A_l, M = pull(M_l, "M")) 
    tbl <- left_join(x = tbl, y = cd, by = c("name"))
    
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
#' The function uses the function `hoeffd` from the `Hmisc` package to 
#' calculate the values.
#' 
#' @param tbl `tibble`, as obtained from the function `MAvalues`
#' @param name `character`, name of the returned lists
#' 
#' @examples
#' library(dplyr)
#' library(SummarizedExperiment)
#' 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' sample <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' featData <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = featData, colData = sample)
#' 
#' tbl <- MAvalues(se)
#' hd_r <- hoeffDValues(tbl, "raw")
#' 
#' ## normalized values
#' se_n <- se
#' assay(se_n) <- assay(se) %>% normalize(., "sum")
#' tbl_n <- MAvalues(se_n, group = "all")
#' hd_n <- hoeffDValues(tbl_n, "normalized")
#' 
#' ## transformed values
#' se_t <- se
#' assay(se_t) <- assay(se_n) %>% transform(., "log2")
#' tbl_t <- MAvalues(se_t, group = "all")
#' hd_t <- hoeffDValues(tbl_t, "transformed")
#' 
#' @importFrom Hmisc hoeffd
#' @importFrom rlang .data
#' @import tidyr
#'
#' @return named list of lists
#' 
#' @export
hoeffDValues <- function(tbl, name = "raw") {
    
    ## create a wide tibble with the samples as columns and features as rows 
    ## for A and M values
    A <- pivot_wider(tbl, id_cols = c("Feature", "name"), values_from = "A")
    M <- pivot_wider(tbl, id_cols = c("Feature", "name"), values_from = "M")
    
    ## create the D statistic between M and A values, 
    ## only return these comparisons (not A vs. A and M vs. M, i.e. index 
    ## by [2, 1])
    hd_l <- tbl %>% 
        mutate(name = factor(.data$name, levels = unique(.data$name))) %>% 
        group_by(.data$name) %>%  
        summarise(hd_l = Hmisc::hoeffd(A, M)$D[2, 1]) 
    hd_l <- pull(hd_l, .data$hd_l)
    
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
#' The function `hoeffDPlot` creates via `ggplot` a violin plot per factor,
#' a jitter plot of the data points and (optionally) connects the points
#' via lines. 
#' 
#' @details 
#' The function `hoeffDPlot` will create the violin plot and jitter plot 
#' according to the specified order given to the function (`...`). `hoeffDPlot`
#' will thus internally refactor the names of the supplied lists according
#' to the order. 
#' 
#' @param df `data.frame` containing one or multiple columns containing the 
#' Hoeffding's D statistics
#' @param lines `logical`, should points belonging to the same sample be 
#' connected
#'
#' @examples
#' library(dplyr)
#' library(SummarizedExperiment)
#' 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' sample <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' featData <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = featData, colData = sample)
#' 
#' tbl <- MAvalues(se)
#' hd_r <- hoeffDValues(tbl, "raw")
#' 
#' ## normalized values
#' se_n <- se
#' assay(se_n) <- assay(se) %>% normalize(., "sum")
#' tbl_n <- MAvalues(se_n, group = "all")
#' hd_n <- hoeffDValues(tbl_n, "normalized")
#' 
#' df <- data.frame(raw = hd_r, normalized = hd_n)
#' hoeffDPlot(df, lines = TRUE)
#' hoeffDPlot(df, lines = FALSE)
#' 
#' @return 
#' `gg` object from `ggplot`
#' 
#' @export
hoeffDPlot <- function(df, lines = TRUE) {
    
    ## set lines to FALSE if only one list is supplied
    if (ncol(df) == 1) lines <- FALSE
    
    ## obtain the supplied order
    cols <- colnames(df)
    
    ## add the sample names as a column and create a long format of the df
    df <- data.frame(sample = rownames(df), df)
    df <- pivot_longer(df, cols = 2:ncol(df))
    
    ## refactor the names according to the supplied order (cols)
    names_f <- factor(df$name, cols)
    df$name <- names_f

    df <- df %>%
        mutate(x = as.numeric(names_f))

    df$x_jitter <- jitter(df$x)
    # ## do the actual plotting
    g <- ggplot(df) + 
        geom_violin(aes_string(x = "name", y = "value"), na.rm = TRUE) + 
        suppressWarnings(geom_point(
            aes_string(x = "x_jitter", y = "value", color = "name",
                    text = "sample")))
    if (lines) g <- g + geom_line(
        aes_string(x = "x_jitter", y = "value", group = "sample"))
    g <- g + ylab("Hoeffding's D statistic") + theme_bw()
    ggplotly(g, tooltip = c("text", "y"))
}

#' @name MAplot
#'
#' @title Create a MA plot
#'
#' @description 
#' The function creates a 2D histogram of M and A values.
#' 
#' @details 
#' `MAplot` returns a 2D hex histogram instead of a classical scatterplot due to 
#' computational reasons and better visualization of overlaying points.
#' The argument `plot` specifies the sample (refering to `colData(se)$name`)
#' to be plotted. If `plot = "all"`, MA values for all samples will be plotted
#' (samples will be plotted in facets). 
#' If the number of features (`tbl$Features`) is below 1000, points will be 
#' plotted (via `geom_points`), otherwise hexagons will be plotted
#' (via `geom_hex`).
#'
#' @param tbl `tibble` containing the M and A values, as obtained from the 
#' `MAvalues` function
#' @param group `character`, one of `colnames(colData(se))` 
#' (`se` used in `MAvalues`) or `"all"`
#' @param plot `character`, one of `colData(se)$name` (`se` used in `MAvalues`)
#' or `"all"`
#'
#' @return 
#' `gg` object from `ggplot`
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
#' se <- SummarizedExperiment(assay = a, rowData = featData, colData = sample)
#' 
#' tbl <- MAvalues(se, group = "all")
#' MAplot(tbl, group = "all", plot = "all")
#' 
#' @export
MAplot <- function(tbl, group = c("all", colnames(tbl)), 
    plot = c("all", unique(tbl[["name"]]))) {
    
    group <- match.arg(group)
    
    ## get the number of features (this will govern if points will be plotted
    ## or hexagons)
    n <- pull(tbl, "Feature")
    n <- unique(n)
    n <- length(n)
    

    A <- pull(tbl, "A")
    M <- pull(tbl, "M")
    x_lim <- c(min(A, na.rm = TRUE), max(A, na.rm = TRUE))
    x_lim <- ifelse(is.infinite(x_lim), NA, x_lim)
    y_lim <- c(min(M, na.rm = TRUE), max(M, na.rm = TRUE))
    y_lim <- ifelse(is.infinite(y_lim), NA, y_lim)
    
    if (!("all" %in% plot)) {
        tbl <- filter(tbl, tbl[["name"]] %in% plot)
    }
    
    ## create a formula depending on the group argument for facet_wrap 
    if (group == "all" | group == "name") {
        fm <- formula(paste("~", quote(name))) 
    } else {
        fm <- formula(paste("~", group, "+", quote(name)))
    }
    
    ## do the actual plotting
    g <- ggplot(tbl, aes_string(x = "A", y = "M")) 
    if (n >= 1000) {
        g <- g + geom_hex()
    } else {
        g <- g + geom_point()
    }
    
    if (group != "name") {
        g <- g + facet_wrap(fm) + theme(aspect.ratio = 1)
    } else {
        g <- g + coord_fixed()
    }
    
    if (!any(is.na(x_lim))) g <- g + xlim(x_lim)
    if (!any(is.na(y_lim))) g <- g + ylim(y_lim)
    g + theme_bw()
}

#' @name createDfFeature
#' 
#' @title Create data frame of (count/intensity) values for a selected feature
#' along data processing steps
#' 
#' @description 
#' The function `createDfFeature` takes as input a list of matrices and 
#' returns the row `feature` of each matrix as a column of a `data.frame`.
#' The function `createDfFeature` provides the input for the function
#' `featurePlot`. 
#' 
#' @details 
#' Internal usage in `shinyQC`
#' 
#' @param l `list` containing matrices at different processing steps
#' @param feature `character`, element of `rownames` of the matrices in `l`
#' 
#' @return `data.frame`
#' 
#' @examples 
#' x1 <- matrix(1:100, ncol = 10, nrow = 10, 
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

#' 
#' @name featurePlot
#' 
#' @title Create a plot of (count/intensity) values over the samples
#' 
#' @description 
#' The function `featurePlot` creates a plot of (count/intensity) values for 
#' different data processing steps (referring to columns in the `data.frame`) 
#' over the different samples (referring to rows in the `data.frame`).
#' 
#' @details 
#' Internal usage in `shinyQC`.
#' 
#' @param df `data.frame`
#' 
#' @return `gg`
#' 
#' @examples 
#' x1 <- matrix(1:100, ncol = 10, nrow = 10, 
#'     dimnames = list(paste("feature", 1:10), paste("sample", 1:10)))
#' x2 <- x1 + 5
#' x3 <- x2 + 10
#' l <- list(x1 = x1, x2 = x2, x3 = x3)
#' df <- createDfFeature(l, "feature 1")
#' featurePlot(df)
#' 
#' @export
featurePlot <- function(df) {
    
    ## add a sample column
    if (!is.data.frame(df)) stop("df is not a data.frame")
    tbl <- tibble(sample = rownames(df), df)
    tbl$sample <- factor(x = tbl$sample, levels = tbl$sample)
    tbl <- pivot_longer(tbl, cols = 2:ncol(tbl))
    
    ggplot(tbl, aes_string(x = "sample", y = "value")) + 
        geom_point(aes_string(color = "name")) + 
        geom_line(aes_string(group = "name", color = "name")) + 
        xlab("sample") + ylab("value") + 
        theme_bw() + theme(axis.text.x = element_text(angle = 90))
}

#' @name cvFeaturePlot
#' 
#' @title Plot of feature-wise coefficient of variation values
#' 
#' @description 
#' The function `cvFeaturePlot` returns a `plotly` plot of coefficient 
#' of variation values. It will create a violin plot and superseded points
#' of coefficient of variation values per list entry of `l`.
#' 
#' @details 
#' `lines = TRUE` will connect the points belonging to the same feature with a 
#' line. If there are less than two features, the violin plot will not be
#' plotted. 
#'  
#' @param l `list` containing matrices
#' @param lines `logical`
#' 
#' @return `plotly`
#' 
#' @examples
#' x1 <- matrix(1:100, ncol = 10, nrow = 10, 
#'     dimnames = list(paste("feature", 1:10), paste("sample", 1:10)))
#' x2 <- x1 + 5
#' x3 <- x2 + 10
#' l <- list(x1 = x1, x2 = x2, x3 = x3)
#' cvFeaturePlot(l, lines = FALSE)
#' 
#' @export
cvFeaturePlot <- function(l, lines = FALSE) {
    
    names_l <- names(l)
    l_cv <- lapply(seq_along(l), function(x) cv(t(l[[x]]), name = names_l[x]))
    
    ## create df containing cv values
    df <- data.frame(feature = rownames(l[[1]]), l_cv)
    df <- pivot_longer(df, cols = 2:ncol(df))
    df$name <- factor(df$name, levels = names(l))
    df <- df %>% 
        mutate(x = as.numeric(as.factor(df$name)))
    
    df$x_jitter <- jitter(df$x)

    # ## do the actual plotting
    g <- ggplot(df) 
    if (nrow(df) > 2) g <- g + 
        geom_violin(aes_string(x = "name", y = "value"), na.rm = TRUE) 
    g <- g + suppressWarnings(geom_point(
            aes_string(x = "x_jitter", y = "value", color = "name",
                                                        text = "feature")))
    if (lines) g <- g + geom_line(
        aes_string(x = "x_jitter", y = "value", group = "feature"))
    g <- g + ylab("coefficient of variation") + xlab("processing step") +
        theme_bw() + theme(legend.position = "none") 
    ggplotly(g, tooltip = c("text", "y"))
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


#' @name normalize
#'
#' @title Normalize a data sets (reduce technical sample effects)
#'
#' @description
#' The function `normalize` performs normalization by sum of the 
#' (count/intensity) values per sample or quantile division per sample
#' or by quantile normalization (adjusting the distributions that they become
#' identical in statistical distributions). The divisor for quantile division
#' (e.g., the 75% quantile per sample) can be specified by the `probs` argument.
#' Quantile normalization is performed by using the `normalize.quantiles` 
#' function from `preprocessCore`.
#' 
#' @details
#' Internal usage in `shinyQC`.
#' 
#' @param x `data.frame`, `tibble`, or `matrix`
#' @param method `character`
#' @param probs `numeric`, ranging between `[0, 1)`. `probs` is used as the 
#' divisor for quantile division in `method = "quantile division"`
#'
#' @examples
#' x <- matrix(1:100, nrow = 10, ncol = 10, 
#'         dimnames = list(1:10, paste("sample", 1:10)))
#' normalize(x, "sum")
#' 
#' @return `matrix`
#'
#' @importFrom limma normalizeQuantiles
#'  
#' @export
normalize <- function(x, 
    method = c("none", "sum", "quantile division", "quantile"), probs) {
    
    method <- match.arg(method)
    
    x_n <- x %>% as.matrix()
    if (method == "none") {
        x_n <- x_n
    }
    if (method == "sum") {
        x_n <- apply(x_n, 2, function(x) x / sum(x, na.rm = TRUE))
    }
    if (method == "quantile division") {
        x_n <- apply(x_n, 2, 
                function(x) x / quantile(x, probs = probs, na.rm = TRUE))
    }
    if (method == "quantile") {
        x_n <- limma::normalizeQuantiles(x_n, ties = TRUE)
    }
    
    rownames(x_n) <- rownames(x)
    colnames(x_n) <- colnames(x)
    return(x_n)
}

#' @name transform
#'
#' @title Transform the (count/intensity) values of a `data.frame`, `tibble` or
#' `matrix` 
#'
#' @description
#' The function `transform` transforms the (count/intensity) values of a 
#' `data.frame`, `tibble` or `matrix`. It uses either `log2`, variance 
#' stabilizing normalisation (`vsn`) or no transformation method (pass-through,
#' `none`). The object
#' `x` has the samples in the columns and the features in the rows.
#'
#' @details 
#' Internal use in `shinyQC`.
#' 
#' @param x `data.frame`, `tibble`, or `matrix`
#' @param method `character`, one of `"none"`, `"log2"` or `"vsn"`
#'
#' @examples
#' x <- matrix(1:1000, nrow = 100, ncol = 10, 
#'         dimnames = list(1:100, paste("sample", 1:10)))
#' transform(x, "none")
#' transform(x, "log2")
#' transform(x, "vsn")
#'
#' @return `matrix`
#' 
#' @importFrom vsn vsn2
#' @import dplyr 
#' 
#' @export
transform <- function(x, method = c("none", "log2", "vsn")) {
    
    method <- match.arg(method)
    
    x_t <- x %>% as.matrix()
    
    # if (method == "none") {
    #     x_t <- x
    # }
    if (method == "vsn") {
        x_t <- vsn2(x_t)
        x_t <- x_t@hx
    }
    if (method == "log2") {
        x_t <- log2(x_t)
    }
    
    rownames(x_t) <- rownames(x) 
    colnames(x_t) <- colnames(x)
    return(x_t)
}

#' @name batch
#'
#' @title Remove batch effects from (count/intensity) values of a 
#' `SummarizedExperiment`
#'
#' @description
#' The function `batch` removes the batch effect of (count/intensity) values of 
#' a `SummarizedExperiment`. It uses either the `removeBatchEffect` function 
#' or no batch effect correction method (pass-through, 
#' `none`).
#'
#' @details 
#' Internal use in `shinyQC`.
#' 
#' @param se `SummarizedExperiment`, `tibble`, or `matrix`
#' @param method `character`, one of `"none"` or `"removeBatchEffect"`
#' @param batchColumn `character`, one of 
#'
#' @examples
#' x <- matrix(1:1000, nrow = 100, ncol = 10, 
#'         dimnames = list(1:100, paste("sample", 1:10)))
#' transform(x, "none")
#' transform(x, "log2")
#' transform(x, "vsn")
#'
#' @return `matrix`
#' 
#' @importFrom limma removeBatchEffect
#' @import dplyr 
#' 
#' @export
batch <- function(se, 
    method = c("none", "removeBatchEffect (limma)"), batchColumn) {
    
    method <- match.arg(method)
    a <- assay(se)
    x_b <- a %>% as.matrix()
    
    if (method == "removeBatchEffect (limma)") {
        cD <- colData(se)
        if (!batchColumn %in% colnames(cD)) {
            stop("batchColumn not in colnames(colData(se))")
        }
        
        batch <- cD[, batchColumn]
        x_b <- removeBatchEffect(x_b, batch = batch)
    }
    
    rownames(x_b) <- rownames(a)
    colnames(x_b) <- colnames(a)
    
    return(x_b)
    
}

#' @name impute
#' 
#' @title Impute missing values in a `data.frame`, `tibble`, `matrix`
#' 
#' @description 
#' The function `impute` imputes missing values based on one of the following 
#' principles: Bayesian missing value imputation (`BPCA`), k-nearest 
#' neighbor averaging (`KNN`), Malimum likelihood-based imputation method using
#' the EM algorithm (`MLE`), replacement by the smallest non-missing value
#' in the data (`Min`), replacement by the minimal value observed as
#' the q-th quantile (`MinDet`, default `q = 0.01`), and replacement by
#' random draws from a Gaussian distribution centred to a minimal value 
#' (`MinProb`). 
#' 
#' The function is a wrapper function from the functions `impute_bpca`, 
#' `impute_knn`, `impute_mle`, and `impute_min` from the `MsCoreUtils` package
#' and the functions `impute.MinDet` and `impute.MinProb` from the `imputeLCMD`
#' package. 
#' 
#' @param x `data.frame`, `tibble`, or `matrix`
#' @param method `character`, one of `"BPCA"`, `"kNN"`, `"MLE`, `"Min"`, 
#' `"MinDet"`, or `"MinProb"`
#' 
#' @examples
#' x <- matrix(1:100, nrow = 10, ncol = 10, 
#'     dimnames = list(1:10, paste("sample", 1:10)))
#' x[c(1, 5, 8), 1:5] <- NA
#' impute(x, method = "kNN")
#' 
#' @return `matrix`
#' 
#' @importFrom MsCoreUtils impute_bpca impute_knn impute_mle impute_min
#' @importFrom imputeLCMD impute.MinDet impute.MinProb
#' @import dplyr 
#' 
#' @export
impute <- function(x, 
    method = c("BPCA", "kNN", "MLE", "Min", "MinDet", "MinProb")) {

    method <- match.arg(method)

    ## convert the data.frame into matrix
    x_i <- x %>% as.matrix()

    if (method == "BPCA")
        x_i <- x_i %>% impute_bpca()
    if (method == "kNN")
        x_i <- x_i %>% impute_knn()
    if (method == "MLE")
        x_i <- x_i %>% impute_mle()
    if (method == "Min")
        x_i <- x_i %>% impute_min()
    if (method == "MinDet")
        x_i <- x_i %>% impute.MinDet()
    if (method == "MinProb")
        x_i <- x_i %>% impute.MinProb()
    return(x_i)
}


