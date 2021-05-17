#' @name hist_sample_num
#'
#' @title Return the number of a category
#'
#' @description `hist_sample_num` returns the number of a category 
#' (e.g. sample types) as a `tbl`.
#' The function will retrieve first the column `category` in `colData(se)`. 
#' The function will return a `tbl` containing the numerical 
#' values of the quantities.
#'
#' @param se `SummarizedExperiment` object
#' @param category `character`, corresponding to a column in `colData(se)`
#'
#' @return `tbl`
#' 
#' @examples 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 4), rep("2", 6)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' hist_sample_num(se, category = "type")
#' 
#' @importFrom tibble tibble
#' @importFrom SummarizedExperiment colData
#' 
#' @export
hist_sample_num <- function(se, category = "type") { 

    ## retrieve the sample type
    df <- SummarizedExperiment::colData(se)[[category]]
    df[is.na(df)] <- "NA"
    
    ## retrieve the number of samples per sample type
    tab <- table(df)
    tbl <- tibble::tibble(names = names(tab), values = as.vector(tab))
    return(tbl)
}

#' @name hist_sample
#'
#' @title Plot a histogram of the number of a category
#'
#' @description `hist_sample` plots the number of a category (e.g. sample types)
#' as a histogram. It use the returned `tbl` from `hist_sample_num`.
#'
#' @param tbl `tbl` as returned by `hist_sample_num`
#' @param category `character`, x-axis label of the plot 
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
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 4), rep("2", 6)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' tbl <- hist_sample_num(se, category = "type")
#' hist_sample(tbl)
#' 
#' @importFrom ggplot2 ggplot aes_string geom_bar ggtitle ylab xlab theme_bw
#' @importFrom ggplot2 theme element_text
#' @importFrom plotly ggplotly
#' 
#' @export
hist_sample <- function(tbl, category = "type") {
    ## do the actual plotting
    p <- ggplot2::ggplot(tbl, ggplot2::aes_string(x = "names", y = "values")) + 
        ggplot2::geom_bar(stat = "identity") + 
        ggplot2::ggtitle("Number of samples") + 
        ggplot2::ylab("number") + ggplot2::xlab(category) + 
        ggplot2::theme_bw() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    plotly::ggplotly(p = p, tooltip = c("x", "y"))
}

#' @name mosaic
#' 
#' @title Mosaic plot for two factors in colData(se)
#' 
#' @description 
#' The function `mosaic` creates a mosaic plot of two factors from
#' an `SummararizedExperiment` object. The columns `f1` and `f2` 
#' are taken from `colData(se)`.
#'
#' @details
#' Code partly taken from
#' https://stackoverflow.com/questions/21588096/pass-string-to-facet-grid-ggplot2
#'
#' @param se `SummarizedExperiment` object
#' @param f1 `character`, `f1` is one of the column names in `colData(se)`
#' @param f2 `character`, `f2` is one of the column names in `colData(se)`
#'
#' @return `gg` object from `ggplot2`
#'
#' @examples
#' ## create se
#' set.seed(1)
#' a <- matrix(rnorm(100), nrow = 10, ncol = 10,
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' cD <- data.frame(name = colnames(a), 
#'     type = c(rep("1", 5), rep("2", 5)),
#'     cell_type = c("A", "B"))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
#' 
#' mosaic(se, "cell_type", "type")
#' 
#' @importFrom rlang := .data
#' @importFrom dplyr group_by summarise mutate ungroup n
#' @importFrom SummarizedExperiment colData
#' @importFrom ggplot2 ggplot aes_string geom_bar geom_text position_stack
#' @importFrom ggplot2 facet_grid scale_fill_brewer theme_bw ylab
#' @importFrom ggplot2 scale_y_continuous theme element_text
#' 
#' @export
mosaic <- function(se, f1, f2) {
    
    df <- SummarizedExperiment::colData(se) %>% 
        as.data.frame() %>% 
        dplyr::group_by(!!f1 := get(f1), !!f2 := get(f2)) %>% 
        dplyr::summarise(count = dplyr::n()) %>%
        dplyr::mutate(cut.count = sum(.data$count), 
            prop = (.data$count/sum(.data$count)))
    
    ## set prop to 1 when f1 == f2 (by default this will be the proportion
    ## of f1 on the total samples)
    if (f1 == f2) {
        df$prop <- 1
        df$cut.count <- df$count
    }
    
    df <- dplyr::mutate(df, 
            prop_percent = paste(round(.data$prop, 1)*100, "%", sep = ""))
    df <- dplyr::ungroup(df)
    
    ## create label for facet (contains the proportion of f1 on total samples)
    sample_percent <- round(df$cut.count / ncol(se) * 100, 2) 
    
    df$f1_labs <- paste0(df[[f1]], " (", sample_percent, "%)")
    
    ## plotting
    ggplot2::ggplot(df, ggplot2::aes_string(x = f1, y = deparse(quote(prop)), 
                        width = deparse(quote(cut.count)), fill = f2)) +
        ggplot2::geom_bar(stat = "identity", position = "fill", 
            colour = "black") +
        ggplot2::geom_text(ggplot2::aes_string(label = "prop_percent"), 
            angle = 90, position = ggplot2::position_stack(vjust = 0.5)) +
        ggplot2::facet_grid(~ f1_labs, scales = "free_x", space = "free_x") +
        ggplot2::scale_fill_brewer() + ggplot2::theme_bw() + 
        ggplot2::ylab("proportion (%)") + 
        ggplot2::scale_y_continuous(labels = function(x) x * 100) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
}

