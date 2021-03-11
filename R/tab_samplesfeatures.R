#' @name hist_sample
#'
#' @title Return the number of a category
#'
#' @description `hist_sample_num` returns the number of a category 
#' (e.g. sample types) as a `tibble`.
#' The function will retrieve first a column in `colData(se)`. 
#' The function will return a `tibble` containing the numerical 
#' values of the quantities.
#'
#' @param se `SummarizedExperiment` object
#' @param category `character`, corresponding to a column in `colData(se)`
#'
#' @return `tibble`
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
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = rD, colData = cD)
#' 
#' hist_sample_num(se, category = "type")
#' 
#' @import ggplot2
#' 
#' @export
hist_sample_num <- function(se, category = "type") { 

    
    ## retrieve the sample type
    df <- colData(se)[[category]]
    
    ## retrieve the number of samples per sample type
    tab <- table(df)
    tbl <- tibble(names = names(tab), values = as.vector(tab))
    return(tbl)
}

#' @name hist_sample
#'
#' @title Plot a histogram of the number of a category
#'
#' @description `hist_sample` plots the number of a category (e.g. sample types)
#' as a histogram. It use the returned `tibble` from `hist_sample_num`.
#'
#' @param tbl `tibble` as returned by `hist_sample_num`
#' @param category `character`, x-axis label of the plot 
#'
#' @return `gg` object from `ggplot2`
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
#' tbl <- hist_sample_num(se, category = "type")
#' hist_sample(tbl)
#' 
#' @import ggplot2
#' 
#' @export
hist_sample <- function(tbl, category = "type") {
    ## do the actual plotting
    p <- ggplot(tbl, aes_string(x = "names", y = "values")) + 
        geom_bar(stat = "identity") + ggtitle("Number of samples") + 
        ylab("number") + xlab(category) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 90))
    ggplotly(p = p, tooltip = c("x", "y"))
}
   


#' @name mosaic
#' 
#' @title Mosaic plot for two factors in colData(se)
#' 
#' @description 
#' The function `mosaic` creates a mosaic plot of two factors from
#' an `SummararizedExperiment` object. The columns `f1` and `f2` are taken from `colData(se)`.
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
#' library(dplyr)
#' library(SummarizedExperiment)
#' 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' sample <- data.frame(name = colnames(a), 
#'     type = c(rep("1", 5), rep("2", 5)),
#'     cell_type = c("A", "B"))
#' featData <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = featData, colData = sample)
#' 
#' mosaic(se, "cell_type", "type")
#' 
#' @importFrom rlang :=
#' 
#' @export
mosaic <- function(se, f1, f2) {
    
    df <- colData(se) %>% 
        as.data.frame() %>% 
        group_by(!!f1 := get(f1), !!f2 := get(f2)) %>% 
        summarise(count = n()) %>%
        mutate(cut.count = sum(count),
               prop = (count/sum(count)))
    
    ## set prop to 1 when f1 == f2 (by default this will be the proportion
    ## of f1 on the total samples)
    if (f1 == f2) {
        df$prop <- 1
        df$cut.count <- df$count
    }
    
    df <- df %>%
        mutate(prop_percent = paste(round(.data$prop, 1)*100, "%", sep = "")) %>% 
        ungroup()
    
    ## create label for facet (contains the proportion of f1 on total samples)
    sample_percent <- round(df$cut.count / ncol(se) * 100, 2) 
    
    df$f1_labs <- paste0(df[[f1]], " (", sample_percent, "%)")
    
    ## plotting
    ggplot(df, aes_string(x = f1, y = deparse(quote(prop)), #
                          width = deparse(quote(cut.count)), fill = f2)) +
        geom_bar(stat = "identity", position = "fill", colour = "black") +
        geom_text(aes_string(label = "prop_percent"), angle = 90,
                  position = position_stack(vjust = 0.5)) +
        facet_grid(~f1_labs, scales = "free_x", space = "free_x") +
        scale_fill_brewer() + theme_bw() + ylab("proportion (%)") + 
        scale_y_continuous(labels= function(x) x*100) +
        theme(axis.text.x = element_text(angle = 90))
}

