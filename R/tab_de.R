#' @name volcanoPlot
#' 
#' @title Volcano plot of fold changes/differences against p-values
#' 
#' @description 
#' The function `volcanoPlot` creates a volcano plot. On the y-axis the
#' -log10(p-values) are displayed, while on the x-axis the fold 
#' changes/differences are displayed.
#' The output of the function `volcanoPlot` differs depending on the
#' `type` parameter. For `type == "ttest"`, the fold changes are plotted;
#' for `type == "proDA"`, the  differences are plotted. 
#' 
#' @details
#' Internal use in `shinyQC`.
#' 
#' @param df `data.frame` as received from `topTable` (`ttest`) or 
#' `test_diff` (proDA)
#' @param type `character`
#' 
#' @examples 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' a_i <- imputeAssay(a, method = "MinDet")
#' cD <- data.frame(sample = colnames(a), 
#'     type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'                                 rowData = rD, colData = cD)
#' se_i <- SummarizedExperiment::SummarizedExperiment(assay = a_i, 
#'                                 rowData = rD, colData = cD)
#' 
#' ## create model and contrast matrix
#' modelMatrix_expr <- stats::formula("~ 0 + type")
#' contrast_expr <- "type1-type2"
#' modelMatrix <- model.matrix(modelMatrix_expr, data = colData(se))
#' contrastMatrix <- limma::makeContrasts(contrasts = contrast_expr, 
#'                                 levels = modelMatrix)
#' 
#' ## ttest
#' fit <- limma::lmFit(a_i, design = modelMatrix)
#' fit <- limma::contrasts.fit(fit, contrastMatrix)
#' fit <- limma::eBayes(fit, trend = TRUE)
#' df_ttest <- limma::topTable(fit, n = Inf, adjust = "fdr", p = 0.05)
#' df_ttest <- cbind(name = rownames(df_ttest), df_ttest)
#' 
#' ## plot
#' volcanoPlot(df_ttest, type = "ttest")
#' 
#' ## proDA
#' \donttest{
#' fit <- proDA::proDA(a, design = modelMatrix)
#' df_proDA <- proDA::test_diff(fit = fit, contrast = contrast_expr,
#'         sort_by = "adj_pval")
#' 
#' ## plot
#' volcanoPlot(df_proDA, type = "proDA")
#' }
#' 
#' @importFrom limma makeContrasts eBayes topTable
#' @importFrom stats model.matrix
#' @importFrom proDA proDA test_diff
#' @importFrom dplyr `%>%`
#' @importFrom ggplot2 ggplot aes_string geom_point ylab xlab theme_bw
#' @importFrom plotly ggplotly style
#' 
#' @return `plotly`
#' 
#' @export
volcanoPlot <- function(df, type = c("ttest", "proDA")) {
    
    type <- match.arg(type)
    if (type == "ttest") {
        
        ## add -log10(pvalue)
        df <- cbind(df, log10pvalue = -log10(df$P.Value))
        p <- ggplot2::ggplot(df, aes_string(x = "logFC", y = "log10pvalue")) + 
            ggplot2::geom_point() + 
            ggplot2::ylab("-log10(p-value)") + 
            ggplot2::xlab("log fold change") + 
            ggplot2::theme_bw()
        pp <- plotly::ggplotly(p) 
        
        ## define the ttest-specific style for the tooltip
        fc_text <- paste("log fold change:", signif(pp$x$data[[1]]$x, 3))
        pvalue_text <- paste("p-value:", signif(df$P.Value, 3))
    }
    
    if (type == "proDA") {
        
        ## add -log10(pvalue)
        df <- cbind(df, log10pvalue = -log10(df$pval))
        p <- ggplot2::ggplot(df, 
                ggplot2::aes_string(x = "diff", y = "log10pvalue")) + 
            ggplot2::geom_point() + 
            ggplot2::ylab("-log10(p-value)") + 
            ggplot2::xlab("difference") + 
            ggplot2::theme_bw()
        pp <- plotly::ggplotly(p, 
            tooltip = c("pval", "diff", "log10pvalue", "name"))
        
        ## define the proDA-specific style for the tooltip
        fc_text <- paste("diff:", signif(pp$x$data[[1]]$x, 3))
        pvalue_text <- paste("p-value:", signif(df$pval, 3))
    }
    
    ## define the general style for the tooltip
    log10_text <- paste("-log10(p-value):", signif(pp$x$data[[1]]$y, 3))
    name_text <- paste("name:", df$name)
    
    ## return 
    pp %>% 
        plotly::style(pp, text = paste0(name_text, "</br></br>",  pvalue_text,
                                "</br>", fc_text, "</br>", log10_text))
}
