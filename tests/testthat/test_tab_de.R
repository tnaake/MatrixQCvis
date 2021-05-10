#' @importFrom SummarizedExperiment SummarizedExperiment colData
#' @importFrom limma makeContrasts lmFit contrasts.fit eBayes topTable
#' @importFrom proDA proDA test_diff
#' @importFrom stats model.matrix

## function volcanoPlot
test_that("volcanoPlot", {
    
    ## create se
    a <- matrix(1:100, nrow = 10, ncol = 10, 
                dimnames = list(1:10, paste("sample", 1:10)))
    a[c(1, 5, 8), 1:5] <- NA
    set.seed(1)
    a <- a + rnorm(100)
    a_i <- imputeAssay(a, method = "MinDet")
    cD <- data.frame(sample = colnames(a), type = c(rep("1", 5), rep("2", 5)))
    rD <- data.frame(spectra = rownames(a))
    se <- SummarizedExperiment::SummarizedExperiment(assay = a, rowData = rD, 
        colData = cD)
    se_i <- SummarizedExperiment::SummarizedExperiment(assay = a_i, rowData = rD, 
        colData = cD)
    
    modelMatrix_expr <- formula("~ 0 + type")
    contrast_expr <- "type1-type2"
    modelMatrix <- stats::model.matrix(modelMatrix_expr, 
        data = SummarizedExperiment::colData(se))
    contrastMatrix <- limma::makeContrasts(contrasts = contrast_expr, 
        levels = modelMatrix)
    
    ## ttest
    fit <- limma::lmFit(a_i, design = modelMatrix)
    fit <- limma::contrasts.fit(fit, contrastMatrix)
    fit <- limma::eBayes(fit, trend = TRUE)
    df_ttest <- limma::topTable(fit, n = Inf, adjust = "fdr", p = 0.05)
    df_ttest <- cbind(name = rownames(df_ttest), df_ttest)

    ## proDA
    fit <- proDA::proDA(a, design = modelMatrix)
    df_proDA <- proDA::test_diff(fit = fit, contrast = contrast_expr,
                   sort_by = "adj_pval")
    
    expect_is(volcanoPlot(df_ttest, type = "ttest"), "plotly")
    expect_is(volcanoPlot(df_proDA, type = "proDA"), "plotly")
    expect_error(volcanoPlot(df_ttest, type = "proDA"), "non-numeric argument")
    expect_error(volcanoPlot(df_proDA, type = "ttest"), "non-numeric argument")
    expect_error(volcanoPlot(NULL, type = "ttest"), "non-numeric argument")
    expect_error(volcanoPlot(NULL, type = "proDA"), "non-numeric argument")
    expect_error(volcanoPlot(df_proDA, type = NULL), 
        "non-numeric argument")
    expect_error(volcanoPlot(df_proDA, type = "a"), "should be one of ")
    
})
