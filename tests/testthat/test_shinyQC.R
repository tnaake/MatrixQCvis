#' @importFrom SummarizedExperiment SummarizedExperiment 


## function shinyQC
test_that("shinyQC", {
    ## create SummarizedExperiment that doesn't comply with the expectations
    se <- SummarizedExperiment::SummarizedExperiment(
        assays = list(counts = matrix(100 * runif(100 * 8), 100, 8)),
        colData = S4Vectors::DataFrame(sample = paste0("S", 1:8),
            group = sample(LETTERS[1:2], 8, replace = TRUE),
            name = paste0("S", 1:8))
    )
    expect_error(shinyQC(se), "rownames[(]se[)] is NULL")
    rownames(se) <- 1:100
    expect_error(shinyQC(se), "colnames[(]se[)] is NULL")
    colnames(se) <- 1:8
    tmp <- 1
    expect_error(shinyQC(tmp), "se is not of class 'SummarizedExperiment'")
    se_tmp <- se
    colnames(colData(se_tmp))[1] <- "rowname"
    expect_error(shinyQC(se_tmp), 
        "colData[(]se[)] should not have column 'rowname'")
})

## function .initialize_server
test_that(".initialize_server", {
    expect_is(MatrixQCvis:::.initialize_server, "function")
})
