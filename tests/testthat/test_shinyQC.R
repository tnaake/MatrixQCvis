#' @importFrom SummarizedExperiment SummarizedExperiment 


## function shinyQC
test_that("shinyQC", {
    ## create SummarizedExperiment that doesn't comply with the expectations
    se <- SummarizedExperiment::SummarizedExperiment(
        assays = list(counts = matrix(100 * runif(100 * 8), 100, 8)),
        colData = data.frame(sample = paste0("S", seq_len(8)),
            group = sample(LETTERS[c(1, 2)], 8, replace = TRUE),
            name = paste0("S", seq_len(8)))
    )
    expect_error(shinyQC(se), "rownames[(]se[)] is NULL")
    rownames(se) <- seq_len(100)
    expect_error(shinyQC(se), "colnames[(]se[)] is NULL")
    colnames(se) <- seq_len(8)
    tmp <- 1
    expect_error(shinyQC(tmp), "se is not of class 'SummarizedExperiment'")
})

## function .initialize_server
test_that(".initialize_server", {
    se <- SummarizedExperiment::SummarizedExperiment(
        assays = list(counts = matrix(100 * runif(100 * 8), 100, 8)),
        colData = data.frame(sample = paste0("S", seq_len(8)),
            group = sample(LETTERS[c(1, 2)], 8, replace = TRUE),
            name = paste0("S", seq_len(8)))
    )
    expect_error(MatrixQCvis:::.initialize_server(se = se, input = new.env(),
        output = new.env, session = new.env(), missingValue = NULL,
        envir = new.env()), "missingValue has to be logical")
    expect_error(MatrixQCvis:::.initialize_server(se = se, input = new.env(),
        output = new.env, session = new.env(), missingValue = TRUE,
        envir = NULL), "envir has to be of class environment")
    expect_is(MatrixQCvis:::.initialize_server, "function")
})

