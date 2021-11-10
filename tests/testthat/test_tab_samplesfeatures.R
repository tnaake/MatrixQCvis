#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom tibble tibble

## create se
a <- matrix(1:100, nrow = 10, ncol = 10, 
    dimnames = list(1:10, paste("sample", 1:10)))
a[c(1, 5, 8), 1:5] <- NA
set.seed(1)
a <- a + rnorm(100)
cD <- data.frame(sample = colnames(a), type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
    rowData = rD, colData = cD)

## function hist_sample
test_that("hist_sample_num and hist_sample", {
    tbl <- hist_sample_num(se, category = "type")
    g <- hist_sample(tbl)
    
    
    expect_equal(tbl, 
        tibble::tibble(names = c("1", "2"), values = as.integer(c(5.0, 5.0))))
    expect_is(g, "plotly")
    # expect_error(hist_sample_num(se, category = "foo"), 
    #     "Input must be a vector")
    expect_error(hist_sample_num(NULL, category = "type"),
        "unable to find an inherited method for function")
})

## function mosaic
test_that("mosaic", {
    g <- mosaic(se, "sample", "type")
    
    expect_error(mosaic(NULL, "sample", "type"), 
        "unable to find an inherited method for function")
    expect_error(mosaic(se, "foo", "type"), "'arg' should be one of ")
    expect_error(mosaic(se, "sample", "foo"), "'arg' should be one of ")
    expect_is(g, "gg")
}) 

