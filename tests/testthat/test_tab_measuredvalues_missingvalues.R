#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom dplyr pull
#' @importFrom tibble tibble is_tibble

## create se
a <- matrix(1:100, nrow = 10, ncol = 10, 
            dimnames = list(1:10, paste("sample", 1:10)))
a[c(1, 5, 8), 1:5] <- NA
set.seed(1)
a <- a + rnorm(100)
cD <- data.frame(sample = colnames(a), type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment::SummarizedExperiment(assay = a, rowData = rD, 
    colData = cD)

## function samplesMeasuredMissing
test_that("samplesMeasuredMissing", {
    measured <- c(7, 7, 7, 7, 7, 10, 10, 10, 10, 10)
    missing <- c(3, 3, 3, 3, 3, 0, 0, 0, 0, 0)
    x <- samplesMeasuredMissing(se)
    expect_equal(dplyr::pull(x, "name"), 
        c("sample 1", "sample 2", "sample 3", "sample 4", "sample 5", 
          "sample 6", "sample 7", "sample 8", "sample 9", "sample 10"))
    expect_equal(as.numeric(dplyr::pull(x, "measured")), measured)
    expect_equal(as.numeric(dplyr::pull(x, "missing")), missing)
    expect_is(x, "tbl")
    expect_error(samplesMeasuredMissing(NULL), "unable to find an inherited")
    expect_error(samplesMeasuredMissing("foo"), "unable to find an inherited")
    expect_error(samplesMeasuredMissing(1:10), "unable to find an inherited")
})

## function barplotSamplesMeasuredMissing
test_that("barplotSamplesMeasuredMissing", {
    x <- samplesMeasuredMissing(se)
    expect_is(barplotSamplesMeasuredMissing(x, measured = TRUE), "plotly")
    expect_is(barplotSamplesMeasuredMissing(x, measured = FALSE), "plotly")
    ##expect_error(barplotSamplesMeasuredMissing(NULL, measured = FALSE), "not found")
    expect_error(barplotSamplesMeasuredMissing("foo", measured = FALSE), 
        "must be a ")
    expect_error(barplotSamplesMeasuredMissing(1:10, measured = FALSE), 
        "must be a ")
    expect_error(barplotSamplesMeasuredMissing(x, measured = NULL), 
        "argument is of length zero")
    
})


## function histFeature
test_that("histFeature", {
    x <- data.frame(A_1 = c(1, 1, 1), A_2 = c(1, NA, 1), A_3 = c(1, NA, 1), 
        B_1 = c(1, 1, 1), B_2 = c(NA, 1, 1), B_3 = c(NA, 1, 1))
    g <- histFeature(x, binwidth = 1, measured = TRUE)
    
    expect_error(histFeature(x = NULL), 
        "must be an array of at least two dimensions")
    expect_error(histFeature(x = df, measured = ""), 
        "argument is not interpretable as logical")
    # expect_warning(histFeature(x = df, binwidth = ""), 
    #     "`width` must be a numeric scalar")
    #expect_warning(histFeature(x = df, binwidth = 0), 
    #     "`binwidth` must be positive")
    expect_is(g, "plotly")
})

## function measuredCategory
test_that("measuredCategory", {
    mc_t <- measuredCategory(se, measured = TRUE, category = "type")
    mc_f <- measuredCategory(se, measured = FALSE, category = "type")
    
    tbl_1 <- data.frame( 
        "1" = c(0, 5, 5, 5, 0, 5, 5, 0, 5, 5), 
        "2" = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)) |> as.matrix()
    colnames(tbl_1) <- c(1, 2)
    tbl_2 <- data.frame(
        "1" = c(5, 0, 0, 0, 5, 0, 0, 5, 0, 0), 
        "2" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) |> as.matrix()
    colnames(tbl_2) <- c(1, 2)
    
    expect_true(all(mc_t == tbl_1))
    expect_true(all(mc_f == tbl_2))
    expect_true(is.matrix(mc_t))
    expect_true(is.matrix(mc_f))
    expect_equal(dim(mc_t), c(10, 2))
    expect_equal(dim(mc_f), c(10, 2))
    expect_error(
        measuredCategory(se = se, measured = TRUE, category = "foo"), 
        "'arg' should be one of ")
    expect_error(
        measuredCategory(se = NULL, measured = TRUE, category = "type"),
        "trying to get slot")
    expect_error(
        measuredCategory(se = se, measured = "", category = "type"),
        "argument is not interpretable as logical")
})

## function histFeatureCategory
test_that("histFeatureCategory", { 
    g <- histFeatureCategory(se = se, measured = TRUE, 
        category = "type", binwidth = 2)
    expect_error(
        histFeatureCategory(NULL, measured = TRUE, category = "type", 
            binwidth = 2), "trying to get slot")
    expect_error(
        histFeatureCategory(se, measured = "", category = "type", 
            binwidth = 2), "argument is not interpretable as logical")
    expect_error(
        histFeatureCategory(se, measured = TRUE, category = "foo", 
            binwidth = 2), "'arg' should be one of ")
    expect_is(g, "plotly")
})

## function upset_category
test_that("upset_category", {
    g <- upsetCategory(se, category = "type")
    expect_error(upsetCategory(NULL, category = "type"), "unable to find an")
    expect_error(upsetCategory(se, category = "foo"), "should be one of ")
    expect_equal(upsetCategory(se, category = "type", measured = FALSE), NULL)
    expect_error(upsetCategory(se, category = "type", measured = ""),
        "argument is not interpretable as logical")
    expect_is(g, "upset")
    
    se_2 <- SummarizedExperiment(
        assays = list(counts = matrix(100 * runif(100 * 8), 100, 8)),
        colData = DataFrame(sample = paste0("S", 1:8),
                            type = sample(LETTERS[1:2], 8, replace = TRUE),
                            name = paste0("S", 1:8))
    )
    assay(se_2)[5, 1] <- NA
    g <- upsetCategory(se_2, category = "type", measured = FALSE)
    expect_equal(g, NULL)
    
})

## function extractComb
test_that("extractComb", {
    expect_equal(
        extractComb(se = se, combination = "1", measured = TRUE, 
            category = "type"), "no features for this combination")
    expect_equal(
        extractComb(se = se, combination = "2", measured = TRUE, 
            category = "type"), c("1", "5", "8"))
    expect_equal(
        extractComb(se = se, combination = c("1", "2"), measured = TRUE, 
            category = "type"), c("2", "3", "4", "6", "7", "9", "10"))
    expect_equal(
        extractComb(se = se, combination = "1", measured = FALSE, 
            category = "type"), c("1", "5", "8"))
    expect_equal(
        extractComb(se = se, combination = "2", measured = FALSE, 
            category = "type"), "no features for this combination")
    expect_equal(
        extractComb(se = se, combination = c("1", "2"), measured = FALSE, 
            category = "type"), "no features for this combination")
    expect_error(
        extractComb(se = se, combination = "1", measured = "", 
            category = "type"), "argument is not interpretable as logical")
    expect_error(
        extractComb(se = se, combination = "1", measured = TRUE, 
            category = "foo"), 
        "'arg' should be one of ")
    expect_error(
        extractComb(se = NULL, combination = "1", measured = TRUE, 
            category = "type"), "trying to get slot")
})

