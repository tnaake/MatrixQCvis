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

## function samples_memi
test_that("samples_memi", {
    measured <- c(7, 10, 7, 7, 7, 7, 10, 10, 10, 10)
    missing <- c(3, 0, 3, 3, 3, 3, 0, 0, 0, 0)
    x <- samples_memi(se)
    expect_equal(dplyr::pull(x, "measured"), measured)
    expect_equal(dplyr::pull(x, "missing"), missing)
    expect_is(x, "tbl")
    expect_error(samples_memi(NULL), "unable to find an inherited method for")
    expect_error(samples_memi("foo"), "unable to find an inherited method for")
    expect_error(samples_memi(1:10), "unable to find an inherited method for")
})

## function barplot_samples_memi
test_that("barplot_samples_memi", {
    x <- samples_memi(se)
    expect_is(barplot_samples_memi(x, measured = TRUE), "plotly")
    expect_is(barplot_samples_memi(x, measured = FALSE), "plotly")
    ##expect_error(barplot_samples_memi(NULL, measured = FALSE), "not found")
    expect_error(barplot_samples_memi("foo", measured = FALSE), 
        "must be a data frame, or other object coercible by")
    expect_error(barplot_samples_memi(1:10, measured = FALSE), 
        "must be a data frame, or other object coercible by")
    expect_error(barplot_samples_memi(x, measured = NULL), 
        "argument is of length zero")
    
})


## function hist_feature
test_that("hist_feature", {
    x <- data.frame(A_1 = c(1, 1, 1), A_2 = c(1, NA, 1), A_3 = c(1, NA, 1), 
        B_1 = c(1, 1, 1), B_2 = c(NA, 1, 1), B_3 = c(NA, 1, 1))
    g <- hist_feature(x, binwidth = 1, measured = TRUE)
    
    expect_error(hist_feature(x = NULL), 
        "must be an array of at least two dimensions")
    expect_error(hist_feature(x = df, measured = ""), 
        "argument is not interpretable as logical")
    # expect_warning(hist_feature(x = df, binwidth = ""), 
    #     "`width` must be a numeric scalar")
    #expect_warning(hist_feature(x = df, binwidth = 0), 
    #     "`binwidth` must be positive")
    expect_is(g, "plotly")
})

## function measured_category
test_that("measured_category", {
    mc_t <- measured_category(se, measured = TRUE, category = "type")
    mc_f <- measured_category(se, measured = FALSE, category = "type")
    
    tbl_1 <- tibble::tibble(feature = 1:10, 
        "1" = c(0, 5, 5, 5, 0, 5, 5, 0, 5, 5), 
        "2" = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
    colnames(tbl_1) <- c("feature", 1, 2)
    tbl_2 <- tibble::tibble(feature = 1:10, 
        "1" = c(5, 0, 0, 0, 5, 0, 0, 5, 0, 0), 
        "2" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    colnames(tbl_2) <- c("feature", 1, 2)
    
    expect_true(all(mc_t == tbl_1))
    expect_true(all(mc_f == tbl_2))
    expect_true(tibble::is_tibble(mc_t))
    expect_true(tibble::is_tibble(mc_f))
    expect_equal(dim(mc_t), c(10, 3))
    expect_equal(dim(mc_f), c(10, 3))
    expect_error(
        measured_category(se = se, measured = TRUE, category = "foo"), 
        "'arg' should be one of ")
    expect_error(
        measured_category(se = NULL, measured = TRUE, category = "type"),
        "unable to find an inherited method for function")
    expect_error(
        measured_category(se = se, measured = "", category = "type"),
        "argument is not interpretable as logical")
})

## function hist_feature_category
test_that("hist_feature_category", { 
    g <- hist_feature_category(se = se, measured = TRUE, 
        category = "type", binwidth = 2)
    expect_error(
        hist_feature_category(NULL, measured = TRUE, category = "type", 
            binwidth = 2), "unable to find an inherited method for function")
    expect_error(
        hist_feature_category(se, measured = "", category = "type", 
            binwidth = 2), "argument is not interpretable as logical")
    expect_error(
        hist_feature_category(se, measured = TRUE, category = "foo", 
            binwidth = 2), "'arg' should be one of ")
    expect_is(g, "plotly")
})

## function upset_category
test_that("upset_category", {
    g <- upset_category(se, category = "type")
    expect_error(upset_category(NULL, category = "type"),
        "unable to find an inherited method for function")
    expect_error(upset_category(se, category = "foo"),
        "should be one of ")
    expect_equal(upset_category(se, category = "type", measured = FALSE), NULL)
    expect_error(upset_category(se, category = "type", measured = ""),
        "argument is not interpretable as logical")
    expect_is(g, "upset")
    
    se <- SummarizedExperiment(
        assays = list(counts = matrix(100 * runif(100 * 8), 100, 8)),
        colData = DataFrame(sample = paste0("S", 1:8),
                            type = sample(LETTERS[1:2], 8, replace = TRUE),
                            name = paste0("S", 1:8))
    )
    assay(se)[5, 1] <- NA
    g <- upset_category(se, category = "type", measured = FALSE)
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
            category = "type"), 
        "unable to find an inherited method for function")
})

