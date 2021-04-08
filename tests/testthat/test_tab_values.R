## create se
a <- matrix(1:1000, nrow = 100, ncol = 10, 
            dimnames = list(1:100, paste("sample", 1:10)))
a[c(1, 5, 8), 1:5] <- NA
set.seed(1)
a <- a + rnorm(1000)
sample <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
featData <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment(assay = a, rowData = featData, colData = sample)

## function create_boxplot
test_that("create_boxplot", {
    x <- assay(se)
    g <- create_boxplot(x)
    
    expect_error(create_boxplot(se), "no method for coercing this S4 class")
    expect_error(create_boxplot(x, "test", "", TRUE), "not interpretable as logical")
    expect_error(create_boxplot(x, "test", TRUE, ""), "invalid argument type")
    # expect_error(create_boxplot(x, se, TRUE, TRUE),
    #     "no method for coercing this S4 class")
    expect_is(g, "gg")
})

## function driftPlot
test_that("driftPlot", {
    expect_is(
        driftPlot(se = se, aggregation = "median", category = "type", 
            orderCategory = "type", level = "all", method = "loess"), "gg")
  expect_error(driftPlot(se = se, aggregation = "", category = "type", 
      orderCategory = "type", level = "all", method = "loess"), 
      "should be one of")
  expect_error(driftPlot(se = se, aggregation = "median", category = "foo", 
      orderCategory = "type", level = "all", method = "loess"),
      "should be one of")
  expect_error(driftPlot(se = se, aggregation = "median", category = "type", 
      orderCategory = "foo", level = "all", method = "loess"),
      "should be one of")
  expect_error(driftPlot(se = se, aggregation = "median", category = "type", 
      orderCategory = "type", level = "foo", method = "loess"),
      "should be one of")
  expect_error(driftPlot(se = se, aggregation = "median", category = "type", 
      orderCategory = "type", level = "all", method = "foo"),
      "should be one of")
  
  
})

## function cv
test_that("cv", {
    x <- matrix(1:100, ncol = 5)
    expect_equal(cv(x)$raw, 
        c(56.343617, 19.396983, 11.715009,  8.391603,  6.537105))
    expect_equal(names(cv(x)), "raw")
    expect_equal(names(cv(x, NULL)), NULL)
    expect_true(is.numeric(cv(x)$raw))
    expect_error(cv("foo"), "must have a positive length")
    expect_error(cv(1:100), "must have a positive length")
})

## function plotCV
test_that("plotCV", {
    x1 <- matrix(1:100, ncol = 5)
    x2 <- matrix(101:200, ncol = 5)
    df <- data.frame(cv(x1, "raw"), cv(x2, "normalized"))
    g <- plotCV(df)
    expect_error(plotCV(NULL), "df is not a data.frame")
    expect_error(plotCV(NA), "df is not a data.frame")
    expect_error(plotCV(x1), "df is not a data.frame")
    expect_is(g, "gg")
})

## function ECDF
test_that("ECDF", {
  
    sample_1 <- colnames(se)[1]
    x <- assay(se)
    g <- ECDF(se, sample_1, "all")
    
    expect_error(ECDF(x, sample_1, "all"), "unable to find an inherited method")
    expect_error(ECDF(se, "", "all"), "'arg' should be one of")
    expect_error(ECDF(se, sample_1, "test"), "'arg' should be one of")
    expect_is(g, "gg")
})

## function distShiny
test_that("distShiny", {
    x <- assay(se)

    matTest <- matrix(
        c(0,        173.0173, 349.8123, 
          173.0173, 0,        176.7957,
          349.8123, 176.7957, 0), byrow = TRUE, ncol = 3, nrow = 3,
        dimnames = list(c(colnames(x)[1:3]), c(colnames(x)[1:3])))
    
    expect_error(distShiny(x, "test"), "invalid distance method")
    expect_equal(distShiny(x[1:3, 1:3], method = "euclidean"), matTest, 
                 tolerance = 1e-02)
    expect_true(is.matrix(distShiny(x)))
})

## function distSample
test_that("distSample", {
    x <- assay(se)
    d <- distShiny(x, method = "euclidean")
    g <- distSample(d, se, label = "type")
    
    expect_error(distSample(d[1:3, 1:3], se = se, label = "type"), 
        "number of observations in top")
    expect_error(distSample(d[, 1:3], se = se, label = "type"), 
        "number of observations in top")
    expect_error(distSample(d, se = se[, 1:3], label = "type"), 
        "number of observations in top")
    expect_is(g, "Heatmap")
})

## function sumDistSample
test_that("sumDistSample", {
    x <- assay(se)
    d <- distShiny(x, method = "euclidean")
    g <- sumDistSample(d)
    
    ##expect_error(sumDistSample(d, se), "no method for coercing this S4 class")
    ##expect_error(sumDistSample(matrix(1:3)), "requires the following aesthetics")
    expect_error(sumDistSample(se), 
        "must be an array of at least two dimensions")
    expect_is(g, "plotly")
})

## function MAvalues
test_that("MAvalues", {
    tbl_test <- tibble(Feature = as.character(c(rep(2, 3), rep(3, 3))), 
        name = rep(colnames(se)[1:3], 2),
        A = c(4.149180, 5.535751, 5.785051, 4.144537, 5.534471, 5.785192),
        M = c( -6.044885, 2.274541, 3.770344, -6.061178, 2.278427, 3.782751),
        type = "1")
    ma <- MAvalues(se = se, group = "all")
    
    expect_error(MAvalues(assay(se), log2 = TRUE, "all"), 
        "unable to find an inherited method for function")
    expect_error(MAvalues(se, log2 = TRUE, group = ""), "'arg' should be one of")
    expect_error(MAvalues(se, log2 = "", group = "all"), 
        "argument is not interpretable as logical")
    expect_true(is.data.frame(ma))
    expect_equal(dim(ma), c(1000, 5))
    expect_true(is.character(ma$Feature))
    expect_true(is.character(ma$name))
    expect_true(is.numeric(ma$A))
    expect_true(is.numeric(ma$M))
    expect_true(is.character(ma$type))
    expect_equal(MAvalues(se[2:3, 1:3], group = "all"), tbl_test, tolerance = 1e-07)
})

## function hoeffDvalues
test_that("hoeffDValues", {
    tbl <- MAvalues(se)
    l_test <- list(raw = c(0.9901381, 0.7174001, 0.2431791, 0.8799139,
        0.9270406, 0.9566922, 0.9671259, 0.9773905, 0.9788422, 0.9766581))
    names(l_test$raw) <- colnames(se)
    
    hD <- hoeffDValues(tbl)

    expect_error(hoeffDValues(tbl[, !colnames(tbl) %in% "A"]),
        "Column `A` doesn't exist")
    expect_error(hoeffDValues(tbl[, !colnames(tbl) %in% "M"]),
        "Column `M` doesn't exist")
    expect_equal(hD, l_test, tolerance = 1e-07)
    expect_true(is.list(hD))
    expect_equal(length(hD), 1)
    expect_equal(length(unlist(hD)), length(unique(tbl$name)))
})

## function hoeffDPlot
test_that("hoeffDPlot", {
    l_1 <- list(raw = 1:10)
    l_2 <- list(normalized = 1:10)
    l_3 <- list(transformed = 1:10)
    l_4 <- list("batch corrected" = 1:10)
    l_5 <- list(imputed = 1:10)
    l_6 <- list(foo = 1:9)
    df <- data.frame(l_1, l_2)
    g <- hoeffDPlot(df)
    
    df <- data.frame(l_1, l_2, l_3, l_4, l_5)
    expect_error(hoeffDPlot(df, lines = ""),
        "argument is not interpretable as logical")
    expect_error({data.frame(l_1, l_6); hoeffDPlot(df)}, 
        "arguments imply differing number of rows")
    expect_is(g, "plotly")
})

## function MAplot
test_that("MAplot", {
    tbl <- MAvalues(se, group = "all")
    g <- MAplot(tbl, group = "all", plot = "all")
    
    expect_error(MAplot(se, "all"), 
        "no applicable method for")
    expect_error(MAplot(tbl, group = "foo", plot = "all"), "should be one of ")
    expect_error(MAplot(tbl, group = "all", plot = "foo"), "should be one of ")
    expect_is(g, "gg")
})

## function createDfFeature
test_that("createDfFeature", {
    
    x1 <- matrix(1:100, ncol = 10, nrow = 10, 
         dimnames = list(paste("feature", 1:10), paste("sample", 1:10)))
    x2 <- x1 + 5
    x3 <- x2 + 10
     
    l <- list(x1 = x1, x2 = x2, x3 = x3)
    df <- createDfFeature(l, "feature 1")
    expect_equal(df$x1, seq(1, 91, 10))
    expect_equal(df$x2, seq(6, 96, 10))
    expect_equal(df$x3, seq(16, 106, 10))
    expect_is(df, "data.frame")
    expect_identical(rownames(df), paste("sample", 1:10))
    expect_error(createDfFeature(l, "foo"), "subscript out of bounds")
    expect_error(createDfFeature("foo", "feature 1"), 
        "incorrect number of dimensions")
})

## function featurePlot
test_that("featurePlot", {
    x1 <- matrix(1:100, ncol = 10, nrow = 10, 
      dimnames = list(paste("feature", 1:10), paste("sample", 1:10)))
    x2 <- x1 + 5
    x3 <- x2 + 10
    
    l <- list(x1 = x1, x2 = x2, x3 = x3)
    df <- createDfFeature(l, "feature 1")
    expect_is(featurePlot(df), "gg")
    expect_error(featurePlot(NULL), "df is not a data.frame")
})

## function cvFeaturePlot
test_that("cvFeaturePlot", {
  x1 <- matrix(1:100, ncol = 10, nrow = 10, 
               dimnames = list(paste("feature", 1:10), paste("sample", 1:10)))
  x2 <- x1 + 5
  x3 <- x2 + 10
  
  l <- list(x1 = x1, x2 = x2, x3 = x3)
  
  expect_is(cvFeaturePlot(l = l, lines = FALSE), "plotly")
  expect_is(cvFeaturePlot(l = l, lines = TRUE), "plotly")
  expect_error(cvFeaturePlot(l = NULL, lines = TRUE), 
      "subset columns that don't exist")
  expect_error(cvFeaturePlot(l = l, lines = "foo"), 
      "argument is not interpretable as logical")
})

## function normalize
test_that("normalize", {
    x <- assay(se)
    x_n <- normalize(x, method = "none")
    x_s <- normalize(x, method = "sum")
    x_qd <- normalize(x, method = "quantile division", probs = 0.75)
    x_q <- normalize(x, method = "quantile")
    expect_error(normalize(se), 
        "no method for coercing this S4 class to a vector")
    expect_error(normalize(x, method = "quantile division"), 
        'argument "probs" is missing, with no default')
    expect_error(normalize(x, "foo"), "'arg' should be one of ")
    expect_equal(x_n, x)
    expect_true(is.matrix(x_n))
    expect_true(is.matrix(x_s))
    expect_true(is.matrix(x_qd))
    expect_true(is.matrix(x_q))
    expect_equal(dim(x_n), dim(x))
    expect_equal(dim(x_s), dim(x))
    expect_equal(dim(x_qd), dim(x))
    expect_equal(dim(x_q), dim(x))
    expect_equal(rownames(x_n), rownames(x))
    expect_equal(rownames(x_s), rownames(x))
    expect_equal(rownames(x_qd), rownames(x))
    expect_equal(rownames(x_q), rownames(x))
    expect_equal(colnames(x_n), colnames(x))
    expect_equal(colnames(x_s), colnames(x))
    expect_equal(colnames(x_qd), colnames(x))
    expect_equal(colnames(x_q), colnames(x))
})

## function transform
test_that("transform", {
    x <- assay(se)
    x_n <- transform(x, method = "none")
    x_l <- transform(x, method = "log2")
    x_v <- transform(x, method = "vsn")
    
    expect_error(transform(se), 
        "no method for coercing this S4 class to a vector")
    expect_error(transform(x, "foo"), "'arg' should be one of ")
    expect_equal(x_n, x)
    expect_true(is.matrix(x_n))
    expect_true(is.matrix(x_l))
    expect_true(is.matrix(x_v))
    expect_equal(dim(x_n), dim(x))
    expect_equal(dim(x_l), dim(x))
    expect_equal(dim(x_v), dim(x))
    expect_equal(rownames(x_n), rownames(x))
    expect_equal(rownames(x_l), rownames(x))
    expect_equal(rownames(x_v), rownames(x))
    expect_equal(colnames(x_n), colnames(x))
    expect_equal(colnames(x_l), colnames(x))
    expect_equal(colnames(x_v), colnames(x))
})

## function batch
test_that("batch", {
  a <- matrix(1:1000, nrow = 100, ncol = 10, 
      dimnames = list(1:100, paste("sample", 1:10)))
  se_b <- se
  assay(se_b) <- a
  x_n <- batch(se_b, method = "none")
  x_l <- batch(se_b, method = "removeBatchEffect (limma)", batchColumn = "type")
  
  expect_error(batch(a), 
      "unable to find an inherited method for function")
  expect_error(batch(se_b, "foo"), "'arg' should be one of ")
  expect_error(batch(se_b, "removeBatchEffect (limma)", "foo"),
      "batchColumn not in")
  expect_equal(x_n, a)
  expect_true(is.matrix(x_n))
  expect_true(is.matrix(x_l))
  expect_equal(dim(x_n), dim(a))
  expect_equal(dim(x_l), dim(a))
  expect_equal(rownames(x_n), rownames(a))
  expect_equal(rownames(x_l), rownames(a))
  expect_equal(colnames(x_n), colnames(a))
  expect_equal(colnames(x_l), colnames(a))
})

## function impute
test_that("impute", {
    x <- assay(se)
    x_bpca <- impute(x, "BPCA")
    x_knn <- impute(x, "kNN")
    x_min <- impute(x, "Min")
    x_mindet <- impute(x, "MinDet")
    x_minprob <- impute(x, "MinProb")
    
    expect_error(impute(se), 
        "no method for coercing this S4 class to a vector")
    expect_error(impute(se, "foo"), "'arg' should be one of")
    expect_true(is.matrix(x_bpca))
    expect_true(is.matrix(x_knn))
    expect_true(is.matrix(x_min))
    expect_true(is.matrix(x_mindet))
    expect_true(is.matrix(x_minprob))
    expect_equal(dim(x_bpca), dim(x))
    expect_equal(dim(x_knn), dim(x))
    expect_equal(dim(x_min), dim(x))
    expect_equal(dim(x_mindet), dim(x))
    expect_equal(dim(x_minprob), dim(x))
    expect_equal(rownames(x_bpca), rownames(x))
    expect_equal(rownames(x_knn), rownames(x))
    expect_equal(rownames(x_min), rownames(x))
    expect_equal(rownames(x_mindet), rownames(x))
    expect_equal(rownames(x_minprob), rownames(x))
    expect_equal(colnames(x_bpca), colnames(x))
    expect_equal(colnames(x_knn), colnames(x))
    expect_equal(colnames(x_min), colnames(x))
    expect_equal(colnames(x_mindet), colnames(x))
    expect_equal(colnames(x_minprob), colnames(x))
})


