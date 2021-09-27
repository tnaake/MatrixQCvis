## create se
a <- matrix(1:100, nrow = 10, ncol = 10,
    dimnames = list(1:10, paste("sample", 1:10)))
a[c(1, 5, 8), 1:5] <- NA
set.seed(1)
a <- a + rnorm(100)
cD <- data.frame(name = colnames(a),
    type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment::SummarizedExperiment(assay = a, rowData = rD, 
                                                                colData = cD)

## function tag_loadMessage
test_that("tag_loadMessage", {
    expect_is(tag_loadMessage(), "shiny.tag.list")
})

## function tag_keepAlive
test_that("tag_keepAlive", {
    expect_is(tag_keepAlive(), "shiny.tag.list")
})

## function sidebar_assayUI
test_that("sidebar_assayUI", {
    expect_is(sidebar_assayUI(), "shiny.tag")
    expect_is(sidebar_assayUI(), "shiny.tag")
})

test_that("sidebar_imputationUI", {
    expect_is(sidebar_imputationUI(), "shiny.tag")
})

## function sidebar_DEUI
test_that("sidebar_DEUI", {
    expect_is(sidebar_DEUI(), "shiny.tag")
})

## function sidebar_excludeSampleUI
test_that("sidebar_excludeSampleUI", {
    expect_is(sidebar_excludeSampleUI(""), "shiny.tag")
})

## excludeSampleServer
test_that("sidebar_excludeSampleServer", {
    shiny::testServer(sidebar_excludeSampleServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        se <- new.env()

        out <- sidebar_excludeSampleServer("", se = se)
        expect_is(out, "shiny.render.function")
    })
})

## sidebar_reportUI
test_that("sidebar_reportUI", {
    expect_is(sidebar_reportUI(), "shiny.tag")
})

## function sidebar_selectAssayUI
test_that("sidebar_selectAssayUI", {
    expect_is(sidebar_selectAssayUI(choicesAssaySE = 1:2), "shiny.tag")
})

## function choiceDataSE
test_that("choiceAssaySE", {
    se_2 <- se
    assays(se_2)[[2]] <- assay(se)
    names(SummarizedExperiment::assays(se_2)) <- c("abc", "def")
    expect_equal(choiceAssaySE(se_2), c("abc", "def"))
    names(SummarizedExperiment::assays(se_2)) <- c("abc", NA)
    expect_error(choiceAssaySE(se_2), "contains NA")
    names(SummarizedExperiment::assays(se_2)) <- NULL
    expect_equal(choiceAssaySE(se_2), 1:2)
})

## function selectAssaySE
test_that("selectAssaySE", {
    se_2 <- se
    assays(se_2)[[2]] <- assay(se)
    names(assays(se_2)) <- c("abc", "def")
    expect_equal(assay(selectAssaySE(se_2, "abc")), 
        SummarizedExperiment::assays(se_2)[[1]])
    expect_equal(colData(selectAssaySE(se_2, "abc")), 
        SummarizedExperiment::colData(se))
    expect_equal(rowData(selectAssaySE(se_2, "abc")), 
        SummarizedExperiment::rowData(se))
    expect_equal(assay(selectAssaySE(se_2, "def")), 
        SummarizedExperiment::assays(se_2)[[2]])
    expect_equal(colData(selectAssaySE(se_2, "def")), 
        SummarizedExperiment::colData(se))
    expect_equal(rowData(selectAssaySE(se_2, "def")), 
        SummarizedExperiment::rowData(se))
    names(assays(se_2)) <- c(1, 2)
    expect_equal(assay(selectAssaySE(se_2, 1)), 
        SummarizedExperiment::assays(se_2)[[1]])
    expect_equal(colData(selectAssaySE(se_2, 1)), 
        SummarizedExperiment::colData(se))
    expect_equal(rowData(selectAssaySE(se_2, 1)), 
        SummarizedExperiment::rowData(se))
    expect_equal(assay(selectAssaySE(se_2, 2)), 
        SummarizedExperiment::assays(se_2)[[2]])
    expect_equal(colData(selectAssaySE(se_2, 2)), 
        SummarizedExperiment::colData(se))
    expect_equal(rowData(selectAssaySE(se_2, 2)), 
        SummarizedExperiment::rowData(se))
    expect_error(selectAssaySE(se_2, "abc"), "not in names")
    expect_error(selectAssaySE(se_2, NULL), 
        "unable to find an inherited method")
})

## selectAssayServer
test_that("selectAssayServer", {
    shiny::testServer(selectAssayServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        se <- new.env()
        selected <- new.env()

        out <- selectAssayServer("", se = se, selected = selected)
        expect_is(out, "reactiveExpr")
    })
})

## function selectSampleSE
test_that("selectSampleSE", {
    expect_equal(selectSampleSE(NULL, NULL, "all"), NULL)
    expect_equal(selectSampleSE(NULL, NULL, "exclude"), NULL)
    expect_equal(selectSampleSE(NULL, NULL, "select"), NULL)
    expect_equal(selectSampleSE(NULL, "foo"), NULL)
    expect_equal(selectSampleSE(se, NULL, "all"), se)
    expect_equal(selectSampleSE(se, NULL, "exclude"), se)
    expect_equal(selectSampleSE(se, NULL, "select"), se)
    expect_equal(selectSampleSE(se, "foo", "all"), se)
    expect_equal(selectSampleSE(se, "sample 2", "all"), se)
    expect_equal(selectSampleSE(se, "sample 2", "exclude"), se[, -2])
    expect_equal(selectSampleSE(se, "sample 2", "select"), se)
    expect_equal(selectSampleSE(se, c("sample 2", "sample 3"), "exclude"), 
        se[, -c(2:3)])
    expect_equal(selectSampleSE(se, c("sample 2", "sample 3"), "select"), se)
    expect_equal(
        selectSampleSE(se, c("sample 2", "sample 3", "sample 4"), "select"), 
        se[, 2:4])
    expect_error(selectSampleSE("foo", "sample 1", "exclude"),
        "incorrect number of dimensions")
    expect_error(selectSampleSE("foo", 
        c("sample 1", "sample 2", "sample 3"), "select"),
        "incorrect number of dimensions")
})

## function selectFeatureSE
test_that("selectFeatureSE", {
    expect_equal(selectFeatureSE(NULL, NULL, "all"), NULL)
    expect_equal(selectFeatureSE(NULL, NULL, "exclude"), NULL)
    expect_equal(selectFeatureSE(NULL, NULL, "select"), NULL)
    expect_equal(selectFeatureSE(NULL, "foo"), NULL)
    expect_equal(selectFeatureSE(se, NULL, "all"), se)
    expect_equal(selectFeatureSE(se, NULL, "exclude"), se)
    expect_equal(selectFeatureSE(se, NULL, "select"), se)
    expect_equal(selectFeatureSE(se, "foo", "all"), se)
    expect_equal(selectFeatureSE(se, "2", "all"), se)
    expect_equal(selectFeatureSE(se, "2", "exclude"), se[-2,])
    expect_equal(selectFeatureSE(se, "2", "select"), se)
    expect_equal(selectFeatureSE(se, c("1", "2"), "select"), se)
    expect_equal(selectFeatureSE(se, c("1", "2", "3"), "select"), se[1:3, ])
    expect_equal(selectFeatureSE(se, c("2", "3"), "exclude"), se[-c(2:3), ])
    expect_error(selectFeatureSE("foo", "1", "exclude"),
        "incorrect number of dimensions")
})

## function updateSE
test_that("updateSE", {
    expect_equal(updateSE(se, SummarizedExperiment::assay(se)), se)
    expect_error(updateSE(se, NULL))
    expect_error(updateSE(se, "foo"),
        "must return a numeric vector")
    expect_error(updateSE(NULL, SummarizedExperiment::assay(se)),
        "unable to find an inherited method for function")
    expect_error(updateSE("", SummarizedExperiment::assay(se)),
        "unable to find an inherited method for function")
})

