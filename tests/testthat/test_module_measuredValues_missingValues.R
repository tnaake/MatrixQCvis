## create se
a <- matrix(1:1000, nrow = 100, ncol = 10,
            dimnames = list(1:100, paste("sample", 1:10)))
a[c(1, 5, 8), 1:5] <- NA
set.seed(1)
a <- a + rnorm(1000)
cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment::SummarizedExperiment(assay = a, rowData = rD, 
    colData = cD)


## tP_barplotMeasuredMissingSampleUI
test_that("tP_barplotMeasuredMissingSampleUI", {
    expect_is(tP_barplotMeasuredMissingSampleUI("MeasuredValues_number", 
        title = "test"), "shiny.tag")
    expect_is(tP_barplotMeasuredMissingSampleUI("MissingValues_number", 
        title = "test"), "shiny.tag")
})

## sampleMeasuredMissingServer
test_that("sampleMeasuredMissingServer", {
    shiny::testServer(sampleMeasuredMissingServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        se <- new.env()

        out <- sampleMeasuredMissingServer("", se = se)
        expect_is(out, "reactive")
    })
})

## barplotMeasuredMissingSampleServer
test_that("barplotMeasuredMissingSampleServer", {
    shiny::testServer(barplotMeasuredMissingSampleServer, {

        input <- new.env()
        output <- new.env()
        session <- new.env()
        samplesMeasuredMissing <- new.env()

        out <- barplotMeasuredMissingSampleServer("", 
            samplesMeasuredMissing = samplesMeasuredMissing,
            measured = TRUE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_histFeatUI
test_that("tP_histFeatUI", {
    expect_is(tP_histFeatUI("MeasuredValues"), "shiny.tag")
    expect_is(tP_histFeatUI("MissingValues"), "shiny.tag")
})

## histFeatServer
test_that("histFeatServer", {
    shiny::testServer(histFeatServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        id <- new.env()
        se <- new.env()
        assay <- new.env()

        out <- histFeatServer("", se = se, assay = assay, measured = TRUE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_histFeatCategoryUI
test_that("tP_histFeatCategoryUI", {
    expect_is(tP_histFeatCategoryUI("MeasuredValues"), "shiny.tag")
    expect_is(tP_histFeatCategoryUI("MissingValues"), "shiny.tag")
})

## histFeatCategoryServer
test_that("histFeatCategoryServer", {
    shiny::testServer(histFeatCategoryServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        id <- new.env()
        se <- new.env()

        out <- histFeatCategoryServer("", se = se, measured = TRUE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_upSetUI
test_that("tP_upSetUI", {
    expect_is(tP_upSetUI("MeasuredValues"), "shiny.tag")
    expect_is(tP_upSetUI("MissingValues"), "shiny.tag")
})

## upSetServer
test_that("upSetServer", {
    shiny::testServer(upSetServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        id <- new.env()
        se <- new.env()

        out <- upSetServer("", se = se, measured = TRUE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_setsUI
test_that("tP_setsUI", {
    expect_is(tP_setsUI("MeasuredValues"), "shiny.tag")
    expect_is(tP_setsUI("MissingValues"), "shiny.tag")
})

## setsServer
test_that("setsServer", {
    shiny::testServer(setsServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        id <- new.env()
        se <- new.env()

        out <- setsServer("", se = se, measured = TRUE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_measuredValues_all
test_that("tP_measuredValues_all", {
    expect_is(tP_measuredValues_all(), "shiny.tag")
})

## tP_missingValues_all
test_that("tP_missingValues_all", {
    expect_is(tP_missingValues_all(), "shiny.tag")
})


