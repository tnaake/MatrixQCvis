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


## tP_barplotMeMiSampleUI
test_that("tP_barplotMeMiSampleUI", {
    expect_is(tP_barplotMeMiSampleUI("MeV_number", title = "test"), "shiny.tag")
    expect_is(tP_barplotMeMiSampleUI("MiV_number", title = "test"), "shiny.tag")
})

## sampleMeMiServer
test_that("sampleMeMiServer", {
    shiny::testServer(sampleMeMiServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        se <- new.env()

        out <- sampleMeMiServer("", se = se)
        expect_is(out, "reactive")
    })
})

## barplotMeMiSampleServer
test_that("barplotMeMiSampleServer", {
    shiny::testServer(barplotMeMiSampleServer, {

        input <- new.env()
        output <- new.env()
        session <- new.env()
        samples_memi <- new.env()

        out <- barplotMeMiSampleServer("", samples_memi = samples_memi,
            measured = TRUE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_histFeatUI
test_that("tP_histFeatUI", {
    expect_is(tP_histFeatUI("MeV"), "shiny.tag")
    expect_is(tP_histFeatUI("MiV"), "shiny.tag")
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
    expect_is(tP_histFeatCategoryUI("MeV"), "shiny.tag")
    expect_is(tP_histFeatCategoryUI("MiV"), "shiny.tag")
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
    expect_is(tP_upSetUI("MeV"), "shiny.tag")
    expect_is(tP_upSetUI("MiV"), "shiny.tag")
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
    expect_is(tP_setsUI("MeV"), "shiny.tag")
    expect_is(tP_setsUI("MiV"), "shiny.tag")
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

## tP_meV_all
test_that("tP_meV_all", {
    expect_is(tP_meV_all(), "shiny.tag")
})

## tP_miV_all
test_that("tP_miV_all", {
    expect_is(tP_miV_all(), "shiny.tag")
})


