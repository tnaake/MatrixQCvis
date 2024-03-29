#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom shiny testServer

## create se
a <- matrix(seq_len(1000), nrow = 100, ncol = 10, 
            dimnames = list(seq_len(100), paste("sample", seq_len(10))))
a[c(1, 5, 8), seq_len(5)] <- NA
set.seed(1)
a <- a + rnorm(1000)
sample <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
featData <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment::SummarizedExperiment(assay = a, rowData = featData, 
    colData = sample)

## tP_histSampleUI
test_that("tP_histSampleUI", {
    expect_is(tP_histSampleUI("test"), "shiny.tag")
})


## histSampleServer
test_that("histSampleServer", {
    shiny::testServer(histSampleServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        
        out <- histSampleServer("", se = se)
        expect_is(out, "shiny.render.function")
    })
})

## tP_mosaicSampleUI
test_that("tP_mosaicSampleUI", {
    expect_is(tP_mosaicSampleUI("test"), "shiny.tag")
})

## mosaicSampleServer
test_that("mosaicSampleServer", {
    shiny::testServer(mosaicSampleServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        
        out <- mosaicSampleServer("", se = se)
        expect_is(out, "shiny.render.function")
    })
})

## tP_samples_all
test_that("tP_samples_all", {
    expect_is(tP_samples_all(), "shiny.tag")
})

