## create se
a <- matrix(seq_len(1000), nrow = 100, ncol = 10, 
            dimnames = list(seq_len(100), paste("sample", seq_len(10))))
a[c(1, 5, 8), seq_len(5)] <- NA
set.seed(1)
a <- a + rnorm(1000)
cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment::SummarizedExperiment(assay = a, rowData = rD, 
                                                                colData = cD)


## tP_PCAUI
test_that("tP_PCAUI", {
    expect_is(tP_PCAUI(""), "shiny.tag")
})

## tP_PCoAUI
test_that("tP_PCoAUI", {
    expect_is(tP_PCoAUI(""), "shiny.tag")
})

## tP_NMDSUI
test_that("tP_NMDSUI", {
    expect_is(tP_NMDSUI(""), "shiny.tag")
})

## tP_tSNEUI
test_that("tP_tSNEUI", {
    expect_is(tP_tSNEUI(""), "shiny.tag")
})

## tSNEUIServer
test_that("tSNEUIServer", {
    shiny::testServer(tSNEUIServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        sample_n <- new.env()
        
        out <- tSNEUIServer("", sample_n = sample_n)
        expect_is(out, "Observer")
    })
})

## tP_umapUI
test_that("tP_umapUI", {
    expect_is(tP_umapUI(""), "shiny.tag")
})

## umapUIServer
test_that("umapUIServer", {
    shiny::testServer(umapUIServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        sample_n <- new.env()
        
        out <- umapUIServer("", sample_n = sample_n)
        expect_is(out, "Observer")
    })
})



## dimRedServer
test_that("dimRedServer", {
    shiny::testServer(dimRedServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        assay <- new.env()
        type <- "PCA"
        label <- "PC"
        params <- new.env()
        innerWidth <- new.env()
        
        out <- dimRedServer("", se = se, assay = assay, type = type, 
                    label = label, params = params, innerWidth = innerWidth)
        expect_is(out, "shiny.render.function")
    })
})

## screePlotServer
test_that("screePlotServer", {

    shiny::testServer(screePlotServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        assay <- new.env()
        center <- new.env()
        scale <- new.env()
        
        out <- screePlotServer("", assay = assay, center = center, 
            scale = scale)

        expect_is(out, "shiny.render.function")
    })
})

## loadingsPlotServer
test_that("loadingsPlotServer", {
    
    shiny::testServer(loadingsPlotServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        assay <- new.env()
        params <- new.env()
        
        out <- loadingsPlotServer("", assay = assay, params = params)
        expect_is(out, "shiny.render.function")
    })
})

## tP_dimensionReduction_all
test_that("tP_intensities_all", {
    expect_is(tP_dimensionReduction_all(), "shiny.tag")
})

