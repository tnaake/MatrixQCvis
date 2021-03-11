## create se
a <- matrix(1:1000, nrow = 100, ncol = 10, 
            dimnames = list(1:100, paste("sample", 1:10)))
a[c(1, 5, 8), 1:5] <- NA
set.seed(1)
a <- a + rnorm(1000)
sample <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
featData <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment(assay = a, rowData = featData, colData = sample)


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
    testServer(tSNEUIServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        se <- new.env()
        
        out <- tSNEUIServer("", se = se)
        expect_is(out, "shiny.render.function")
    })
})

## tP_umapUI
test_that("tP_umapUI", {
    expect_is(tP_umapUI(""), "shiny.tag")
})

## umapUIServer
test_that("umapUIServer", {
    testServer(umapUIServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        se <- new.env()
        
        out <- umapUIServer("", se = se)
        expect_is(out, "shiny.render.function")
    })
})



## dimRedServer
test_that("dimRedServer", {
    testServer(dimRedServer, {
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

    testServer(screePlotServer, {
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

## tP_dimensionReduction_all
test_that("tP_intensities_all", {
    expect_is(tP_dimensionReduction_all(), "shiny.tag")
})

