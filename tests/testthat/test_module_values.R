#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom shiny testServer

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

## function fR_boxplotUI
test_that("fR_boxplotUI", {
    expect_is(fR_boxplotUI("test", name = "test", collapsed = TRUE), "shiny.tag")
    expect_is(fR_boxplotUI("test", name = "test", collapsed = FALSE), "shiny.tag")
})

## tP_boxplotUI
test_that("tP_boxplotUI", {
    expect_is(tP_boxplotUI("test"), "shiny.tag")
})

## boxPlotUIServer
test_that("boxPlotUIServer", {
    shiny::testServer(boxPlotUIServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        se <- new.env()
        missingValue <- new.env()
        
        out <- boxPlotUIServer("", se = se)
        expect_is(out, "Observer")
        out <- boxPlotUIServer("", se = se)
        expect_is(out, "Observer")
    })
})

## boxPlotServer
test_that("boxPlotServer", {
    shiny::testServer(boxPlotServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        orderCategory <- new.env()
        boxLog <- new.env()
        violin <- new.env()
        
        out <- boxPlotServer("", se = se, orderCategory = orderCategory, 
            boxLog = boxLog, violin = violin, type = "test")
        expect_is(out, "shiny.render.function")
    })
})

## driftServer
test_that("driftServer", {
    shiny::testServer(driftServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        se <- new.env()
        se_n <- new.env()
        se_b <- new.env()
        se_t <- new.env()
        se_i <- new.env()
        dataType <- new.env()
        aggregation <- new.env()
        method <- new.env()
        
        out <- driftServer("", se = se, se_n = se_n, se_b = se_b, se_t = se_t, 
            se_i = se_i, missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        out <- driftServer("", se = se, se_n = se_n, se_b = se_b, se_t = se_t, 
            se_i = se_i, missingValue = FALSE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_driftUI
test_that("tP_driftUI", {
    expect_is(tP_driftUI("test"), "shiny.tag")
})


## tP_cvUI
test_that("tP_cvUI", {
    expect_is(tP_cvUI("test"), "shiny.tag")
})

## cvServer
test_that("cvServer", {
    shiny::testServer(cvServer, {
        input <- new.env()
        output <- new.env()
        session <- new.env()
        a_r <- new.env()
        a_n <- new.env()
        a_b <- new.env()
        a_t <- new.env()
        a_i <- new.env()
        
        out <- cvServer("", a_r = a_r, a_n = a_n, a_b = a_b, a_t = a_t,
            a_i = a_i, missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        out <- cvServer("", a_r = a_r, a_n = a_n, a_b = a_b, a_t = a_t,
            a_i = a_i, missingValue = FALSE)
        expect_is(out, "shiny.render.function")
    })
})

## box_meanSdUI
test_that("box_meanSdUI", {
    expect_is(box_meanSdUI("test", name = "test"), "shiny.tag")
})

## tP_meanSdUI
test_that("tP_meanSdUI", {
    expect_is(tP_meanSdUI(""), "shiny.tag")
})

## meanSdServer
test_that("meanSdServer", {
    shiny::testServer(meanSdServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        assay <- new.env()
        
        out <- meanSdServer("", assay = assay, type = "test")
        expect_is(out, "shiny.render.function")
    })
    
})

# tP_maUI
test_that("tP_maUI", {
    expect_is(tP_maUI("test"), "shiny.tag")
})

## maServer
test_that("maServer", {
    shiny::testServer(maServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        se_n <- new.env()
        se_b <- new.env()
        se_t <- new.env()
        se_i <- new.env()
        innerWidth <- new.env()
        
        out <- maServer("", se = se, se_n = se_n, se_b = se_b, se_t = se_t,
            se_i = se_i, innerWidth = innerWidth, missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        out <- maServer("", se = se, se_n = se_n, se_b = se_b, se_t = se_t,
            se_i = se_i, innerWidth = innerWidth, missingValue = FALSE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_ECDFUI
test_that("tP_ECDFUI", {
    expect_is(tP_ECDFUI("test"), "shiny.tag")
})

## ECDFServer
test_that("ECDFServer", {
    shiny::testServer(ECDFServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        se_n <- new.env()
        se_b <- new.env()
        se_t <- new.env()
        se_i <- new.env()
        
        out <- ECDFServer("", se = se, se_n = se_n, se_b = se_b, se_t = se_t,
            se_i = se_i, missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        out <- ECDFServer("", se = se, se_n = se_n, se_b = se_b, se_t = se_t,
            se_i = se_i, missingValue = FALSE)
        expect_is(out, "shiny.render.function")
    })
})

## fR_distUI
test_that("fR_distUI", {
    expect_is(fR_distUI("test", title = "test", collapsed = TRUE), "shiny.tag")
    expect_is(fR_distUI("test", title = "test", collapsed = FALSE), "shiny.tag")
})

## distServer
test_that("distServer", {
    shiny::testServer(distServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        assay <- new.env()
        method <- new.env()
        label <- new.env()
        
        out <- distServer("", se = se, assay = assay, method = method, 
            label = label, type = "test")
        expect_is(out, "shiny.render.function")
    })
})

## tP_distUI
test_that("tP_distUI", {
    expect_is(tP_distUI(), "shiny.tag")
})

## featureServer
test_that("featureServer", {
    shiny::testServer(featureServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- reactive(se)
        a <- reactive(assay(se))
        a_n <- new.env()
        a_b <- new.env()
        a_t <- new.env()
        a_i <- new.env()
        
        out <- featureServer(id = "", se = se, a = a, a_n = a_n, a_b = a_b,
            a_t = a_t, a_i = a_i, missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        out <- featureServer(id = "", se = se, a = a, a_n = a_n, a_b = a_b, 
            a_t = a_t, a_i = a_i, missingValue = FALSE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_featureUI
test_that("tP_featureUI", {
    expect_is(tP_featureUI("test"), "shiny.tag")
})

## tP_values_all
test_that("tP_values_all", {
    expect_is(tP_values_all(), "shiny.tag")
})

