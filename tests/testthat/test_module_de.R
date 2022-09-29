#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom shiny testServer

## create se
a <- matrix(1:100, nrow = 10, ncol = 10, 
             dimnames = list(1:10, paste("sample", 1:10)))
a[c(1, 5, 8), 1:5] <- NA
set.seed(1)
a <- a + rnorm(100)
cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment::SummarizedExperiment(assay = a, rowData = rD, 
    colData = cD)

## tP_colDataUI
test_that("tP_colDataUI", {
    expect_is(tP_colDataUI(""), "shiny.tag")
})

## tP_modelMatrixUI
test_that("tP_modelMatrixUI", {
    expect_is(tP_modelMatrixUI(""), "shiny.tag")
})

## tP_contrastUI
test_that("tP_contrastUI", {
    expect_is(tP_colDataUI(""), "shiny.tag")
})

## tP_topDEUI
test_that("tP_topDEUI", {
    expect_is(tP_topDEUI(""), "shiny.tag")
})

## tP_volcanoUI
test_that("tP_volcanoUI", {
    expect_is(tP_volcanoUI(""), "shiny.tag")
})

## colDataServer
test_that("colDataServer", {
    shiny::testServer(colDataServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        
        out <- colDataServer("", se = se)
        expect_is(out, "shiny.render.function")
    })
})

## validFormulaMMServer
test_that("validFormulaMMServer", {
    shiny::testServer(validFormulaMMServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        expr <- new.env()
        action <- new.env()
        se <- new.env()
        
        out <- validFormulaMMServer("", expr = expr, action = action, se = se)
        expect_is(out, "reactiveExpr")
    })
})

## validExprModelMatrix
test_that("validExprModelMatrix", {
    expect_is(validExprModelMatrix("~type", se = se), "formula")
    expect_is(validExprModelMatrix("~foo", se = se), "NULL")
    expect_is(validExprModelMatrix("~", se = se), "NULL")
    expect_is(validExprModelMatrix("~type", se = NULL), 
        "NULL")
})

## modelMatrixServer
test_that("modelMatrixServer", {
    shiny::testServer(modelMatrixServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        validFormulaMM <- new.env()
        
        out <- modelMatrixServer("modelMatrixServer", se = se, 
            validFormulaMM = validFormulaMM)
        expect_is(out, "reactiveExpr")
    })
})

## modelMatrixUIServer
test_that("modelMatrixUIServer", {
    shiny::testServer(modelMatrixUIServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        modelMatrix <- new.env()
        validFormulaMM <- new.env()
        
        out <- modelMatrixUIServer("", modelMatrix = modelMatrix, 
            validFormulaMM = validFormulaMM)
        expect_is(out, "shiny.render.function")
    })
})

## validExprContrastServer
test_that("validExprContrastServer", {
    shiny::testServer(validExprContrastServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        expr <- new.env()
        action <- new.env()
        modelMatrix <- new.env()
        
        out <- validExprContrastServer("", expr = expr, action = action,
            modelMatrix = modelMatrix)
        expect_is(out, "reactiveExpr")
    })
})

## validExprContrast
test_that("validExprContrast", {
    modelMatrix <- stats::model.matrix(~type, colData(se))
    expect_equal(suppressWarnings(validExprContrast("type2", 
        modelMatrix = modelMatrix)), "type2")
    expect_is(suppressWarnings(validExprContrast("type2-", 
        modelMatrix = modelMatrix)), "NULL")
    expect_is(suppressWarnings(validExprContrast("foo", 
        modelMatrix = modelMatrix)), "NULL")
    expect_is(suppressWarnings(validExprContrast("type2", modelMatrix = NULL)), 
        "NULL")
    modelMatrix <- stats::model.matrix(~type + 0, colData(se))
    expect_equal(suppressWarnings(validExprContrast("type2-type1", 
        modelMatrix = modelMatrix)), "type2-type1")
})

## contrastMatrixServer
test_that("contrastMatrixServer", {
    shiny::testServer(contrastMatrixServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        validExprContrast <- new.env()
        modelMatrix <- new.env()
        
        out <- contrastMatrixServer("", validExprContrast = validExprContrast,
            modelMatrix = modelMatrix)
        expect_is(out, "reactiveExpr")
    })
})

## contrastMatrixUIServer
test_that("contrastMatrixUIServer", {
    shiny::testServer(contrastMatrixUIServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        validFormulaMM <- new.env()
        validExprContrast <- new.env()
        contrastMatrix <- new.env() 
        
        out <- contrastMatrixUIServer("", validFormulaMM = validFormulaMM,
            validExprContrast = validExprContrast, 
            contrastMatrix = contrastMatrix)
        expect_is(out, "shiny.render.function")
    })
})

## topDEUIServer
test_that("topDEUIServer", {
    shiny::testServer(topDEUIServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        type <- new.env()
        validFormulaMM <- new.env()
        validExprContrast <- new.env()
        testResult <- new.env()
        
        out <- topDEUIServer("", type = type, 
            validFormulaMM = validFormulaMM, 
            validExprContrast = validExprContrast, testResult = testResult)
        expect_is(out, "shiny.render.function")
    })
})

## fitServer
test_that("fitServer", {
    shiny::testServer(fitServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        assay <- new.env()
        validFormulaMM <- new.env()
        modelMatrix <- new.env()
        contrastMatrix <- new.env()
        
        out <- fitServer("", assay = assay, 
            modelMatrix = modelMatrix, contrastMatrix = contrastMatrix)
        expect_is(out, "reactiveExpr")
    })
})

## testResultServer
test_that("testResultServer", {
    shiny::testServer(testResultServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        type <- new.env()
        fit_ttest <- new.env()
        fit_proDA <- new.env()
        validFormulaMM <- new.env()
        validExprContrast <- new.env()
        
        out <- testResultServer("", type = type, 
            fit_ttest = fit_ttest, fit_proDA = fit_proDA, 
            validFormulaMM = validFormulaMM, 
            validExprContrast = validExprContrast)
        expect_is(out, "reactiveExpr")
    })
})

## volcanoUIServer
test_that("volcanoUIServer", {
    shiny::testServer(volcanoUIServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        type <- new.env()
        validFormulaMM <- new.env()
        validExprContrast <- new.env()
        testResult <- new.env()
        
        out <- volcanoUIServer("", type = type, 
            validFormulaMM = validFormulaMM, 
            validExprContrast = validExprContrast, testResult = testResult)
        expect_is(out, "shiny.render.function")
    })
})

## tP_DE_all
test_that("tP_DE_all", {
    expect_is(tP_DE_all(), "shiny.tag")
    expect_is(tP_DE_all(), "shiny.tag")
})

