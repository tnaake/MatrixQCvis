library(SummarizedExperiment)

## create se
a <- matrix(1:100, nrow = 10, ncol = 10, 
             dimnames = list(1:10, paste("sample", 1:10)))
a[c(1, 5, 8), 1:5] <- NA
set.seed(1)
a <- a + rnorm(100)
cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
rD <- data.frame(spectra = rownames(a))
se <- SummarizedExperiment(assay = a, rowData = rD, colData = cD)

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
    testServer(colDataServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        se <- new.env()
        
        out <- colDataServer("", se = se, missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        out <- colDataServer("", se = se, missingValue = FALSE)
        expect_is(out, "shiny.render.function")
    })
})

## validFormulaMMServer
test_that("validFormulaMMServer", {
    testServer(validFormulaMMServer, {
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
    expect_error(validExprModelMatrix("~type", se = NULL), 
        "unable to find an inherited method for function ")
})

## modelMatrixServer
test_that("modelMatrixServer", {
    testServer(modelMatrixServer, {
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
    testServer(modelMatrixUIServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        modelMatrix <- new.env()
        validFormulaMM <- new.env()
        
        out <- modelMatrixUIServer("", modelMatrix = modelMatrix, 
            validFormulaMM = validFormulaMM, missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        out <- modelMatrixUIServer("", modelMatrix = modelMatrix, 
            validFormulaMM = validFormulaMM, missingValue = FALSE)
        expect_is(out, "shiny.render.function")
    })
})

## validExprContrastServer
test_that("validExprContrastServer", {
    testServer(validExprContrastServer, {
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
    modelMatrix <- model.matrix(~type, colData(se))
    expect_equal(validExprContrast("type2", modelMatrix = modelMatrix), "type2")
    expect_equal(validExprContrast("type2-", modelMatrix = modelMatrix), "type2")
    expect_is(validExprContrast("foo", modelMatrix = modelMatrix), "NULL")
    expect_is(validExprContrast("type2", modelMatrix = NULL), "NULL")
})

## contrastMatrixServer
test_that("contrastMatrixServer", {
    testServer(contrastMatrixServer, {
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
    testServer(contrastMatrixUIServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        validFormulaMM <- new.env()
        validExprContrast <- new.env()
        contrastMatrix <- new.env() 
        
        out <- contrastMatrixUIServer("", validFormulaMM = validFormulaMM,
            validExprContrast = validExprContrast, 
            contrastMatrix = contrastMatrix, missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        out <- contrastMatrixUIServer("", validFormulaMM = validFormulaMM,
            validExprContrast = validExprContrast, 
            contrastMatrix = contrastMatrix, missingValue = FALSE)
        expect_is(out, "shiny.render.function")
    })
})

## topDEUIServer
test_that("topDEUIServer", {
    testServer(topDEUIServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        type <- new.env()
        validFormulaMM <- new.env()
        validExprContrast <- new.env()
        testResult <- new.env()
        
        out <- topDEUIServer("", type = type, 
            validFormulaMM = validFormulaMM, 
            validExprContrast = validExprContrast, testResult = testResult,
            missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        ut <- topDEUIServer("", type = type, 
            validFormulaMM = validFormulaMM, 
            validExprContrast = validExprContrast, testResult = testResult,
            missingValue = FALSE)
        expect_is(out, "shiny.render.function")
    })
})

## fitServer
test_that("fitServer", {
    testServer(fitServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        assay <- new.env()
        validFormulaMM <- new.env()
        modelMatrix <- new.env()
        contrastMatrix <- new.env()
        
        out <- fitServer("", assay = assay, validFormulaMM = validFormulaMM,
            modelMatrix = modelMatrix, contrastMatrix = contrastMatrix)
        expect_is(out, "reactiveExpr")
    })
})

## testResultServer
test_that("testResultServer", {
    testServer(testResultServer, {
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
    testServer(volcanoUIServer, {
        input <- new.env()    
        output <- new.env()
        session <- new.env()
        type <- new.env()
        validFormulaMM <- new.env()
        validExprContrast <- new.env()
        testResult <- new.env()
        
        out <- volcanoUIServer("", type = type, 
            validFormulaMM = validFormulaMM, 
            validExprContrast = validExprContrast, testResult = testResult,
            missingValue = TRUE)
        expect_is(out, "shiny.render.function")
        out <- volcanoUIServer("", type = type, 
            validFormulaMM = validFormulaMM, 
            validExprContrast = validExprContrast, testResult = testResult,
            missingValue = TRUE)
        expect_is(out, "shiny.render.function")
    })
})

## tP_DE_all
test_that("tP_DE_all", {
    expect_is(tP_DE_all(), "shiny.tag")
    expect_is(tP_DE_all(), "shiny.tag")
})
