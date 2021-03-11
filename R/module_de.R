################################################################################
################################### colData ####################################
################################################################################

#' @name tP_colDataUI
#' 
#' @title Tab panel UI for tab panel 'colData'
#' 
#' @description
#' The module defines the UI in the tab panel 'colData'.
#' 
#' @details 
#' `tP_colDataUI` returns the HTML code for the tab-pane 'colData'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' 
#' @return 
#' `shiny.tag` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_colDataUI("test")
#' 
#' @noRd
tP_colDataUI <- function(id) {
    
    ns <- NS(id)
    tabPanel(title = "Sample meta-data",
        fluidRow(width = 12,
            column(12, uiOutput(ns("helperUI"))), 
            column(width = 12, 
                dataTableOutput(outputId = ns("colDt")) 
            )  
        )
    )
}

#' @name colDataServer
#' 
#' @title Module for server expressions of tab panel 'colData'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'colData'.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` and `reactive` value
#' @param missingValue `logical`, will load the respective help page for a
#' `SummarizedExperiment` containign missing or non-missing data
#' 
#' @return 
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
colDataServer <- function(id, se, missingValue) {
    moduleServer(
        id, 
        function(input, output, session) {
            
            output$helperUI <- renderUI({
                
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                
                br()  %>% 
                    helper(content = helperFile)
            })
            
            output$colDt <- renderDataTable({
                colData(se())
            }, options = list(pageLength = 20))
        }
    )
}

################################################################################
################################ Model matrix ##################################
################################################################################

#' @name tP_modelMatrixUI
#' 
#' @title Tab panel UI for tab panel 'Model matrix'
#' 
#' @description
#' The module defines the UI in the tab panel 'Model matrix'.
#' 
#' @details 
#' `tP_modelMatrixUI` returns the HTML code for the tab-pane 'Model matrix'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' 
#' @return 
#' `shiny.tag` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_modelMatrixUI("test")
#' 
#' @noRd
tP_modelMatrixUI <- function(id) {
    
    ns <- NS(id)
    tabPanel(title = "Model matrix", 
        fluidRow(width = 12,
            column(12, uiOutput(ns("helperUI"))),
            column(width = 12, 
                uiOutput(outputId = ns("modelMatrixTab")) 
            )  
        )
    )
}

#' @name validFormulaMMServer
#' 
#' @title Module for server expressions of tab 'DE'
#' 
#' @description
#' The module defines the server expressions in the tab 
#' 'DE'. The function returns `NULL` if the formula is not valid with the
#' `SummarizedExperiment`, otherwise the formula is returned. 
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param expr `character` and `reactive` value
#' @param action `reactive` value
#' @param se `SummarizedExperiment` and `reactive` value
#' 
#' @return 
#' `reactive` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
validFormulaMMServer <- function(id, expr, action, se) {
    moduleServer(
        id,
        function(input, output, session) {
            
            validFormulaMM <- reactive({
                action()
                fMM <- isolate(expr())
                validExprModelMatrix(expr = fMM, se = se())
            })
            
            return(validFormulaMM)
        }
    )
}

#' @name validExprModelMatrix
#' 
#' @title Validate if expr is a formula for the model matrix
#' 
#' @description
#' The function `validExprModelMatr` returns either a `formula` object if 
#' `expr` is compatible with `se` or `NULL` if `expr` is not compatible with 
#' `se`. Specifically, the function accesses `colnames(colData(se))` and 
#' checks if all terms in `expr` are present in `colnames(colData(se))`
#' 
#' @details
#' Internal usage in `validFormulaMMServer` within `shinyQC`.
#' 
#' @param expr `character`
#' @param se `SummarizedExperiment`
#'  
#' @return 
#' A `formula` object or `NULL`
#' 
#' @examples 
#' library(dplyr)
#' library(SummarizedExperiment)
#' 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = rD, colData = cD)
#' 
#' validExprModelMatrix("~type", se = se) ## returns a formula
#' validExprModelMatrix("~foo", se = se) ## returns NULL
#' 
#' @noRd
validExprModelMatrix <- function(expr, se) {
    
    fMM <- tryCatch(as.formula(expr), error = function(e) NULL)
    
    if (!is.null(fMM)) {
        
        fMM_l <- as.list(fMM)
        fMM_l_c <- fMM_l[[length(fMM_l)]]
        fMM_l_c <- as.character(fMM_l_c)
        fMM_l_c <- fMM_l_c[!(fMM_l_c %in% c("+", "0", "1"))]
        
        if (all(fMM_l_c %in% colnames(colData(se))) & length(fMM_l_c) != 0) {
            fMM <- fMM
        } else {
            fMM <- NULL
        }
    } else {
        fMM <- NULL
    }
    return(fMM)
}

#' @name modelMatrixServer
#'  
#' @title Module for server expressions of tab panel 'Model matrix'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Model matrix'. The function returns a reactive expression of the 
#' `model.matrix` given a formula and a `SummarizedExperiment`.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` and `reactive` value
#' @param validFormulaMM `formula` and `reactive` value
#' 
#' @return 
#' `reactive` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
modelMatrixServer <- function(id, se, validFormulaMM) {
    moduleServer(
        id,
        function(input, output, session) {
            
            modelMatrix <- reactive({
                if (!is.null(validFormulaMM())) {
                    model.matrix(validFormulaMM(), data = colData(se()))
                }
            })
            
            return(modelMatrix)
        }
    )
}

#' @name modelMatrixUIServer
#' 
#' @title Module for server expressions of tab panel 'Model matrix'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Model matrix'.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param modelMatrix `matrix` and `reactive` value
#' @param validFormulaMM `formula` and `reactive` value
#' @param missingValue `logical`, will load the respective help page for a
#' `SummarizedExperiment` containign missing or non-missing data
#' 
#' @return 
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
modelMatrixUIServer <- function(id, modelMatrix, validFormulaMM, missingValue) {
    moduleServer(
        id, 
        function(input, output, session) {
            
            output$helperUI <- renderUI({
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                br() %>% helper(content = helperFile)
            })
            
            output$modelMatrixTab <- renderUI({
                ns <- session$ns
                ## if there is a valid formula return the DataTable
                if (!is.null(validFormulaMM())) {
                    output$dtMM <- renderDataTable({
                        mM <- modelMatrix() %>% as.matrix()
                        cbind(rownames(mM), mM)
                    }, options = list(pageLength = 20))
                    dataTableOutput(ns("dtMM"))
                } else {
                    ## show that the formula is not valid
                    output$textfMM <- renderText("formula for Model Matrix not valid with the given colData")
                    verbatimTextOutput(ns("textfMM"))
                }
                
            })
            
        }
    )
}  

################################################################################
############################### Contrast matrix ################################
################################################################################
#' @name tP_contrastUI
#' 
#' @title Tab panel UI for tab panel 'Contrast matrix'
#' 
#' @description
#' The module defines the UI in the tab panel 'Contrast matrix'.
#' 
#' @details 
#' `tP_contrastUI` returns the HTML code for the tab-pane 'Contrast matrix'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' 
#' @return 
#' `shiny.tag` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_contrastUI("test")
#' 
#' @noRd
tP_contrastUI <- function(id) {
    
    ns <- NS(id)
    tabPanel(title = "Contrast matrix", 
        fluidRow(width = 12,
            column(12, uiOutput(ns("helperUI"))),
            column(width = 12, 
                uiOutput(outputId = ns("contrastMatrixTab")) 
            )
        ) 
    )
}

#' @name validExprContrastServer
#' 
#' @title Module for server expressions of tab 'DE'
#' 
#' @description
#' The module defines the server expressions in the tab 'DE'. The function
#' returns `NULL` if the expression is not compatible with the `modelMatrix`,
#' otherwise the expression is returned.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param expr `character` and `reactive` value
#' @param action `reactive` value
#' @param modelMatrix `matrix` and `reactive` value
#' 
#' @return 
#' `reactive` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
validExprContrastServer <- function(id, expr, action, modelMatrix) {
    moduleServer(
        id,
        function(input, output, session) {
            
            validExprC <- reactive({
                action()
                contrasts <- isolate(expr())
                validExprContrast(contrasts = contrasts,
                    modelMatrix = modelMatrix())
            })
            
            return(validExprC)
        }
    )
}

#' @name validExprContrast
#' 
#' @title Validate if expr is an expression for contrasts
#' 
#' @description
#' The function `validExprContrast` returns either a `formula` object if 
#' `expr` is compatible with `modelMatrix` or `NULL` if `expr` is not 
#' compatible with `modelMatrix`. Specifically, the function accesses 
#' `colnames(modelMatrix)` and checks if all terms in `expr` are present in 
#' `colnames(modelMatrix)`
#' 
#' @details
#' Internal usage in `validExprConstrastServer` within `shinyQC`.
#' 
#' @param expr `character`
#' @param modelMatrix `model.matrix` as returned from `limma::model.matrix`
#'  
#' @return 
#' A `character` or `NULL`
#' 
#' @examples 
#' library(limma)
#' 
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' 
#' modelMatrix <- model.matrix(~type, cD)
#' 
#' validExprContrast("type2", modelMatrix = modelMatrix) ## returns a formula
#' validExprContrast("foo", modelMatrix = modelMatrix) ## returns NULL
#' 
#' @noRd
validExprContrast <- function(contrasts, modelMatrix) {
    
    contrasts_c <- strsplit(contrasts, split = "-")[[1]]
    
    if (all(contrasts_c %in% colnames(modelMatrix)) & 
        length(contrasts_c) %in% 1:2) {
        contrasts <- paste(contrasts_c, sep = "-")
    } else {
        contrasts <- NULL
    }
    
    return(contrasts)
}

#' @name contrastMatrixServer
#' 
#' @title Module for server expressions of tab panel 'Contrast matrix'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Contrast matrix'. The function returns the contrast matrix given a 
#' expression and a model matrix.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param validExprContrast `character` and `reactive` value
#' @param modelMatrix `matrix` and `reactive value`
#' 
#' @return 
#' `reactive` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom limma makeContrasts
#' 
#' @noRd
contrastMatrixServer <- function(id, validExprContrast, modelMatrix) {
    moduleServer(
        id,
        function(input, output, session) {
            contrastMatrix <- reactive({
                if (!is.null(validExprContrast())) {
                    makeContrasts(contrasts = validExprContrast(), levels = modelMatrix())
                }
            })
            
            return(contrastMatrix)
        }
    )
}

#' @name contrastMatrixUIServer
#' 
#' @title Module for server expressions of tab panel 'Contrast matrix'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Contrast matrix'.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param validFormulaMM `formula` and `reactive` value
#' @param validExprContrast `character` and `reactive` value
#' @param contrastMatrix `matrix` and `reactive` value
#' @param missingValue `logical`, will load the respective help page for a
#' `SummarizedExperiment` containign missing or non-missing data
#' 
#' @return 
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
contrastMatrixUIServer <- function(id, validFormulaMM, validExprContrast, 
        contrastMatrix, missingValue) {
    moduleServer(
        id,
        function(input, output, session) {
            
            output$helperUI <- renderUI({
                
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                br() %>% helper(content = helperFile)
                
            })
            
            output$contrastMatrixTab <- renderUI({
                ns <- session$ns
                ## if there is a valid formula return the DataTable
                if (!is.null(validFormulaMM()) & !is.null(validExprContrast())) {
                    output$dtCM <- renderDataTable({
                        cM <- contrastMatrix()
                        cbind(rownames(cM), cM)
                    }, options = list(pageLength = 20))
                    dataTableOutput(ns("dtCM"))
                } else {
                    if (is.null(validFormulaMM())) {
                        ## show that the formula for Model Matrix is not valid
                        output$textfCM <- renderText("formula for Model Matrix not valid with the given colData")
                        
                    } else {
                        ## show that the expression of contrasts is not valid
                        output$textfCM <- renderText("contrast formula not valid with the given Model Matrix")
                    }
                    verbatimTextOutput(ns("textfCM"))
                }
            })
            
        }
    )
}

################################################################################
################################### Top DE #####################################
################################################################################
#' @name tP_topDEUI
#' 
#' @title Tab panel UI for tab panel 'Top DE'
#' 
#' @description
#' The module defines the UI in the tab panel 'Top DE'.
#' 
#' @details 
#' `tP_topDEUI` returns the HTML code for the tab-pane 'Top DE'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' 
#' @return 
#' `shiny.tag` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_topDEUI("test")
#' 
#' @noRd
tP_topDEUI <- function(id) {
    
    ns <- NS(id)
    tabPanel(title = "Top DE", 
        fluidRow(width = 12,
            column(12, uiOutput(ns("helperUI"))),
            column(width = 12, 
                uiOutput(outputId = ns("topDE")) 
            )  
        )
    )
}

#' @name topDEUIServer
#' 
#' @title Module for server expressions of tab panel 'Top DE'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Top DE'.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param type `reactive` value
#' @param validFormulaMM `formula` and `reactive` value
#' @param validExprContrast `character` and `reactive` value
#' @param testResult `matrix` and `reactive` value
#' @param missingValue `logical`, will load the respective help page for a
#' `SummarizedExperiment` containign missing or non-missing data
#' 
#' @return 
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
topDEUIServer <- function(id, type, validFormulaMM, validExprContrast, 
        testResult, missingValue) {
    
    moduleServer(
        id,
        function(input, output, session) {
            
            output$helperUI <- renderUI({
                
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                br()  %>% 
                    helper(content = helperFile)

            })
            
            output$topDE <- renderUI({
                ns <- session$ns
                if (!is.null(validFormulaMM())) {
                    
                    if (type() == "ttest") {
                        output$dtTest <- renderDataTable({
                            testResult()
                        })
                        return(dataTableOutput(ns("dtTest")))
                    } 
                    
                    if (type() == "proDA") {
                        if (!is.null(validExprContrast())) {
                            output$dtTest <- renderDataTable({
                                testResult()
                            })
                            return(dataTableOutput(ns("dtTest")))
                        } else {
                            output$textTest <- renderText("expression for contrasts not valid with the given colData")
                            return(verbatimTextOutput(ns("textTest")))
                        } 
                    }
                    
                } else { ## no valid formula for Model Matrix
                    output$textTest <- renderText("formula for Model Matrix not valid with the given colData")
                    return(verbatimTextOutput(ns("textTest")))
                }
                
            })
        }
    )
}


#' @name fitServer
#' 
#' @title Module for server expressions of tab panel 'Top DE'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Top DE'.
#' 
#' @details
#' Internal function for `shinyQC`. For `eBayes`, the `trend` argument is set
#' by default to `TRUE`. The `trend=TRUE` argument will model 
#' any mean-variance trend in the data; this is required when there is a 
#' mean-variance trend (otherwise the estimates of the prior d.f. and 
#' subsequent eBayes shrinkage would be confounded by systematic 
#' mean-dependent differences from a common prior variance).
#' 
#' @param id `character`
#' @param assay `matrix` and `reactive` value, obtained from 
#' `assay(SummarizedExperiment)`
#' @param validFormulaMM `formula` and `reactive` value
#' @param modelMatrix `matrix` and `reactive` value
#' @param contrastMatrix `matrix` and `reactive` value
#' 
#' @return 
#' `reactive` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom limma lmFit contrasts.fit
#' 
#' @noRd
fitServer <- function(id, assay, validFormulaMM, modelMatrix, contrastMatrix) {
    moduleServer(
        id,
        function(input, output, session) {
            
            fit <- reactive({
                if (!is.null(validFormulaMM())) {
                    
                    if (id == "ttest") {
                        fit <- lmFit(assay(), design = modelMatrix())
                        if (!is.null(contrastMatrix())) 
                            fit <- contrasts.fit(fit, contrastMatrix())
                        fit <- eBayes(fit, trend = TRUE, robust = TRUE)
                    }
                    if (id == "proDA") {
                        fit <- proDA(assay(), design = modelMatrix()) 
                    }
                    
                    return(fit)
                } 
            })
        }
    )
}

#' @name testResultsServer
#' 
#' @title Module for server expressions of tab panel 'Top DE'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Top DE'.
#' 
#' @details
#' Internal function for `shinyQC`. 
#' 
#' @param id `character`
#' @param type `reactive` value
#' @param fit_ttest `matrix` and `reactive` value
#' @param fit_proDA `matrix` and `reactive` value
#' @param validFormulaMM `formula` and `reactive` value
#' @param validExprContrast `character` and `reactive` value
#' 
#' @return 
#' `reactive` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
testResultServer <- function(id, type, fit_ttest, fit_proDA, validFormulaMM, 
        validExprContrast) {
    moduleServer(
        id,
        function(input, output, session) {
            testResult <- reactive({
                
                if (!is.null(validFormulaMM())) {
                    if (type() == "ttest") {
                        t <- topTable(fit_ttest(), number = Inf, 
                                adjust.method = "fdr", p.value = 0.05)
                        t <- cbind(name = rownames(t), t)
                    }
                    if (type() == "proDA") {
                        t <- test_diff(fit = fit_proDA(), contrast = validExprContrast(),
                                       sort_by = "adj_pval")
                    }
                    return(t)
                }
            })
            return(testResult)
        }
    )
}

################################################################################
################################ Volcano plot ##################################
################################################################################

#' @name tP_volcanoUI
#' 
#' @title Tab panel UI for tab panel 'Volcano plot'
#' 
#' @description
#' The module defines the UI in the tab panel 'Volcano plot'.
#' 
#' @details 
#' `tP_volcanoUI` returns the HTML code for the tab-pane 'Volcano plot'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' 
#' @return 
#' `shiny.tag` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_volcanoUI("test")
#'
#' @noRd
tP_volcanoUI <- function(id) {
    
    ns <- NS(id)
    tabPanel(title = "Volcano plot", 
        fluidRow(width = 12,
            column(12, uiOutput(ns("helperUI"))),
            column(width = 12,
                uiOutput(outputId = ns("volcano"))
            )
        )
    )
}

#' @name volcanoUIServer
#' 
#' @title Module for server expressions of tab panel 'Volcano plot'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Volcano plot'.
#' 
#' @details
#' Internal function for `shinyQC`. 
#' 
#' @param id `character`
#' @param type `reactive` value
#' @param validFormulaMM `formula` and `reactive` value
#' @param validExprContrast `character` and `reactive` value
#' @param testResult `matrix` and `reactive` value
#' @param missingValue `logical`, will load the respective help page for a
#' `SummarizedExperiment` containign missing or non-missing data
#' 
#' @return 
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny tagList
#' @importFrom htmlwidgets saveWidget
#' 
#' @noRd
volcanoUIServer <- function(id, type, validFormulaMM, validExprContrast, 
        testResult, missingValue) {

    moduleServer(
        id, 
        function(input, output, session) {
            
            output$helperUI <- renderUI({
                
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                br()  %>% helper(content = helperFile)
            })
            
            output$volcano <- renderUI({
                ns <- session$ns
                if (!is.null(validFormulaMM())) {
                    
                    if (!is.null(validExprContrast())) {
                        
                        p_volcano <- reactive({
                            volcanoPlot(testResult(), type())
                        })
                        
                        output$plotVolcano <- renderPlotly({
                            p_volcano()
                        })
                        
                        output$downloadPlot <- downloadHandler(
                            filename = function() {
                                paste("Volcano_", type(), ".html")
                            },
                            content = function(file) {
                                saveWidget(p_volcano(), file)
                            }
                        )
                        
                        ## return plot and downloadButton
                        tagList(
                            plotlyOutput(ns("plotVolcano")),
                            downloadButton(outputId = ns("downloadPlot"), "")    
                        )
                        
                    } else {
                        output$textVolcano <- renderText({
                            "expression for contrasts not valid with the given colData"})
                        verbatimTextOutput(ns("textVolcano"))
                    }
                } else {
                    output$textVolcano <- renderText({
                        "formula for Model Matrix not valid with the given colData"})
                    verbatimTextOutput(ns("textVolcano"))
                }
            })
        }
    )
}

#' @name tP_DE_all
#' 
#' @title Tab panel UI for tab panel 'DE'
#' 
#' @description 
#' The module defines the UI for the tab panel 'DE'.
#' 
#' @details 
#' `tP_DE_all` returns the HTML code for the tab-pane 
#' 'DE'. Internal function for `shinyQC`.
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' tP_DE_all()
#' 
#' @noRd
tP_DE_all <- function() {
    tabPanel("DE",
        tabBox(title = "", width = 12,
            tP_colDataUI(id = "colData"),
            tP_modelMatrixUI(id = "modelMatrix"),
            tP_contrastUI(id = "contrast"),
            tP_topDEUI(id = "topDE"),
            tP_volcanoUI(id = "volcano")
        )
    )    
}

