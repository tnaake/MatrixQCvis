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
#' \code{tP_colDataUI} returns the HTML code for the tab-pane 'colData'. 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' 
#' @return 
#' \code{shiny.tag} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_colDataUI("test")
#' 
#' @importFrom shiny NS tabPanel fluidRow column uiOutput dataTableOutput
#' 
#' @noRd
tP_colDataUI <- function(id) {
    
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Sample meta-data",
        shiny::fluidRow(width = 12,
            shiny::column(12, shiny::uiOutput(ns("helperUI"))), 
            shiny::column(width = 12, 
                shiny::dataTableOutput(outputId = ns("colDt")) 
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
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param se \code{SummarizedExperiment} and \code{reactive} value
#' @param missingValue \code{logical}, will load the respective help page for a
#' \code{SummarizedExperiment} containign missing or non-missing data
#' 
#' @return 
#' \code{shiny.render.function} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI renderDataTable br
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
colDataServer <- function(id, se, missingValue) {
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            output$helperUI <- shiny::renderUI({
                
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                
                shiny::br() |> 
                    shinyhelper::helper(content = helperFile)
            })
            
            output$colDt <- shiny::renderDataTable({
                SummarizedExperiment::colData(se())
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
#' \code{tP_modelMatrixUI} returns the HTML code for the tab-pane 
#' 'Model matrix'. Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' 
#' @return 
#' \code{shiny.tag} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_modelMatrixUI("test")
#' 
#' @importFrom shiny NS tabPanel fluidRow column uiOutput
#' 
#' @noRd
tP_modelMatrixUI <- function(id) {
    
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Model matrix", 
        shiny::fluidRow(width = 12,
            shiny::column(width = 12, shiny::uiOutput(ns("helperUI"))),
            shiny::column(width = 12, 
                shiny::uiOutput(outputId = ns("modelMatrixTab")) 
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
#' 'DE'. The function returns \code{NULL} if the formula is not valid with the
#' \code{SummarizedExperiment}, otherwise the formula is returned. 
#' 
#' @details
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param expr \code{character} and \code{reactive} value
#' @param action \code{reactive} value
#' @param se \code{SummarizedExperiment} and \code{reactive} value
#' 
#' @return 
#' \code{reactive} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive isolate
#' 
#' @noRd
validFormulaMMServer <- function(id, expr, action, se) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            validFormulaMM <- shiny::reactive({
                action()
                fMM <- shiny::isolate(expr())
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
#' The function \code{validExprModelMatr} returns either a \code{formula} 
#' object if \code{expr} is compatible with \code{se} or \code{NULL} 
#' if \code{expr} is not compatible with \code{se}. Specifically, the 
#' function accesses \code{colnames(colData(se))} and checks if all terms 
#' in \code{expr} are present in \code{colnames(colData(se))}.
#' 
#' @details
#' Internal usage in \code{validFormulaMMServer} within \code{shinyQC}.
#' 
#' @param expr \code{character}
#' @param se \code{SummarizedExperiment}
#'  
#' @return 
#' A \code{formula} object or \code{NULL}
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
#' @importFrom SummarizedExperiment colData
#' @importFrom stats as.formula
#' @noRd
validExprModelMatrix <- function(expr, se) {
    
    fMM <- tryCatch(stats::as.formula(expr), error = function(e) NULL)
    
    if (!is.null(fMM)) {
        
        fMM_l <- as.list(fMM)
        fMM_l_c <- fMM_l[[length(fMM_l)]]
        fMM_l_c <- as.character(fMM_l_c)
        fMM_l_c <- fMM_l_c[!(fMM_l_c %in% c("+", "0", "1"))]
        
        if (all(fMM_l_c %in% colnames(SummarizedExperiment::colData(se))) & 
                                                        length(fMM_l_c) != 0) {
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
#' \code{model.matrix} given a formula and a \code{SummarizedExperiment}.
#' 
#' @details
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param se \code{SummarizedExperiment} and \code{reactive} value
#' @param validFormulaMM \code{formula} and \code{reactive} value
#' 
#' @return 
#' \code{reactive} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive
#' @importFrom stats model.matrix
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
modelMatrixServer <- function(id, se, validFormulaMM) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            modelMatrix <- shiny::reactive({
                if (!is.null(validFormulaMM())) {
                    stats::model.matrix(validFormulaMM(), 
                        data = SummarizedExperiment::colData(se()))
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
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param modelMatrix \code{matrix} and \code{reactive} value
#' @param validFormulaMM \code{formula} and \code{reactive} value
#' @param missingValue \code{logical}, will load the respective help page for a
#' \code{SummarizedExperiment} containign missing or non-missing data
#' 
#' @return 
#' \code{shiny.render.function} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI renderDataTable renderText br
#' @importFrom shiny dataTableOutput verbatimTextOutput
#' @importFrom shinyhelper helper 
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
modelMatrixUIServer <- function(id, modelMatrix, validFormulaMM, missingValue) {
    moduleServer(
        id, 
        function(input, output, session) {
            
            output$helperUI <- shiny::renderUI({
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                shiny::br() |> shinyhelper::helper(content = helperFile)
            })
            
            output$modelMatrixTab <- shiny::renderUI({
                ns <- session$ns
                ## if there is a valid formula return the DataTable
                if (!is.null(validFormulaMM())) {
                    output$dtMM <- shiny::renderDataTable({
                        mM <- modelMatrix() |> as.matrix()
                        cbind(rownames(mM), mM)
                    }, options = list(pageLength = 20))
                    shiny::dataTableOutput(ns("dtMM"))
                } else {
                    ## show that the formula is not valid
                    output$textfMM <- shiny::renderText(
                        c("formula for Model Matrix not valid with",
                            "the given colData"))
                    shiny::verbatimTextOutput(ns("textfMM"))
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
#' \code{tP_contrastUI} returns the HTML code for the tab-pane 
#' 'Contrast matrix'. Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' 
#' @return 
#' \code{shiny.tag} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_contrastUI("test")
#' 
#' @importFrom shiny NS tabPanel fluidRow column uiOutput
#' 
#' @noRd
tP_contrastUI <- function(id) {
    
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Contrast matrix", 
        shiny::fluidRow(width = 12,
            shiny::column(12, shiny::uiOutput(ns("helperUI"))),
            shiny::column(width = 12, 
                shiny::uiOutput(outputId = ns("contrastMatrixTab")) 
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
#' returns \code{NULL} if the expression is not compatible with the 
#' \code{modelMatrix}, otherwise the expression is returned.
#' 
#' @details
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param expr \code{character} and \code{reactive} value
#' @param action \code{reactive} value
#' @param modelMatrix \code{matrix} and \code{reactive} value
#' 
#' @return 
#' \code{reactive} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive isolate
#' 
#' @noRd
validExprContrastServer <- function(id, expr, action, modelMatrix) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            validExprC <- shiny::reactive({
                action()
                contrasts <- shiny::isolate(expr())
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
#' The function \code{validExprContrast} returns either a \code{formula} object 
#' if \code{expr} is compatible with \code{modelMatrix} or \code{NULL} 
#' if \code{expr} is not compatible with \code{modelMatrix}. Specifically, 
#' the function accesses \code{colnames(modelMatrix)} and checks if all 
#' terms in \code{expr} are present in \code{colnames(modelMatrix)}.
#' 
#' @details
#' Internal usage in \code{validExprConstrastServer} within \code{shinyQC}.
#' 
#' @param expr \code{character}
#' @param modelMatrix \code{model.matrix} as returned from 
#' \code{limma::model.matrix}
#'  
#' @return 
#' A \code{character} or \code{NULL}
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
        length(contrasts_c) %in% c(1, 2)) {
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
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param validExprContrast \code{character} and \code{reactive} value
#' @param modelMatrix \code{matrix} and \code{reactive} value
#' 
#' @return 
#' \code{reactive} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom limma makeContrasts
#' @importFrom shiny moduleServer reactive
#' 
#' @noRd
contrastMatrixServer <- function(id, validExprContrast, modelMatrix) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            contrastMatrix <- shiny::reactive({
                if (!is.null(validExprContrast())) {
                    limma::makeContrasts(contrasts = validExprContrast(), 
                        levels = modelMatrix())
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
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param validFormulaMM \code{formula} and \code{reactive} value
#' @param validExprContrast \code{character} and \code{reactive} value
#' @param contrastMatrix \code{matrix} and \code{reactive} value
#' @param missingValue \code{logical}, will load the respective help page for a
#' \code{SummarizedExperiment} containing missing or non-missing data
#' 
#' @return 
#' \code{shiny.render.function} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI renderDataTable dataTableOutput br
#' @importFrom shiny  renderText verbatimTextOutput
#' @importFrom shinyhelper helper
#' 
#' @noRd
contrastMatrixUIServer <- function(id, validFormulaMM, validExprContrast, 
        contrastMatrix, missingValue) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            output$helperUI <- shiny::renderUI({
                
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                shiny::br() |> shinyhelper::helper(content = helperFile)
                
            })
            
            output$contrastMatrixTab <- shiny::renderUI({
                ns <- session$ns
                ## if there is a valid formula return the DataTable
                if (!is.null(validFormulaMM()) & 
                                !is.null(validExprContrast())) {
                    output$dtCM <- shiny::renderDataTable({
                        cM <- contrastMatrix()
                        cbind(rownames(cM), cM)
                    }, options = list(pageLength = 20))
                    shiny::dataTableOutput(ns("dtCM"))
                } else {
                    if (is.null(validFormulaMM())) {
                        ## show that the formula for Model Matrix is not valid
                        output$textfCM <- shiny::renderText(
                            c("formula for Model Matrix not valid", 
                            "with the given colData"))
                        
                    } else {
                        ## show that the expression of contrasts is not valid
                        output$textfCM <- shiny::renderText(
                            c("contrast formula not valid with", 
                                "the given Model Matrix"))
                    }
                    shiny::verbatimTextOutput(ns("textfCM"))
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
#' \code{tP_topDEUI} returns the HTML code for the tab-pane 'Top DE'. 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' 
#' @return 
#' \code{shiny.tag} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_topDEUI("test")
#' 
#' @importFrom shiny NS tabPanel fluidRow column uiOutput
#' 
#' @noRd
tP_topDEUI <- function(id) {
    
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Top DE", 
        shiny::fluidRow(width = 12,
            shiny::column(12, shiny::uiOutput(ns("helperUI"))),
            shiny::column(width = 12, 
                shiny::uiOutput(outputId = ns("topDE")) 
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
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param type \code{reactive} value
#' @param validFormulaMM \code{formula} and \code{reactive} value
#' @param validExprContrast \code{character} and \code{reactive} value
#' @param testResult \code{matrix} and \code{reactive} value
#' @param missingValue \code{logical}, will load the respective help page for a
#' \code{SummarizedExperiment} containing missing or non-missing data
#' 
#' @return 
#' \code{shiny.render.function} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI renderDataTable dataTableOutput br
#' @importFrom shiny renderText verbatimTextOutput
#' @importFrom shinyhelper helper
#' @importFrom ggplot2 ggsave
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
topDEUIServer <- function(id, type, validFormulaMM, validExprContrast, 
        testResult, missingValue) {
    
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            output$helperUI <- shiny::renderUI({
                
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                shiny::br()  |> 
                    shinyhelper::helper(content = helperFile)

            })
            
            output$topDE <- shiny::renderUI({
                ns <- session$ns
                if (!is.null(validFormulaMM())) {
                    
                    if (type() == "ttest") {
                        output$dtTest <- shiny::renderDataTable({
                            testResult()
                        })
                        return(shiny::dataTableOutput(ns("dtTest")))
                    } 
                    
                    if (type() == "proDA") {
                        if (!is.null(validExprContrast())) {
                            output$dtTest <- shiny::renderDataTable({
                                testResult()
                            })
                            return(shiny::dataTableOutput(ns("dtTest")))
                        } else {
                            output$textTest <- shiny::renderText(
                                c("expression for contrasts not valid with",
                                    "the given colData"))
                            return(shiny::verbatimTextOutput(ns("textTest")))
                        } 
                    }
                    
                } else { ## no valid formula for Model Matrix
                    output$textTest <- shiny::renderText(
                        c("formula for Model Matrix not valid with the",
                            "given colData"))
                    return(shiny::verbatimTextOutput(ns("textTest")))
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
#' Internal function for \code{shinyQC}. For \code{eBayes}, the \code{trend}
#' argument is set by default to \code{TRUE}. The \code{trend=TRUE} argument 
#' will model any mean-variance trend in the data; this is required when 
#' there is a mean-variance trend (otherwise the estimates of the prior d.f. and 
#' subsequent eBayes shrinkage would be confounded by systematic 
#' mean-dependent differences from a common prior variance).
#' 
#' @param id \code{character}
#' @param assay \code{matrix} and \code{reactive} value, obtained from 
#' \code{assay(SummarizedExperiment)}
#' @param validFormulaMM \code{formula} and \code{reactive} value
#' @param modelMatrix \code{matrix} and \code{reactive} value
#' @param contrastMatrix \code{matrix} and \code{reactive} value
#' 
#' @return 
#' \code{reactive} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive
#' @importFrom limma lmFit contrasts.fit eBayes
#' @importFrom proDA proDA
#' 
#' @noRd
fitServer <- function(id, assay, validFormulaMM, modelMatrix, contrastMatrix) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            fit <- shiny::reactive({
                if (!is.null(validFormulaMM())) {
                    
                    if (id == "ttest") {
                        fit <- limma::lmFit(assay(), design = modelMatrix())
                        if (!is.null(contrastMatrix())) 
                            fit <- limma::contrasts.fit(fit, contrastMatrix())
                        fit <- limma::eBayes(fit, trend = TRUE, robust = TRUE)
                    }
                    if (id == "proDA") {
                        fit <- proDA::proDA(assay(), design = modelMatrix()) 
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
#' Internal function for \code{shinyQC}. 
#' 
#' @param id \code{character}
#' @param type \code{reactive} value
#' @param fit_ttest \code{matrix} and \code{reactive} value
#' @param fit_proDA \code{matrix} and \code{reactive} value
#' @param validFormulaMM \code{formula} and \code{reactive} value
#' @param validExprContrast \code{character} and \code{reactive} value
#' 
#' @return 
#' \code{reactive} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive
#' @importFrom limma topTable
#' @importFrom proDA test_diff
#' 
#' @noRd
testResultServer <- function(id, type, fit_ttest, fit_proDA, validFormulaMM, 
        validExprContrast) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            testResult <- shiny::reactive({
                
                if (!is.null(validFormulaMM())) {
                    if (type() == "ttest") {
                        t <- limma::topTable(fit_ttest(), number = Inf, 
                                adjust.method = "fdr", p.value = 0.05)
                        t <- cbind(name = rownames(t), t)
                    }
                    if (type() == "proDA") {
                        t <- proDA::test_diff(fit = fit_proDA(), 
                            contrast = validExprContrast(), 
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
#' \code{tP_volcanoUI} returns the HTML code for the tab-pane 'Volcano plot'. 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' 
#' @return 
#' \code{shiny.tag} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_volcanoUI("test")
#'
#' @importFrom shiny NS tabPanel fluidRow column uiOutput
#' @noRd
tP_volcanoUI <- function(id) {
    
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Volcano plot", 
        shiny::fluidRow(width = 12,
            shiny::column(12, shiny::uiOutput(ns("helperUI"))),
            shiny::column(width = 12,
                shiny::uiOutput(outputId = ns("volcano"))
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
#' Internal function for \code{shinyQC}. 
#' 
#' @param id \code{character}
#' @param type \code{reactive} value
#' @param validFormulaMM \code{formula} and \code{reactive} value
#' @param validExprContrast \code{character} and \code{reactive} value
#' @param testResult \code{matrix} and \code{reactive} value
#' @param missingValue \code{logical}, will load the respective help page for a
#' \code{SummarizedExperiment} containing missing or non-missing data
#' 
#' @return 
#' \code{shiny.render.function} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI reactive downloadHandler renderText
#' @importFrom shiny verbatimTextOutput br tagList
#' @importFrom plotly renderPlotly
#' @importFrom shinyhelper helper
#' @importFrom htmlwidgets saveWidget
#' 
#' @noRd
volcanoUIServer <- function(id, type, validFormulaMM, validExprContrast, 
        testResult, missingValue) {

    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            output$helperUI <- shiny::renderUI({
                
                helperFile <- paste("tabPanel_DE_missingValue_", 
                    missingValue, sep = "")
                shiny::br() |> shinyhelper::helper(content = helperFile)
            })
            
            output$volcano <- shiny::renderUI({
                ns <- session$ns
                if (!is.null(validFormulaMM())) {
                    
                    if (!is.null(validExprContrast())) {
                        
                        p_volcano <- shiny::reactive({
                            volcanoPlot(testResult(), type())
                        })
                        
                        output$plotVolcano <- plotly::renderPlotly({
                            p_volcano()
                        })
                        
                        output$downloadPlot <- shiny::downloadHandler(
                            filename = function() {
                                paste("Volcano_", type(), ".html")
                            },
                            content = function(file) {
                                htmlwidgets::saveWidget(p_volcano(), file)
                            }
                        )
                        
                        ## return plot and downloadButton
                        shiny::tagList(
                            plotly::plotlyOutput(ns("plotVolcano")),
                            shiny::downloadButton(
                                outputId = ns("downloadPlot"), "")
                        )
                        
                    } else {
                        output$textVolcano <- shiny::renderText({
                            c("expression for contrasts not",
                                "valid with the given colData")})
                        shiny::verbatimTextOutput(ns("textVolcano"))
                    }
                } else {
                    output$textVolcano <- shiny::renderText({
                        c("formula for Model Matrix not valid",
                            "with the given colData")})
                    shiny::verbatimTextOutput(ns("textVolcano"))
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
#' \code{tP_DE_all} returns the HTML code for the tab-pane 
#' 'DE'. Internal function for \code{shinyQC}.
#' 
#' @return 
#' \code{shiny.tag} with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' tP_DE_all()
#' 
#' @importFrom shiny tabPanel 
#' @importFrom shinydashboard tabBox
#' 
#' @noRd
tP_DE_all <- function() {
    shiny::tabPanel("DE",
        shinydashboard::tabBox(title = "", width = 12,
            tP_colDataUI(id = "colData"),
            tP_modelMatrixUI(id = "modelMatrix"),
            tP_contrastUI(id = "contrast"),
            tP_topDEUI(id = "topDE"),
            tP_volcanoUI(id = "volcano")
        )
    )    
}

