#' @name tag_loadMessage
#' 
#' @title Create a banner to indicate that the shiny session is busy
#' 
#' @description 
#' The function `tag_loadMessage` will display a banner within a shiny session
#' in the case of shiny being busy. The function will display the loadmessage
#' `Loading...` in the top of the page. 
#' 
#' @details 
#' Internal function for `shinyQC`
#' 
#' @return `shiny.tag.list` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tag_loadMessage()
#' 
#' @noRd
tag_loadMessage <- function() {
    l <- tagList(
        ##
        tags$head(tags$script("
            $(document).on('shiny:connected', function(e) {
                Shiny.onInputChange('innerWidth', window.innerWidth);
            });
            $(window).resize(function(e) {
                Shiny.onInputChange('innerWidth', window.innerWidth);
            });
        ")),
        ## loading panel when app busy
        tags$head(tags$style(type="text/css", "
                #loadmessage {
                    position: fixed;
                    top: 0px;
                    left: 0px;
                    width: 100%;
                    padding: 5px 0px 5px 0px;
                    text-align: center;
                    font-weight: bold;
                    font-size: 100%;
                    color: #000000;
                    background-color: #6495ED;
                    z-index: 105;
                }
        ")),
        conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
            tags$div("Loading...",id = "loadmessage"))      
    )
    return(l)
}

#' @name tag_keepAlive
#' 
#' @title Send data back and forth through the websocket to keep the app busy
#' 
#' @description 
#' The function `tag_keepAlive` will send data back and forth through the 
#' websocket. When running the app via the server and the app is getting idle,
#' it will crash. Stop this behavior by exchanging data via the websocket.
#' 
#' @details 
#' Internal function for `shinyQC`
#' 
#' @return `shiny.tag.list` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tag_keepAlive()
#' 
#' @noRd
tag_keepAlive <- function() {
    
    l <- tagList(
        tags$head(HTML(
            "
            <script>
            var socket_timeout_interval
            var n = 0
            $(document).on('shiny:connected', function(event) {
            socket_timeout_interval = setInterval(function(){
            Shiny.onInputChange('keepAlive', n++)
            }, 15000)
            });
            $(document).on('shiny:disconnected', function(event) {
            clearInterval(socket_timeout_interval)
            });
            </script>
            "
            )
        )
    )
    return(l)
}

#' @name sidebar_assayUI
#' 
#' @title Sidebar UI for normalization, transformation, and batch correction  
#' 
#' 
#' @description 
#' The module defines the UI in the sidebar for the data manipulation 
#' (normlization, transformation, and batch correction). 
#' 
#' @details 
#' `sidebar_assayUI` returns the HTML code for the sidebar in the tabs
#' `Values` and `Dimension Reduction`. Internal function for `shinyQC`.
#' 
#' @return `shiny.tag.list` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' sidebar_assayUI()
#' 
#' @noRd
sidebar_assayUI <- function() {
    conditionalPanel(
        condition = "input.tabs == 'Dimension Reduction' | input.tabs == 'Values'",
        ## select type of normalization
        selectInput(inputId = "normalization",
            label = strong("Normalization method"),
            choices = c("none", "sum", "quantile division", "quantile"),
            selected = "none"),
        conditionalPanel(
            condition = "input.normalization == 'quantile division'",
            uiOutput("quantDiv")),

        ## select type of transformation
        selectInput(inputId = "transformation",
            label = strong("Transformation method"),
            choices = c("none", "log2", "vsn"), selected = "log2"),

        ## select type of batch correction
        selectInput(inputId = "batch",
            label = strong("Batch correction method"),
            choices = c("none", "removeBatchEffect (limma)"), 
            selected = "none"),
        conditionalPanel(
            condition = "input.batch == 'removeBatchEffect (limma)'",
            uiOutput("batchCol"))
        )
    
}

#' @name sidebar_imputationUI
#' 
#' @title Conditional sidebar UI for imputation
#' 
#' @description 
#' The module defines the UI in the sidebar for the data imputation. 
#' 
#' @details 
#' `sidebar_assayUI` returns the HTML code for the sidebar in the tabs
#' `Values` and `Dimension Reduction`. Internal function for `shinyQC`.
#' 
#' @return `shiny.tag.list` with HTML content
#' 
#' @param missingValue `logical` (if `FALSE` do not show `selectInput` for 
#' imputation)
#' 
#' @author Thomas Naake
#' 
#' @examples
#' sidebar_imputationUI()
#' 
#' @noRd
sidebar_imputationUI <- function() {
    ## select type of imputation
    conditionalPanel("output.missingVals == 'TRUE' & (input.tabs == 'Dimension Reduction' | input.tabs == 'Values')", 
        selectInput(inputId = "imputation",
            label = strong("Imputation method"),
            choices = c("BPCA", "kNN", "MLE", "Min", "MinDet", "MinProb"),
            selected = "MinDet"))
}

#' @name sidebar_DEUI
#' 
#' @title Sidebar UI for the tab 'DE'
#' 
#' @description 
#' The module defines the UI in the tab 'DE'. It allows for specifying the
#' formula for the model matrix and the contrasts. Furthermore, it provides 
#' the UI for selecting the type of test to perform (ttest or proDA). 
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @return `shiny.tag.list` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' sidebar_DEUI()
#' 
#' @noRd
sidebar_DEUI <- function() {
    conditionalPanel(condition = "input.tabs == 'DE'",
        radioButtons(inputId = "DEtype",
            label = "Choose method/test for DE",
            choices = list("Moderated t-test (limma)" ="ttest",
                                "Wald test (proDA)" = "proDA")),
        textInput(inputId = "modelMat",
            label = "Select levels for Model Matrix", 
            value = "~", placeholder = "~ treatment"),
        actionButton(inputId = "actionModelMat",
            label = "Update formula for Model Matrix"),
        textInput(inputId = "contrastMat",
            label = "Select contrast(s)", 
            value = "", placeholder = "treatment"),
        actionButton(inputId = "actionContrasts",
            label = "Update contrasts"))
}

#' @name sidebar_excludeSampleUI
#' 
#' @title Sidebar UI for removing columns from `SummarizedExperiment` objects 
#' 
#' @description 
#' The module defines the UI in the sidebar for removing columns in the
#' `SummarizedExperiment` object. 
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' 
#' @return `shiny.tag.list` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
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
#' se <- SummarizedExperiment(assays = list(a, a+10), 
#'                                 rowData = rD, colData = cD)
#'
#' sidebar_excludeSampleUI("select")
#' 
#' @noRd
sidebar_excludeSampleUI <- function(id) {
    ns <- NS(id)
    conditionalPanel(condition = "input.tabs != 'DE'",
        radioButtons(inputId = ns("mode"), label = "Select samples",
            choices = list("all" = "all", "exclude" = "exclude", 
                                                        "select" = "select")),
        uiOutput(outputId = ns("excludeSamplesUI"))
    )
}

#' @name sidebar_excludeSampleServer
#' 
#' @title Server for sidebar UI for removing columns from 
#' `SummarizedExperiment` objects 
#' 
#' @description 
#' The module defines the server for the 
#' UI in the sidebar for removing columns in the
#' `SummarizedExperiment` object.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object
#' 
#' @return `shiny.server.function` 
#' 
#' @author Thomas Naake
#' 
#' @noRd
sidebar_excludeSampleServer <- function(id, se) {
    moduleServer(
        id,
        function(input, output, session) {
            
            output$excludeSamplesUI <- renderUI({
                selectInput(inputId = session$ns("excludeSamples"),
                            label = NULL, choices = colnames(se),
                            multiple = TRUE)
            })
        }
    )
}


#' @name sidebar_reportUI
#' 
#' @title Sidebar UI for creating a `knitr` report 
#' 
#' @description 
#' The module defines the UI in the sidebar for creating a `knitr` report.
#' 
#' @details 
#' Internal function for `shinyQC`
#' 
#' @return `shiny.tag.list` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' sidebar_reportUI()
#' 
#' @noRd
sidebar_reportUI <- function() {
    downloadButton("report", "Generate report")
}


#' @name sidebar_stopUI
#' 
#' @title Sidebar UI for stopping the app from running 
#' 
#' @description 
#' The module defines the UI in the sidebar for stopping the `shiny` 
#' application. Upon execution, this will also trigger the export of the 
#' modified data set.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param app_server `logical`
#' 
#' @return `shiny.tag.list` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' app_server <- TRUE
#' sidebar_stopUI(app_server)
#' 
#' @noRd
sidebar_stopUI <- function(app_server) {
    if (!app_server) 
        conditionalPanel(condition = "input.tabs != 'DE'",
                            actionButton("stop", "Stop and export data set")
        )
}

#' @name sidebar_selectAssayUI
#' 
#' @title Sidebar UI for selecting the assay in multi-assay 
#' `SummarizedExperiment` objects
#' 
#' @description 
#' The module defines the UI in the sidebar for selecting the assay in 
#' multi-assay `SummarizedExperiment` objects. 
#' 
#' @details 
#' `sidebar_selectAssayUI` will only displayed if the `SummarizedExperiment` 
#' object contains more than one assay. This behavior will be governed by the
#' condition `output.lengthAssays == 'TRUE'`; within the server
#' expression the module `selectAssayServer` (`id = "select"`) will return
#' the `character` `"TRUE"` if there is more than one assay in `se`, otherwise
#' it will return `"FALSE"` which will not display the `selectInput`. 
#' Internal function for `shinyQC`.
#' 
#' @param choicesAssaySE `character` or `numeric`
#'
#' @return `shiny.tag.list` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' choicesAssaySE <- 1:2
#' sidebar_selectAssayUI(choicesAssaySE = choicesAssaySE)
#' 
#' @noRd
sidebar_selectAssayUI <- function(choicesAssaySE) {
    conditionalPanel(
        condition = "output.lengthAssays == 'TRUE'", 
        selectInput(inputId = "assaySelected",
                    label = "Select data input", choices = choicesAssaySE, 
                    selected = 1, multiple = FALSE))
}

#' @name choiceAssaySE
#' 
#' @title Return vector with names of assays of multi-assay 
#' `SummarizedExperiment`
#' 
#' @description 
#' The function `choiceAssaySE` will return a vector of length
#' `length(assays(se))`. It will contain either the assigned names to 
#' `assays(se)` (if any) or a vector with the seuence from 1 to 
#' `length(assays(se))`.
#'
#' @details 
#' The function returns the `choices` for the `selectInput` UI to select
#' the assay. Internal usage in `shinyQC`.
#' 
#' @param se `SummarizedExperiment` object
#' 
#' @return 
#' `character` 
#' 
#' @author Thomas Naake
#' 
#' @examples
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
#' se <- SummarizedExperiment(assays = list(a, a+10), 
#'                                     rowData = rD, colData = cD)
#' 
#' choiceAssaySE(se = se)
#' 
#' @importFrom SummarizedExperiment assays
#' @noRd
choiceAssaySE <- function(se) {
    a <- assays(se)
    names_a <- names(a)
    
    if (NA %in% names_a) stop("names(assays(se)) contains NA")
    
    if (length(names_a) == length(a)) {
        choice <- names_a
    } else {
        choice <- seq_len(length(a))
    }
    
    return(choice)
}


#' @name selectAssaySE
#' 
#' @title Select assay from multi-assay `SummarizedExperiment` object
#' 
#' @description 
#' The function `selectAssaySE` will assign the `selected` assay to the 
#' `SummarizedExperiment`. `selectAssaySE` will return the `assay`, together
#' with the `colData` and `rowData`, as a `SummarizedExperiment`. 
#' 
#' @details 
#' The function truncates multi-assay `SummarizedExperiment` objects to 
#' `assays(se)` with length 1. Internal usage in `shinyQC`.
#' 
#' @param se `SummarizedExperiment` object
#' @param selected `numeric` vector, index of the `assay` to be returned
#' 
#' @return 
#' `SummarizedExperiment` object
#' 
#' @author Thomas Naake
#' 
#' @examples
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
#' se <- SummarizedExperiment(assays = list(a, a+10), 
#'                                     rowData = rD, colData = cD)
#' 
#' selectAssaySE(se = se, selected = 1)
#' selectAssaySE(se = se, selected = 2)
#' 
#' @importFrom SummarizedExperiment assays rowData
#' @noRd
selectAssaySE <- function(se, selected = 1) {
    
    if (length(names(assays(se))) > 1) {
        a <- assay(se, i = selected)
        cD <- colData(se)
        rD <- rowData(se)
        se <- SummarizedExperiment(assays = a, rowData = rD, colData = cD)    
    }
    
    return(se)
}



#' @name selectAssayServer
#' 
#' @title Module for server expressions for selecting assay in multi-assay
#' `SummarizedExperiment` objects 
#' 
#' @description
#' The module defines the server expressions for selecting the assay in 
#' multi-assay `SummarizedExperiment` objects.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment`
#' @param selected `reactive` expression and `character`/`numeric`
#' 
#' @return
#' `reactive` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
selectAssayServer <- function(id, se, selected) {
    moduleServer(
        id,
        function(input, output, session) {

            se_sel <- reactive({
                selectAssaySE(se, selected = selected())
            })
            
            return(se_sel)
        }
    )
}

#' @name selectSampleSE
#' 
#' @title Select sample(s) from a `SummarizedExperiment` object
#' 
#' @description 
#' The function `selectSampleSE` removes/adds samples from a 
#' `SummarizedExperiment` as specified by the `selection` argument and the
#' `mode` argument.
#' 
#' @details 
#' If the supplied vector (`selection`) is of length 0, the object `se` will 
#' be returned as is. 
#' 
#' @param se `SummarizedExperiment` object
#' @param selection `character` vector, containing the samples to exclude/add 
#' (`colnames(se)`)
#' 
#' @return 
#' `SummarizedExperiment` object
#' 
#' @author Thomas Naake
#' 
#' @examples
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
#' samplesToExclude <- c("sample 1")
#' selectSampleSE(se = se, selection = samplesToExclude, mode = "exclude")
#' 
#' @noRd
selectSampleSE <- function(se, selection, 
                                    mode = c("all", "exclude", "select")) {
    
    mode <- match.arg(mode)
    
    if (mode == "exclude") {
        if (length(selection) > 0) {
            se <- se[, !(colnames(se) %in% selection)]  
        }    
    }
    
    if (mode == "select") {
        if (length(selection) >= 3) {
            se <- se[, colnames(se) %in% selection]    
        }
    } 
    
    return(se)
    
}

#' @name selectFeatureSE
#' 
#' @title Select feature(s) from a `SummarizedExperiment` object
#' 
#' @description 
#' The function `selectFeatureSE` removes/adds feature from a 
#' `SummarizedExperiment` as specified by the `selection` argument and the
#' `mode` argument.
#' 
#' @details 
#' If the supplied vector (`selection`) is of length 0, the object `se` will 
#' be returned as is (if `mode = "exclude"`). 
#' If the supplied vector (`selection`) is of length <3, the object `se` will 
#' be returned as is (if `mode = "select"`).
#' 
#' @param se `SummarizedExperiment` object
#' @param selection `character` vector, containing the samples to exclude/add 
#' (`colnames(se)`)
#' 
#' @return 
#' `SummarizedExperiment` object
#' 
#' @author Thomas Naake
#' 
#' @examples
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
#' featuresToExclude <- c("1", "2")
#' selectFeatureSE(se = se, selection = featuresToExclude, mode = "exclude")
#' 
#' @noRd
selectFeatureSE <- function(se, selection, 
                                    mode = c("all", "exclude", "select")) {
    
    mode <- match.arg(mode)
    
    if (mode == "exclude") {
        if (length(selection) > 0) {
            se <- se[!(rownames(se) %in% selection), ]  
        }    
    }
    
    if (mode == "select") {
        if (length(selection) >= 3) {
            se <- se[rownames(se) %in% selection, ]    
        } 
    } 
    
    return(se)
}

#' @name updateSE
#' 
#' @title Update the `SummarizedExperiment` by the slot array
#' 
#' @description 
#' The function `updateSE` updates the `assay` slot of a `SummarizedExperiment`
#' object, `se`, with a supplied `assay` object. The function returns the 
#' updated object. 
#' 
#' @details 
#' Internal function in `shinyQC`.
#' 
#' @param se `SummarizedExperiment` object
#' @param assay `matrix` with same dimensions as `assay(se)`
#' 
#' @return
#' `SummarizedExperiment` object
#' 
#' @author Thomas Naake
#' 
#' @examples
#' assayOld <- assay(se)
#' assayNew <- assayOld + 1
#' updateSE(se, assayNew)
#' 
#' @importFrom SummarizedExperiment assay 'assay<-'
#' 
#' @noRd
updateSE <- function(se, assay) {
    req(assay)
    assay(se) <- assay
    return(se)
}


#' @name missingValuesSE
#' 
#' @title Check if there are missing values in the assay slot(s) of 
#' the `SummarizedExperiment`
#' 
#' @description 
#' The function `missingValuesSE` checks if the `assay` slot(s) of a 
#' `SummarizedExperiment` object, `se`, contains `NA` values.
#' 
#' @details 
#' Internal function in `shinyQC`.
#' 
#' @param se `SummarizedExperiment` object
#' 
#' @return
#' `logical`
#' 
#' @author Thomas Naake
#' 
#' @examples
#' missingValuesSE(se)
#' 
#' @importFrom SummarizedExperiment assay assays
#' 
#' @noRd
missingValuesSE <- function(se) {
    len <- length(assays(se))
    l_na <- lapply(seq_len(len), function(i) any(is.na(assay(se, i))))
    any(unlist(l_na))
}
