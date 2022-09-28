#' @name tag_loadMessage
#' 
#' @title Create a banner to indicate that the shiny session is busy
#' 
#' @description 
#' The function \code{tag_loadMessage} will display a banner within a shiny 
#' session in the case of shiny being busy. The function will display the 
#' loadmessage \code{Loading...} in the top of the page. 
#' 
#' @details 
#' Internal function for \code{shinyQC}
#' 
#' @return \code{shiny.tag.list} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tag_loadMessage()
#' 
#' @importFrom shiny conditionalPanel tagList tags
#' 
#' @noRd
tag_loadMessage <- function() {
    shiny::tagList(
        shiny::tags$head(shiny::tags$script("
            $(document).on('shiny:connected', function(e) {
                Shiny.onInputChange('innerWidth', window.innerWidth);
            });
            $(window).resize(function(e) {
                Shiny.onInputChange('innerWidth', window.innerWidth);
            });
        ")),
        ## loading panel when app busy
        shiny::tags$head(shiny::tags$style(type="text/css", "
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
        shiny::conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
            shiny::tags$div("Loading...",id = "loadmessage"))      
    )
}

#' @name tag_keepAlive
#' 
#' @title Send data back and forth through the websocket to keep the app busy
#' 
#' @description 
#' The function \code{tag_keepAlive} will send data back and forth through the 
#' websocket. When running the app via the server and the app is getting idle,
#' it will crash. Stop this behavior by exchanging data via the websocket.
#' 
#' @details 
#' Internal function for \code{shinyQC}
#' 
#' @return \code{shiny.tag.list} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tag_keepAlive()
#' 
#' @importFrom shiny tagList HTML tags
#' 
#' @noRd
tag_keepAlive <- function() {
    
    shiny::tagList(
        shiny::tags$head(shiny::HTML(
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
}

#' @name sidebar_assayUI
#' 
#' @title Sidebar UI for normalization, transformation, and batch correction  
#' 
#' @description 
#' The module defines the UI in the sidebar for the data manipulation 
#' (normlization, transformation, and batch correction). 
#' 
#' @details 
#' \code{sidebar_assayUI} returns the HTML code for the sidebar in the tabs
#' \code{Values} and \code{Dimension Reduction}. Internal function for \code{shinyQC}.
#' 
#' @return \code{shiny.tag.list} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' sidebar_assayUI()
#' 
#' @importFrom shiny conditionalPanel selectInput uiOutput strong
#' 
#' @noRd
sidebar_assayUI <- function() {
    shiny::conditionalPanel(
        condition = "input.tabs == 'Dimension Reduction' | input.tabs == 'Values'",
        
        ## select type of normalization
        shiny::selectInput(inputId = "normalization",
            label = shiny::strong("Normalization method"),
            choices = c("none", "sum", "quantile division", "quantile"),
            selected = "none"),
        shiny::conditionalPanel(
            condition = "input.normalization == 'quantile division'",
            shiny::sliderInput("quantile", label = "Quantile",
                min = 0, max = 1, value = 0.75)),
        
        ## select type of batch correction
        shiny::selectInput(inputId = "batch",
            label = shiny::strong("Batch correction method"),
            choices = c("none", "removeBatchEffect (limma)"), 
            selected = "none"),
        ## create conditional panel to select batch variable, use observe to
        ## update the choices
        shiny::conditionalPanel(
            condition = "input.batch == 'removeBatchEffect (limma)'",
                shiny::selectInput("batchCol",
                    label = "Select column containing batch information",
                    choices = "name")),
        
        ## select type of transformation
        shiny::selectInput(inputId = "transformation",
            label = shiny::strong("Transformation method"),
            choices = c("none", "log", "log2", "vsn"), selected = "none")
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
#' \code{sidebar_assayUI} returns the HTML code for the sidebar in the tabs
#' \code{Values} and \code{Dimension Reduction}. Internal function for 
#' \code{shinyQC}.
#' 
#' @return \code{shiny.tag.list} with HTML content
#' 
#' @param missingValue \code{logical} (if \code{FALSE} do not show 
#' \code{selectInput} for 
#' imputation)
#' 
#' @author Thomas Naake
#' 
#' @examples
#' sidebar_imputationUI()
#' 
#' @importFrom shiny conditionalPanel selectInput strong
#' 
#' @noRd
sidebar_imputationUI <- function() {
    ## select type of imputation
    shiny::conditionalPanel("output.missingVals == 'TRUE' & (input.tabs == 'Dimension Reduction' | input.tabs == 'Values')", 
        shiny::selectInput(inputId = "imputation",
            label = shiny::strong("Imputation method"),
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
#' Internal function for \code{shinyQC}.
#' 
#' @return \code{shiny.tag.list} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' sidebar_DEUI()
#' 
#' @importFrom shiny conditionalPanel radioButtons textInput actionButton
#' 
#' @noRd
sidebar_DEUI <- function() {
    shiny::conditionalPanel(condition = "input.tabs == 'DE'",
        shiny::radioButtons(inputId = "DEtype",
            label = "Choose method/test for DE",
            choices = list("Moderated t-test (limma)" ="ttest",
                                "Wald test (proDA)" = "proDA")),
        shiny::textInput(inputId = "modelMat",
            label = "Select levels for Model Matrix", 
            value = "~", placeholder = "~ treatment"),
        shiny::actionButton(inputId = "actionModelMat",
            label = "Update formula for Model Matrix"),
        shiny::textInput(inputId = "contrastMat",
            label = "Select contrast(s)", 
            value = "", placeholder = "treatment"),
        shiny::actionButton(inputId = "actionContrasts",
            label = "Update contrasts"))
}

#' @name sidebar_excludeSampleUI
#' 
#' @title Sidebar UI for removing columns from \code{SummarizedExperiment} 
#' objects 
#' 
#' @description 
#' The module defines the UI in the sidebar for removing columns in the
#' \code{SummarizedExperiment} object. 
#' 
#' @details 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' 
#' @return \code{shiny.tag.list} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#'
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assays = list(a, a+10), 
#'                                 rowData = rD, colData = cD)
#'
#' sidebar_excludeSampleUI("select")
#' 
#' @importFrom shiny NS conditionalPanel radioButtons selectizeInput
#' 
#' @noRd
sidebar_excludeSampleUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::conditionalPanel(condition = "input.tabs != 'DE'",
        shiny::radioButtons(inputId = ns("mode"), label = "Select samples",
            choices = list("all" = "all", "exclude" = "exclude", 
                                                        "select" = "select")),
        shiny::selectizeInput(inputId = ns("excludeSamples"),
            label = NULL, choices = "samples", multiple = TRUE)
    )
}

#' @name sidebar_excludeSampleServer
#' 
#' @title Server for sidebar UI for removing columns from 
#' \code{SummarizedExperiment} objects 
#' 
#' @description 
#' The module defines the server for the 
#' UI in the sidebar for removing columns in the
#' \code{SummarizedExperiment} object.
#' 
#' @details 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param se \code{SummarizedExperiment} object
#' 
#' @return \code{shiny.server.function} 
#' 
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer updateSelectizeInput observe
#' 
#' @noRd
sidebar_excludeSampleServer <- function(id, se) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            shiny::observe({
                shiny::updateSelectizeInput(session = session, 
                    inputId = "excludeSamples",
                    choices = colnames(se), server = TRUE)
            })
        }
    )
}


#' @name sidebar_reportUI
#' 
#' @title Sidebar UI for creating a \code{knitr} report 
#' 
#' @description 
#' The module defines the UI in the sidebar for creating a \code{knitr} report.
#' 
#' @details 
#' Internal function for \code{shinyQC}
#' 
#' @return \code{shiny.tag.list} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' sidebar_reportUI()
#' 
#' @importFrom shiny downloadButton
#' 
#' @noRd
sidebar_reportUI <- function() {
    shiny::downloadButton("report", "Generate report")
}

#' @name sidebar_selectAssayUI
#' 
#' @title Sidebar UI for selecting the assay in multi-assay 
#' \code{SummarizedExperiment} objects
#' 
#' @description 
#' The module defines the UI in the sidebar for selecting the assay in 
#' multi-assay \code{SummarizedExperiment} objects. 
#' 
#' @details 
#' \code{sidebar_selectAssayUI} will only displayed if the 
#' \code{SummarizedExperiment} object contains more than one assay. This 
#' behavior will be governed by the condition 
#' \code{output.lengthAssays == 'TRUE'}; within the server
#' expression the module \code{selectAssayServer} (\code{id = "select"}) will 
#' return the \code{character} \code{"TRUE"} if there is more than one assay 
#' in \code{se}, otherwise it will return \code{"FALSE"} which will not 
#' display the \code{selectInput}. Internal function for \code{shinyQC}.
#' 
#' @param choicesAssaySE \code{character} or \code{numeric}
#'
#' @return \code{shiny.tag.list} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' choicesAssaySE <- 1:2
#' sidebar_selectAssayUI(choicesAssaySE = choicesAssaySE)
#' 
#' @importFrom shiny conditionalPanel selectInput
#' @noRd
sidebar_selectAssayUI <- function(choicesAssaySE) {
    shiny::conditionalPanel(
        condition = "output.lengthAssays == 'TRUE'", 
        shiny::selectInput(inputId = "assaySelected",
            label = "Select data input", choices = choicesAssaySE, 
            selected = 1, multiple = FALSE))
}

#' @name choiceAssaySE
#' 
#' @title Return vector with names of assays of multi-assay 
#' \code{SummarizedExperiment}
#' 
#' @description 
#' The function \code{choiceAssaySE} will return a vector of length
#' \code{length(assays(se))}. It will contain either the assigned names to 
#' \code{assays(se)} (if any) or a vector with the seuence from 1 to 
#' \code{length(assays(se))}.
#'
#' @details 
#' The function returns the \code{choices} for the \code{selectInput} UI to 
#' select the assay. Internal usage in \code{shinyQC}.
#' 
#' @param se \code{SummarizedExperiment} object
#' 
#' @return 
#' \code{character} 
#' 
#' @author Thomas Naake
#' 
#' @examples 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assays = list(a, a+10), 
#'                                     rowData = rD, colData = cD)
#' 
#' choiceAssaySE(se = se)
#' 
#' @importFrom SummarizedExperiment assays
#' @noRd
choiceAssaySE <- function(se) {
    a <- SummarizedExperiment::assays(se)
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
#' @title Select assay from multi-assay \code{SummarizedExperiment} object
#' 
#' @description 
#' The function \code{selectAssaySE} will assign the \code{selected} assay to the 
#' \code{SummarizedExperiment}. \code{selectAssaySE} will return the \code{assay}, 
#' together with the \code{colData} and \code{rowData}, as a 
#' \code{SummarizedExperiment}. 
#' 
#' @details 
#' The function truncates multi-assay \code{SummarizedExperiment} objects to 
#' \code{assays(se)} with length 1. Internal usage in \code{shinyQC}.
#' 
#' @param se \code{SummarizedExperiment} object
#' @param selected \code{numeric} vector, index of the \code{assay} to be returned
#' 
#' @return 
#' \code{SummarizedExperiment} object
#' 
#' @author Thomas Naake
#' 
#' @examples
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assays = list(a, a+10), 
#'                                     rowData = rD, colData = cD)
#' 
#' selectAssaySE(se = se, selected = 1)
#' selectAssaySE(se = se, selected = 2)
#' 
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom SummarizedExperiment assay assays colData rowData assayNames
#' @noRd
selectAssaySE <- function(se, selected = 1) {
    
    if (length(SummarizedExperiment::assayNames(se)) > 1) {
        se <- SummarizedExperiment::SummarizedExperiment(
            assays = SummarizedExperiment::assay(se, i = selected), 
            rowData = SummarizedExperiment::rowData(se), 
            colData = SummarizedExperiment::colData(se))    
    }
    
    se
}



#' @name selectAssayServer
#' 
#' @title Module for server expressions for selecting assay in multi-assay
#' \code{SummarizedExperiment} objects 
#' 
#' @description
#' The module defines the server expressions for selecting the assay in 
#' multi-assay \code{SummarizedExperiment} objects.
#' 
#' @details 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param se \code{SummarizedExperiment}
#' @param selected \code{reactive} expression and \code{character}/\code{numeric}
#' 
#' @return
#' \code{reactive} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive
#'
#' @noRd
selectAssayServer <- function(id, se, selected) {
    shiny::moduleServer(
        id,
        function(input, output, session) {

            se_sel <- shiny::reactive({
                selectAssaySE(se, selected = selected())
            })
            
            return(se_sel)
        }
    )
}

#' @name selectSampleSE
#' 
#' @title Select sample(s) from a \code{SummarizedExperiment} object
#' 
#' @description 
#' The function \code{selectSampleSE} removes/adds samples from a 
#' \code{SummarizedExperiment} as specified by the \code{selection} argument 
#' and the \code{mode} argument.
#' 
#' @details 
#' If the supplied vector (\code{selection}) is of length 0, the object 
#' \code{se} will be returned as is. 
#' 
#' @param se \code{SummarizedExperiment} object
#' @param selection \code{character} vector, containing the samples to 
#' exclude/add (\code{colnames(se)})
#' 
#' @return 
#' \code{SummarizedExperiment} object
#' 
#' @author Thomas Naake
#' 
#' @examples
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
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
#' @title Select feature(s) from a \code{SummarizedExperiment} object
#' 
#' @description 
#' The function \code{selectFeatureSE} removes/adds feature from a 
#' \code{SummarizedExperiment} as specified by the \code{selection} argument and the
#' \code{mode} argument.
#' 
#' @details 
#' If the supplied vector (\code{selection}) is of length 0, the object \code{se} will 
#' be returned as is (if \code{mode = "exclude"}). 
#' If the supplied vector (\code{selection}) is of length <3, the object \code{se} will 
#' be returned as is (if \code{mode = "select"}).
#' 
#' @param se \code{SummarizedExperiment} object
#' @param selection \code{character} vector, containing the samples to exclude/add 
#' (\code{colnames(se)})
#' 
#' @return 
#' \code{SummarizedExperiment} object
#' 
#' @author Thomas Naake
#' 
#' @examples
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, 
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' a[c(1, 5, 8), 1:5] <- NA
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
#'     rowData = rD, colData = cD)
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
#' @title Update the \code{SummarizedExperiment} by the slot array
#' 
#' @description 
#' The function \code{updateSE} updates the \code{assay} slot of a \code{SummarizedExperiment}
#' object, \code{se}, with a supplied \code{assay} object. The function returns the 
#' updated object. 
#' 
#' @details 
#' Internal function in \code{shinyQC}.
#' 
#' @param se \code{SummarizedExperiment} object
#' @param assay \code{matrix} with same dimensions as \code{assay(se)}
#' 
#' @return
#' \code{SummarizedExperiment} object
#' 
#' @author Thomas Naake
#' 
#' @examples
#' assayOld <- SummarizedExperiment::assay(se)
#' assayNew <- assayOld + 1
#' updateSE(se, assayNew)
#' 
#' @importFrom SummarizedExperiment assay 'assay<-'
#' @importFrom shiny req
#'
#' @noRd
updateSE <- function(se, assay) {
    shiny::req(assay)
    SummarizedExperiment::assay(se, withDimnames = FALSE) <- assay
    
    return(se)
}


#' @name missingValuesSE
#' 
#' @title Check if there are missing values in the assay slot(s) of 
#' the \code{SummarizedExperiment}
#' 
#' @description 
#' The function \code{missingValuesSE} checks if the \code{assay} slot(s) of a 
#' \code{SummarizedExperiment} object, \code{se}, contains \code{NA} values.
#' 
#' @details 
#' Internal function in \code{shinyQC}.
#' 
#' @param se \code{SummarizedExperiment} object
#' 
#' @return
#' \code{logical}
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
    len <- length(SummarizedExperiment::assays(se))
    l_na <- lapply(seq_len(len), 
        function(i) any(is.na(SummarizedExperiment::assay(se, i))))
    any(unlist(l_na))
}
