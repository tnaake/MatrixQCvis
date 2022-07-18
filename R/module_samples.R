#' @name tP_histSampleUI
#' 
#' @title Tab panel UI for tab panel 'Histogram'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Histogram': a \code{plotOutput} 
#' and a \code{selectInput} for the selection of the categorical variable. 
#' 
#' @details 
#' \code{tP_histSampleUI} returns the HTML code for the tab-pane 'Histogram'. 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param se \code{SummarizedExperiment} object
#' 
#' @return \code{shiny.tag} with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_histSampleUI("test")
#' 
#' @importFrom shiny NS tabPanel downloadButton uiOutput
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_histSampleUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Histogram", 
        plotly::plotlyOutput(outputId = ns("histSample")),
        shiny::downloadButton(outputId = ns("downloadPlot"), ""),
        shiny::uiOutput(outputId = ns("typeHistUI"))
    )
}


#' @name histSampleServer
#' 
#' @title Module for server expressions of tab panel 'Histogram'
#' 
#' @description 
#' The function defines the server output for the tab panel 'Histogram': the
#' hitogram plot.
#' 
#' @details 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param se \code{SummarizedExperiment} object and \code{reactive} value
#' 
#' @return 
#' \code{shiny.render.function} expression
#' 
#' @author Thomas Naake
#'
#' @importFrom shiny moduleServer renderUI selectInput reactive downloadHandler
#' @importFrom shiny req
#' @importFrom htmlwidgets saveWidget
#' @importFrom plotly renderPlotly
#' @importFrom SummarizedExperiment colData
#'
#' @noRd
histSampleServer <- function(id, se) {
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            output$typeHistUI <- shiny::renderUI({
                shiny::selectInput(inputId = session$ns("typeHist"), 
                    label = "Categorical variable", 
                    choices = colnames(SummarizedExperiment::colData(se())))
            })
            
            histTbl <- shiny::reactive({
                hist_sample_num(se(), category = input$typeHist)
            })
            
            p_hist <- shiny::reactive({
                hist_sample(histTbl(), category = input$typeHist)
            })
            
            output$histSample <- plotly::renderPlotly({
                shiny::req(input$typeHist)
                p_hist()
            })
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("Histogram_", input$typeHist, ".html", sep = "")
                },
                content = function(file) {
                    htmlwidgets::saveWidget(p_hist(), file)
                }
            )
        }
    )
}


#' @name tP_mosaicSampleUI
#' 
#' @title Tab panel UI for tab panel 'Mosaic'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Mosaic': a \code{plotOutput} 
#' and two \code{selectInput} elements, one for \code{'Categorical variable 1'} and 
#' one for \code{'Categorical variable 2'}
#' 
#' @details 
#' \code{tP_mosaicSampleUI} returns the HTML code for the tab-pane 'Mosaic'. 
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
#' tP_mosaicSampleUI("test")
#' 
#' @importFrom shiny NS tabPanel plotOutput downloadButton uiOutput
#' @importFrom shinyhelper helper
#' 
#' @noRd
tP_mosaicSampleUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Mosaic", 
        shiny::plotOutput(outputId = ns("mosaicSample")) |>
            shinyhelper::helper(content = "tabPanel_mosaicSample"),
        shiny::downloadButton(outputId = ns("downloadPlot"), ""),
        shiny::uiOutput(outputId = ns("mosaicVarUI"))
        )
}

#' @name mosaicSampleServer
#' 
#' @title Module for server expressions of tab panel 'Mosaic'
#' 
#' @description 
#' The module defines the output for the tab panel 'Mosaic'.
#' 
#' @details 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param se \code{SummarizedExperiment} object and a \code{reactive} value
#' 
#' @return 
#' \code{shiny.render.function} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI tagList selectInput reactive
#' @importFrom shiny renderPlot req downloadHandler
#' @importFrom SummarizedExperiment colData
#' @importFrom ggplot2 ggsave
#' 
#' @noRd
mosaicSampleServer <- function(id, se) {
    shiny::moduleServer(
        id, 
        function(input, output, session) {

            output$mosaicVarUI <- shiny::renderUI({
                cn <- colnames(SummarizedExperiment::colData(se()))
                shiny::tagList(
                    shiny::selectInput(inputId = session$ns("mosaicf1"), 
                        label = "Categorical variable 1", 
                        choices = cn, selected = "type"),
                    shiny::selectInput(inputId = session$ns("mosaicf2"),
                        label = "Categorical variable 2", 
                        choices = cn,
                        selected = "type"))
            })
            
            p_mosaic <- shiny::reactive({
                mosaic(se(), f1 = input$mosaicf1, f2 = input$mosaicf2)
            })
            
            output$mosaicSample <- shiny::renderPlot({
                shiny::req(input$mosaicf1, input$mosaicf2)
                p_mosaic()
            })
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("Mosaic_", input$mosaicf1, "_", input$mosaicf2, 
                        ".pdf", sep = "")
                },
                content = function(file) {
                    ggplot2::ggsave(file, p_mosaic(), device = "pdf")
                }
            )
        }
    )
}

#' @name tP_samples_all
#' 
#' @title Tab panel UI for tab panel 'Samples'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Samples'.
#' 
#' @details 
#' \code{tP_samples_all} returns the HTML code for the tab-pane 'Samples'. 
#' Internal function for \code{shinyQC}.
#' 
#' @return 
#' \code{shiny.tag} with HTML content
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
#' tP_samples_all()
#' 
#' @importFrom shiny tabPanel 
#' @importFrom shinydashboard tabBox
#' @noRd
tP_samples_all <- function() {
    shiny::tabPanel("Samples",
        shinydashboard::tabBox(title = "", width = 12,
            tP_histSampleUI(id = "Sample_hist"),
            tP_mosaicSampleUI(id = "Sample_mosaic")
        )
    )
}
