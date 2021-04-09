#' @name tP_histSampleUI
#' 
#' @title Tab panel UI for tab panel 'Histogram'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Histogram': a `plotOutput` 
#' and a `selectInput` for the selection of the categorical variable. 
#' 
#' @details 
#' `tP_histSampleUI` returns the HTML code for the tab-pane 'Histogram'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object
#' 
#' @return `shiny.tag` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_histSampleUI("test")
#' 
#' @noRd
tP_histSampleUI <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Histogram", 
        plotlyOutput(outputId = ns("histSample")),
        downloadButton(outputId = ns("downloadPlot"), ""),
        uiOutput(outputId = ns("typeHistUI"))
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
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object and `reactive` value
#' 
#' @return 
#' `shiny.render.function` expression
#' 
#' @author Thomas Naake
#'
#' @importFrom htmlwidgets saveWidget
#'
#' @noRd
histSampleServer <- function(id, se) {
    moduleServer(
        id, 
        function(input, output, session) {
            
            output$typeHistUI <- renderUI({
                selectInput(inputId = session$ns("typeHist"), 
                            label = "Categorical variable", 
                            choices = colnames(colData(se())))
            })
            
            histTbl <- reactive({
                hist_sample_num(se(), category = input$typeHist)
            })
            
            p_hist <- reactive({
                hist_sample(histTbl(), category = input$typeHist)
            })
            
            output$histSample <- renderPlotly({
                req(input$typeHist)
                p_hist()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function() {
                    paste("Histogram_", input$typeHist, ".html", sep = "")
                },
                content = function(file) {
                    saveWidget(p_hist(), file)
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
#' The module defines the UI for the tab panel 'Mosaic': a `plotOutput` 
#' and two `selectInput` elements, one for `'Categorical variable 1'` and 
#' one for `'Categorical variable 2'`
#' 
#' @details 
#' `tP_mosaicSampleUI` returns the HTML code for the tab-pane 'Mosaic'. 
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
#' tP_mosaicSampleUI("test")
#' 
#' @noRd
tP_mosaicSampleUI <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Mosaic", 
        plotOutput(outputId = ns("mosaicSample")) %>% 
            helper(content = "tabPanel_mosaicSample"),
        downloadButton(outputId = ns("downloadPlot"), ""),
        uiOutput(outputId = ns("mosaicVarUI"))
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
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object and a `reactive` value
#' 
#' @return 
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
mosaicSampleServer <- function(id, se) {
    moduleServer(
        id, 
        function(input, output, session) {

            output$mosaicVarUI <- renderUI({
                
                tagList(
                    selectInput(inputId = session$ns("mosaicf1"), 
                        label = "Categorical variable 1", 
                        choices = colnames(colData(se())),
                        selected = "type"),
                    selectInput(inputId = session$ns("mosaicf2"),
                        label = "Categorical variable 2", 
                        choices = colnames(colData(se())),
                        selected = "type"))
            })
            
            p_mosaic <- reactive({
                mosaic(se(), f1 = input$mosaicf1, f2 = input$mosaicf2)
            })
            
            output$mosaicSample <- renderPlot({
                req(input$mosaicf1, input$mosaicf2)
                p_mosaic()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function() {
                    paste("Mosaic_", input$mosaicf1, "_", input$mosaicf2, 
                        ".pdf", sep = "")
                },
                content = function(file) {
                    ggsave(file, p_mosaic(), device = "pdf")
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
#' `tP_samples_all` returns the HTML code for the tab-pane 'Samples'. 
#' Internal function for `shinyQC`.
#' 
#' @return 
#' `shiny.tag` with HTML content
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
#' @noRd
tP_samples_all <- function() {
    tabPanel("Samples",
        tabBox(title = "", width = 12,
            tP_histSampleUI(id = "Sample_hist"),
            tP_mosaicSampleUI(id = "Sample_mosaic")
        )
    )
    
}
