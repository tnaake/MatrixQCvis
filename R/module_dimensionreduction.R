#' @name coordsUI
#' 
#' @title UI for `selectInput` for dimension selection
#' 
#' @description 
#' The module defines the UI in the tab panel 'Dimension reduction'. It allows
#' to switch between the different coordinates. 
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param df `data.frame` as obtained by the function `ordination`
#' @param name `character` title/name for row
#' @param x `character`, character string for `input$x`
#' @param y `character`, character string for `input$y`
#' @param session `shiny` session
#' 
#' @examples
#' df <- ordination(x, "PCA")
#' coordsUI(df)
#' 
#' @import shiny
#' 
#' @noRd
coordsUI <- function(df, name = "PC", 
        x = "ordination_x", y = "ordination_y", session = session) {
    
    fluidRow(
        column(12, name),
        column(6, 
            selectInput(session$ns(x), label = "x-axis",
                    choices = colnames(df)[-1],
                    selected = colnames(df)[-1][1])),
        column(6, 
            selectInput(session$ns(y), label = "y-axis",
                    choices = colnames(df)[-1],
                    selected = colnames(df)[-1][2]))
    )
}


#' @name tP_PCAUI
#' 
#' @title Tab panel UI for tab panel 'PCA'
#' 
#' @description
#' The module defines the UI in the tab panel 'PCA'.
#' 
#' @details 
#' `tP_PCAUI` returns the HTML code for the tab-pane 'PCA'. 
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
#' tP_PCAUI("test")
#' 
#' @noRd
tP_PCAUI <- function(id) {
    ns <- NS(id)
    tabPanel(title = "PCA", 
        fluidRow(
            column(12, 
                plotlyOutput(outputId = ns("plot"), height = "auto") %>% 
                    helper(content = "tabPanel_PCA"),
                downloadButton(outputId = ns("downloadPlot"), ""))
        ),
        fluidRow(
            box(title = "Parameters", width = 6, collapsible = TRUE, 
                column(12, 
                    uiOutput(outputId = ns("coords"))),
                column(3, 
                    checkboxInput(inputId = ns("scale"),
                        label = "scale", value = TRUE)),
                column(3, 
                    checkboxInput(inputId = ns("center"),
                        label = "center", value = TRUE)),
                column(6, 
                    uiOutput(outputId = ns("highlightUI")))
            ),
            box(title = "Scree plot", width = 6, 
                collapsible = TRUE, collapsed = TRUE,
                plotOutput(outputId = ns("PCAVarplot"))
            )
        )
    )
}

#' @name tP_PCoAUI
#' 
#' @title Tab panel UI for tab panel 'PCoA'
#' 
#' @description
#' The module defines the UI in the tab panel 'PCoA'.
#' 
#' @details 
#' `tP_PCoAUI` returns the HTML code for the tab-pane 'PCoA'. 
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
#' tP_PCoAUI("test")
#' 
#' @noRd
tP_PCoAUI <- function(id) {
    ns <- NS(id)
    tabPanel(title = "PCoA", 
        fluidRow(
            column(12, 
                plotlyOutput(outputId = ns("plot"), height = "auto") %>% 
                    helper(content = "tabPanel_PCoA"),
                downloadButton(outputId = ns("downloadPlot"), ""))
            ), 
        fluidRow(
            box(title = "Parameters", width = 6, collapsible = TRUE, 
                column(12, 
                    uiOutput(ns("coords"))
                ),
                column(6, 
                    selectInput(inputId = ns("dist"),
                        label = "Distance measure",
                        choices = c("euclidean", "maximum", 
                            "manhattan", "canberra", "minkowski"), 
                        selected = "euclidean")
                ),
                column(6, 
                    uiOutput(outputId = ns("highlightUI"))
                )
            )
        )
    )
}

#' @name tP_NMDSUI
#' 
#' @title Tab panel UI for tab panel 'NMDS'
#' 
#' @description
#' The module defines the UI in the tab panel 'NMDS'.
#' 
#' @details
#' `tP_NMDSUI` returns the HTML code for the tab-pane 'NMDS'. 
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
#' tP_NMDSUI("test")
#' 
#' @noRd
tP_NMDSUI <- function(id) {
    ns <- NS(id)
    tabPanel(title = "NMDS", 
        fluidRow(
            column(12, 
                plotlyOutput(outputId = ns("plot"), height = "auto") %>% 
                    helper(content = "tabPanel_NMDS"),
                downloadButton(outputId = ns("downloadPlot"), ""))), 
        fluidRow(
            box(title = "Parameters", width = 6, collapsible = TRUE, 
                column(12, 
                    uiOutput(outputId = ns("coords"))
                ),
                column(6, 
                    selectInput(inputId = ns("dist"),
                        label = "Distance measure",
                        choices = c("euclidean", "maximum", 
                            "manhattan", "canberra", "minkowski"), 
                        selected = "euclidean")
                ),
                column(6, 
                    uiOutput(outputId = ns("highlightUI"))
                )
            )
        )
    )
}

#' @name tP_tSNEUI
#' 
#' @title Tab panel UI for tab panel 'tSNE'
#' 
#' @description
#' The module defines the UI in the tab panel 'tSNE'.
#' 
#' @details
#' `tP_tSNEUI` returns the HTML code for the tab-pane 'tSNE'. 
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
#' tP_tSNEUI("test")
#' 
#' @noRd
tP_tSNEUI <- function(id) {
    ns <- NS(id)
    tabPanel(title = "tSNE", 
        fluidRow(
            column(12, 
                plotlyOutput(outputId = ns("plot"), height = "auto") %>% 
                    helper(content = "tabPanel_tSNE"),
                downloadButton(outputId = ns("downloadPlot"), ""))),
        fluidRow(
            box(title = "Principal components", status = "primary", width = 12,
                collapsible = TRUE, collapsed = TRUE,
                fluidRow(
                    column(6, plotOutput(outputId = ns("PCAVarplotPerm"))),
                    column(6, plotOutput(outputId = ns("PCAVarPvalueplot")))
                )
            )
        ),
        fluidRow(
            box(title = "Parameters", width = 6, collapsible = TRUE, 
                uiOutput(outputId = ns("coords")),
                column(6, uiOutput(outputId = ns("perplexityUI"))),
                column(6, 
                    sliderInput(inputId = ns("maxIter"),
                        label = "Number of iterations", 
                        min = 100, max = 10000, value = 1000)),
                column(6, uiOutput(outputId = ns("initialDimsUI"))),
                column(6, 
                    sliderInput(inputId = ns("dims"),
                        label = "Output dimensionality",
                        min = 2, max = 3, value = 3, step = 1)),
                column(3, 
                    checkboxInput(inputId = ns("scale"),
                        label = "scale", value = TRUE)),
                column(3, 
                    checkboxInput(inputId = ns("center"),
                        label = "center", value = TRUE)),
                column(6, 
                    uiOutput(outputId = ns("highlightUI"))
                )
            )
        )
    )
}



#' @name tSNEUIServer
#' 
#' @title Server for UI for tSNE
#' 
#' @description 
#' The module defines the UI in the tab panel 'tSNE'.
#' 
#' @details
#' `tP_sSNEUIServer` defines the UI for the tab-pane 'tSNE' from the server-side. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` and `reactive` value
#' 
#' @return 
#' `shiny.render.function`
#' 
#' @noRd
tSNEUIServer <- function(id, se) {
    ns <- NS(id)
    moduleServer(
        id,
        function(input, output, session) {
            
            output$perplexityUI <- renderUI({
                sliderInput(inputId = session$ns("perplexity"), 
                    label = "Perplexity", min = 0,
                    max = ceiling((nrow(colData(se())) - 1) / 3),
                    value = ceiling(nrow(colData(se()))^0.5),
                    step = 1)
            })
            
            output$initialDimsUI <- renderUI({
                sliderInput(inputId = session$ns("initialDims"),
                    label = "Number of retained dimensions in initial PCA",
                    min = 1, max = nrow(colData(se())), 
                    value = 10, step = 1)
            })

        }
    )
    
}


#' @name tP_umapUI
#' 
#' @title Tab panel UI for tab panel 'UMAP'
#' 
#' @description 
#' The module defines the UI in the tab panel 'UMAP'.
#' 
#' @details
#' `tP_umapUI` returns the HTML code for the tab-pane 'UMAP'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object
#' 
#' @return 
#' `shiny.tag` with HTML content
#' 
#' @author Thomas Naake
#' 
#' @examples
#' tP_UMAPUI("test")
#' 
#' @noRd
tP_umapUI <- function(id) {
    ns <- NS(id)
    tabPanel(title = "UMAP", 
        fluidRow(
            column(12, 
                plotlyOutput(outputId = ns("plot"), height = "auto") %>% 
                    helper(content = "tabPanel_UMAP"),
                downloadButton(outputId = ns("downloadPlot"), ""))), 
        fluidRow(
            box(title = "Parameters", width = 6, collapsible = TRUE, 
                uiOutput(outputId = ns("coords")),
                column(6, 
                    sliderInput(
                        inputId = ns("minDist"), 
                        label = "Minimum distance", min = 0.01, 
                        max = 10, value = 0.1)),
                column(6, uiOutput(outputId = ns("nNeighborsUI"))),
                column(6, 
                    sliderInput(
                        inputId = ns("spread"), 
                        label = "Spread", min = 0.01, max = 10, value = 1)),
                column(6, 
                    uiOutput(outputId = ns("highlightUI"))
                )
            )
        )
    )
}

#' @name umapUIServer
#' 
#' @title Server for UI for UMAP
#' 
#' @description 
#' The module defines the UI in the tab panel 'UMAP'.
#' 
#' @details
#' `umapUIServer` defines the UI for the tab-pane 'UMAP' from the server-side. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` and `reactive` value
#' 
#' @return 
#' `shiny.render.function`
#' 
#' @noRd
umapUIServer <- function(id, se) {
    moduleServer(
        id,
        function(input, output, session) {
            
            output$nNeighborsUI <- renderUI({
                sliderInput(
                    inputId = session$ns("nNeighbors"), 
                    label = "Number of neighbors", min = 2, 
                    max = nrow(colData(se())), value = 15, step = 1)
            })
        }
    )
}

#' @name dimRedServer
#' 
#' @title Module for server expressions of tab panel 'Dimension reduction'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Dimension reduction'.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param assay `matrix` and `reactive` value, obtained from 
#' `assay(SummarizedExperiment)`
#' @param type `character`
#' @param label `character`
#' @param params `reactiveValues`
#' @param innerWidth `numeric` and `reactive` value, specifying the width of 
#' the window size
#' 
#' @return 
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
#' 
#' @importFrom htmlwidgets saveWidget
dimRedServer <- function(id, se, assay, type = "PCA", label = "PC", params, 
    innerWidth) {
    
    moduleServer(
        id, 
        function(input, output, session) {
            
            ## create data.frame with new coordinates: x, type, params 
            tbl_vals <- reactive({
                ordination(assay(), type, reactiveValuesToList(params()))
            })
            
            ## create an expression that retrieves information on the columns of
            ## a_ordinationPlot (from PCA, PCoA, or NMDS), i.e. the principal
            ## components or axis (remove the first column = namesDf)
            output$coords <- renderUI({
                fluidRow(
                    column(6,
                        selectInput(session$ns("x"), label = "x-axis", 
                            choices = colnames(tbl_vals())[-1],
                            selected = colnames(tbl_vals())[-1][1])),
                    column(6,
                        selectInput(session$ns("y"), label = "y-axis", 
                            choices = colnames(tbl_vals())[-1],
                            selected = colnames(tbl_vals())[-1][2]))
                )
            })
            
            output$highlightUI <- renderUI({
                ns <- session$ns
                selectInput(inputId = ns("highlight"), label = "Highlight", 
                            choices = c("none", colnames(colData(se()))))
            })
            
            ## reactive plot for ordination plots
            output$plot <- renderPlotly({
                req(input$x)
                ordinationPlot(tbl_vals(), se = se(), 
                    highlight = input$highlight, input$x, input$y, height = innerWidth() * 3 / 5)
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function() {
                    paste(type, "_", input$x, "_", input$y, "_", 
                        input$highlight, ".html", sep = "")
                },
                content = function(file) {
                    saveWidget(
                        ordinationPlot(tbl = tbl_vals(), se = se(), 
                            highlight = input$highlight, 
                            x_coord = input$x, y_coord = input$y), file)
                }
            )
        }
    )
}

#' @name screePlotServer
#' 
#' @title Module for server expressions of tab panels 'PCA' and 'tSNE'
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Dimension Reduction', specifically for the tab panels 'PCA' and 'tSNE'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param assay `matrix` and `reactive` value, obtained from 
#' `assay(SummarizedExperiment)`
#' @param center `logical` and `reactive` value
#' @param scale `logical` and `reactive` value
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
screePlotServer <- function(id, assay, center, scale) {
    moduleServer(
        id, 
        function(input, output, session) {
            
            var_x <- reactive({
                explVar(assay(), center = center(), scale = scale())
            })
            
            if (id == "PCA") {
                ## Scree plot for PCA Panel
                output$PCAVarplot <- renderPlot({
                    plotPCAVar(var_x(), NULL)
                })  
            } else { ## tSNE
                var_perm <- reactive({
                    permuteExplVar(assay(), n = 10, center = center(), scale = scale()) 
                })
                
                ## Scree plot for tSNE panel including permuted values
                output$PCAVarplotPerm <- renderPlot({
                    plotPCAVar(var_x(), var_perm())
                })
                
                output$PCAVarPvalueplot <- renderPlot({
                    plotPCAVarPvalue(var_x(), var_perm())
                })
            }
        }
    )
}

#' @name tP_dimensionReduction_all
#' 
#' @title Tab panel UI for tab panel 'Dimension Reduction'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Dimension Reduction'.
#' 
#' @details 
#' `tP_dimensionReduction_all` returns the HTML code for the tab-pane 
#' 'Dimension Reduction'. Internal function for `shinyQC`.
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples 
#' tP_dimensionReduction_all()
#' 
#' @noRd
tP_dimensionReduction_all <- function() {
    tabPanel("Dimension Reduction",
        tabBox(title = "", width = 12,
            tP_PCAUI(id = "PCA"),
            tP_PCoAUI(id = "PCoA"),
            tP_NMDSUI(id = "NMDS"),
            tP_tSNEUI(id = "tSNE"),
            tP_umapUI(id = "UMAP")
        )
    )    
}

