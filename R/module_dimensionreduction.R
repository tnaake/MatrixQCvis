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
#' @author Thomas Naake
#' 
#' @importFrom shiny fluidRow column selectInput
#' 
#' @noRd
coordsUI <- function(df, name = "PC", 
        x = "ordination_x", y = "ordination_y", session = session) {
    
    shiny::fluidRow(
        shiny::column(12, name),
        shiny::column(6, 
            shiny::selectInput(session$ns(x), label = "x-axis",
                    choices = colnames(df)[-1],
                    selected = colnames(df)[-1][1])),
        shiny::column(6, 
            shiny::selectInput(session$ns(y), label = "y-axis",
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
#' @importFrom shiny NS tabPanel fluidRow column downloadButton uiOutput
#' @importFrom shiny checkboxInput plotOutput
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_PCAUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "PCA", 
        shiny::fluidRow(
            shiny::column(12, 
                plotly::plotlyOutput(outputId = ns("plot"), height = "auto") |>
                    shinyhelper::helper(content = "tabPanel_PCA"),
                shiny::downloadButton(outputId = ns("downloadPlot"), ""))
        ),
        shiny::fluidRow(
            shinydashboard::box(title = "Parameters", width = 6, 
                collapsible = TRUE, 
                shiny::column(12, 
                    shiny::uiOutput(outputId = ns("coords"))),
                shiny::column(3, 
                    shiny::checkboxInput(inputId = ns("scale"),
                        label = "scale", value = TRUE)),
                shiny::column(3, 
                    shiny::checkboxInput(inputId = ns("center"),
                        label = "center", value = TRUE)),
                shiny::column(6, 
                    shiny::uiOutput(outputId = ns("highlightUI")))
            ),
            shinydashboard::box(title = "Scree plot", width = 6, 
                collapsible = TRUE, collapsed = TRUE,
                shiny::plotOutput(outputId = ns("PCAVarplot"))
            ),
            shinydashboard::box(title = "Loadings plot",
                collapsible = TRUE, collapsed = TRUE,
                plotly::plotlyOutput(outputId = ns("PCALoadings"))
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
#' @importFrom shiny NS tabPanel fluidRow column uiOutput selectInput
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' 
#' @noRd
tP_PCoAUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "PCoA", 
        shiny::fluidRow(
            shiny::column(12, 
                plotly::plotlyOutput(outputId = ns("plot"), 
                                                        height = "auto") |>
                    shinyhelper::helper(content = "tabPanel_PCoA"),
                shiny::downloadButton(outputId = ns("downloadPlot"), ""))
            ), 
        shiny::fluidRow(
            shinydashboard::box(title = "Parameters", width = 6, 
                collapsible = TRUE, 
                shiny::column(12, 
                    shiny::uiOutput(ns("coords"))
                ),
                shiny::column(6, 
                    shiny::selectInput(inputId = ns("dist"),
                        label = "Distance measure",
                        choices = c("euclidean", "maximum", 
                            "manhattan", "canberra", "minkowski"), 
                        selected = "euclidean")
                ),
                shiny::column(6, 
                    shiny::uiOutput(outputId = ns("highlightUI"))
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
#' @importFrom shiny NS tabPanel fluidRow column downloadButton 
#' @importFrom shiny uiOutput selectInput
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_NMDSUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "NMDS", 
        shiny::fluidRow(
            shiny::column(12, 
                plotly::plotlyOutput(outputId = ns("plot"), height = "auto") |>
                    shinyhelper::helper(content = "tabPanel_NMDS"),
                shiny::downloadButton(outputId = ns("downloadPlot"), ""))), 
        shiny::fluidRow(
            shinydashboard::box(title = "Parameters", width = 6, 
                collapsible = TRUE, 
                shiny::column(12, 
                    shiny::uiOutput(outputId = ns("coords"))
                ),
                shiny::column(6, 
                    shiny::selectInput(inputId = ns("dist"),
                        label = "Distance measure",
                        choices = c("euclidean", "maximum", 
                            "manhattan", "canberra", "minkowski"), 
                        selected = "euclidean")
                ),
                shiny::column(6, 
                    shiny::uiOutput(outputId = ns("highlightUI"))
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
#' @importFrom shiny NS tabPanel fluidRow column downloadButton 
#' @importFrom shiny uiOutput selectInput plotOutput sliderInput checkboxInput
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_tSNEUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "tSNE", 
        shiny::fluidRow(
            shiny::column(12, 
                plotly::plotlyOutput(outputId = ns("plot"), height = "auto") |>
                    shinyhelper::helper(content = "tabPanel_tSNE"),
                shiny::downloadButton(outputId = ns("downloadPlot"), ""))),
        shiny::fluidRow(
            shinydashboard::box(
                title = "Principal components", status = "primary", width = 12,
                collapsible = TRUE, collapsed = TRUE,
                shiny::fluidRow(
                    shiny::column(6, 
                        shiny::plotOutput(outputId = ns("PCAVarplotPerm"))),
                    shiny::column(6, 
                        shiny::plotOutput(outputId = ns("PCAVarPvalueplot")))
                )
            )
        ),
        shiny::fluidRow(
            shinydashboard::box(title = "Parameters", width = 6, 
                collapsible = TRUE, 
                shiny::uiOutput(outputId = ns("coords")),
                shiny::column(6, 
                    shiny::uiOutput(outputId = ns("perplexityUI"))),
                shiny::column(6, 
                    shiny::sliderInput(inputId = ns("maxIter"),
                        label = "Number of iterations", 
                        min = 100, max = 10000, value = 1000)),
                shiny::column(6, 
                    shiny::uiOutput(outputId = ns("initialDimsUI"))),
                shiny::column(6, 
                    shiny::sliderInput(inputId = ns("dims"),
                        label = "Output dimensionality",
                        min = 2, max = 3, value = 3, step = 1)),
                shiny::column(3, 
                    shiny::checkboxInput(inputId = ns("scale"),
                        label = "scale", value = TRUE)),
                shiny::column(3, 
                    shiny::checkboxInput(inputId = ns("center"),
                        label = "center", value = TRUE)),
                shiny::column(6, 
                    shiny::uiOutput(outputId = ns("highlightUI"))
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
#' `tP_sSNEUIServer` defines the UI for the tab-pane 'tSNE' from the 
#' server-side. Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` and `reactive` value
#' 
#' @author Thomas Naake
#' 
#' @return 
#' `shiny.render.function`
#' 
#' @importFrom shiny NS moduleServer renderUI sliderInput
#' @importFrom plotly plotlyOutput
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
tSNEUIServer <- function(id, se) {
    ns <- shiny::NS(id)
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            cD_n <- reactive(nrow(SummarizedExperiment::colData(se())))
            
            output$perplexityUI <- shiny::renderUI({
                req(cD_n())
                shiny::sliderInput(inputId = session$ns("perplexity"), 
                    label = "Perplexity", min = 1,
                    max = ceiling((cD_n() - 1) / 3),
                    value = ceiling(cD_n()^0.5),
                    step = 1)
            })
            
            output$initialDimsUI <- shiny::renderUI({
                req(cD_n())
                shiny::sliderInput(inputId = session$ns("initialDims"),
                    label = "Number of retained dimensions in initial PCA",
                    min = 1, max = cD_n(), value = 3, step = 1)
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
#' @importFrom shiny NS tabPanel fluidRow column downloadButton
#' @importFrom shiny uiOutput selectInput
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' 
#' @examples
#' tP_UMAPUI("test")
#' 
#' @noRd
tP_umapUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "UMAP", 
        shiny::fluidRow(
            shiny::column(12, 
                plotly::plotlyOutput(outputId = ns("plot"), 
                    height = "auto") |>
                    shinyhelper::helper(content = "tabPanel_UMAP"),
                shiny::downloadButton(outputId = ns("downloadPlot"), ""))), 
        shiny::fluidRow(
            shinydashboard::box(title = "Parameters", width = 6, 
                collapsible = TRUE, 
                shiny::uiOutput(outputId = ns("coords")),
                shiny::column(6, 
                    shiny::sliderInput(
                        inputId = ns("minDist"), 
                        label = "Minimum distance", min = 0.01, 
                        max = 10, value = 0.1)),
                shiny::column(6, 
                    shiny::uiOutput(outputId = ns("nNeighborsUI"))),
                shiny::column(6, 
                    shiny::sliderInput(
                        inputId = ns("spread"), 
                        label = "Spread", min = 0.01, max = 10, value = 1)),
                shiny::column(6, 
                    shiny::uiOutput(outputId = ns("highlightUI"))
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
#' @author Thomas Naake
#' 
#' @return 
#' `shiny.render.function`
#' 
#' @importFrom shiny moduleServer renderUI sliderInput
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
umapUIServer <- function(id, se) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            output$nNeighborsUI <- shiny::renderUI({
                shiny::sliderInput(
                    inputId = session$ns("nNeighbors"), 
                    label = "Number of neighbors", min = 2, 
                    max = nrow(SummarizedExperiment::colData(se())), 
                        value = 15, step = 1)
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
#' @importFrom shiny moduleServer reactive renderUI fluidRow column selectInput
#' @importFrom shiny reactiveValuesToList downloadHandler req
#' @importFrom plotly renderPlotly
#' @importFrom htmlwidgets saveWidget
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
#' 
dimRedServer <- function(id, se, assay, type = "PCA", label = "PC", params, 
    innerWidth) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            ## create data.frame with new coordinates: x, type, params 
            tbl_vals <- shiny::reactive({
                ordination(x = assay(), type = type, 
                    params = reactiveValuesToList(params()))
            })
            
            ## create an expression that retrieves information on the columns of
            ## a_ordinationPlot (from PCA, PCoA, or NMDS), i.e. the principal
            ## components or axis (remove the first column = namesDf)
            output$coords <- shiny::renderUI({
                cn_tbl <- colnames(tbl_vals())[-1]
                shiny::fluidRow(
                    shiny::column(6,
                        shiny::selectInput(session$ns("x"), label = "x-axis", 
                            choices = cn_tbl, selected = cn_tbl[1])),
                    shiny::column(6,
                        shiny::selectInput(session$ns("y"), label = "y-axis", 
                            choices = cn_tbl, selected = cn_tbl[2]))
                )
            })
            
            output$highlightUI <- shiny::renderUI({
                ns <- session$ns
                shiny::selectInput(
                    inputId = ns("highlight"), label = "Highlight", 
                    choices = c("none", 
                        colnames(SummarizedExperiment::colData(se()))))
            })
            
            ## reactive plot for ordination plots
            output$plot <- plotly::renderPlotly({
                shiny::req(input$x)
                
                if (id %in% c("PCA", "PCoA")) {
                    
                    params_l <- shiny::reactiveValuesToList(params())
                    explainedVar <- explVar(assay(), 
                        params = params_l, type = id)
                    ordinationPlot(tbl_vals(), se = se(), 
                        highlight = input$highlight, 
                        x_coord = input$x, y_coord = input$y,
                        explainedVar = explainedVar, 
                        height = innerWidth() * 3 / 5)

                } else {

                    ordinationPlot(tbl_vals(), se = se(), 
                        highlight = input$highlight, 
                        x_coord = input$x, y_coord = input$y, 
                        explainedVar = NULL, 
                        height = innerWidth() * 3 / 5)

                }
            })

            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste(type, "_", input$x, "_", input$y, "_", 
                        input$highlight, ".html", sep = "")
                },
                content = function(file) {
                    htmlwidgets::saveWidget(
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
#' (scree plot)
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
#' @importFrom shiny moduleServer reactive renderPlot
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#'
#' @author Thomas Naake
#' 
#' @noRd
screePlotServer <- function(id, assay, center, scale) {
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            var_x <- shiny::reactive({
                explVar(assay(), 
                    params = list(center = center(), scale = scale()), 
                    type = "PCA")
            })
            
            if (id == "PCA") {
                ## Scree plot for PCA Panel
                output$PCAVarplot <- shiny::renderPlot({
                    plotPCAVar(var_x(), NULL)
                })  
            } else { ## tSNE
                var_perm <- shiny::reactive({
                    permuteExplVar(assay(), n = 10, center = center(), 
                            scale = scale()) 
                })
                
                ## Scree plot for tSNE panel including permuted values
                output$PCAVarplotPerm <- shiny::renderPlot({
                    plotPCAVar(var_x(), var_perm())
                })
                
                output$PCAVarPvalueplot <- shiny::renderPlot({
                    plotPCAVarPvalue(var_x(), var_perm())
                })
            }
            
        }
    )
}

#' @name loadingsPlotServer
#' 
#' @title Module for server expressions of tab panel 'PCA' (loading plot)
#' 
#' @description
#' The module defines the server expressions in the tab panel 
#' 'Dimension Reduction', specifically for the tab panel 'PCA'. It defines 
#' the loading plot server expression
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
#' @importFrom shiny moduleServer reactive reactiveValuesToList
#' @importFrom plotly renderPlotly
#' 
#' @noRd
loadingsPlotServer <- function(id, assay, params) {
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            tbl_loadings <- shiny::reactive({
                tblPCALoadings(assay(), shiny::reactiveValuesToList(params()))
            })

            output$PCALoadings <- plotly::renderPlotly({
                plotPCALoadings(tbl_loadings(), 
                    x_coord = input$x, y_coord = input$y) 
            })

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
#' @importFrom shiny tabPanel 
#' @importFrom shinydashboard tabBox
#' 
#' @noRd
tP_dimensionReduction_all <- function() {
    shiny::tabPanel("Dimension Reduction",
        shinydashboard::tabBox(title = "", width = 12,
            tP_PCAUI(id = "PCA"),
            tP_PCoAUI(id = "PCoA"),
            tP_NMDSUI(id = "NMDS"),
            tP_tSNEUI(id = "tSNE"),
            tP_umapUI(id = "UMAP")
        )
    )    
}

