#' @name tP_PCAUI
#' 
#' @title Tab panel UI for tab panel 'PCA'
#' 
#' @description
#' The module defines the UI in the tab panel 'PCA'.
#' 
#' @details 
#' \code{tP_PCAUI} returns the HTML code for the tab-pane 'PCA'. 
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
#' tP_PCAUI("test")
#' 
#' @importFrom shiny NS tabPanel fluidRow column downloadButton
#' @importFrom shiny checkboxInput plotOutput
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_PCAUI <- function(id) {
    ns <- shiny::NS(id)
    tabPanel(title = "PCA", value = "PCA",
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
                    shiny::fluidRow(
                        shiny::column(6,
                            shiny::selectInput(inputId = ns("x"), 
                                label = "x-axis", choices = "PC1", 
                                selected = "PC1")),
                        shiny::column(6,
                            shiny::selectInput(inputId = ns("y"), 
                                label = "y-axis", choices = "PC2", 
                                selected = "PC2"))
                    )), 
                shiny::column(3, 
                    shiny::checkboxInput(inputId = ns("scale"),
                        label = "scale", value = TRUE)),
                shiny::column(3, 
                    shiny::checkboxInput(inputId = ns("center"),
                        label = "center", value = TRUE)),
                shiny::column(6, 
                    shiny::selectInput(inputId = ns("highlight"), 
                        label = "Highlight", choices = "none"))
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
#' \code{tP_PCoAUI} returns the HTML code for the tab-pane 'PCoA'. 
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
                    shiny::fluidRow(
                        shiny::column(6,
                            shiny::selectInput(inputId = ns("x"), 
                                label = "x-axis", choices = "Axis.1", 
                                selected = "Axis.1")),
                        shiny::column(6,
                            shiny::selectInput(inputId = ns("y"), 
                                label = "y-axis", choices = "Axis.2", 
                                selected = "Axis.2"))
                    ))
            ),
                shiny::column(6, 
                    shiny::selectInput(inputId = ns("dist"),
                        label = "Distance measure",
                        choices = c("euclidean", "maximum", 
                            "manhattan", "canberra", "minkowski"), 
                        selected = "euclidean")
                ),
                shiny::column(6, 
                    shiny::selectInput(inputId = ns("highlight"), 
                         label = "Highlight", choices = "none")
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
#' \code{tP_NMDSUI} returns the HTML code for the tab-pane 'NMDS'. 
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
                    shiny::fluidRow(
                        shiny::column(6,
                            shiny::selectInput(inputId = ns("x"), 
                                label = "x-axis", choices = "MDS1", 
                                selected = "MDS1")),
                        shiny::column(6,
                            shiny::selectInput(inputId = ns("y"), 
                                label = "y-axis", choices = "MDS2", 
                                selected = "MDS2"))
                    )),
                shiny::column(6, 
                    shiny::selectInput(inputId = ns("dist"),
                        label = "Distance measure",
                        choices = c("euclidean", "maximum", 
                            "manhattan", "canberra", "minkowski"), 
                        selected = "euclidean")
                ),
                shiny::column(6, 
                    shiny::selectInput(inputId = ns("highlight"), 
                        label = "Highlight", choices = "none")
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
#' \code{tP_tSNEUI} returns the HTML code for the tab-pane 'tSNE'. 
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
                shiny::fluidRow(
                    shiny::column(6,
                        shiny::selectInput(inputId = ns("x"), label = "x-axis", 
                            choices = "X1", selected = "X1")),
                    shiny::column(6,
                        shiny::selectInput(inputId = ns("y"), label = "y-axis", 
                            choices = "X2", selected = "X2"))
                ),
                shiny::column(6, 
                    shiny::sliderInput(inputId = ns("perplexity"), 
                        label = "Perplexity", min = 1, max = 5, 2, step = 1)),
                shiny::column(6, 
                    shiny::sliderInput(inputId = ns("maxIter"),
                        label = "Number of iterations", 
                        min = 100, max = 10000, value = 1000)),
                shiny::column(6, 
                    shiny::sliderInput(inputId = ns("initialDims"),
                        label = "Number of retained dimensions in initial PCA",
                        min = 1, max = 5, value = 3, step = 1)),
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
                    shiny::selectInput(inputId = ns("highlight"), 
                        label = "Highlight", choices = "none")
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
#' \code{tP_sSNEUIServer} defines the UI for the tab-pane 'tSNE' from the 
#' server-side. Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param sample_n \code{numeric(1)} and \code{reactive} value, number of 
#' samples
#' 
#' @author Thomas Naake
#' 
#' @return 
#' \code{shiny.render.function}
#' 
#' @importFrom shiny NS moduleServer updateSliderInput
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tSNEUIServer <- function(id, sample_n) {
    ns <- shiny::NS(id)
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            shiny::observe({
                shiny::updateSliderInput(session = session, 
                    inputId = "perplexity", 
                    max = ceiling((sample_n() - 1) / 3),
                    value = ceiling(sample_n()^0.5))
            })
            
            shiny::observe({
                shiny::updateSliderInput(session = session, 
                    inputId = "initialDims", max = sample_n())
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
#' \code{tP_umapUI} returns the HTML code for the tab-pane 'UMAP'. 
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param se \code{SummarizedExperiment} object
#' 
#' @return 
#' \code{shiny.tag} with HTML content
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
                shiny::fluidRow(
                    shiny::column(6,
                        shiny::selectInput(inputId = ns("x"), label = "x-axis", 
                            choices = "X1", selected = "X1")),
                    shiny::column(6,
                        shiny::selectInput(inputId = ns("y"), label = "y-axis", 
                            choices = "X2", selected = "X2"))
                ),
                shiny::column(6, 
                    shiny::sliderInput(
                        inputId = ns("minDist"), 
                        label = "Minimum distance", min = 0.01, 
                        max = 10, value = 0.1)),
                shiny::column(6, 
                    shiny::sliderInput(inputId = ns("nNeighbors"), 
                        label = "Number of neighbors", min = 2, 
                        max = 10, value = 10, step = 1)),
                shiny::column(6, 
                    shiny::sliderInput(
                        inputId = ns("spread"), 
                        label = "Spread", min = 0.01, max = 10, value = 1)),
                shiny::column(6, 
                    shiny::selectInput(inputId = ns("highlight"), 
                        label = "Highlight", choices = "none")
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
#' \code{umapUIServer} defines the UI for the tab-pane 'UMAP' from the 
#' server-side. Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param sample_n \code{numeric(1)} and \code{reactive} value, number of 
#' samples
#'
#' @author Thomas Naake
#' 
#' @return 
#' \code{shiny.render.function}
#' 
#' @importFrom shiny moduleServer updateSliderInput
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
umapUIServer <- function(id, sample_n) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            shiny::observe({
                shiny::updateSliderInput(session = session,
                    inputId = "nNeighbors", max = sample_n)
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
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param assay \code{matrix} and \code{reactive} value, obtained from 
#' \code{assay(SummarizedExperiment)}
#' @param type \code{character}
#' @param label \code{character}
#' @param params \code{reactiveValues}
#' @param innerWidth \code{numeric} and \code{reactive} value, specifying the 
#' width of the window size
#' @param selectedTab \code{character} and \code{reactive} value, specifying
#' the selected tab panel, e.g. \code{"PCA"}
#' 
#' @return 
#' \code{shiny.render.function} expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive fluidRow column updateSelectInput
#' @importFrom shiny reactiveValuesToList downloadHandler req bindCache
#' @importFrom plotly renderPlotly
#' @importFrom htmlwidgets saveWidget
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
#' 
dimRedServer <- function(id, se, assay, type = "PCA", label = "PC", params, 
    innerWidth, selectedTab) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            ## create reactiveValues to store results from dimensionReduction,
            ## only update/calculate when the user has selected the tab
            ## (selectedTab() == id) and if the values were not previously 
            ## calcuated (rv$run), update also if there are changes to the 
            ## parameters
            rv <- reactiveValues(
                run = FALSE, dimensionReduction_list = NULL, coordinates = NULL,
                explainedVar = NULL, innerWidth = "600px")
            
            observeEvent({reactiveValuesToList(params()); se(); assay()}, {
                rv$run <- FALSE
            })
            
            observeEvent({selectedTab(); rv$run}, {
                if (selectedTab() == id & !rv$run) {
                    rv$dimensionReduction_list <- dimensionReduction(
                        x = assay(), type = type, 
                        params = reactiveValuesToList(params()))
                    rv$coordinates <- rv$dimensionReduction_list[[1]]
                    
                    if (id %in% c("PCA", "PCoA")) {
                        rv$explainedVar <- explVar(
                            d = rv$dimensionReduction_list[[2]], type = id)
                    }
                    rv$run <- TRUE
                }
            })
            
            observeEvent(innerWidth(), {
                rv$innerWidth <- innerWidth() * 3 / 5
            })
            
            ## create an expression that retrieves information on the columns of
            ## dimensionReduction tbl (from PCA, PCoA, or NMDS), i.e. the 
            ## principal components or axis (remove the last column = namesDf)
            shiny::observe({
                if (!is.null(rv$coordinates)) {
                    cn <- colnames(rv$coordinates)
                    cn <- cn[-length(cn)]
                    
                    shiny::updateSelectInput(session = session, inputId = "x", 
                        choices = cn, selected = cn[1])
                    shiny::updateSelectInput(session = session, inputId = "y", 
                        choices = cn, selected = cn[2])
                }
            })
            
            shiny::observe({
                shiny::updateSelectInput(session = session, 
                    inputId = "highlight", 
                    choices = c("none", colnames(se()@colData)))
            })
            
            ## reactive plot for dimension reduction plots
            output$plot <- plotly::renderPlotly({
                
                if (!is.null(rv$coordinates)) {
                    dimensionReductionPlot(rv$coordinates, se = se(), 
                            highlight = input$highlight, 
                            x_coord = input$x, y_coord = input$y,
                            explainedVar = rv$explainedVar,
                            height = rv$innerWidth)
                }
            })

            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste(type, "_", input$x, "_", input$y, "_", 
                        input$highlight, ".html", sep = "")
                },
                content = function(file) {
                    htmlwidgets::saveWidget(
                        dimensionReductionPlot(tbl = rv$coordinates, se = se(), 
                            highlight = input$highlight, 
                            x_coord = input$x, y_coord = input$y,
                            explainedVar = rv$explainedVar,
                            height = rv$innerWidth), file)
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
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param assay \code{matrix} and \code{reactive} value, obtained from 
#' \code{assay(SummarizedExperiment)}
#' @param center \code{logical} and \code{reactive} value
#' @param scale \code{logical} and \code{reactive} value
#' 
#' @return
#' \code{shiny.render.function} expression
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
                pca <- dimensionReduction(assay(), 
                    params = list(center = center(), scale = scale()), 
                    type = "PCA")
                explVar(d = pca[[2]], type = "PCA")
            })
            
            if (id == "PCA") {
                ## Scree plot for PCA Panel
                output$PCAVarplot <- shiny::renderPlot({
                    plotPCAVar(var_x(), NULL)
                })  
            } else { ## tSNE
                var_perm <- shiny::reactive({
                    permuteExplVar(assay(), n = 10, center = center(), 
                        scale = scale(), sample_n = 5000) 
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
#' Internal function for \code{shinyQC}.
#' 
#' @param id \code{character}
#' @param assay \code{matrix} and \code{reactive} value, obtained from 
#' \code{assay(SummarizedExperiment)}
#' @param center \code{logical} and \code{reactive} value
#' @param scale \code{logical} and \code{reactive} value
#' 
#' @return
#' \code{shiny.render.function} expression
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
#' \code{tP_dimensionReduction_all} returns the HTML code for the tab-pane 
#' 'Dimension Reduction'. Internal function for \code{shinyQC}.
#' 
#' @return 
#' \code{shiny.tag} with HTML content
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
            id = "dimensionReductionTab",
            tP_PCAUI(id = "PCA"),
            tP_PCoAUI(id = "PCoA"),
            tP_NMDSUI(id = "NMDS"),
            tP_tSNEUI(id = "tSNE"),
            tP_umapUI(id = "UMAP")
        )
    )    
}

