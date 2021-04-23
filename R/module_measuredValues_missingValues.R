#' @name tP_barplotMeMiSampleUI
#' 
#' @title Tab panel UI for tab panel 'Number of Features'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Number of Features'.
#' 
#' @details
#' `tP_histFeatUI` returns the HTML code for the tab-pane 'Number of Features'.
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param title `character`
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' tP_barPlotMeMiSampleUI("test")
#' 
#' @importFrom shiny NS tabPanel downloadButton
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_barplotMeMiSampleUI <- function(id, title = "Number of measured features") {
    ns <- shiny::NS(id)
    
    if (id == "MeV_number") {
        helper_file <- "tabPanel_barNumberFeature_measured"
    }
    if (id == "MiV_number") {
        helper_file <- "tabPanel_barNumberFeature_missing"
    }
    
    shiny::tabPanel(title = title,
        plotly::plotlyOutput(ns("barplotNumber")) %>% 
            shinyhelper::helper(content = helper_file),
        shiny::downloadButton(outputId = ns("downloadPlot"), "")
    )
}

#' @name sampleMeMiServer
#' 
#' @title Module for server expressions of tab panel 'Number of features'
#' 
#' @description 
#' The module defines the server expressions for the tab panel 
#' 'Number of features'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` and `reactive` value
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive
#' 
#' @noRd
sampleMeMiServer <- function(id, se) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            shiny::reactive({
                samples_memi(se())
            })
            
        }
    )
}


#' @name barplotMeMiSampleServer
#' 
#' @title Module for server expressions of tab panel 'Number of features'
#' 
#' @description 
#' The module defines the server expressions for the tab panel 
#' 'Number of features'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param samples_memi `tibble` and `reactive` value
#' @param measured `logical`
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive downloadHandler
#' @importFrom plotly renderPlotly
#' @importFrom htmlwidgets saveWidget
#' 
#' @noRd
barplotMeMiSampleServer <- function(id, samples_memi, measured = TRUE) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            p_barplotNumber <- shiny::reactive({
                barplot_samples_memi(samples_memi(), measured = measured)
            })
            
            output$barplotNumber <- plotly::renderPlotly({
                p_barplotNumber()
            })
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("Number_of_features_measured_", 
                                            measured, ".html", sep = "")
                },
                content = function(file) {
                    htmlwidgets::saveWidget(p_barplotNumber(), file)
                }
            )

        }
    )
}



#' @name tP_histFeatUI
#' 
#' @title Tab panel UI for tab panel 'Histogram Features'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Histogram Features'.
#' 
#' @details
#' `tP_histFeatUI` returns the HTML code for the tab-pane 'Histogram Features'. 
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
#' tP_histFeatUI("test")
#' 
#' @importFrom shiny NS tabPanel downloadButton uiOutput
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_histFeatUI <- function(id) {
    ns <- shiny::NS(id)
    
    if (id == "MeV") {
        helper_file <-  "tabPanel_histFeature_measured"
    } 
    if (id == "MiV") {
        helper_file <- "tabPanel_histFeature_missing"
    }
    
    shiny::tabPanel(title = "Histogram Features",
        plotly::plotlyOutput(ns("histFeature")) %>% 
            shinyhelper::helper(content = helper_file),
        shiny::downloadButton(outputId = ns("downloadPlot_hist"), ""),
        shiny::uiOutput(outputId = ns("binwidthUI"))
    )
    
}


#' @name histFeatServer
#' 
#' @title Module for server expressions of tab panel 'Histogram Features'
#' 
#' @description 
#' The module defines the server expressions for the tab panel 
#' 'Histogram Features'
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param assay `matrix` and `reactive` value, obtained from 
#' `assay(SummarizedExperiment`
#' @param measured `logical`
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI reactive sliderInput
#' @importFrom shiny downloadHandler 
#' @importFrom htmlwidgets saveWidget
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
histFeatServer <- function(id, se, assay, measured = TRUE) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            output$binwidthUI <- shiny::renderUI({
                shiny::sliderInput(session$ns("binwidth"), 
                    "Binwidth: ", min = 1, 
                    max = ncol(se()), value = 1, step = 1)
            })
            
            p_hist_feature <- shiny::reactive({
                hist_feature(assay(), binwidth = input$binwidth, 
                    measured = measured)
            })
            
            output$histFeature <- plotly::renderPlotly({
                p_hist_feature()
            })
            
            output$downloadPlot_hist <- shiny::downloadHandler(
                filename = function() {
                    paste("Histogram_features_measured_", 
                                                measured, ".html", sep = "")
                },
                content = function(file) {
                    htmlwidgets::saveWidget(p_hist_feature(), file)
                }
            )
        }
    )
}
    
    
    
#' @name tP_histFeatCategoryUI
#' 
#' @title Tab panel UI for tab panel 'Histogram Features along variable'
#' 
#' @description 
#' The function defines the UI for the tab panel 
#' 'Histogram Features along variable'.
#' 
#' @details 
#' `tP_histFeatCategoryUI` returns the HTML code for the tab-pane
#' 'Histogram Features along variable'. Internal function for `shinyQC`.
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
#' tP_histFeatCategoryUI("test")
#' 
#' @importFrom shiny NS tabPanel downloadButton uiOutput
#' @importFrom plotly plotlyOutput
#' @importFrom shinyhelper helper
#' 
#' @noRd
tP_histFeatCategoryUI <- function(id) {
    ns <- shiny::NS(id)
    
    if (id == "MeV") {
        helper_file <- "tabPanel_histFeatureSample_measured"
    }
    if (id == "MiV") {
        helper_file <- "tabPanel_histFeatureSample_missing"
    }
    
    shiny::tabPanel(title = "Histogram Features along variable",
        plotly::plotlyOutput(ns("histFeatureCategory")) %>%
            shinyhelper::helper(content = helper_file),
        shiny::downloadButton(outputId = ns("downloadPlot_histFeat"), ""),
        shiny::uiOutput(ns("binwidthCUI")),
        shiny::uiOutput(ns("categoryHistUI"))
    )
}

#' @name histFeatCategoryServer
#' 
#' @title Module for server expressions of tab panel
#' 'Histogram Features along variable'
#' 
#' @description 
#' The function defines the output for the tab panel 
#' 'Histogram Features along variable'
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object and `reactive` values
#' @param measured `logical`
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI selectInput reactive req
#' @importFrom shiny downloadHandler
#' @importFrom htmlwidgets saveWidget
#' @importFrom plotly renderPlotly
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
histFeatCategoryServer <- function(id, se, measured = TRUE) {
    
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            output$categoryHistUI <- shiny::renderUI({
                shiny::selectInput(
                    inputId = session$ns("categoryHist"),
                    label = "Variable for stratification",
                    choices = colnames(SummarizedExperiment::colData(se())),
                    selected = "type")
            })
            
            output$binwidthCUI <- shiny::renderUI({
                sliderInput(session$ns("binwidthC"),
                    label = "Binwidth (# features per sample type): ", 
                    step = 1, min = 1, value = 1,
                    max = max(as.vector(
                        table(SummarizedExperiment::colData(se())[[input$categoryHist]]))))
            })
            
            p_histFeatureCategory <- shiny::reactive({
                hist_feature_category(se(), binwidth = input$binwidthC, 
                    measured = measured, category = input$categoryHist)
            })
            
            output$histFeatureCategory <- plotly::renderPlotly({
                shiny::req(input$binwidthC)
                p_histFeatureCategory()
            })
            
            output$downloadPlot_histFeat <- shiny::downloadHandler(
                filename = function() {
                    paste("Histogram_features_along_variable_measured_", 
                        measured, ".html", sep = "")
                },
                content = function(file) {
                    htmlwidgets::saveWidget(p_histFeatureCategory(), file)
                }
            )
            
        }
    )
}

#' @name tP_upSetUI
#' 
#' @title Tab panel UI for tab panel ''UpSet'
#' 
#' @description 
#' The module defines the UI for the tab panel 'UpSet'.
#' 
#' @details
#' `tP_upSetUI` returns the HTML code for the tab-pane 'UpSet'. 
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
#' tP_upSetUI("test")
#' 
#' @importFrom shiny tabPanel plotOutput downloadButton uiOutput
#' @importFrom shinyhelper helper
#' 
#' @noRd
#' 
tP_upSetUI <- function(id) {
    ns <- shiny::NS(id)
    
    if (id == "MeV") {
        helper_file <- "tabPanel_upSet_measured"
    } 
    if (id == "MiV") {
        helper_file <- "tabPanel_upSet_missing"
    }
    
    tabPanel(title = "UpSet", 
        shiny::plotOutput(ns("upsetSample")) %>% 
            shinyhelper::helper(content = helper_file),
        shiny::downloadButton(outputId = ns("downloadPlot"), ""),
        shiny::uiOutput(ns("categoryUpSetUI"))
    )
}

#' @name upSetServer
#' 
#' @title Module for server expressions of tab panel 'UpSet'
#' 
#' @description 
#' The function defines the server expressions for the tab panel 'UpSet'.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object and `reactive` values
#' @param measured `logical`
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI selectInput req reactive
#' @importFrom shiny downloadHandler renderPlot
#' @importFrom ggplot2 ggsave
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
upSetServer <- function(id, se, measured = TRUE) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            output$categoryUpSetUI <- shiny::renderUI({
                shiny::selectInput(
                    inputId = session$ns("categoryUpSet"), 
                    label = "Variable for stratification", 
                    choices = colnames(SummarizedExperiment::colData(se())),
                    selected = "type")
            })
            
            p_upset <- shiny::reactive({
                upset_category(se(), category = input$categoryUpSet, 
                    measured = measured)
            })
            
            output$upsetSample <- shiny::renderPlot({
                shiny::req(input$categoryUpSet)
                p_upset()
            })
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("Upset_measured_", measured, ".pdf", sep = "")
                },
                content = function(file) {
                    ggplot2::ggsave(file, p_upset(), device = "pdf")
                }
            )
            
            
        }
    )
}

#' @name tP_setsUI
#' 
#' @title Tab panel UI for tab panel 'Sets'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Sets'.
#' 
#' @details 
#' `tP_setsUI` returns the HTML code for the tab-pane 'Sets'. 
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
#' tP_setUI("test")
#' 
#' @importFrom shiny NS uiOutput textOutput tabPanel
#' @importFrom shinyhelper helper
#' @noRd 
tP_setsUI <- function(id) {
    ns <- shiny::NS(id)
    
    if (id == "MeV") {
        helper_file <- "tabPanel_sets_measured"
    } 
    if (id == "MiV") {
        helper_file <- "tabPanel_sets_missing"
    }
    
    shiny::tabPanel(title = "Sets", 
        shiny::uiOutput(ns("checkboxCategoryUI")) %>% 
            shinyhelper::helper(content = helper_file),
        shiny::textOutput(ns("combinationText"))
    )
}

#' @name setsServer
#' 
#' @title Module for server expressions of tab panel 'Sets'
#' 
#' @description 
#' The module defines the server expressions for the tab panel 'Sets'.
#' 
#' @details
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object and `reactive` values
#' @param measured `logical`
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI checkboxGroupInput renderText
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
setsServer <- function(id, se, measured = TRUE) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            output$checkboxCategoryUI <- shiny::renderUI({
                shiny::checkboxGroupInput(session$ns("checkboxCategory"), 
                    label = "Select sets", 
                    choices = unique(SummarizedExperiment::colData(se())[[input$categoryUpSet]])) 
            })
            
            output$combinationText <- shiny::renderText({
                extractComb(se(), combination = input$checkboxCategory, 
                    category = input$categoryUpSet, measured = measured)
            })
        }
    )
}


#' @name tP_meV_all
#' 
#' @title Tab panel UI for tab panel 'Measured Values'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Measured Values'.
#' 
#' @details 
#' `tP_meV_all` returns the HTML code for the tab-pane 'Measured Values'. 
#' Internal function for `shinyQC`.
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' tP_meV_all()
#' 
#' @importFrom shiny tabPanel 
#' @importFrom shinydashboard tabBox
#' 
#' @noRd 
tP_meV_all <- function() {
    shiny::tabPanel("Measured Values",
        shinydashboard::tabBox(title = "", width = 12,
            tP_barplotMeMiSampleUI(id = "MeV_number", 
                title = "Number of features"),
            tP_histFeatUI(id = "MeV"),
            tP_histFeatCategoryUI(id = "MeV"),
            tP_upSetUI(id = "MeV"),
            tP_setsUI(id = "MeV")
        )
    )    
}

#' @name tP_miV_all
#' 
#' @title Tab panel UI for tab panel 'Missing Values'
#' 
#' @description 
#' The module defines the UI for the tab panel 'Missing Values'.
#' 
#' @details 
#' `tP_miV_all` returns the HTML code for the tab-pane 'Missing Values'. 
#' Internal function for `shinyQC`.
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' tP_miV_all()
#' 
#' @importFrom shiny tabPanel 
#' @importFrom shinydashboard tabBox
#' 
#' @noRd
tP_miV_all <- function() {
        shiny::tabPanel("Missing Values",
            shinydashboard::tabBox(title = "", width = 12,
                tP_barplotMeMiSampleUI(id = "MiV_number",
                    title = "Number of features"),
                tP_histFeatUI(id = "MiV"),
                tP_histFeatCategoryUI(id = "MiV"),
                tP_upSetUI(id = "MiV"),
                tP_setsUI(id = "MiV")
            )
        )
}