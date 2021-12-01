################################################################################
################################### Values #####################################
################################################################################

#' @name fR_boxplotUI
#'
#' @title Fluid row UI for tab panel 'Boxplot/Violin plot'
#'
#' @description
#' The module defines the UI for the tab panel 'Boxplot/Violin plot'.
#'
#' @details 
#' `fR_boxplotUI` returns the HTML code for the tab-pane 'Boxplot/Violin plot'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param name `character`
#' @param collapsed `logical`
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' fR_boxplotUI("test", "name")
#' 
#' @importFrom shiny NS fluidRow downloadButton plotOutput
#' @importFrom shinydashboard box
#' 
#' @noRd
fR_boxplotUI <- function(id, name, collapsed) {
    ns <- shiny::NS(id)
    shiny::fluidRow(
        shinydashboard::box(title = name, width = 12, collapsible = TRUE, 
            collapsed = collapsed,
            shiny::plotOutput(outputId = ns("boxplot")),
            shiny::downloadButton(outputId = ns("downloadPlot"), ""))
    )
}

#' @name tP_boxplotUI
#' 
#' @title Tab panel UI for tab panel 'Boxplot/Violin plot'
#' 
#' @description 
#' The function defines the UI for the tab panel 'Boxplot/Violin plot'. It 
#' serves as a wrapper for the function `fR_boxplotUI`.
#' 
#' @details 
#' `tP_boxplotUI` returns the HTML code for the tab-pane 'Boxplot/Violin plot'.
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
#' tP_boxplotUI("test")
#' 
#' @importFrom shiny NS tabPanel fluidRow column conditionalPanel
#' @importFrom shiny uiOutput selectInput radioButtons HTML
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_boxplotUI <- function(id) {
    
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Boxplot/Violin plot",
        shiny::fluidRow(
            shiny::column(6, 
                shiny::radioButtons(inputId = "boxLog",
                    label = shiny::HTML("Display log2 values? <br>
                        (only for 'raw', 'normalized' and 'batch corrected')"),
                    choices = list("no log2", "log2"),
                    selected = "no log2")),
            shiny::column(3, 
                shiny::radioButtons(inputId = "violinPlot",
                    label = "Type of display", 
                    choices = list("boxplot", "violin"), 
                     selected = "boxplot")),
            shiny::column(3, shiny::uiOutput(ns("orderCategoryUI")))
        ),
        fR_boxplotUI("boxRaw", "raw", collapsed = FALSE),
        fR_boxplotUI("boxNorm", "normalized", collapsed = TRUE),
        fR_boxplotUI("boxBatch", "batch corrected", collapsed = TRUE),
        fR_boxplotUI("boxTransf", "transformed", collapsed = TRUE),
        shiny::conditionalPanel("output.missingVals == 'TRUE'", 
            fR_boxplotUI("boxImp", "imputed", collapsed = TRUE))
    )
}

#' @name boxPlotServer
#' 
#' @title Module for server expressions for the UI of tab panel 
#' 'Boxplot/Violin plot' 
#' 
#' @description
#' The module defines the server expressions for parts of the UI for the tab 
#' panel 'Boxplot/Violin plot'. It will load different helper files depending
#' on `missingValue`
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param missingValue `logical` (will load different helper files)
#' 
#' @return
#' `shiny.render.function` expression
#' 
#' @importFrom shiny moduleServer renderUI
#' @importFrom shinyhelper helper
#' @importFrom SummarizedExperiment SummarizedExperiment
#'
#' @author Thomas Naake
#' 
#' @noRd
boxPlotUIServer <- function(id, missingValue, se) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {

            output$orderCategoryUI <- shiny::renderUI({
                
                helperFile <- paste("tabPanel_boxplot_missingValue_", 
                                    missingValue, sep = "")

                shiny::selectInput(inputId = session$ns("orderCategory"), 
                    label = "Select variable to order samples",
                    choices = colnames(SummarizedExperiment::colData(se))) |> 
                        shinyhelper::helper(content = helperFile)
            })

        }
    )
}

#' @name boxPlotServer
#' 
#' @title Module for server expressions of tab panel 'Boxplot/Violin plot' 
#' 
#' @description
#' The module defines the server expressions for the tab panel 
#' 'Boxplot/Violin plot'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment`
#' @param boxLog `reactive` expression and `logical`
#' @param violin `reactive` expression and `character`
#' @param type `character`
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive renderPlot downloadHandler req
#' @importFrom ggplot2 ggsave
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
boxPlotServer <- function(id, se, orderCategory, boxLog, violin, type) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            logValues <- shiny::reactive({
                if (boxLog() == "log2") {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            })
            
            vP <- shiny::reactive({
                shiny::req(violin())
                if (violin() == "violin") {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            })
            
            ## create the actual plot
            p_boxplot <- shiny::reactive({
                create_boxplot(se = se(), orderCategory = orderCategory(), 
                    title = "", log2 = logValues(), 
                    violin = vP())
            })
            output$boxplot <- shiny::renderPlot({
                p_boxplot()
            })
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("Boxplot_violinplot_", type, ".pdf", sep = "")
                },
                content = function(file) {
                    ggplot2::ggsave(file, p_boxplot(), device = "pdf")
                }
            )
            
            
        }
    )
}

################################################################################
################################# drift/trend ##################################
################################################################################
#' @name tP_driftUI
#'
#' @title Tab panel UI for tab panel 'Trend/drift'
#'
#' @description
#' The module defines the UI for the tab panel 'Trend/drift'.
#'
#' @details 
#' `tP_driftUI` returns the HTML code for the tab-pane 
#' 'Trend/drift'. Internal function for `shinyQC`.
#' 
#' @param id `character`
#'
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#'
#' @examples
#' tP_driftUI("test", se)
#' 
#' @importFrom shiny NS tabPanel fluidRow downloadButton
#' @importFrom shiny uiOutput selectInput
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_driftUI <- function(id) {

    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Trend/drift",
        shiny::fluidRow(
            shinydashboard::box(width = 12, collapsible = FALSE,
                collapsed = FALSE,
                shiny::uiOutput(ns("plotDriftUI")),
                shiny::downloadButton(outputId = ns("downloadPlot"), "")
            ),
            shinydashboard::box(width = 12, collapsible = TRUE, 
                collapsed = FALSE, title = "Parameters",
                column(6, 
                    shiny::uiOutput(ns("categoryUI")),
                    shiny::uiOutput(ns("levelUI")),
                    shiny::uiOutput(ns("orderCategoryUI"))),
                column(6,
                    shiny::selectInput(inputId = ns("aggregation"), 
                        label = "Select aggregation",
                        choices = list("sum", "median"), selected = "sum"),
                    shiny::selectInput(inputId = ns("method"),
                        label = "Select smoothing method",
                        choices = list("LOESS" = "loess", "linear model" = "lm"),
                        selected = "loess"),
                    shiny::uiOutput(outputId = ns("dataUI")))   
            ),
        )
    )
}

#' @name driftServer
#' 
#' @title Module for server expressions of tab panel 'Trend/drift' 
#' 
#' @description
#' The module defines the server expressions for the tab panel 
#' 'Trend/drift'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` and `reactive` value
#' @param se_n `SummarizedExperiment` and `reactive` value
#' @param se_b `SummarizedExperiment` and `reactive` value
#' @param se_t `SummarizedExperiment` and `reactive` value
#' @param se_i `SummarizedExperiment` and `reactive` value
#' @param missingValue `logical` (if `FALSE` do not show option for imputed)
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI selectInput reactive req
#' @importFrom plotly renderPlotly
#' @importFrom htmlwidgets saveWidget
#' @importFrom SummarizedExperiment colData
#' @importFrom shinyhelper helper
#' 
#' @noRd
driftServer <- function(id, se, se_n, se_b, se_t, se_i, missingValue) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {

            output$dataUI <- shiny::renderUI({
                
                if (missingValue) {
                    choices_l <- list("raw", "normalized", "batch corrected",
                                        "transformed", "imputed")
                } else {
                    choices_l <- list("raw", "normalized", "batch corrected",
                                        "transformed")
                }
                shiny::selectInput(inputId = session$ns("data"),
                    label = "Select data input",
                    choices = choices_l, selected = "raw")
            })
            
            se_drift <- shiny::reactive({
                shiny::req(input$data)
                if (input$data == "raw") se <- se()
                if (input$data == "normalized") se <- se_n()
                if (input$data == "batch corrected") se <- se_b()
                if (input$data == "transformed") se <- se_t()
                if (input$data == "imputed") se <- se_i()
                se
            })

            cD <- shiny::reactive(SummarizedExperiment::colData(se()))
            
            output$categoryUI <- shiny::renderUI({
                shiny::selectInput(
                    inputId = session$ns("category"),
                    label = "Select variable",
                    choices = colnames(cD()))
            })
            
            output$levelUI <- shiny::renderUI({
                shiny::req(input$category)
                shiny::selectInput(
                    inputId = session$ns("levelSel"), 
                    label = "Select level to highlight", 
                    choices = c("all", unique(cD()[[input$category]])))
            })
            
            output$orderCategoryUI <- shiny::renderUI({
                shiny::selectInput(
                    inputId = session$ns("orderCategory"),
                    label = "Select variable to order samples",
                    choices = colnames(cD()))    
            })
            
            
            p_drift <- shiny::reactive({
                driftPlot(se = se_drift(), aggregation = input$aggregation, 
                    category = input$category, 
                    orderCategory = input$orderCategory,
                    level = input$levelSel, method = input$method)
            })
            
            output$plotDrift <- plotly::renderPlotly({
                shiny::req(input$levelSel)
                p_drift()
            })
            
            output$plotDriftUI <- shiny::renderUI({
                helperFile <- paste("tabPanel_drift_missingValue_", 
                    missingValue, sep = "")
                plotly::plotlyOutput(outputId = session$ns("plotDrift")) |>
                    shinyhelper::helper(content = helperFile)
            })
            
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("Drift_", input$aggregation, "_", 
                        input$category, "_", 
                        input$levelSel, "_", input$method, ".html", sep = "")
                },
                content = function(file) {
                    htmlwidgets::saveWidget(p_drift(), file)
                }
            )
        }
    )
}


################################################################################
########################### coefficient of variation ###########################
################################################################################

#' @name tP_cvUI
#'
#' @title Tab panel UI for tab panel 'Coefficient of Variation'
#'
#' @description
#' The module defines the UI for the tab panel 'Coefficient of Variation'.
#'
#' @details 
#' `fR_boxplotUI` returns the HTML code for the tab-pane 
#' 'Coefficient of Variation'. Internal function for `shinyQC`.
#' 
#' @param id `character`
#'
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#'
#' @examples
#' fR_cvUI("test")
#' 
#' @importFrom shiny NS tabPanel fluidRow downloadButton uiOutput
#' @importFrom shinydashboard box
#' 
#' @noRd
tP_cvUI <- function(id) {
    
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Coefficient of variation",
        shiny::fluidRow(
            shinydashboard::box(width = 12, collapsible = FALSE, 
                collapsed = FALSE,
                shiny::uiOutput(outputId = ns("cvUI")),
                shiny::column(6, 
                    shiny::uiOutput(outputId = ns("dataUI"))),
                shiny::column(6, 
                    shiny::downloadButton(outputId = ns("downloadPlot"), "")))
                
        )
    )
}

#' @name cvServer
#' 
#' @title Module for server expressions of tab panel 'Coefficient of variation' 
#' 
#' @description
#' The module defines the server expressions for the tab panel 
#' 'Coefficient of variation'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param a_r `matrix` and `reactive` value
#' @param a_n `matrix` and `reactive` value
#' @param a_b `matrix` and `reactive` value
#' @param a_t `matrix` and `reactive` value
#' @param a_i `matrix` and `reactive` value
#' @param missingValue `logical` (if `FALSE` do not use imputed values)
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive renderUI selectInput renderPlot
#' @importFrom shiny downloadHandler
#' @importFrom shinyhelper helper
#' @importFrom ggplot2 ggsave
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
cvServer <- function(id, a_r, a_n, a_b, a_t, a_i, missingValue) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            ## create the cv values
            cv_r <- shiny::reactive({
                cv(a_r(), name = "raw")
            })

            cv_n <- shiny::reactive({
                cv(a_n(), name = "normalized")
            })
            
            cv_b <- shiny::reactive({
                cv(a_b(), name = "batch corrected")
            })

            cv_t <- shiny::reactive({
                cv(a_t(), name = "transformed")
            })

            cv_i <- shiny::reactive({
                cv(a_i(), name = "imputed")
            })
            
            output$dataUI <- shiny::renderUI({
                if (missingValue) {
                    choices_l <-  c("raw", "normalized", "batch corrected",
                        "transformed", "imputed")
                } else {
                    choices_l <- c("raw", "normalized", "batch corrected",
                        "transformed")
                }
                shiny::selectInput(inputId = session$ns("data"), 
                    label = "Data set", choices = choices_l, 
                    multiple = TRUE, selected = "raw")
            })

            ## create a reactive data.frame containing the cv values
            df_cv <- shiny::reactive({
                df <- data.frame(row.names = colnames(a_r()))
                if ("raw" %in% input$data) df <- cbind(df, cv_r())
                if ("normalized" %in% input$data) df <- cbind(df, cv_n())
                if ("batch corrected" %in% input$data) df <- cbind(df, cv_b())
                if ("transformed" %in% input$data) df <- cbind(df, cv_t())
                if ("imputed" %in% input$data) df <- cbind(df, cv_i())
                df
            })

            p_cv <- shiny::reactive({
                plotCV(df = df_cv())
            })
            
            ## create the actual plot
            output$cv <- shiny::renderPlot({
                p_cv()
            })
            
            output$cvUI <- shiny::renderUI({
                helperFile <- paste("tabPanel_cv_missingValue_",
                    missingValue, sep = "")
                shiny::plotOutput(outputId = session$ns("cv")) |> 
                    shinyhelper::helper(content = helperFile)
                
            })
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("CV.pdf", sep = "")
                },
                content = function(file) {
                    ggplot2::ggsave(file, p_cv(), device = "pdf")
                }
            )
            
        }
    )
}

################################################################################
################################# mean-sd plot #################################
################################################################################
#' @name box_meanSdUI
#' 
#' @title Box UI for tab panel 'mean-sd plot'
#' 
#' @description 
#' The module defines the UI in the tab panel 'mean-sd plot'.
#' 
#' @details 
#' `box_meanSdUI` returns the HTML code for the tab-pane 'mean-sd plot'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param name `character`, name/title for the box
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' box_meanSdUI("test", "test")
#' 
#' @importFrom shiny NS plotOutput downloadButton
#' @importFrom shinydashboard box
#' 
#' @noRd
box_meanSdUI <- function(id, name) {
    ns <- shiny::NS(id)
    shinydashboard::box(title = name, width = 6, collapsible = TRUE,
        shiny::plotOutput(outputId = ns("meanSd")),
        shiny::downloadButton(outputId = ns("downloadPlot"), "")
    )
}

#' @name tP_meanSdUI
#' 
#' @title Tab panel UI for tab panel 'mean-sd plot'
#' 
#' @description 
#' The module defines the UI in the tab panel 'mean-sd plot'. It serves as a 
#' wrapper for the function `box_meanSdUI`.
#' 
#' @details 
#' `tP_meanSdUI` returns the HTML code for the tab-pane 'mean-sd plot'. 
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
#' tP_meanSdUI("")
#' 
#' @importFrom shiny tabPanel fluidRow uiOutput conditionalPanel
#' 
#' @noRd
tP_meanSdUI <- function(id) {

    ns <- shiny::NS(id)
    shiny::tabPanel(title = "mean-sd plot",
        ## call here the module UIs box_meanSdUI
        shiny::fluidRow(width = 12,
            shiny::column(12, shiny::uiOutput(ns("helperUI"))),
            box_meanSdUI("meanSdTransf", "transformed"), 
            shiny::conditionalPanel("output.missingVals == 'TRUE'",
                box_meanSdUI("meanSdImp", "imputed"))
        )
    )
}

#' @name meanSdUIServer
#' 
#' @title Module for server expressions for UI of tab panel 'mean-sd plot'
#' 
#' @description 
#' The module defines the server expressions for the UI of the tab panel 
#' 'mean-sd plot'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param missingValue `logical` (will load different help files)
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI
#' @importFrom shinyhelper helper
#' 
#' @noRd
meanSdUIServer <- function(id, missingValue) {
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            output$helperUI <- shiny::renderUI({
                
                helperFile <- paste("tabPanel_meanSd_missingValue_", 
                                    missingValue, sep = "")
                shiny::br() |> shinyhelper::helper(content = helperFile)
            })
            
        }
    )
}


#' @name meanSdServer
#' 
#' @title Module for server expressions of tab panel 'mean-sd plot'
#' 
#' @description 
#' The module defines the server expressions for the tab panel 'mean-sd plot'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param assay `matrix` and `reactive` expression, obtained from 
#' `assay(SummarizedExperiment)`
#' @param type `character`
#' @param missingValue `logical` (if `FALSE` do not show box for imputed)
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive renderPlot downloadHandler
#' @importFrom ggplot2 ggsave
#' 
#' @noRd
#' 
#' @importFrom vsn meanSdPlot
meanSdServer <- function(id, assay, type, missingValue) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {

            p_meansd <- shiny::reactive({
                req(assay())
                vsn::meanSdPlot(assay(), ranks = TRUE)$gg
            })
            output$meanSd <- shiny::renderPlot({
                p_meansd()
            })
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("Meansd_", type, ".pdf", sep = "")
                },
                content = function(file) {
                    ggplot2::ggsave(file, p_meansd(), device = "pdf")
                }
            )
        }
    )
}

################################################################################
#################################### MA plot ###################################
################################################################################
#' @name tP_maUI
#' 
#' @title Tab panel UI for tab panel 'MA plot'
#' 
#' @description 
#' The module defines the UI in the tab panel 'MA plot'.
#' 
#' @details 
#' `tP_maUI` returns the HTML code for the tab-pane 'MA plot'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param missingValue `logical` (if `FALSE` do not show option for imputed)
#'
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' tP_maUI("test")
#' 
#' @importFrom shiny NS tabPanel fluidRow downloadButton
#' @importFrom shiny uiOutput plotOutput checkboxInput
#' @importFrom shinydashboard box
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
tP_maUI <- function(id) {
    
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "MA plot", 
        shiny::uiOutput(ns("MAUI")),
        shiny::fluidRow(
            shinydashboard::box(title = "MA plot per sample", width = 12, 
                collapsible = TRUE, 
                shiny::plotOutput(outputId = ns("MAplot"), height = "auto"),
                shiny::downloadButton(outputId = ns("downloadPlotMA"), ""),
                shiny::uiOutput(ns("MAtypeUI"))
                
            )
        ),
        shiny::fluidRow(
            shinydashboard::box(title = "Hoeffding's D statistic", width = 12, 
                collapsible = TRUE,
                plotly::plotlyOutput(outputId = ns("hoeffDPlot")),
                shiny::downloadButton(outputId = ns("downloadPlothD"), ""),
                shiny::checkboxInput(inputId = ns("hDLines"),
                    label = "lines", value = FALSE)
            )
        )
    )
}


#' @name maServer
#' 
#' @title Module for server expressions of tab panel 'MA plot'
#' 
#' @description 
#' The module defines the server expressions in the tab panel 'MA plot'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object and `reactive` value, containing 
#' raw values
#' @param se_n `SummarizedExperiment` object and `reactive` value, containing 
#' normalized values
#' @param se_b `SummarizedExperiment` object and `reactive` value, containing
#' batch corrected values
#' @param se_t `SummarizedExperiment` object and `reactive` value, containing 
#' transformed values
#' @param se_i `SummarizedExperiment` object and `reactive` value, containing 
#' imputed values
#' @param innerWidth `numeric` and `reactive` value, specifying the width of 
#' the window size
#' @param missingValue `logical` (if `FALSE` do not show values for imputed)
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI fluidRow selectInput column req 
#' @importFrom shiny downloadHandler bindCache
#' @importFrom shinyhelper helper
#' @importFrom plotly renderPlotly
#' @importFrom htmlwidgets saveWidget
#' @importFrom SummarizedExperiment colData
#' 
#' @noRd
maServer <-  function(id, se, se_n, se_b, se_t, se_i, innerWidth, 
                                                                missingValue) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            output$MAUI <- shiny::renderUI({
                
                ## access the colData slot and add the rownames as a new column to cD
                ## (will add the column "rowname")
                cD <- SummarizedExperiment::colData(se()) |> as.data.frame()
                cD_rn <- tibble::rownames_to_column(cD)
                
                helperFile <- paste("tabPanel_MA_missingValue_", 
                                                        missingValue, sep = "")
                shiny::fluidRow(
                    shiny::column(6, 
                        shiny::selectInput(
                            inputId = session$ns("groupMA"), 
                            label = "group",
                            choices = c("all", colnames(cD)),
                            selected = "all") 
                    ),
                    shiny::column(6, 
                        shiny::selectInput(
                            inputId = session$ns("plotMA"),
                            label = "plot", multiple = TRUE,
                            choices = cD_rn$rowname) 
                    )
                ) |> 
                    shinyhelper::helper(content = helperFile)
            })
            
            output$MAtypeUI <- shiny::renderUI({
                if (missingValue) {
                    choices_l <- list("raw", "normalized", "batch corrected", 
                                        "transformed", "imputed")
                } else {
                    choices_l <- list("raw", "normalized", "batch corrected", 
                                        "transformed")
                }
                
                shiny::selectInput(
                    inputId = session$ns("MAtype"), 
                    label = "Data set for the MA plot",
                    choices = choices_l, selected = "raw")
            })
            
            ## create MA values: se, log2, group
            vals_r <- shiny::reactive({
                if (any(SummarizedExperiment::assay(se()) < 0, na.rm = TRUE)) {
                    log2_se <- FALSE 
                } else {
                    log2_se <- TRUE
                }
                MAvalues(se(), log2_se, input$groupMA)}) |>
                    shiny::bindCache(se(), input$groupMA, cache = "session")
            
            vals_n <- shiny::reactive({
                if (any(SummarizedExperiment::assay(se_n()) < 0, na.rm = TRUE)) {
                    log2_se <- FALSE
                } else {
                    log2_se <- TRUE
                }
                MAvalues(se_n(), log2_se, input$groupMA)}) |>
                shiny::bindCache(se_n(), input$groupMA, cache = "session")
            
            vals_b <- shiny::reactive({
                if (any(SummarizedExperiment::assay(se_b()) < 0, na.rm = TRUE)) {
                    log2_se <- FALSE 
                } else {
                    log2_se <- TRUE
                }
                MAvalues(se_b(), log2_se, input$groupMA)}) |>
                shiny::bindCache(se_b(), input$groupMA, cache = "session")

            
            vals_t <- shiny::reactive({
                MAvalues(se_t(), FALSE, input$groupMA)}) |>
                    shiny::bindCache(se_t(), input$groupMA, cache = "session")

            vals_i <- shiny::reactive({
                MAvalues(se_i(), FALSE, input$groupMA)}) |>
                    shiny::bindCache(se_i(), input$groupMA, cache = "session")

            ## MA plots: MA values, group
            p_ma <- shiny::reactive({
                shiny::req(input$MAtype)
                if (length(input$plotMA) == 0) {
                    ma_plot <- "all"
                } else {
                    ma_plot <- input$plotMA
                }
                    
                    
                if (input$MAtype == "raw") {
                    ma <- MAplot(vals_r(), group = input$groupMA, 
                        plot = ma_plot)
                }
                if (input$MAtype == "normalized") {
                    ma <- MAplot(vals_n(), group = input$groupMA,
                        plot = ma_plot)
                }
                if (input$MAtype == "batch corrected") {
                    ma <- MAplot(vals_b(), group = input$groupMA,
                        plot = ma_plot)
                }
                if (input$MAtype == "transformed") {
                    ma <- MAplot(vals_t(), group = input$groupMA, 
                        plot = ma_plot)
                }
                if (input$MAtype == "imputed") {
                    ma <- MAplot(vals_i(), group = input$groupMA,
                        plot = ma_plot)
                }
                ma
            })
            
            output$MAplot <- shiny::renderPlot({
                shiny::req(p_ma())
                p_ma()
            }, 
                height = shiny::reactive(ifelse(!is.null(innerWidth()), 
                                                    innerWidth() * 3 / 5, 0))
            )
            
            output$downloadPlotMA <- shiny::downloadHandler(
                filename = function() {
                    paste("MA_", input$MAtype, "_", input$groupMA, "_", 
                        input$plotMA,".pdf", sep = "")
                },
                content = function(file) {
                    ggplot2::ggsave(file, p_ma(), device = "pdf")
                }
            )

            ## Hoeffding's D values: MA values, title for plot
            hD_r <- shiny::reactive(hoeffDValues(vals_r(), "raw")) |>
                shiny::bindCache(vals_r(), cache = "session")
            hD_n <- shiny::reactive(hoeffDValues(vals_n(),  "normalized")) |>
                shiny::bindCache(vals_n(), cache = "session")
            hD_b <- shiny::reactive(hoeffDValues(vals_b(), "batch corrected")) |>
                shiny::bindCache(vals_b(), cache = "session")
            hD_t <- shiny::reactive(hoeffDValues(vals_t(), "transformed")) |>
                shiny::bindCache(vals_t(), cache = "session")
            hD_i <- shiny::reactive(hoeffDValues(vals_i(), "imputed")) |>
                shiny::bindCache(vals_i(), cache = "session")

            ## create reactive data.frame for the hoeffDPlot function
            hoeffD_df <- shiny::reactive({
                if (missingValue) {
                    df <- data.frame(raw = hD_r(), normalized = hD_n(), 
                        `batch corrected` = hD_b(), transformed = hD_t(), 
                        imputed = hD_i())    
                } else {
                    df <- data.frame(raw = hD_r(), normalized = hD_n(), 
                        `batch corrected` = hD_b(), transformed = hD_t())
                }
                df
            })
            
            ## Hoeffding's D plots: lists (Hoeffding's D values), lines
            output$hoeffDPlot <- plotly::renderPlotly({
                hoeffDPlot(hoeffD_df(), lines = input$hDLines)  
            })
            
            output$downloadPlothD <- shiny::downloadHandler(
                filename = function() {
                    paste("Hoeffding_D_", input$groupMA, ".html")  
                },
                content = function(file) {
                    htmlwidgets::saveWidget(
                        hoeffDPlot(hoeffD_df(), lines = input$hDLines), file)
                }
            )
        }
    )
}

################################################################################
##################################### ECDF #####################################
################################################################################

#' @name tP_ECDFUI
#' 
#' @title Tab panel UI for tab panel 'ECDF'
#' 
#' @description 
#' The module defines the UI in the tab panel 'ECDF'
#'  
#' @details 
#' `tP_ECDFUI` returns the HTML code for the tab-pane 'ECDF'. 
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
#' tP_ECDFUI("test")
#' 
#' @importFrom shiny NS tabPanel plotOutput fluidRow column uiOutput
#' @importFrom shinyhelper helper
#' 
#' @noRd
tP_ECDFUI <- function(id) {
    
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "ECDF",
        shiny::plotOutput(outputId = ns("ECDF")) |> 
            shinyhelper::helper(content = "tabPanel_ecdf"),
        shiny::downloadButton(outputId = ns("downloadPlot"), ""),
        shiny::fluidRow( 
            shiny::column(4, shiny::uiOutput(outputId = ns("typeECDFUI"))),
            shiny::column(4, shiny::uiOutput(outputId = ns("sampleECDFUI"))),
            shiny::column(4, shiny::uiOutput(outputId = ns("groupECDFUI")))
        )
    )

}

#' @name ECDFServer
#' 
#' @title Module for server expressions of tab panel 'ECDF'
#' 
#' @description 
#' The module defines the server expressions in the tab panel 'ECDF'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object and `reactive` value, containing 
#' raw values
#' @param se_n `SummarizedExperiment` object and `reactive` value, containing 
#' normalized values
#' @param se_b `SummarizedExperiment` object and `reactive` value, containing
#' batch corrected values
#' @param se_t `SummarizedExperiment` object and `reactive` value, containing 
#' transformed values
#' @param se_i `SummarizedExperiment` object and `reactive` value, containing 
#' imputed values
#' @param missingValue `logical` (if `FALSE` do not show option for imputed)
#' 
#' @return
#' `shiny.render.function` expression
#' 
#' @importFrom shiny moduleServer renderUI selectInput req reactive
#' @importFrom shiny downloadHandler renderPlot
#' @importFrom ggplot2 ggsave
#' @importFrom SummarizedExperiment colData
#' @importFrom tibble rownames_to_column
#'
#' @author Thomas Naake
#' 
#' @noRd
ECDFServer <- function(id, se, se_n, se_b, se_t, se_i, missingValue) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {
            
            output$typeECDFUI <- shiny::renderUI({
                
                if (missingValue) {
                    choices_l <- list("raw", "normalized", "batch corrected",
                                            "transformed", "imputed")
                } else {
                    choices_l <- list("raw", "normalized", "batch corrected",
                                            "transformed")
                }
                shiny::selectInput(inputId = session$ns("typeECDF"),
                    label = "Data set for the ECDF plot",
                    choices = choices_l,
                    selected = "raw")
            })
            
            output$sampleECDFUI <- shiny::renderUI({
                
                cD <- SummarizedExperiment::colData(se()) |> as.data.frame()
                cD <- tibble::rownames_to_column(cD)
                
                shiny::selectInput(
                    inputId = session$ns("sampleECDF"), 
                        label = "Sample", 
                        choices = cD$rowname, 
                        selected = cD$rowname[1])
            })
            
            output$groupECDFUI <- shiny::renderUI({
                selectInput(
                    inputId = session$ns("groupECDF"), 
                    label = "group",
                    choices = c("all", 
                        colnames(SummarizedExperiment::colData(se()))),
                    selected = "all")
            })
            
            ## ECDF plots: se, sample, group
            se_sel <- shiny::reactive({
                req(input$typeECDF)
                if (input$typeECDF == "raw") SE <- se()
                if (input$typeECDF == "normalized") SE <- se_n()
                if (input$typeECDF == "batch corrected") SE <- se_b()
                if (input$typeECDF == "transformed") SE <- se_t()
                if (input$typeECDF == "imputed") SE <- se_i()
                SE
            })
            
            p_ecdf <- shiny::reactive({
                req(se_sel())
                ECDF(se = se_sel(), sample = input$sampleECDF, 
                    group = input$groupECDF)
            })
            
            output$ECDF <- shiny::renderPlot({
                p_ecdf()
            })
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("ECDF_", input$typeECDF, "_", input$groupECDF, "_", 
                        input$sampleECDF, ".pdf", sep = "")
                },
                content = function(file) {
                    ggplot2::ggsave(file, p_ecdf(), device = "pdf")
                }
            )
        }
    )
}


################################################################################
################################### distances ##################################
################################################################################

#' @name fR_distUI
#' 
#' @title Fluid row UI for tab panel 'Distance matrix'
#' 
#' @description
#' The module defines the UI in the tab panel 'Distance matrix'.
#' 
#' @details 
#' `fR_distUI` returns the HTML code for the tab-pane 'Distance Matrix'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param title `character` name/title of the box
#' @param collapsed `logical`
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' fR_distUI("test", "test")
#' 
#' @importFrom shiny NS fluidRow downloadButton 
#' @importFrom shinydashboard box
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
fR_distUI <- function(id, title, collapsed = TRUE) {
    ns <- shiny::NS(id)
    shiny::fluidRow(shinydashboard::box(title = title, width = 12, 
                collapsible = TRUE, collapsed = collapsed, 
        shiny::column(6, 
            shiny::plotOutput(outputId = ns("distSample")),
            shiny::downloadButton(outputId = ns("downloadPlotDist"), "")),
        shiny::column(6, 
            plotly::plotlyOutput(outputId = ns("distSampleSum")),
            shiny::downloadButton(outputId = ns("downloadPlotSum"), ""))))
}

#' @name tP_distUI
#' 
#' @title Tab panel UI for tab panel 'Distance matrix'
#' 
#' @description
#' The function defines the UI in the tab panel 'Distance matrix'. It serves
#' as a wrapper for `fR_distUI`.
#' 
#' @details 
#' `tP_distUI` returns the HTML code for the tab-pane 'Distance matrix'. 
#' Internal function for `shinyQC`.
#' 
#' @param missingValue `logical` (if `FALSE` do not show box for imputed)
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples 
#' tP_distUI()
#' 
#' @importFrom shiny tabPanel uiOutput fluidRow column selectInput
#' @importFrom shinydashboard box
#'
#' @noRd
tP_distUI <- function() {

    shiny::tabPanel(title = "Distance matrix",
        shiny::uiOutput("distUI-distRawUI"),
        fR_distUI(id = "distNorm", title = "normalized", 
            collapsed = TRUE),
        fR_distUI(id = "distBatch", title = "batch corrected", 
            collapsed = TRUE),
        fR_distUI(id = "distTransf", title = "transformed", 
            collapsed = TRUE),
        shiny::conditionalPanel("output.missingVals == 'TRUE'",
            fR_distUI(id = "distImp", title = "imputed", 
                collapsed = TRUE)),
        shiny::fluidRow(
            shinydashboard::box(title = "Parameters", width = 6,
                collapsible = TRUE, collapsed = FALSE,
                shiny::column(12, uiOutput(outputId = "groupDistUI")),
                shiny::column(12, 
                shiny::selectInput(inputId = "methodDistMat", 
                    label = "distance method", 
                    choices = c("euclidean", "maximum", "canberra", 
                        "minkowski"), 
                    selected = "euclidean"))
            )
        )
    )
}

#' @name tP_distUIServer
#' 
#' @title Server for tab panel UI for tab panel 'Distance matrix'
#' 
#' @description
#' The function defines the server for the UI in the tab panel 
#' 'Distance matrix'.
#' 
#' @details 
#' `tP_distUIServer` returns the server for the `tP_distUI`. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param missingValue `logical` (if `FALSE` do not show box for imputed)
#' 
#' @return 
#' `shiny.reactive.expression` with HTML content
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer renderUI 
#' @importFrom shinyhelper helper
#' 
#' @noRd
distUIServer <- function(id, missingValue) {
    shiny::moduleServer(
        id,
        function(input, output, session) {

            output$distRawUI <- shiny::renderUI({
                helperFile <- paste("tabPanel_distMat_missingValue_", 
                    missingValue, sep = "")
                
                fR_distUI(id = session$ns("distRaw"), title = "raw", 
                    collapsed = FALSE) |>
                    shinyhelper::helper(content = helperFile) 
            })
        }
    )
}

#' @name distServer
#' 
#' @title Module for server expressions of tab panel 'Distance matrix'
#' 
#' @description 
#' The module defines the server expressions in the tab panel 'Distance matrix'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object and `reactive` value
#' @param assay `matrix` and `reactive` value
#' @param method `character` and `reactive` value, one of `"euclidean"`, 
#' `"mannhattan"`, `"canberra"`, or `"minkowski"`
#' @param label `character` and `reactive` value, specified the annotation of 
#' the `ComplexHeatmap`
#' @param type `character`
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer reactive downloadHandler req
#' @importFrom plotly renderPlotly partial_bundle
#' @importFrom htmlwidgets saveWidget
#' @importFrom ComplexHeatmap draw
#' @importFrom grDevices pdf dev.off
#' 
#' 
#' @noRd
distServer <- function(id, se, assay, method, label, type) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {

            d <- shiny::reactive({
                distShiny(assay(), method = method())
            })
            
            ## plot of distance matrix
            p_dist <- shiny::reactive({
                distSample(d(), se(), label = label(), title = "") 
            })
            
            output$distSample <- shiny::renderPlot({
                shiny::req(label())
                p_dist()  
            })
            
            output$downloadPlotDist <- shiny::downloadHandler(
                filename = function() {
                    paste("Distance_", type, "_", method(), ".pdf", sep = "")
                },
                content = function(file) {
                    grDevices::pdf(file)
                    p = p_dist()
                    ComplexHeatmap::draw(p)
                    grDevices::dev.off()
                    ##htmlwidgets::saveWidget(partial_bundle(p_dist()), file)
                }
            )
            
            ## plot of sum of distances
            p_sumDist <- shiny::reactive({
                sumDistSample(d(), title = "")
            })
            
            output$distSampleSum <- plotly::renderPlotly({
                p_sumDist()
            })

            output$downloadPlotSum <- shiny::downloadHandler(
                filename = function() {
                    paste("Sum_distance_", type, "_", 
                        method(), ".html", sep = "")
                },
                content = function(file) {
                    htmlwidgets::saveWidget(partial_bundle(p_sumDist()), file)
                }
            )
        }
    )
    
}

################################################################################
################################### Features ###################################
################################################################################

#' @name tP_featureUI
#' 
#' @title UI for tab panel 'Features'
#' 
#' @description
#' The module defines the UI in the tab panel 'Features'.
#' 
#' @details 
#' `tP_featureUI` returns the HTML code for the tab-pane 'Distance Matrix'. 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param title `character` name/title of the box
#' @param collapsed `logical`
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' fR_distUI("test", "test")
#' 
#' @importFrom shiny NS tabPanel plotOutput column radioButtons 
#' @importFrom shiny selectizeInput checkboxInput downloadButton
#' @importFrom shinydashboard box
#' @importFrom plotly plotlyOutput
#' @importFrom shinyhelper helper
#' 
#' @noRd
tP_featureUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tabPanel(title = "Features",
        shinydashboard::box(title = "", width = 12, collapsible = TRUE, 
            collapsed = FALSE,
            shiny::plotOutput(outputId = ns("Features")) |> 
                shinyhelper::helper(content = "tabPanel_Features"),
            shiny::column(4, 
                shiny::downloadButton(outputId = ns("downloadPlot"), "")),
            shiny::column(4, shiny::uiOutput(outputId = ns("dataUI"))),
            shiny::column(4, 
                shiny::selectizeInput(inputId = ns("selectFeature"), 
                    choices = NULL, label = "Select feature"))
        ),
        shinydashboard::box(title = "", width = 12, collapsible = TRUE, 
            collapsed = FALSE,
            plotly::plotlyOutput(outputId = ns("FeaturesCV")),
            shiny::column(6, 
                shiny::downloadButton(outputId = ns("downloadPlotCV"), "")),
            shiny::column(6, 
                shiny::checkboxInput(inputId = ns("FeatureLines"),
                label = "lines", value = FALSE))
        ),
        shiny::radioButtons(inputId = ns("mode"), label = "Select features",
            choices = list("all" = "all", "exclude" = "exclude", 
                                                        "select" = "select")),
        shiny::selectizeInput(inputId = ns("excludeFeature"), choices = NULL, 
            label = NULL, multiple = TRUE)
    )
}

#' @name featureServer
#' 
#' @title Module for server expressions of tab panel 'Features'
#' 
#' @description 
#' The module defines the server expressions in the tab panel 'Features'.
#' 
#' @details 
#' Internal function for `shinyQC`.
#' 
#' @param id `character`
#' @param se `SummarizedExperiment` object
#' @param a `matrix` and `reactive` value, containing raw values
#' @param a_n `matrix` and `reactive` value, containing normalized values
#' @param a_b `matrix` and `reactive` value, containing batch corrected values
#' @param a_t `matrix` and `reactive` value, containing transformed values
#' @param a_i `matrix` and `reactive` value, containing imputed values
#' @param missingValue `logical` (if `FALSE` do not show option for imputed)
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @importFrom shiny moduleServer observe updateSelectizeInput renderUI 
#' @importFrom shiny selectInput reactive req renderPlot downloadHandler
#' @importFrom ggplot2 ggsave
#' @importFrom plotly renderPlotly
#' @importFrom htmlwidgets saveWidget 
#'
#' @noRd
featureServer <- function(id, se, a, a_n, a_b, a_t, a_i, missingValue) {
    
    shiny::moduleServer(
        id, 
        function(input, output, session) {

            shiny::observe({
                shiny::updateSelectizeInput(session = session, 
                    inputId = "selectFeature", 
                    choices = as.list(rownames(a())), server = TRUE)
            })

            shiny::observe({
                shiny::updateSelectizeInput(session = session, 
                    inputId = "excludeFeature", 
                    choices = rownames(se), server = TRUE)    
            })
            

            output$dataUI <- shiny::renderUI({
                if (missingValue) {
                    choices_l <-  c("raw", "normalized", "batch.corrected",
                        "transformed", "imputed")   
                } else {
                    choices_l <- c("raw", "normalized", "batch.corrected",
                        "transformed")
                }
                shiny::selectInput(inputId = session$ns("data"), 
                    label = "Data set", choices = choices_l, multiple = TRUE, 
                    selected = choices_l)
            })
            
            ## create a reactive data.frame containing the assay values
            l_assays <- shiny::reactive({
                shiny::req(a_i())
                if (missingValue) {
                    l <- list(raw = a(), normalized = a_n(),
                        `batch.corrected` = a_b(), transformed = a_t(), 
                        imputed = a_i())
                } else {
                    l <- list(raw = a(), normalized = a_n(),
                        `batch.corrected` = a_b(), transformed = a_t())
                }
                l
            })
            
            df_feature <- shiny::reactive({
                l <- l_assays()
                l <- l[names(l) %in% input$data]
                if (length(l) > 0) {
                    createDfFeature(l, feature = input$selectFeature)
                } else {
                    NULL
                }
            })
                
            p_feature <- shiny::reactive({
                if (!is.null(df_feature()))
                    featurePlot(df_feature())
            })
            
            output$Features <- shiny::renderPlot({
                p_feature()
            })
            
            output$downloadPlot <- shiny::downloadHandler(
                filename = function() {
                    paste("Features_", input$selectFeature, ".pdf", sep = "")
                },
                content = function(file) {
                    ggplot2::ggsave(file, p_feature(), device = "pdf")
                }
            )
            
            output$FeaturesCV <- plotly::renderPlotly({
                cvFeaturePlot(l_assays(), lines = input$FeatureLines)
            })
            
            output$downloadPlotCV <- shiny::downloadHandler(
                filename = function() {
                    paste("Features_CV_lines_", input$FeatureLines, 
                                                            ".html", sep = "")
                },
                content = function(file) {
                    htmlwidgets::saveWidget(
                        cvFeaturePlot(l_assays(), lines = input$FeatureLines),
                        file)
                }
            )
        }
    )
}

#' @name tP_values_all
#' 
#' @title Tab panel UI for tab panel 'Values'
#' 
#' @description  
#' The module defines the UI for the tab panel 'Values'.
#' 
#' @details 
#' `tP_values_all` returns the HTML code for the tab-pane 'Values'. 
#' Internal function for `shinyQC`.
#' 
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples
#' tP_values_all()
#' 
#' @importFrom shiny tabPanel 
#' @importFrom shinydashboard tabBox
#' 
#' @noRd
tP_values_all <- function() {
    shiny::tabPanel("Values",
        shinydashboard::tabBox(title = "", width = 12,
            tP_boxplotUI(id = "boxUI"),
            tP_driftUI(id = "drift"),
            tP_cvUI(id = "cv"),
            tP_meanSdUI("meanSd"),
            tP_maUI(id = "MA"),
            tP_ECDFUI(id = "ECDF"),
            tP_distUI(),
            tP_featureUI(id = "features")
        )
    )
}
