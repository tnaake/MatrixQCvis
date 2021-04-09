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
#' @noRd
fR_boxplotUI <- function(id, name, collapsed) {
    ns <- NS(id)
    fluidRow(
        box(title = name, width = 12, collapsible = TRUE, collapsed = collapsed,
            plotOutput(outputId = ns("boxplot")),
            downloadButton(outputId = ns("downloadPlot"), ""))
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
#' @noRd
tP_boxplotUI <- function(id) {
    
    ns <- NS(id)
    tabPanel(title = "Boxplot/Violin plot",
        fluidRow(
            column(6, 
                radioButtons(inputId = "boxLog",
                    label = HTML("Display log2 values? <br>
                        (only for 'raw' and 'normalized')"),
                    choices = list("no log2", "log2"),
                    selected = "no log2")),
            column(6, uiOutput(ns("violinPlotUI")))
        ),
        fR_boxplotUI("boxRaw", "raw", collapsed = FALSE),
        fR_boxplotUI("boxNorm", "normalized", collapsed = TRUE),
        fR_boxplotUI("boxTransf", "transformed", collapsed = TRUE),
        fR_boxplotUI("boxBatch", "batch corrected", collapsed = TRUE),
        conditionalPanel("output.missingVals == 'TRUE'", 
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
#' @author Thomas Naake
#' 
#' @noRd
boxPlotUIServer <- function(id, missingValue) {
    
    moduleServer(
        id, 
        function(input, output, session) {
            output$violinPlotUI <- renderUI({
                
                helperFile <- paste("tabPanel_boxplot_missingValue_", 
                                    missingValue, sep = "")
                
                radioButtons(inputId = session$ns("violinPlot"),
                    label = "Type of display", 
                    choices = list("boxplot", "violin"), 
                    selected = "boxplot") %>% 
                        helper(content = helperFile)
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
#' @param assay `matrix`
#' @param boxLog `reactive` expression and `logical`
#' @param violin `reactive` expression and `character`
#' @param type `character`
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
boxPlotServer <- function(id, assay, boxLog, violin, type) {
    
    moduleServer(
        id, 
        function(input, output, session) {
            
            logValues <- reactive({
                if (boxLog() == "log2") {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            })
            
            vP <- reactive({
                req(violin())
                if (violin() == "violin") {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            })
            
            ## create the actual plot
            p_boxplot <- reactive({
                create_boxplot(assay(), title = "", log2 = logValues(), 
                    violin = vP())
            })
            output$boxplot <- renderPlot({
                p_boxplot()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function() {
                    paste("Boxplot_violinplot_", type, ".pdf", sep = "")
                },
                content = function(file) {
                    ggsave(file, p_boxplot(), device = "pdf")
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
#' @noRd
tP_driftUI <- function(id) {

    ns <- NS(id)
    tabPanel(title = "Trend/drift",
        fluidRow(
            box(width = 12, collapsible = FALSE,
                collapsed = FALSE,
                uiOutput(ns("plotDriftUI")),
                downloadButton(outputId = ns("downloadPlot"), "")
            ),
            box(width = 6, collapsible = TRUE, 
                collapsed = FALSE,
                uiOutput(ns("categoryUI")),
                uiOutput(ns("levelUI")),
                uiOutput(ns("dataUI"))
            ),
            box(width = 6, collapsible = TRUE,
                collapsed = TRUE,
                selectInput(inputId = ns("aggregation"), 
                    label = "Select aggregation",
                    choices = list("sum", "median"), selected = "sum"),
                selectInput(inputId = ns("method"),
                    label = "Select smoothing method",
                    choices = list("LOESS" = "loess", "linear model" = "lm"),
                    selected = "loess"),
                uiOutput(outputId = ns("orderCategoryUI"))
            )
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
#' @param se_t `SummarizedExperiment` and `reactive` value
#' @param se_b `SummarizedExperiment` and `reactive` value
#' @param se_i `SummarizedExperiment` and `reactive` value
#' @param missingValue `logical` (if `FALSE` do not show option for imputed)
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
driftServer <- function(id, se, se_n, se_t, se_b, se_i, missingValue) {
    
    moduleServer(
        id, 
        function(input, output, session) {

            output$dataUI <- renderUI({
                
                if (missingValue) {
                    choices_l <- list("raw", "normalized", "transformed", 
                                        "batch corrected", "imputed")
                } else {
                    choices_l <- list("raw", "normalized", "transformed", 
                                        "batch corrected")
                }
                selectInput(inputId = session$ns("data"),
                    label = "Select data input",
                    choices = choices_l, selected = "raw")
            })
            
            se_drift <- reactive({
                req(input$data)
                if (input$data == "raw") se <- se()
                if (input$data == "normalized") se <- se_n()
                if (input$data == "transformed") se <- se_t()
                if (input$data == "batch corrected") se <- se_b()
                if (input$data == "imputed") se <- se_i()
                se
            })

            output$categoryUI <- renderUI({
                selectInput(
                    inputId = session$ns("category"),
                    label = "Select variable",
                    choices = colnames(colData(se())))
            })
            
            output$levelUI <- renderUI({
                req(se_drift())
                selectInput(
                    inputId = session$ns("levelSel"), 
                    label = "Select level to highlight", 
                    choices = c("all", 
                        unique(colData(se_drift())[[input$category]])))
            })
            
            output$orderCategoryUI <- renderUI({
                selectInput(
                    inputId = session$ns("orderCategory"),
                    label = "Select variable to order samples",
                    choices = colnames(colData(se())))    
            })
            
            
            p_drift <- reactive({
                driftPlot(se = se_drift(), aggregation = input$aggregation, 
                    category = input$category, 
                    orderCategory = input$orderCategory,
                    level = input$levelSel, method = input$method)
            })
            
            output$plotDrift <- renderPlot({
                req(input$levelSel)
                p_drift()
            })
            
            output$plotDriftUI <- renderUI({
                helperFile <- paste("tabPanel_drift_missingValue_", 
                    missingValue, sep = "")
                plotOutput(outputId = session$ns("plotDrift")) %>%
                    helper(content = helperFile)
            })
            
            
            output$downloadPlot <- downloadHandler(
                filename = function() {
                    paste("Drift_", input$aggregation, "_", 
                        input$category, "_", 
                        input$levelSel, "_", input$method, ".pdf", sep = "")
                },
                content = function(file) {
                    ggsave(file, p_drift(), device = "pdf")
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
#' @noRd
tP_cvUI <- function(id) {
    
    ns <- NS(id)
    tabPanel(title = "Coefficient of variation",
        fluidRow(
            box(width = 12, collapsible = FALSE, 
                collapsed = FALSE,
                uiOutput(outputId = ns("cvUI")),
                downloadButton(outputId = ns("downloadPlot"), ""))
                
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
#' @param a_t `matrix` and `reactive` value
#' @param a_b `matrix` and `reactive` value
#' @param a_i `matrix` and `reactive` value
#' @param missingValue `logical` (if `FALSE` do not use imputed values)
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
cvServer <- function(id, a_r, a_n, a_t, a_b, a_i, missingValue) {
    
    moduleServer(
        id, 
        function(input, output, session) {
            
            ## create the cv values
            cv_r <- reactive({
                cv(a_r(), name = "raw")
            })

            cv_n <- reactive({
                cv(a_n(), name = "normalized")
            })

            cv_t <- reactive({
                cv(a_t(), name = "transformed")
            })

            cv_b <- reactive({
                cv(a_b(), name = "batch corrected")
            })

            cv_i <- reactive({
                cv(a_i(), name = "imputed")
            })

            ## create a reactive data.frame containing the cv values
            df_cv <- reactive({
                if (missingValue) {
                    data.frame(cv_r(), cv_n(), cv_t(), cv_b(), cv_i())    
                } else {
                    data.frame(cv_r(), cv_n(), cv_t(), cv_b())
                }
            })

            p_cv <- reactive({
                plotCV(df = df_cv())
            })
            
            ## create the actual plot
            output$cv <- renderPlot({
                p_cv()
            })
            
            output$cvUI <- renderUI({
                helperFile <- paste("tabPanel_cv_missingValue_",
                    missingValue, sep = "")
                plotOutput(outputId = session$ns("cv")) %>% 
                    helper(content = helperFile)
                
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function() {
                    paste("CV.pdf", sep = "")
                },
                content = function(file) {
                    ggsave(file, p_cv(), device = "pdf")
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
#' @noRd
box_meanSdUI <- function(id, name) {
    ns <- NS(id)
    box(title = name, width = 6, collapsible = TRUE,
        plotOutput(outputId = ns("meanSd")),
        downloadButton(outputId = ns("downloadPlot"), "")
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
#' @return 
#' `shiny.tag` with HTML content
#'
#' @author Thomas Naake
#' 
#' @examples 
#' tP_meanSdUI()
#' 
#' @noRd
tP_meanSdUI <- function() {

    tabPanel(title = "mean-sd plot",
        ## call here the module UIs box_meanSdUI
        fluidRow(
            box_meanSdUI("meanSdTransf", "transformed"),
            uiOutput("meanSd-meanSdBatchUI"),
        conditionalPanel("output.missingVals == 'TRUE'",  
            fluidRow(column = 6,
                box_meanSdUI("meanSdImp", "imputed")))
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
#' @noRd
meanSdUIServer <- function(id, missingValue) {
    moduleServer(
        id, 
        function(input, output, session) {
            
            output$meanSdBatchUI <- renderUI({
                helperFile <- paste("tabPanel_meanSd_missingValue_", 
                                                        missingValue, sep = "")
                box_meanSdUI("meanSdBatch", "batch corrected") %>%
                    helper(content = helperFile)
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
#' @noRd
#' 
#' @importFrom vsn meanSdPlot
meanSdServer <- function(id, assay, type, missingValue) {
    
    moduleServer(
        id, 
        function(input, output, session) {

            p_meansd <- reactive({
                meanSdPlot(assay(), ranks = TRUE)$gg
            })
            output$meanSd <- renderPlot({
                p_meansd()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function() {
                    paste("Meansd_", type, ".pdf", sep = "")
                },
                content = function(file) {
                    ggsave(file, p_meansd(), device = "pdf")
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
#' @noRd
tP_maUI <- function(id) {
    
    ns <- NS(id)
    tabPanel(title = "MA plot", 
        uiOutput(ns("MAUI")),
        fluidRow(
            box(title = "MA plot per sample", width = 12, 
                collapsible = TRUE, 
                plotOutput(outputId = ns("MAplot"), height = "auto"),
                downloadButton(outputId = ns("downloadPlotMA"), ""),
                uiOutput(ns("MAtypeUI"))
                
            )
        ),
        fluidRow(
            box(title = "Hoeffding's D statistic", width = 12, 
                collapsible = TRUE,
                plotlyOutput(outputId = ns("hoeffDPlot")),
                downloadButton(outputId = ns("downloadPlothD"), ""),
                checkboxInput(inputId = ns("hDLines"),
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
#' @param se_t `SummarizedExperiment` object and `reactive` value, containing 
#' transformed values
#' @param se_b `SummarizedExperiment` object and `reactive` value, containing
#' batch corrected values
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
#' @importFrom htmlwidgets saveWidget
#' 
#' @noRd
maServer <-  function(id, se, se_n, se_t, se_b, se_i, innerWidth, 
                                                                missingValue) {
    
    moduleServer(
        id, 
        function(input, output, session) {
            
            output$MAUI <- renderUI({
                
                helperFile <- paste("tabPanel_MA_missingValue_", 
                                                        missingValue, sep = "")
                fluidRow(
                    column(6, 
                        selectInput(
                            inputId = session$ns("groupMA"), 
                            label = "group",
                            choices = c("all", colnames(colData(se()))),
                            selected = "all") 
                    ),
                    column(6, 
                        selectInput(
                            inputId = session$ns("plotMA"),
                            label = "plot", multiple = TRUE,
                            choices = colData(se())$name) 
                    )
                ) %>% 
                    helper(content = helperFile)
            })
            
            output$MAtypeUI <- renderUI({
                if (missingValue) {
                    choices_l <- list("raw", "normalized", "transformed",
                                        "batch corrected", "imputed")
                } else {
                    choices_l <- list("raw", "normalized", "transformed",
                                        "batch corrected")
                }
                
                selectInput(
                    inputId = session$ns("MAtype"), 
                    label = "Data set for the MA plot",
                    choices = choices_l, selected = "raw")
            })
            
            ## create MA values: se, log2, group
            vals_r <- reactive({
                log2_se <- if (any(assay(se()) < 0) ) FALSE else TRUE
                MAvalues(se(), log2_se, input$groupMA)}) %>%
                bindCache(se(), input$groupMA, cache = "session")
            
            vals_n <- reactive({
                log2_se <- if (any(assay(se_n()) < 0) ) FALSE else TRUE
                MAvalues(se_n(), log2_se, input$groupMA)}) %>%
                bindCache(se_n(), input$groupMA, cache = "session")
            
            vals_t <- reactive(MAvalues(se_t(), FALSE, input$groupMA)) %>%
                bindCache(se_t(), input$groupMA, cache = "session")
            
            vals_b <- reactive(MAvalues(se_b(), FALSE, input$groupMA)) %>%
                bindCache(se_b(), input$groupMA, cache = "session")
            
            vals_i <- reactive(MAvalues(se_i(), FALSE, input$groupMA)) %>%
                bindCache(se_i(), input$groupMA, cache = "session")

            ## MA plots: MA values, group
            p_ma <- reactive({
                req(input$MAtype)
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
                if (input$MAtype == "transformed") {
                    ma <- MAplot(vals_t(), group = input$groupMA, 
                        plot = ma_plot)
                }
                if (input$MAtype == "batch corrected") {
                    ma <- MAplot(vals_b(), group = input$groupMA,
                        plot = ma_plot)
                }
                if (input$MAtype == "imputed") {
                    ma <- MAplot(vals_i(), group = input$groupMA,
                        plot = ma_plot)
                }
                ma
            })
            
            output$MAplot <- renderPlot({
                req(p_ma())
                p_ma()
            }, 
                height = reactive(ifelse(!is.null(innerWidth()), 
                                                    innerWidth() * 3 / 5, 0))
            )
            
            output$downloadPlotMA <- downloadHandler(
                filename = function() {
                    paste("MA_", input$MAtype, "_", input$groupMA, "_", 
                        input$plotMA,".pdf", sep = "")
                },
                content = function(file) {
                    ggsave(file, p_ma(), device = "pdf")
                }
            )

            ## Hoeffding's D values: MA values, title for plot
            hD_r <- reactive(hoeffDValues(vals_r(), "raw")) %>%
                bindCache(vals_r(), cache = "session")
            hD_n <- reactive(hoeffDValues(vals_n(),  "normalized")) %>%
                bindCache(vals_n(), cache = "session")
            hD_t <- reactive(hoeffDValues(vals_t(), "transformed")) %>%
                bindCache(vals_t(), cache = "session")
            hD_b <- reactive(hoeffDValues(vals_b(), "batch corrected")) %>%
                bindCache(vals_b(), cache = "session")
            hD_i <- reactive(hoeffDValues(vals_i(), "imputed")) %>%
                bindCache(vals_i(), cache = "session")

            ## create reactive data.frame for the hoeffDPlot function
            hoeffD_df <- reactive({
                if (missingValue) {
                    df <- data.frame(raw = hD_r(), normalized = hD_n(), 
                        transformed = hD_t(), 
                        `batch corrected` = hD_b(), imputed = hD_i())    
                } else {
                    df <- data.frame(raw = hD_r(), normalized = hD_n(), 
                        transformed = hD_t(), 
                        `batch corrected` = hD_b())
                }
                df
            })
            
            ## Hoeffding's D plots: lists (Hoeffding's D values), lines
            output$hoeffDPlot <- renderPlotly({
                hoeffDPlot(hoeffD_df(), lines = input$hDLines)  
            })
            
            output$downloadPlothD <- downloadHandler(
                filename = function() {
                    paste("Hoeffding_D_", input$groupMA, ".html")  
                },
                content = function(file) {
                    saveWidget(
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
#' @noRd
tP_ECDFUI <- function(id) {
    
    ns <- NS(id)
    tabPanel(title = "ECDF",
        plotOutput(outputId = ns("ECDF")) %>% 
            helper(content = "tabPanel_ecdf"),
        downloadButton(outputId = ns("downloadPlot"), ""),
        fluidRow( 
            column(4, uiOutput(outputId = ns("typeECDFUI"))),
            column(4, uiOutput(outputId = ns("sampleECDFUI"))),
            column(4, uiOutput(outputId = ns("groupECDFUI")))
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
#' @param se_t `SummarizedExperiment` object and `reactive` value, containing 
#' transformed values
#' @param se_b `SummarizedExperiment` object and `reactive` value, containing
#' batch corrected values
#' @param se_i `SummarizedExperiment` object and `reactive` value, containing 
#' imputed values
#' @param missingValue `logical` (if `FALSE` do not show option for imputed)
#' 
#' @return
#' `shiny.render.function` expression
#'
#' @author Thomas Naake
#' 
#' @noRd
ECDFServer <- function(id, se, se_n, se_t, se_b, se_i, missingValue) {
    
    moduleServer(
        id, 
        function(input, output, session) {
            
            output$typeECDFUI <- renderUI({
                
                if (missingValue) {
                    choices_l <- list("raw", "normalized", "transformed", 
                                        "batch corrected", "imputed")
                } else {
                    choices_l <- list("raw", "normalized", "transformed", 
                                        "batch corrected")
                }
                selectInput(inputId = session$ns("typeECDF"), 
                    label = "Data set for the ECDF plot",
                    choices = choices_l,
                    selected = "raw")
            })
            
            output$sampleECDFUI <- renderUI({
                selectInput(
                    inputId = session$ns("sampleECDF"), 
                        label = "Sample", 
                        choices = colData(se())$name, 
                        selected = colData(se())$name[1])
            })
            
            output$groupECDFUI <- renderUI({
                selectInput(
                    inputId = session$ns("groupECDF"), 
                    label = "group",
                    choices = c("all", colnames(colData(se()))),
                    selected = "all")
            })
            
            ## ECDF plots: se, sample, group
            p_ecdf <- reactive({
                req(input$typeECDF)
                if (input$typeECDF == "raw") SE <- se()
                if (input$typeECDF == "normalized") SE <- se_n()
                if (input$typeECDF == "transformed") SE <- se_t()
                if (input$typeECDF == "batch corrected") SE <- se_b()
                if (input$typeECDF == "imputed") SE <- se_i()
                ECDF(SE, input$sampleECDF, input$groupECDF)
            })
            
            output$ECDF <- renderPlot({
                p_ecdf()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function() {
                    paste("ECDF_", input$typeECDF, "_", input$groupECDF, "_", 
                        input$sampleECDF, ".pdf", sep = "")
                },
                content = function(file) {
                    ggsave(file, p_ecdf(), device = "pdf")
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
#' @noRd
fR_distUI <- function(id, title, collapsed = TRUE) {
    ns <- NS(id)
    fluidRow(box(title = title, width = 12, collapsible = TRUE, 
                    collapsed = collapsed, 
        column(6, 
            plotOutput(outputId = ns("distSample")),
            downloadButton(outputId = ns("downloadPlotDist"), "")),
        column(6, 
            plotlyOutput(outputId = ns("distSampleSum")),
            downloadButton(outputId = ns("downloadPlotSum"), ""))))
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
#' @noRd
tP_distUI <- function() {

    tabPanel(title = "Distance matrix",
        uiOutput("distUI-distRawUI"),
        fR_distUI(id = "distNorm", title = "normalized", 
            collapsed = TRUE),
        fR_distUI(id = "distTransf", title = "transformed", 
            collapsed = TRUE),
        fR_distUI(id = "distBatch", title = "batch corrected", 
            collapsed = TRUE),
        conditionalPanel("output.missingVals == 'TRUE'",
            fR_distUI(id = "distImp", title = "imputed", 
                collapsed = TRUE)),
        fluidRow(
            box(title = "Parameters", width = 6,
                collapsible = TRUE, collapsed = FALSE,
                column(12, uiOutput(outputId = "groupDistUI")),
                column(12, 
                selectInput(inputId = "methodDistMat", 
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
#' @noRd
distUIServer <- function(id, missingValue) {
    moduleServer(
        id,
        function(input, output, session) {

            output$distRawUI <- renderUI({
                helperFile <- paste("tabPanel_distMat_missingValue_", 
                    missingValue, sep = "")
                
                fR_distUI(id = session$ns("distRaw"), title = "raw", 
                    collapsed = FALSE) %>%
                    helper(content = helperFile) 
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
#' @importFrom ComplexHeatmap draw
#' @importFrom grDevices pdf dev.off
#' @noRd
distServer <- function(id, se, assay, method, label, type) {
    
    moduleServer(
        id, 
        function(input, output, session) {
            
            
            d <- reactive({
                distShiny(assay(), method = method())
            })
            
            ## plot of distance matrix
            p_dist <- reactive({
                distSample(d(), se(), label = label(), title = "") 
            })
            
            output$distSample <- renderPlot({
                req(label())
                p_dist()  
            })
            
            output$downloadPlotDist <- downloadHandler(
                filename = function() {
                    paste("Distance_", type, "_", method(), ".pdf", sep = "")
                },
                content = function(file) {
                    pdf(file)
                    p = p_dist()
                    draw(p)
                    dev.off()
                }
            )
            
            ## plot of sum of distances
            p_sumDist <- reactive({
                sumDistSample(d(), title = "")
            })
            
            output$distSampleSum <- renderPlotly({
                p_sumDist()
            })

            output$downloadPlotSum <- downloadHandler(
                filename = function() {
                    paste("Sum_distance_", type, "_", 
                        method(), ".html", sep = "")
                },
                content = function(file) {
                    saveWidget(p_sumDist(), file)
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
#' @noRd
tP_featureUI <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Features",
        box(title = "", width = 12, collapsible = TRUE, collapsed = FALSE,
            plotOutput(outputId = ns("Features")) %>% 
                helper(content = "tabPanel_Features"),
            column(6, downloadButton(outputId = ns("downloadPlot"), "")),
            column(6, uiOutput(outputId = ns("selectFeatureUI")))
        ),
        box(title = "", width = 12, collapsible = TRUE, collapsed = FALSE,
            plotlyOutput(outputId = ns("FeaturesCV")),
            column(6, downloadButton(outputId = ns("downloadPlotCV"), "")),
            column(6, checkboxInput(inputId = ns("FeatureLines"),
                label = "lines", value = FALSE))
        ),
        radioButtons(inputId = ns("mode"), label = "Select features",
            choices = list("all" = "all", "exclude" = "exclude", 
                                                        "select" = "select")),
        uiOutput(outputId = ns("excludeFeaturesUI"))
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
#' @importFrom ComplexHeatmap draw
#' @noRd
featureServer <- function(id, se, a, a_n, a_t, a_b, a_i, missingValue) {
    
    moduleServer(
        id, 
        function(input, output, session) {
            
            output$selectFeatureUI <- renderUI({
                selectInput(inputId = session$ns("selectFeature"), 
                                                    label = "Select feature",
                    choices = as.list(rownames(a())))
            })
            
            output$excludeFeaturesUI <- renderUI({
                selectInput(inputId = session$ns("excludeFeature"),
                            label = NULL, choices = rownames(se),
                            multiple = TRUE)
            })
            
            l_assays <- reactive({
                req(a_i())
                if (missingValue) {
                    l <- list(raw = a(), normalized = a_n(), 
                        transformed = a_t(), `batch.corrected` = a_b(),
                        imputed = a_i())
                } else {
                    l <- list(raw = a(), normalized = a_n(),
                        transformed = a_t(), `batch.corrected` = a_b())
                }
                l
            })
            
            df_feature <- reactive({
                createDfFeature(l_assays(), feature = input$selectFeature)
            })
                
            p_feature <- reactive({
                featurePlot(df_feature())
            })
            
            output$Features <- renderPlot({
                p_feature()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function() {
                    paste("Features_", input$selectFeature, ".pdf", sep = "")
                },
                content = function(file) {
                    ggsave(file, p_feature(), device = "pdf")
                }
            )
            
            output$FeaturesCV <- renderPlotly({
                cvFeaturePlot(l_assays(), lines = input$FeatureLines)
            })
            
            output$downloadPlotCV <- downloadHandler(
                filename = function() {
                    paste("Features_CV_lines_", input$FeatureLines, 
                                                            ".html", sep = "")
                },
                content = function(file) {
                    saveWidget(
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
#' @noRd
tP_values_all <- function() {
    tabPanel("Values",
        tabBox(title = "", width = 12,
            tP_boxplotUI(id = "boxUI"),
            tP_driftUI(id = "drift"),
            tP_cvUI(id = "cv"),
            tP_meanSdUI(),
            tP_maUI(id = "MA"),
            tP_ECDFUI(id = "ECDF"),
            tP_distUI(),
            tP_featureUI(id = "features")
        )
    )
}
