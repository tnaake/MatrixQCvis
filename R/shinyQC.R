#' @name shinyQC
#'
#' @title Shiny application for initial QC exploration of -omics data sets
#'
#' @description
#' The shiny application allows to explore -omics
#' data sets especially with a focus on quality control. `shinyQC` gives
#' information on the type of samples included (if this was previously
#' specified within the `SummarizedExperiment` object). It gives information
#' on the number of missing and measured values across features and across
#' sets (e.g. quality control samples, control, and treatment groups, only
#' displayed for `SummarizedExperiment` objects that contain missing values).
#' `shinyQC` includes functionality to display (count/intensity) values 
#' across samples (to detect drifts in intensity values during the 
#' measurement), to display
#' mean-sd plots, MA plots, ECDF plots, and distance plots between samples.
#' `shinyQC` includes functionality to perform dimensionality reduction
#' (currently limited to PCA, PCoA, NMDS, tSNE, and UMAP). Additionally,
#' it includes functionality to perform differential expression analysis
#' (currently limited to moderated t-tests and the Wald test).
#'
#' @details
#' `shinyQC` allows to subset the supplied `SummarizedExperiment` object. On
#' exit of the shiny application, the following objects are returned in a
#' list: the matrix with (count/intensity) values for `raw`, `normalized`, 
#' `transformed`, `batch corrected` (and `imputed`). The object will 
#' only returned if `app_server = FALSE`.
#' 
#' If the `se` argument is omitted the app will load an interface that allows 
#' for data upload.
#'
#' @param se `SummarizedExperiment` object (can be omitted)
#' @param app_server `logical` (set to `TRUE` if run under a server environment)
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shinyhelper observe_helpers helper
#' @importFrom plotly renderPlotly ggplotly style plotlyOutput
#' @importFrom grDevices hcl.colors
#' @importFrom methods formalArgs
#' @importFrom stats as.formula dist formula ks.test model.matrix prcomp 
#' @importFrom stats quantile reformulate sd setNames
#' @importFrom vsn vsn2 meanSdPlot
#' @importFrom SummarizedExperiment assay colData SummarizedExperiment 
#'
#' @examples 
#' library(dplyr)
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
#' \donttest{shinyQC(se)}
#' 
#' @author Thomas Naake
#' 
#' @return `list` with matrices
#'
#' @export
shinyQC <- function(se, app_server = FALSE) {
    
    has_se <- !missing(se)
    if (has_se) {
        if (is(se) != "SummarizedExperiment") 
            stop("se is not of class 'SummarizedExperiment'")
        if (!("name" %in% colnames(colData(se))))
            stop("column 'name' not found in colData(se)")
        
        ## retrieve the names of `assays(se)` and return a character for 
        ## choices in the selectInput UI that allows for switching between 
        ## the different assays
        choicesAssaySE <- choiceAssaySE(se)
    } else {
        choicesAssaySE <- NULL
        se <- NULL
    }

    if (app_server) {
        host <- getOption("shiny.host", "0.0.0.0")
    } else {
        host <- getOption("shiny.host", "127.0.0.1")
    }
    
    ## assign function for landing page
    landingPage = createLandingPage()

    ## define UI
    ui <- shinyUI(dashboardPage(skin = "black",
        dashboardHeader(title = "MatrixQCvis"),
        dashboardSidebar(
            #fileInput("upload", "Upload...")
            ## Sidebar with a slider input
            useShinyjs(debug = TRUE),
            hidden(
                div(id = "sidebarPanelSE",
                    tag_loadMessage(),
                    tag_keepAlive(),
                    ## sidebar for tabs 'Values' and 'Dimension Reduction'
                    ## for normalizing, transforming, batch correcting and 
                    ## imputing data
                    sidebar_assayUI(),
                    sidebar_imputationUI(),
                    ## create sidebar for tab 'DE' (input for model 
                    ## matrix/contrasts) sidebar_UI()
                    sidebar_DEUI(), 
                    ## sidebar for excluding samples from se_r, generating 
                    ## report and exporting data set
                    sidebar_excludeSampleUI(id = "select"), 
                    sidebar_reportUI(),
                    hidden(
                        div(id = "sidebarStop",
                            sidebar_stopUI(app_server = app_server))
                    ),
                    ## sidebar for selecting assay in multi-assay 
                    ## SummarizedExperiment
                    sidebar_selectAssayUI(choicesAssaySE = choicesAssaySE)
            ))
            
        ),

        dashboardBody(fluidRow(
            tags$head( 
                tags$script(
                    type="text/javascript",'$(document).ready(function(){
                    $(".main-sidebar").css("height","100%");
                    $(".main-sidebar .sidebar").css({"position":"relative","max-height": "100%","overflow": "auto"})
                    })')),
            useShinyjs(debug = TRUE),
            hidden(
                div(id = "tabPanelSE",
                    tabsetPanel(type = "tabs",
                        ## tabPanel for tab "Samples"
                        tP_samples_all(),
                        ## tabPanel for tab "Values"
                        tP_values_all(),
                        ## tabPanel for tab "Dimension Reduction"
                        tP_dimensionReduction_all(),
                        ## tabPanel for tab "DE"
                        tP_DE_all(),
                    id = "tabs") ## end tabsetPanel
                )
            )
        ),
        div(id = "uploadSE", 
            uiOutput("allPanels")
        ), ###########################################################

    )))

    ## define server function
    server <- function(input, output, session) {
        
        if (!has_se) {
            FUN <- function(SE, MISSINGVALUE) {
                .initialize_server(se = SE, input = input, output = output, 
                    session = session, missingValue = MISSINGVALUE)
            }
            landingPage(FUN, input = input, output = output, session = session, 
                app_server = app_server)
        } else {
            missingValue <- missingValuesSE(se)
            ## tabPanel for tab "Measured Values"
            if (missingValue) insertTab(inputId = "tabs", tP_meV_all(),
                target = "Samples", position = "after")
            ## tabPanel for tab "Missing Values"
            if (missingValue) insertTab(inputId = "tabs", tP_miV_all(),
                target = "Measured Values", position = "after")
            show("tabPanelSE")
            show("sidebarPanelSE")
            if (!app_server) show("sidebarStop")
            .initialize_server(se = se, input = input, output = output, 
                session = session, missingValue = missingValue)
        }
        
        #output$text <- renderText(input$launch)
        
    } ## end of server

    ## run the app
    app <- list(ui = ui, server = server)
    
    runApp(app, host = host, launch.browser = !app_server, port = 3838)
}

#' @name .initialize_server
#' 
#' @title Server initialization of `shinyQC`
#' 
#' @description 
#' The function `.initialize_server` defines most of the server function in 
#' `shinyQC`. Within the server function of `shinyQC`, `.initialize_server` is
#' called in different context depending if the `se` was assigned or not. 
#' 
#' @param se `SummarizedExperiment`
#' @param input `shiny` input object
#' @param output `shiny` output object
#' @param session `shiny` session object
#' 
#' @return 
#' Observers and reactive server expressions for all app elements
#' 
#' @importFrom SummarizedExperiment assays
#' @importFrom shiny downloadHandler
#' @importFrom rmarkdown render
#' 
#' @author Thomas Naake
#' 
#' @noRd
.initialize_server <- function(se, input, output, session, 
                                                        missingValue = TRUE) {
    
    output$keepAlive <- renderText({
        req(input$keepAlive)
        paste("keep alive", input$keepAlive)
    })
    
    output$missingVals <- renderText({missingValue})
    outputOptions(output, "missingVals", suspendWhenHidden = FALSE)
    
    ## create server to select assay in multi-assay se
    output$lengthAssays <- renderText({
        if (length(assays(se)) > 1) {
            "TRUE"
        } else {
            "FALSE"
        }
    })
    
    ## set suspendWhenHidden to FALSE to retrieve lengthAssays
    ## even if it is not called explicitly (e.g. by renderText)
    outputOptions(output, "lengthAssays", suspendWhenHidden = FALSE)
    
    se_sel <- selectAssayServer("select", se = se, 
        selected = reactive(input$assaySelected))
    
    se_feat <- reactive({
        selectFeatureSE(se_sel(), 
            selection = input[["features-excludeFeature"]], 
            mode = input[["features-mode"]])
    })
    
    sidebar_excludeSampleServer("select", se = se)
    
    ## uses 'helpfiles' directory by default
    ## we use the withMathJax parameter to render formulae
    observe_helpers(withMathJax = TRUE,
        help_dir = paste(find.package("MatrixQCvis"), "helpfiles", sep = "/"))
    
    ## create reactive SummarizedExperiment objects for raw, normalized, 
    ## transformed and imputed data
    se_r <- reactive({selectSampleSE(se = se_feat(), 
        selection = input[["select-excludeSamples"]], 
        mode = input[["select-mode"]])})
    
    ## create SummarizedExperiment objects with updated assays
    se_r_n <- reactive({updateSE(se = se_r(), assay = a_n())})
    se_r_t <- reactive({updateSE(se = se_r(), assay = a_t())})
    se_r_b <- reactive({updateSE(se = se_r(), assay = a_b())})
    se_r_i <- reactive({updateSE(se = se_r(), assay = a_i())})
    
    ## TAB: Samples
    ## barplot about number for sample type
    histSampleServer("Sample_hist", se = se_r)
    mosaicSampleServer("Sample_mosaic", se = se_r)
    
    ## TAB: Measured values and Missing values
    ## barplot number of measured/missing features per sample
    samples_memi_tbl <- sampleMeMiServer("MeMiTbl", se = se_r)
    barplotMeMiSampleServer("MeV_number", samples_memi = samples_memi_tbl, 
                                                            measured = TRUE)
    barplotMeMiSampleServer("MiV_number", samples_memi = samples_memi_tbl, 
                                                            measured = FALSE)
    
    ## sync input[["MeV-categoryHist"]] with input[["MeV-categoryUpSet"]]
    observe({
        input[["MeV-categoryHist"]]
        ## update upon change of MeV-categoryHist MeV-categoryUpSet to the
        ## value of MeV-categoryHist
        updateCheckboxInput(session, "MeV-categoryUpSet", NULL, 
            input[["MeV-categoryHist"]])
    })
    observe({
        input[["MeV-categoryUpSet"]]
        ## update upon change of MeV-categoryUpSet MeV-categoryHist to the
        ## value of MeV-categoryUpSet
        updateCheckboxInput(session, "MeV-categoryHist", NULL, 
            input[["MeV-categoryUpSet"]])
    })
    
    ## sync input[["MiV-categoryHist"]] with input[["MiV-categoryUpSet"]]
    observe({
        input[["MiV-categoryHist"]]
        ## update upon change of MiV-categoryHist MiV-categoryUpSet to the
        ## value of MiV-categoryHist
        updateCheckboxInput(session, "MiV-categoryUpSet", NULL, 
            input[["MiV-categoryHist"]])
    })
    observe({
        input[["MiV-categoryUpSet"]]
        ## update upon change of MiV-categoryUpSet MiV-categoryHist to the
        ## value of MiV-categoryUpSet
        updateCheckboxInput(session, "MiV-categoryHist", NULL, 
            input[["MiV-categoryUpSet"]])
    })
    
    ## tab: Histogram Features
    ## histogram for measured values across samples per feature
    histFeatServer("MeV", se = se_r, assay = a, measured = TRUE)
    histFeatServer("MiV", se = se_r, assay = a, measured = FALSE)
    
    ## tab: Histogram Features along variable (e.g. sample type)
    histFeatCategoryServer("MeV", se = se_r, measured = TRUE)
    histFeatCategoryServer("MiV", se = se_r, measured = FALSE)
    
    ## tab: UpSet (UpSet plot with set of measured features)
    upSetServer("MeV", se = se_r, measured = TRUE)
    upSetServer("MiV", se = se_r, measured = FALSE)
    
    ## tab: Sets
    setsServer("MeV", se = se_r, measured = TRUE)
    setsServer("MiV", se = se_r, measured = FALSE)
    
    
    ## TAB: Values and Dimension reduction plots
    
    ## create reactive for assay slot
    a <- reactive({assay(se_r())})
    
    ## reactive expression for data transformation, returns a matrix with
    ## normalized values
    a_n <- reactive({
        req(a(), input$normalization)
        ## input$normalization is either "none", "sum", "quantile division",
        ## "quantile"
        normalize(a(), method = input$normalization, probs = input$quantile)
    })
    
    output$quantDiv <- renderUI({
        sliderInput("quantile", label = "Quantile",
            min = 0, max = 1, value = 0.75)
    })
    
    ## reactive expression for data transformation, returns a matrix with
    ## transformed values
    a_t <- reactive({
        req(input$transformation, a_n())
        
        ## input$transformation is either "none", "log2", or "vsn"
        transform(a_n(), method = input$transformation)
    })
    
    ## reactive expression for data transformation, returns a matrix with
    ## transformed values
    a_b <- reactive({
        req(input$batch, a_t()) 
        
        ## input$batch is either "none" or "removeBatchEffect (limma)"
        batch(se_r_t(), method = input$batch, batchColumn = input$batchCol)
    })
    
    output$batchCol <- renderUI({
        selectInput("batchCol", 
            label = "Select column containing batch information",
            choices = colnames(colData(se)))
    })
    
    observeEvent({req(input$batch); input$batch}, {
        if (input$tabs == "Values" & input$batch != "none") {
            showModal(modalDialog(
                "It seems you have applied a batch correction method in the 'Values' tab.",
                "Please make sure to assess the existence and strength of the batch effect before and after applying the batch correction method.",
                "The most informative plots are the dimension reduction plots.",
                title = "Attention!", easyClose = TRUE))
        }
    })
    
    ## reactive expression for data imputation, returns a matrix with
    ## imputed values
    a_i <- reactive({
        req(input$imputation, a_b())
        if (missingValue) {
            ## impute missing values of  the data.frame with transformed values
            impute(a_b(), input$imputation)    
        } else {
            a_b()
        }
    })
    
    ## TAB: Values
    ## boxplots
    boxPlotUIServer("boxUI", missingValue = missingValue)
    boxPlotServer("boxRaw", assay = a, boxLog = reactive(input$boxLog),
        violin = reactive(input[["boxUI-violinPlot"]]), type = "raw")
    boxPlotServer("boxNorm", assay = a_n, boxLog = reactive(input$boxLog),
        violin = reactive(input[["boxUI-violinPlot"]]), type = "normalized")
    boxPlotServer("boxTransf", assay = a_t, boxLog = function() FALSE,
        violin = reactive(input[["boxUI-violinPlot"]]), type = "transformed")
    boxPlotServer("boxBatch", assay = a_b, boxLog = function() FALSE,
        violin = reactive(input[["boxUI-violinPlot"]]), type = "transformed")
    boxPlotServer("boxImp", assay = a_i, boxLog = function() FALSE,
        violin = reactive(input[["boxUI-violinPlot"]]), type = "imputed")
        
    ## drift
    driftServer("drift", se = se_r, se_n = se_r_n, se_t = se_r_t, 
        se_b = se_r_b, se_i = se_r_i, missingValue = missingValue)
    
    ## coefficient of variation
    cvServer("cv", a_r = a, a_n = a_n, a_t = a_t, a_b = a_b, a_i = a_i,
        missingValue = missingValue)
    
    ## mean-sd plot
    meanSdUIServer("meanSd", missingValue = missingValue)
    meanSdServer("meanSdTransf", assay = a_t, type = "transformed")
    meanSdServer("meanSdBatch", assay = a_b, type = "batch corrected")
    meanSdServer("meanSdImp", assay = a_i, type = "imputed")
    
    ## MA plot
    maServer("MA", se = se_r, se_n = se_r_n, se_t = se_r_t,
        se_b = se_r_b, se_i = se_r_i, innerWidth = reactive(input$innerWidth),
        missingValue = missingValue)
    
    ## ECDF
    ECDFServer("ECDF", se = se_r, se_n = se_r_n, se_t = se_r_t,
        se_b = se_r_b, se_i = se_r_i, missingValue = missingValue)
    
    ## distances
    distUIServer("distUI", missingValue = missingValue)
    distServer("distUI-distRaw", se = se_r, assay = a,
        method = reactive(input$methodDistMat), 
        label = reactive(input$groupDist), type = "raw")
    distServer("distNorm", se = se_r, assay = a_n,
        method = reactive(input$methodDistMat), 
        label = reactive(input$groupDist), type = "normalized")
    distServer("distTransf", se = se_r, assay = a_t, 
        method = reactive(input$methodDistMat),
        label = reactive(input$groupDist), type = "transformed")
    distServer("distBatch", se = se_r, assay = a_b,
        method = reactive(input$methodDistMat), 
        label = reactive(input$groupDist), type = "batch corrected")
    distServer("distImp", se = se_r, assay = a_i,
        method = reactive(input$methodDistMat), 
        label = reactive(input$groupDist), type = "imputed")
    
    output$groupDistUI <- renderUI({
        selectInput(inputId = "groupDist", label = "annotation",
            choices = colnames(colData(se)))
    })
    
    ## Features
    featureServer("features", se = se, a = a, a_n = a_n, a_t = a_t, a_b = a_b,
        a_i = a_i, missingValue = missingValue)
    
    ## TAB: Dimension reduction
    ## observe handlers to sync "scale" and "center" between the 'PCA' and
    ## 'tSNE' tab within the 'Dimension reduction' tab
    observe({
        input[["PCA-scale"]]
        ## update upon change of PCA-scale tSNE-scale to the value of
        ## PCA-scale
        updateCheckboxInput(session, "tSNE-scale", NULL, input[["PCA-scale"]])
    })
    observe({
        input[["tSNE-scale"]]
        ## update upon change of tSNE-scale PCA-scale to the value of
        ## tSNE-scale
        updateCheckboxInput(session, "PCA-scale", NULL, input[["tSNE-scale"]])
    })
    observe({
        input[["PCA-center"]]
        ## update upon change of PCA-center tSNE-center to the value of
        ## PCA-center
        updateCheckboxInput(session, "tSNE-center", NULL, input[["PCA-center"]])
    })
    observe({
        input[["tSNE-center"]]
        ## update upon change of tSNE-center PCA-center to the value of
        ## tSNE-center
        updateCheckboxInput(session, "PCA-center", NULL, input[["tSNE-center"]])
    })
    
    ## observe handlers to sync "distance" method between the 'PCoA' and
    ## 'NMDS' tab within the 'Dimension reduction' tab
    observe({
        input[["PCoA-dist"]]
        ## update upon change of PCoA-dist NMDS-dist to the value of
        ## PCoA-dist
        updateCheckboxInput(session, "NMDS-dist", NULL, input[["PCoA-dist"]])
    })
    observe({
        input[["NMDS-dist"]]
        ## update upon change of NMDS-dist PCoA-dist to the value of NMDS-dist
        updateCheckboxInput(session, "PCoA-dist", NULL, input[["NMDS-dist"]])
    })
    
    ## create reactive values that stores the parameters for the dimension
    ## reduction plots
    params <- reactiveValues(
        "center" = TRUE, "scale" = FALSE, ## for PCA
        "method" = "euclidean", ## for PCoA and NMDS
        "perplexity" = 1, "max_iter" = 1000, "initial_dims" = 10, ## for tSNE
        "dims" = 3, "pca_center" = TRUE, "pca_scale" = FALSE, ## for tSNE
        "min_dist" = 0.1, "n_neighbors" = 15, "spread" = 1) ## for UMAP
    
    ## change the reactive values upon the user input changes
    observe({
        params$center <- input[["PCA-center"]]
        params$scale <- input[["PCA-scale"]]
        params$method <- input[["PCoA-dist"]]
        params$perplexity <- input[["tSNE-perplexity"]]
        params$max_iter <- input[["tSNE-maxIter"]]
        params$initial_dims <- input[["tSNE-initialDims"]]
        params$dims <- input[["tSNE-dims"]]
        params$pca_center <- input[["PCA-center"]]
        params$pca_scale <- input[["PCA-scale"]]
        params$min_dist <- input[["UMAP-minDist"]]
        params$n_neighbors <- input[["UMAP-nNeighbors"]]
        params$spread <- input[["UMAP-spread"]]
    })
    
    ## server modules for the dimensional reduction plots
    
    dimRedServer("PCA", se = se_r, assay = a_i, type = "PCA", 
        label = "PC", params = reactive(params), 
        innerWidth = reactive(input$innerWidth))
    dimRedServer("PCoA", se = se_r, assay = a_i, type = "PCoA", 
        label = "axis", params = reactive(params),
        innerWidth = reactive(input$innerWidth))
    dimRedServer("NMDS", se = se_r, assay = a_i, type = "NMDS",
        label = "MDS", params = reactive(params),
        innerWidth = reactive(input$innerWidth))
    dimRedServer("tSNE", se = se_r, assay = a_i, type = "tSNE",
        label = "dimension", params = reactive(params),
        innerWidth = reactive(input$innerWidth))
    tSNEUIServer("tSNE", se = se_r)
    dimRedServer("UMAP", se = se_r, assay = a_i, type = "UMAP",
        label = "axis", params = reactive(params),
        innerWidth = reactive(input$innerWidth))
    umapUIServer("UMAP", se = se_r)
    
    
    ## run additional server modules for the scree plots (only for the
    ## tabs 'PCA' and 'tSNE') and loading plot
    screePlotServer("PCA", assay = a_i,
        center = reactive(input[["PCA-center"]]),
        scale = reactive(input[["PCA-scale"]]))
    loadingsPlotServer("PCA", assay = a_i, params = reactive(params))
    screePlotServer("tSNE", assay = a_i,
        center = reactive(input[["tSNE-center"]]),
        scale = reactive(input[["tSNE-scale"]]))
    
    ## TAB: Differential Expression (DE)
    ## create data.frame with colData of the supplied se
    colDataServer("colData", se = se_r, missingValue = missingValue)
    
    ## check if the supplied formula (input$modelMat) is valid and return
    ## NULL if otherwise
    validFormulaMM <- validFormulaMMServer("modelMatrix", 
        expr = reactive(input$modelMat), 
        action = reactive(input$actionModelMat), se = se_r)
    
    ## create the matrix of the Model Matrix using the validFormulaMM
    modelMatrix <- modelMatrixServer("modelMatrix", se = se_r, 
        validFormulaMM = validFormulaMM)
    
    ## create the data.frame of the Model Matrix to display
    modelMatrixUIServer("modelMatrix", modelMatrix = modelMatrix, 
        validFormulaMM = validFormulaMM, missingValue = missingValue)
    
    ## check if the supplied formula/expr (input$contrastMat) is vald and 
    ## return NULL if otherwise
    validExprContrast <- validExprContrastServer("contrast", 
        expr = reactive(input$contrastMat), 
        action = reactive(input$actionContrasts), modelMatrix = modelMatrix)
    
    ## create the matrix of the Contrast Matrix using the validExprContrast
    contrastMatrix <- contrastMatrixServer("contrast", 
        validExprContrast = validExprContrast, modelMatrix = modelMatrix)
    
    ## create the data.frame of the Contrast Matrix to display
    contrastMatrixUIServer("contrast", validFormulaMM = validFormulaMM, 
        validExprContrast = validExprContrast, contrastMatrix = contrastMatrix,
        missingValue = missingValue)
    
    ## calculate the fit and test results with eBayes (ttest) and 
    ## proDA, cache the results for proDA since it is computationally
    ## expensive
    fit_ttest <- fitServer("ttest", assay = a_i, 
        validFormulaMM = validFormulaMM, modelMatrix = modelMatrix,
        contrastMatrix = contrastMatrix)
    
    fit_proDA <- fitServer("proDA", assay = a_b,
            validFormulaMM = validFormulaMM, modelMatrix = modelMatrix,
            contrastMatrix = contrastMatrix) %>%
        bindCache(a_b(), modelMatrix(), contrastMatrix(), cache = "session")
    
    ## create data.frame with the test results
    testResult <- testResultServer("testServer", 
        type = reactive(input$DEtype), fit_ttest = fit_ttest, 
        fit_proDA = fit_proDA, validFormulaMM = validFormulaMM, 
        validExprContrast = validExprContrast)
    
    
    ## display the test results
    topDEUIServer("topDE", type = reactive(input$DEtype),
        validFormulaMM = validFormulaMM, 
        validExprContrast = validExprContrast, testResult = testResult,
        missingValue = missingValue)
    
    ## create Volcano plot
    volcanoUIServer("volcano", type = reactive(input$DEtype),
        validFormulaMM = validFormulaMM,
        validExprContrast = validExprContrast, testResult = testResult,
        missingValue = missingValue)
    
    ## observer for creating the report
    output$report <- downloadHandler(
        filename = "report_qc.html",
        content = function(file) {
            withProgress(message = "Rendering, please wait!", {
                rep_tmp <- paste(find.package("MatrixQCvis"), 
                    "report/report_qc.Rmd", sep = "/")

                params_l = list(
                    missingValue = missingValue,
                    se_r = se_r(), se_n = se_r_n(), se_t = se_r_t(),
                    se_b = se_r_b(), se_i = se_r_i(),
                    sample_hist = input[["Sample_hist-typeHist"]],
                    sample_mosaic_f1 = input[["Sample_mosaic-mosaicf1"]],
                    sample_mosaic_f2 = input[["Sample_mosaic-mosaicf2"]])
                
                if (missingValue) {
                    params_l <- append(params_l, 
                        list(mev_binwidth = input[["MeV-binwidth"]],
                            mev_binwidthC = input[["MeV-binwidthC"]],
                            mev_hist_category = input[["MeV-categoryHist"]],
                            mev_upset_category = input[["MeV-categoryUpset"]],
                            miv_binwidth = input[["MiV-binwidth"]],
                            miv_binwidthC = input[["MiV-binwidthC"]],
                            miv_hist_category = input[["MiV-categoryHist"]],
                            miv_upset_category = input[["MiV-categoryUpSet"]]))
                } else {
                    params_l <- append(params_l, 
                        list(mev_binwidth = 1, mev_binwidthC = 1,
                            mev_hist_category = NULL, mev_upset_category = NULL,
                            miv_binwidth = 1, miv_binwidthC = 1,
                            miv_hist_category = NULL, 
                            miv_upset_category = NULL))
                }
                params_l <- append(params_l,
                    list(int_log = input[["boxLog"]], 
                        int_violin = input[["violinPlot"]],
                        int_drift_data = input[["drift-data"]],
                        int_drift_aggregation = input[["drift-aggregation"]],
                        int_drift_category = input[["drift-category"]],
                        int_drift_orderCategory = input[["drift-orderCategory"]],
                        int_drift_level = input[["drift-levelSel"]],
                        int_drift_method = input[["drift-method"]],
                        int_ma_data = input[["MA-MAtype"]],
                        int_ma_group = input[["MA-groupMA"]],
                        int_ma_plot = input[["MA-plotMA"]],
                        int_hD_lines = input[["hDLines"]],
                        int_ecdf_data = input[["ECDF-ECDFtype"]],
                        int_ecdf_sample = input[["ECDF-sampleECDF"]],
                        int_ecdf_group = input[["ECDF-groupECDF"]],
                        int_dist_method = input[["methodDistMat"]],
                        int_dist_label = input[["groupDist"]],
                        int_feat_selectFeat = input[["features-selectFeature"]],
                        int_feat_featLine = input[["features-FeatureLines"]],
                        dr_pca_center = params$center,
                        dr_pca_scale = params$scale,
                        dr_pca_highlight = input[["PCA-highlight"]],
                        dr_pca_x = input[["PCA-x"]], 
                        dr_pca_y = input[["PCA-y"]],
                        dr_pcoa_method = params$method,
                        dr_pcoa_highlight = input[["PCoA-highlight"]],
                        dr_pcoa_x = input[["PCoA-x"]], 
                        dr_pcoa_y = input[["PCoA-y"]],
                        dr_nmds_highlight = input[["NMDS-highlight"]],
                        dr_nmds_x = input[["NMDS-x"]], 
                        dr_nmds_y = input[["NMDS-y"]],
                        dr_tsne_perplexity = params$perplexity,
                        dr_tsne_max_iter = params$max_iter,
                        dr_tsne_initial_dims = params$initial_dims,
                        dr_tsne_dims = params$dims,
                        dr_tsne_pca_center = params$pca_center,
                        dr_tsne_pca_scale = params$pca_scale,
                        dr_tsne_highlight = input[["tSNE-highlight"]],
                        dr_tsne_x = input[["tSNE-x"]], 
                        dr_tsne_y = input[["tSNE-y"]],
                        dr_umap_min_dist = params$min_dist,
                        dr_umap_n_neighbors = params$n_neighbors,
                        dr_umap_spread = params$spread,
                        dr_umap_highlight = input[["UMAP-highlight"]],
                        dr_umap_x = input[["UMAP-x"]], 
                        dr_umap_y = input[["UMAP-y"]],
                        de_m_formula = validFormulaMM(),
                        de_c_formula = validExprContrast,
                        de_method = input[["DEtype"]],
                        de_fit_ttest = fit_ttest(),
                        de_fit_proDA = fit_proDA()
                    )
                )
                
                render(input = rep_tmp, output_file = file, params = params_l,
                    envir = new.env(parent=globalenv()))
            })
        }
    )

    ## observer for exiting the app: return the assays
    observeEvent(input$stop, {
        stopApp(
            if (missingValue) {
                list("raw" = a(), "normalized" = a_n(), "transformed" = a_t(),
                        "batch corrected" = a_b(), "imputed" = a_i())
            } else {
                list("raw" = a(), "normalized" = a_n(), "transformed" = a_t(),
                        "batch corrected" = a_b())
            }
        )
    })
}
