#' @importFrom stats prcomp
#' @importFrom ape pcoa
#' @importFrom MASS isoMDS
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom tibble as_tibble 
#' @importFrom SummarizedExperiment SummarizedExperiment

## function dimensionReduction
test_that("dimensionReduction", {
    set.seed(1)
    x <- matrix(rnorm(100000), nrow = 1000, ncol = 100, 
        dimnames = list(1:1000, paste("sample", 1:100)))
    x[, 1:25] <- x[, 1:25] - 50
    x[, 26:50] <- x[, 26:50] - 25
    x[, 51:75] <- x[, 51:75] + 25
    x[, 76:100] <- x[, 76:100] + 50
    #x <- x + rnorm(10000)
    x_foo <- x
    x_foo[1, 3] <- NA
    
    parameters <- list(
        "center" = TRUE, "scale" = FALSE, ## PCA
        "method" = "euclidean", ## PCoA and NMDS
        "perplexity" = 3, "max_iter" = 1000, "initial_dims" = 2, ## tSNE
        "dims" = 2, "pca_center" = TRUE, "pca_scale" = FALSE, theta = 0, ## tSNE
        "min_dist" = 0.1, "n_neighbors" = 5, "spread" = 1) ## UMAP
    
    pca_o <- suppressWarnings(
        dimensionReduction(x, "PCA", params = parameters))
    pcoa_o <- dimensionReduction(x, type = "PCoA", params = parameters)
    suppressWarnings(
        nmds_o <- dimensionReduction(x, "NMDS", params = parameters))
    set.seed(1)
    tsne_o <- dimensionReduction(x, "tSNE", params = parameters)
    umap_o <- dimensionReduction(x, "UMAP", params = parameters)
    
    pca_r <- stats::prcomp(t(x), center = TRUE, scale = FALSE)$x 
    pca_r <- tibble::as_tibble(pca_r)
    pcoa_r <- cmdscale(dist(t(x), method = "euclidean"), k = ncol(x) - 1, 
        eig = FALSE)
    colnames(pcoa_r) <- paste("Axis.", seq_len(99), sep = "")
    pcoa_r <- tibble::as_tibble(pcoa_r)
    suppressWarnings(
        nmds_r <- MASS::isoMDS(dist(t(x), method = "euclidean"), k = 2)$points)
    nmds_r <- tibble::as_tibble(nmds_r)
    colnames(nmds_r) <- c("MDS1", "MDS2")
    tsne_r <- Rtsne::Rtsne(t(x), perplexity = 3, max_iter = 1000,
        initial_dims = 2, dims = 2, pca_center = TRUE, pca_scale = FALSE,
        theta = 0)$Y
    tsne_r <- tibble::as_tibble(tsne_r)
    colnames(tsne_r) <- c("X1", "X2")
    umap_r <- umap::umap(t(x), method = "naive", min_dist = 0.1, 
        n_neighbors = 5, spread = 1)$layout
    umap_r <- tibble::as_tibble(umap_r)
    colnames(umap_r) <- c("X1", "X2")
    
    expect_error(dimensionReduction(x, type = "foo", params = parameters), 
        "'arg' should be one of ")
    suppressWarnings(expect_error(
        dimensionReduction(x_foo, type = "PCA", params = parameters), 
        "infinite or missing values in 'x'"))
    expect_error(dimensionReduction(x_foo, type = "tSNE", params = parameters), 
        "missing values in object")
    expect_error(dimensionReduction(x_foo, type = "UMAP", params = parameters), 
        "missing value where TRUE/FALSE needed")
    expect_true(tibble::is_tibble(pca_o[[1]]))
    expect_true(tibble::is_tibble(pcoa_o[[1]]))
    expect_true(tibble::is_tibble(nmds_o[[1]]))
    expect_true(tibble::is_tibble(tsne_o[[1]]))
    expect_true(tibble::is_tibble(umap_o[[1]]))
    expect_true(is(pca_o[[2]], "prcomp"))
    expect_equal(names(pca_o[[2]]), 
        c("sdev", "rotation", "center", "scale", "x"))
    expect_true(is(pcoa_o[[2]], "list"))
    expect_equal(names(pcoa_o[[2]]), c("points", "eig", "x", "ac", "GOF"))
    expect_true(is(nmds_o[[2]], "list"))
    expect_equal(names(nmds_o[[2]]), c("points", "stress"))
    expect_true(is(tsne_o[[2]], "Rtsne"))
    expect_equal(names(tsne_o[[2]]), 
        c("N", "Y", "costs", "itercosts", "origD", "perplexity", "theta",
            "max_iter", "stop_lying_iter", "mom_switch_iter", "momentum",
            "final_momentum", "eta", "exaggeration_factor"))
    expect_true(is(umap_o[[2]], "umap"))
    expect_equal(names(umap_o[[2]]), c("layout", "data", "knn", "config"))
    expect_equal(dim(pca_o[[1]]), c(100, 101))
    expect_equal(dim(pcoa_o[[1]]), c(100, 100))
    expect_equal(dim(nmds_o[[1]]), c(100, 3))
    expect_equal(dim(tsne_o[[1]]), c(100, 3))
    expect_equal(dim(umap_o[[1]]), c(100, 3))
    
    ## the last column that is removed refers to the column "name"
    expect_equal(pca_o[[1]][, -101], pca_r, tolerance = 1e-07)
    expect_equal(pcoa_o[[1]][, -100], pcoa_r, tolerance = 1e-07)
    expect_equal(nmds_o[[1]][, -3], nmds_r, tolerance = 1e00)
    expect_equal(tsne_o[[1]][, -3], tsne_r, tolerance = 1e01)
    expect_equal(umap_o[[1]][, -3], umap_r, tolerance = 1e01)
    
    cols <- paste("sample", 1:100)
    expect_equal(pca_o[[1]][["name"]], cols)
    expect_equal(pcoa_o[[1]][["name"]], cols)
    expect_equal(nmds_o[[1]][["name"]], cols)
    expect_equal(tsne_o[[1]][["name"]], cols)
    expect_equal(umap_o[[1]][["name"]], cols)
})

## function dimensionReductionPlot
test_that("dimensionReductionPlot", {
     
    ## create se
    a <- matrix(1:100, nrow = 10, ncol = 10, 
                 dimnames = list(1:10, paste("sample", 1:10)))
    set.seed(1)
    a <- a + rnorm(100)
    cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
    rD <- data.frame(spectra = rownames(a))
    se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
        rowData = rD, colData = cD)
    se_error <- se
    colnames(se_error@colData)[1] <- "name"
    
    ## create the data.frame containing the transformed values
    parameters <- list("center" = TRUE, "scale" = FALSE)
    tbl <- dimensionReduction(SummarizedExperiment::assay(se), 
        type = "PCA", params = parameters)[[1]]
    g <- dimensionReductionPlot(tbl = tbl, se = se, highlight = "type", 
        x_coord = "PC1", y_coord = "PC2")
    
    expect_error(dimensionReductionPlot(tbl = tbl), 'argument "se" is missing')
    expect_error(dimensionReductionPlot(tbl = tbl, se = se), 
        'argument "x_coord" is missing')
    expect_error(dimensionReductionPlot(tbl = tbl, se = se,
        highlight = "none"), 'argument "x_coord" is missing')
    expect_error(dimensionReductionPlot(tbl = tbl, se = se, highlight = "none", 
        x_coord = "PC1"), 'argument "y_coord" is missing')
    expect_error(dimensionReductionPlot(tbl = tbl, se = se, highlight = "none", 
        x_coord = "test", y_coord = "PC2"), 
        "object 'test' not found")
    expect_error(dimensionReductionPlot(tbl = tbl[, -11], se = se, highlight = "none", 
        x_coord = "PC1", y_coord = "PC2"), "object 'name' not found")
    expect_error(dimensionReductionPlot(tbl = se, se = se), "must be a ")
    expect_error(dimensionReductionPlot(tbl = tbl, se = "foo"), 
        "trying to get slot")
    expect_error(dimensionReductionPlot(tbl = tbl, se = se, highlight = "foo"), 
        "should be one of")
    expect_is(g, "plotly")
})

## function coordsUI
## requires reactive environment

## function explVar
test_that("explVar", {
    x <- matrix(1:100, nrow = 10, ncol = 10, 
                dimnames = list(1:10, paste("sample", 1:10)))
    set.seed(1)
    x <- x + rnorm(100)
    
    pca <- dimensionReduction(x = x, params = list(center = TRUE, scale = TRUE), 
        type = "PCA")[[2]]
    pcoa <- dimensionReduction(x = x, params = list(method = "euclidean"), 
        type = "PCoA")[[2]]
    
    varExpl_pca <- explVar(d = pca, type = "PCA")
    varExpl_pcoa <- explVar(d = pcoa, type = "PCoA")
    
    expect_error(explVar(d = NA, type = "PCA"), 
        "subscript out of bounds")
    expect_error(explVar(d = NA, type = "PCoA"), 
        "subscript out of bounds")
    suppressWarnings(expect_error(explVar(d = list(), type = "PCA"), 
        "must be the same length as the vector"))
    suppressWarnings(expect_error(explVar(d = list(), type = "PCoA"), 
        "must be the same length as the vector"))
    expect_equal(as.numeric(varExpl_pca), 
        c(9.992575e-01, 2.675309e-04, 2.275137e-04, 1.158747e-04, 5.881982e-05,
            4.652981e-05, 1.865835e-05, 6.885796e-06, 6.705589e-07,
            2.006033e-34),
        tolerance = 1e-07)
    expect_equal(as.numeric(varExpl_pcoa), 
        c(9.992597e-01, 2.673699e-04, 2.267327e-04, 1.145103e-04, 5.908631e-05,
            4.653893e-05, 1.855159e-05, 6.844296e-06, 6.848357e-07),
        tolerance = 1e-07)
    expect_equal(names(varExpl_pca), paste("PC", 1:10, sep = ""))
    expect_equal(names(varExpl_pcoa), paste("Axis.", 1:9, sep = ""))
})

## function permuteExplVar
test_that("permuteExplVar", {
    x <- matrix(1:100, nrow = 10, ncol = 10, 
                dimnames = list(1:10, paste("sample", 1:10)))
    set.seed(1)
    x <- x + rnorm(100)
    x_foo <- x
    x_foo[1, 3] <- NA
    
    varExplPerm <- permuteExplVar(x, n = 10, center = TRUE, scale = TRUE)
    expect_error(permuteExplVar(x_foo, n = 10, center = TRUE, scale = TRUE),
        "infinite or missing values in 'x'")
    expect_error(permuteExplVar(x, n = "", center = TRUE, scale = TRUE),
        "n has to be greater than 0")
    expect_error(permuteExplVar(x, n = 0, center = TRUE, scale = TRUE),
        "n has to be greater than 0")
    expect_error(permuteExplVar(x, n = 10, center = "", scale = TRUE),
        "length of 'center' must equal the number of columns of 'x'")
    expect_error(permuteExplVar(x, n = 10, center = TRUE, scale = ""),
        "length of 'scale' must equal the number of columns of 'x'")
    expect_equal(as.numeric(rowSums(varExplPerm)), rep(1, 10), tolerance = 1e-05)
    expect_equal(dim(varExplPerm), dim(x))
    expect_equal(colnames(varExplPerm), paste("PC", 1:10, sep = ""))
})

## function plotPCAVar
test_that("plotPCAVar", {
    x <- matrix(1:100, nrow = 10, ncol = 10, 
        dimnames = list(1:10, paste("sample", 1:10)))
    set.seed(1)
    x <- x + rnorm(100)
    pca <- dimensionReduction(x, params = list(center = TRUE, scale = TRUE), 
        type = "PCA")[[2]]
    var_x <- explVar(d = pca, type = "PCA")
    var_perm <- permuteExplVar(x, n = 100, center = TRUE, scale = TRUE)
    g <- plotPCAVar(var_x, var_perm)
    
    expect_error(plotPCAVar(NULL, NULL), 
        "arguments imply differing number of rows")
    expect_error(plotPCAVar(var_x, var_x),
        "'x' must be an array of at least two dimensions")
    expect_is(g, "gg")
})

## function plotPCAVarPvalue
test_that("plotPCAVarPvalue", {
    x <- matrix(1:100, nrow = 10, ncol = 10, 
                dimnames = list(1:10, paste("sample", 1:10)))
    set.seed(1)
    x <- x + rnorm(100)
    pca <- dimensionReduction(x, params = list(center = TRUE, scale = TRUE), 
        type = "PCA")[[2]]
    var_x <- explVar(d = pca, type = "PCA")
    var_perm <- permuteExplVar(x, n = 100, center = TRUE, scale = TRUE)
    g <- plotPCAVarPvalue(var_x, var_perm)
    
    expect_error(plotPCAVarPvalue(NULL, NULL), "argument is not a matrix")
    expect_error(plotPCAVarPvalue(var_x, var_x), 
        "arguments imply differing number of rows")
    expect_is(g, "gg")
})

## function tblPCALoadings
test_that("tblPCALoadings", {
    x <- matrix(1:100, nrow = 10, ncol = 10, 
                dimnames = list(1:10, paste("sample", 1:10)))
    params <- list(center = TRUE, scale = TRUE)
    tbl <- tblPCALoadings(x = x, params = params)
    expect_is(tbl, "tbl")
    expect_equal(dim(tbl), c(10, 11))
    expect_equal(tbl$name, as.character(1:10))
    expect_equal(colnames(tbl), c(paste0("PC", 1:10), "name"))
    expect_equal(tbl$PC1, rep(0.316, 10), tolerance = 1e-3)
    expect_equal(tbl$PC2, c(-0.9486833, rep(0.1054093, 9)), tolerance = 1e-3)
    expect_error(tblPCALoadings(x = "foo", params = params), "must be numeric")
    expect_error(tblPCALoadings(x = x, params = "foo"), 
        "argument is not interpretable as logical")
})

## function plotPCALoadings
test_that("plotPCALoadings", {
    x <- matrix(1:100, nrow = 10, ncol = 10, 
                dimnames = list(1:10, paste("sample", 1:10)))
    params <- list(center = TRUE, scale = TRUE)
    tbl <- tblPCALoadings(x = x, params = params)
    expect_is(plotPCALoadings(tbl, "PC1", "PC2"), "plotly")
    expect_error(plotPCALoadings(NULL, "PC1", "PC2"), "object 'PC1' not found")
    expect_error(plotPCALoadings(tbl, "foo", "PC2"), "object 'foo' not found")
    expect_error(plotPCALoadings(tbl, "PC1", "foo"), "object 'foo' not found")
})

