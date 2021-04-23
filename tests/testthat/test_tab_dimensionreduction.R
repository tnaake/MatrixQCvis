#' @importFrom stats prcomp
#' @importFrom ape pcoa
#' @importFrom vegan metaMDS
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom tibble as_tibble 
#' @importFrom SummarizedExperiment SummarizedExperiment

## function ordination
test_that("ordination", {
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
    
    pca_o <- ordination(x, "PCA", params = parameters)
    pcoa_o <- ordination(x, type = "PCoA", params = parameters)
    nmds_o <- ordination(x, "NMDS", params = parameters)
    set.seed(1)
    tsne_o <- ordination(x, "tSNE", params = parameters)
    umap_o <- ordination(x, "UMAP", params = parameters)
    
    pca_r <- stats::prcomp(t(x), center = TRUE, scale = FALSE)$x 
    pca_r <- tibble::as_tibble(pca_r)
    pcoa_r <- cmdscale(dist(t(x), method = "euclidean"), k = ncol(x) - 1, 
        eig = FALSE)
    colnames(pcoa_r) <- paste("Axis.", seq_len(99), sep = "")
    pcoa_r <- tibble::as_tibble(pcoa_r)
    nmds_r <- vegan::metaMDS(dist(t(x), method = "euclidean"))$points 
    nmds_r <- tibble::as_tibble(nmds_r)
    tsne_r <- Rtsne::Rtsne(t(x), perplexity = 3, max_iter = 1000,
        initial_dims = 2, dims = 2, pca_center = TRUE, pca_scale = FALSE,
        theta = 0)$Y
    tsne_r <- tibble::as_tibble(tsne_r)
    colnames(tsne_r) <- c("X1", "X2")
    umap_r <- umap::umap(t(x), method = "naive", min_dist = 0.1, 
        n_neighbors = 5, spread = 1)$layout
    umap_r <- tibble::as_tibble(umap_r)
    colnames(umap_r) <- c("X1", "X2")
    
    expect_error(ordination(x, type = "foo", params = parameters), 
        "'arg' should be one of ")
    expect_error(ordination(x_foo, type = "PCA", params = parameters), 
        "infinite or missing values in 'x'")
    expect_error(ordination(x_foo, type = "tSNE", params = parameters), 
        "missing values in object")
    expect_error(ordination(x_foo, type = "UMAP", params = parameters), 
        "missing value where TRUE/FALSE needed")
    expect_true(tibble::is_tibble(pca_o))
    expect_true(tibble::is_tibble(pcoa_o))
    expect_true(tibble::is_tibble(nmds_o))
    expect_true(tibble::is_tibble(tsne_o))
    expect_true(tibble::is_tibble(umap_o))
    expect_equal(dim(pca_o), c(100, 101))
    expect_equal(dim(pcoa_o), c(100, 100))
    expect_equal(dim(nmds_o), c(100, 3))
    expect_equal(dim(tsne_o), c(100, 3))
    expect_equal(dim(umap_o), c(100, 3))
    expect_equal(pca_o[, -1], pca_r, tolerance = 1e-07)
    expect_equal(pcoa_o[, -1], pcoa_r, tolerance = 1e-07)
    expect_equal(nmds_o[, -1], nmds_r, tolerance = 1e00)
    expect_equal(tsne_o[, -1], tsne_r, tolerance = 1e01)
    expect_equal(umap_o[, -1], umap_r, tolerance = 1e01)
})

## function ordinationPlot
test_that("ordinationPlot", {
     
    ## create se
    a <- matrix(1:100, nrow = 10, ncol = 10, 
                 dimnames = list(1:10, paste("sample", 1:10)))
    set.seed(1)
    a <- a + rnorm(100)
    cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
    rD <- data.frame(spectra = rownames(a))
    se <- SummarizedExperiment::SummarizedExperiment(assay = a, 
        rowData = rD, colData = cD)
    
    ## create the data.frame containing the transformed values
    parameters <- list("center" = TRUE, "scale" = FALSE)
    tbl <- ordination(assay(se), type = "PCA", params = parameters)
    g <- ordinationPlot(tbl = tbl, se = se, highlight = "type", 
        x_coord = "PC1", y_coord = "PC2")
    
    expect_error(ordinationPlot(tbl = tbl), 'argument "se" is missing')
    expect_error(ordinationPlot(tbl = tbl, se = se), 
        'argument "x_coord" is missing')
    expect_error(ordinationPlot(tbl = tbl, se = se,
        highlight = "none"), 'argument "x_coord" is missing')
    expect_error(ordinationPlot(tbl = tbl, se = se, highlight = "none", 
        x_coord = "PC1"), 'argument "y_coord" is missing')
    expect_error(ordinationPlot(tbl = tbl, se = se, highlight = "none", 
        x_coord = "test", y_coord = "PC2"), 
        "object 'test' not found")
    expect_error(ordinationPlot(tbl = tbl[, -1], se = se, highlight = "none", 
        x_coord = "PC1", y_coord = "PC2"), "object 'name' not found")
    expect_error(ordinationPlot(tbl = se, se = se), "must be a data frame")
    expect_error(ordinationPlot(tbl = tbl, se = "foo"), 
        "unable to find an inherited method for function")
    expect_error(ordinationPlot(tbl = tbl, se = se, highlight = "foo"), 
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
    
    params <- list(center = TRUE, scale = TRUE, method = "euclidean")
    varExpl_pca <- explVar(x, params = params, type = "PCA")
    varExpl_pcoa <- explVar(x, params = params, type = "PCoA")
    
    expect_error(explVar(NA, params = params, type = "PCA"), 
        "cannot rescale a constant/zero column to unit variance")
    expect_error(explVar(NA, params = params, type = "PCoA"), 
        "missing value where TRUE/FALSE needed")
    suppressWarnings(expect_error(explVar(1:10, params = params, type = "PCA"), 
        "cannot rescale a constant/zero column to unit variance"))
    expect_error(
        explVar(matrix(1, nrow = 10, ncol = 10), params = params, type = "PCA"), 
        "cannot rescale a constant/zero column to unit variance")
    expect_error(
        explVar(x, params = list(center = "", scale = TRUE), type = "PCA"), 
        "length of 'center' must equal the number of columns")
    expect_error(
        explVar(x, params = list(center = TRUE, scale = ""), type = "PCA"), 
        "length of 'scale' must equal the number of columns")
    expect_error(
        explVar(x, params = list(center = TRUE, scale = ""), type = "PCA"), 
        "length of 'scale' must equal the number of columns")
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
    var_x <- explVar(x, params = list(center = TRUE, scale = TRUE), 
        type = "PCA")
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
    var_x <- explVar(x, params = list(center = TRUE, scale = TRUE), 
        type = "PCA")
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
    expect_equal(colnames(tbl), c("name", paste0("PC", 1:10)))
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

