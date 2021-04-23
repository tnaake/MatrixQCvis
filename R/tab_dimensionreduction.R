#' @name ordination
#'
#' @title Dimensionality reduction with ordination methods PCA,
#' PCoA, NMDS, UMAP and tSNE
#'
#' @description 
#' The function `ordination` creates a `data.frame` with the coordinates of the
#' projected data. The function allows for the following projections:
#' Principal Component Analysis (PCA), Principal Coordinates 
#' Analysis/Multidimensional Scaling (PCoA), Non-metric Multidimensional 
#' scaling (NMDS), t-distributed stochastic neighbor embedding (tSNE), and 
#' Uniform Manifold Approimation and Projection (UMAP).
#' 
#' @details 
#' The function `ordination` is a wrapper around the following 
#' functions `stats::prcomp` (PCA), `ape::pcoa` (PCoA), 
#' `vegan::metaMDS` (NMDS), `Rtsne::Rtsne` (tSNE), and `umap::umap` (UMAP).
#' For the function `umap::umap` the method is set to `naive`. 
#' 
#' @param x `matrix`, containing no missing values, samples are in columns and
#' features are in rows
#' @param type `character`, specifying the type/method to use for 
#' dimensionality reduction. One of `PCA`, `PCoA`, `NMDS`, `tSNE`, or `UMAP`. 
#' @param params `list`, arguments/parameters given to the functions 
#' `stats::prcomp`, `stats::dist`, `Rtsne::Rtsne`, `umap::umap`
#' 
#' @examples 
#' x <- matrix(rnorm(1:10000), ncol = 100)
#' rownames(x) <- paste("feature", 1:nrow(x))
#' colnames(x) <- paste("sample", 1:ncol(x))
#' params <- list(method = "euclidean", ## dist
#'     initial_dims = 10, max_iter = 100, dims = 3, perplexity = 3, ## tSNE
#'     min_dist = 0.1, n_neighbors = 15, spread = 1) ## UMAP
#' ordination(x, type = "PCA", params = params)
#' ordination(x, type = "PCoA", params = params)
#' ordination(x, type = "NMDS", params = params)
#' ordination(x, type = "tSNE", params = params)
#' ordination(x, type = "UMAP", params = params)
#'
#' @author Thomas Naake
#'
#' @importFrom stats prcomp dist cmdscale
#' @importFrom vegan metaMDS
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom methods formalArgs
#' 
#' @return `tbl`
#' 
#' @export
ordination <- function(x, 
    type = c("PCA", "PCoA", "NMDS", "tSNE", "UMAP"), params = list()) {
    
    type <- match.arg(type)
    
    if (type == "PCA") { ## scale, center
        params <- append(params, list(x = t(x)))
        d <- do.call(what = stats::prcomp, params)
        tbl <- tibble::as_tibble(d$x)
        tbl <- tibble::tibble(name = rownames(d$x), tbl)
    }
    if (type == "PCoA") {
        params <- append(params, list(x = t(x)))
        ## truncate params
        params <- params[names(params) %in% methods::formalArgs(stats::dist)] 
        d <- do.call(what = stats::dist, args = params)
        d <- cmdscale(d, k = ncol(x) - 1, eig = FALSE)
        colnames(d) <- paste("Axis.", seq_len(ncol(d)), sep = "")
        tbl <- tibble::as_tibble(d)
        tbl <- tibble::tibble(name = rownames(d), tbl)
    }
    if (type == "NMDS") {
        params <- append(params, list(x = t(x)))
        ## truncate params
        params <- params[names(params) %in% formalArgs(dist)] 
        d <- do.call(what = stats::dist, args = params)
        d <- vegan::metaMDS(d)
        tbl <- tibble::as_tibble(d$points)
        tbl <- tibble::tibble(name = rownames(d$points), tbl)
    }
    if (type == "tSNE") { 
        ## hyperparameters for tSNE: perplexity, max_iter, initial_dims, dims
        params <- append(params, list(X = t(x)))
        d <- do.call(what = Rtsne::Rtsne, args = params) 
        d <- d$Y
        colnames(d) <- paste("X", seq_len(ncol(d)), sep = "")
        tbl <- tibble::as_tibble(d)
        tbl <- tibble::tibble(name = colnames(x), tbl)
    }
    if (type == "UMAP") { 
        ## hyperparameters for UMAP: n_neighbors, min_dist, spread
        ## remove first the method from the dist function then add "naive"
        if ("method" %in% names(params)) 
            params <- params[names(params) != "method"]
        params <- append(params, list(d = t(x), method = "naive"))
        d <- do.call(what = umap::umap, args = params)
        d <- d$layout
        colnames(d) <- paste("X", seq_len(ncol(d)), sep = "")
        tbl <- tibble::as_tibble(d)
        tbl <- tibble::tibble(name = rownames(d), tbl)
    }
    
    return(tbl)
}

#' @name ordinationPlot
#' 
#' @title Plot the coordinates from `ordination` values
#' 
#' @description 
#' The function `ordinationPlot` creates a dimension reduction plot. 
#' The function takes as input the `tibble` object obtained 
#' from the `ordination` function. The `tibble` contains transformed
#' values by one of the ordination methods. 
#' 
#' @details 
#' The function `ordinationPlot` is a wrapper for a `ggplot`/`ggplotly` 
#' expression. 
#'
#' @param tbl `tibble` as obtained by the function `ordination`
#' @param se `SummarizedExperiment`
#' @param highlight `character`, one of `"none"` or `colnames(colData(se))`
#' @param explainedVar NULL or named `numeric`, if `numeric` `explainedVar` 
#' contains the explained variance per principal component (names of 
#' `explainedVar` corresponds to the principal components)
#' @param x_coord `character`, column name of `tbl` that stores x coordinates
#' @param y_coord `character`, column name of `tbl` that stores y coordinates
#' @param height `numeric`, specifying the height of the plot (in pixels)
#' 
#' @examples
#' library(SummarizedExperiment)
#' 
#' ## create se
#' a <- matrix(1:100, nrow = 10, ncol = 10, byrow = TRUE,
#'             dimnames = list(1:10, paste("sample", 1:10)))
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = rD, colData = cD)
#' 
#' pca <- ordination(x = assay(se), type = "PCA", params = list())
#' 
#' ordinationPlot(tbl = pca, se = se, highlight = "type", 
#'     x_coord = "PC1", y_coord = "PC2")
#'
#' @author Thomas Naake
#'
#' @importFrom plotly ggplotly
#' @importFrom dplyr left_join
#' 
#' @return `plotly`
#' 
#' @export
ordinationPlot <- function(tbl, se, 
    highlight = c("none", colnames(colData(se))), 
    explainedVar = NULL, x_coord, y_coord, height = 600) {
    
    highlight <- match.arg(highlight)
    
    cD <- colData(se)
    
    if (highlight == "none") {
        tT <- c("text")
    } else {
        cD_cut <- data.frame(name = cD[, "name"], color = cD[, highlight])
        tbl <- dplyr::left_join(tbl, cD_cut, by = "name", copy = TRUE)
        tT <- c("text", "color")
    }
    
    ## use deparse(quote(namesDf)) to convert object name into
    ## character string
    g <- ggplot2::ggplot(tbl, aes_string(x = x_coord, y = y_coord, 
        text = deparse(quote(name)))) 
    
    if (!is.null(explainedVar)) {
        g <- g + ggplot2::xlab(paste(x_coord, " (", 
            round(explainedVar[[x_coord]]*100, 2), "%)", sep = ""))
        g <- g + ggplot2::ylab(paste(x_coord, " (", 
            round(explainedVar[[y_coord]]*100, 2), "%)", sep = ""))
    } else {
        g <- g + ggplot2::xlab(x_coord) + ggplot2::ylab(y_coord) 
    }
    
    if (highlight == "none") {
        g <- g + ggplot2::geom_point()
    } else {
        g <- g + ggplot2::geom_point(aes_string(color = "color"))
    }
    
    g <- g + ggplot2::coord_fixed(ratio = 1) + ggplot2::theme_classic() + 
        ggplot2::theme(legend.position = "none") 
    
    plotly::ggplotly(g, tooltip = tT, height = height) 
}



#' @name explVar
#'
#' @title Retrieve the explained variance for each principal component and axis
#'
#' @description
#' The function `explVar` calculates the proportion of explained variance for
#' each principal component (PC, `type = "PCA"`) and axis (`type = "PCoA"`). 
#' 
#' @details 
#' `explVar` uses the function `prcomp` from the `stats` package to retrieve
#' the explained standard deviation per PC (`type = "PCA"`) and the function 
#' `cmdscale` from the `stats` package to retrieve the explained variation 
#' based on eigenvalues per Axis (`type = "PCoA"`).
#' 
#' @param x `matrix`, containing no missing values (`NA`), samples in columns
#' and features in rows
#' @param params `list`, containing the parameters for PCA and PCoA. For 
#' `type = "PCA"` these are `center` of type `logical` 
#' (indicating whether the variables should be shifted to be zero centered) and
#' `scale` of type `logical`(indicating whether the variables should be scaled
#' that they have a standard variation of 1). For `type = "PCoA"`, this 
#' is `method` of type `character` (indicating the method for distance 
#' calculation). 
#' @param type `character`, one of `"PCA"` or `"PCoA"`
#' 
#' @return `numeric` vector with the proportion of explained variance for each 
#' PC or Axis
#' 
#' @examples
#' x <- matrix(1:100, nrow = 10, ncol = 10, 
#'     dimnames = list(1:10, paste("sample", 1:10)))
#' set.seed(1)
#' x <- x + rnorm(100)
#' explVar(x = x, params = list(center = TRUE, scale = TRUE), type = "PCA")
#' explVar(x = x, params = list(method = "euclidean"), type = "PCoA")
#' 
#' @author Thomas Naake
#' 
#' @importFrom stats prcomp dist cmdscale
#'
#' @export
explVar <- function(x, params, type = c("PCA", "PCoA")) {

    type <- match.arg(type)

    if (type == "PCA") {

        ## PCA (by prcomp) and retrieve the exlained variance for each PC
        params <- append(params, list(x = t(x)))
        PC <- do.call(what = stats::prcomp, params)
        explVar <- PC$sdev^2 / sum(PC$sdev^2)
        names(explVar) <- paste("PC", seq_len(ncol(x)), sep = "")    
    }

    if (type == "PCoA") {

        params <- append(params, list(x = t(x)))
        ## truncate params
        params <- params[names(params) %in% methods::formalArgs(stats::dist)]
        d <- do.call(what = stats::dist, params)
        cmd <- stats::cmdscale(d, k = ncol(x) - 1, eig = TRUE)
        explVar <- cmd$eig / sum(cmd$eig)
        explVar <- explVar[-length(explVar)]
        names(explVar) <- paste("Axis.", seq_len(ncol(x) - 1), sep = "")
    }

    return(explVar)
}


#' @name permuteExplVar
#' 
#' @title Permute the expression values and retrieve the explained variance
#' 
#' @description 
#' The function `permuteExplVar` determines the explained variance of the 
#' permuted expression matrix (`x`). It is used to determine the optimal 
#' number of PCs for tSNE. 
#' 
#' @details 
#' For the input of tSNE, typically, we want to reduce the 
#' initial number of dimensions linearly with PCA (used as the `initial_dims`
#' arguments in the `Rtsne` funtion). The reduced data set is used for feeding 
#' into tSNE. By plotting the percentage of variance explained by the Princical
#' Components (PCs) we can estimate how many PCs we keep as input into tSNE.
#' However, if we select too many PCs, noise will be included as input to tSNE;
#' if we select too few PCs we might loose the important data structures. 
#' To get a better understanding how many PCs to include, randomization will
#' be employed and the observed variance will be compared to the permuted 
#' variance.
#' 
#' @param x `matrix` or `data.frame`, samples in columns and features in rows
#' @param n `numeric`, number of permutation rounds
#' @param center `logical`, passed to the function `explVar`
#' @param scale `logical`, passed to the function `explVar`
#' 
#' @examples  
#' x <- matrix(1:100, nrow = 10, ncol = 10, 
#'     dimnames = list(1:10, paste("sample", 1:10)))
#' permuteExplVar(x = x, n = 10, center = TRUE, scale = TRUE)
#'
#' @return matrix with explained variance
#'
#' @author Thomas Naake
#'
#' @export
permuteExplVar <- function(x, n = 10, center = TRUE, scale = TRUE) {
    
    if (n < 1) stop("n has to be greater than 0")
    
    var_perm <- lapply(seq_len(n), function(i) {
        ## permute features across the samples
        x_perm <- apply(x, 1, sample)
        
        ## PCA and retrieve the explained variance for each PC
        explVar(x_perm, params = list(center = center, scale = scale), 
            type = "PCA")
    })
    
    ## create a matrix (rbind the list elements)
    var_perm <- do.call(rbind, var_perm)
    
    return(var_perm)
}


#' @name plotPCAVar
#' 
#' @title Plot of explained variance against the principal components
#' 
#' @description
#' The function `plotPCAVar` plots the explained variance (in %) on the y-axis
#' against the principal components for the measured and permuted values.  
#' 
#' @details
#' The argument `var_perm` is optional and visualization of permuted values 
#' can be omitted by setting `var_perm = NULL`.
#' 
#' @param var_x `numeric` (named `numeric` vector)
#' @param var_perm `matrix` with the explained variance obtained by permutation
#' (function `permuteExplVar`)
#' 
#' @examples 
#' x <- matrix(1:100, ncol = 10)
#' var_x <- explVar(x = x, params = list(center = TRUE, scale = TRUE), 
#'     type = "PCA")
#' var_perm <- permuteExplVar(x = x, n = 100, center = TRUE, scale = TRUE)
#' plotPCAVar(var_x = var_x, var_perm = var_perm)
#' 
#' @return 
#' `gg` object from `ggplot`
#' 
#' @author Thomas Naake
#'
#' @export
plotPCAVar <- function(var_x, var_perm = NULL) {
    
    df <- data.frame(PC = names(var_x), values = var_x, group = "measured")
    
    if (!is.null(var_perm)) {
        df_perm <- data.frame(PC = colnames(var_perm), 
            values = colMeans(var_perm), group = "permuted")
        df <- rbind(df, df_perm)
    }
    
    df$PC <- factor(df$PC, levels = names(var_x))
    df$values <- df$values * 100
    
    ## plotting
    g <- ggplot2::ggplot(df, aes_string(x = "PC", y = "values"))  + 
        ggplot2::geom_point(aes_string(color = "group")) + 
        ggplot2::geom_line(aes_string(color = "group", group = "group")) +
        ggplot2::theme_bw() + ggplot2::ylab("explained variance (%)") + 
        ggplot2::xlab("principal components") +
        ggplot2::theme(axis.text.x = element_text(angle = 90))
    
    if (is.null(var_perm)) g <- g + 
        ggplot2::theme(legend.title = element_blank(), legend.position = "none")
    
    return(g)
}

#' @name plotPCAVarPvalue
#'
#' @title Plot p-values for the significance of principal components
#' 
#' @description 
#' The function `plotPCAVarPvalue` plots the p-values of significances of 
#' principal components. Using the visual output, the optimal number of 
#' principal components can be selected. 
#' 
#' @details
#' Internal usage in `shinyQC`.
#' 
#' @param var_x `numeric`, measured variances
#' @param var_perm `matrix`, variances obtained by permutation
#' 
#' @examples
#' x <- matrix(1:100, ncol = 10)
#' var_x <- explVar(x = x, params = list(center = TRUE, scale = TRUE), 
#'     type = "PCA")
#' var_perm <- permuteExplVar(x = x, n = 100, center = TRUE, scale = TRUE)
#' plotPCAVarPvalue(var_x = var_x, var_perm = var_perm)
#' 
#' @return gg object from `ggplot`
#' 
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_hline aes
#' @importFrom ggplot2 ylab xlab theme_bw theme element_text
#'
#' @author Thomas Naake
#' 
#' @export
plotPCAVarPvalue <- function(var_x, var_perm) {
    p_val <- apply(t(var_perm) >= var_x, 1, sum) / nrow(var_perm)
    
    df <- data.frame(PC = names(p_val), values = p_val, group = "pvalue")
    df$PC <- factor(df$PC, levels = df$PC)
    
    ## plotting
    ggplot2::ggplot(df, ggplot2::aes_string(x = "PC", y = "values")) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line(ggplot2::aes_string(group = "group")) + 
        ggplot2::geom_hline(ggplot2::aes(yintercept=0.05), color = "red") +
        ggplot2::ylab("p-value") + ggplot2::xlab("principal components") + 
        ggplot2::theme_bw() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
}

#' @name tblPCALoadings
#' 
#' @title Return tibble with PCA loadings for features
#' 
#' @description 
#' The function `tblPCALoadings` returns a `tibble` with loadings values for the
#' features (row entries) in `x`.
#' 
#' @details 
#' The function `tblPCALoadings` acccesses the list entry `rotation` of the
#' `prcomp` object. 
#' 
#' @param x `matrix`, containing no missing values
#' @param params `list`, arguments/parameters given to the function 
#' `stats::prcomp`
#' 
#' @examples 
#' x <- matrix(rnorm(1:10000), ncol = 100)
#' rownames(x) <- paste("feature", 1:nrow(x))
#' colnames(x) <- paste("sample", 1:ncol(x))
#' params <- list(method = "euclidean", ## dist
#'     initial_dims = 10, max_iter = 100, dims = 3, perplexity = 3, ## tSNE
#'     min_dist = 0.1, n_neighbors = 15, spread = 1) ## UMAP
#' tblPCALoadings(x, params)
#'
#' @return `tibble`
#' 
#' @author Thomas Naake
#' 
#' @export
tblPCALoadings <- function(x, params) {
    params <- append(params, list(x = t(x)))
    d <- do.call(what = stats::prcomp, params)
    tbl <- tibble::as_tibble(d$rotation)
    tbl <- tibble::tibble(name = rownames(d$rotation), tbl)
    return(tbl)
}

#' @name plotPCALoadings
#' 
#' @title Plot for PCA loadings of features
#' 
#' @description 
#' The function `plotPCALoadings` creates a loadings plot of the features.
#' 
#' @details 
#' The function takes as input the output of the function `tblPlotPCALoadings`.
#' It uses the `ggplotly` function from `plotly` to create an interactive plot.
#' 
#' 
#' @param tbl `tibble` as obtained by the function `ordination`
#' @param x_coord `character`, column name of `tbl` that stores x coordinates
#' @param y_coord `character`, column name of `tbl` that stores y coordinates
#' 
#' @examples 
#' x <- matrix(rnorm(1:10000), ncol = 100)
#' rownames(x) <- paste("feature", 1:nrow(x))
#' colnames(x) <- paste("sample", 1:ncol(x))
#' params <- list(method = "euclidean", ## dist
#'     initial_dims = 10, max_iter = 100, dims = 3, perplexity = 3, ## tSNE
#'     min_dist = 0.1, n_neighbors = 15, spread = 1) ## UMAP
#' tbl <- tblPCALoadings(x, params)
#' plotPCALoadings(tbl, x_coord = "PC1", y_coord = "PC2")
#'
#' @return `plotly`
#' 
#' @author Thomas Naake
#' 
#' @importFrom plotly ggplotly
#' 
#' @export
plotPCALoadings <- function(tbl, x_coord, y_coord) {
    
    g <- ggplot2::ggplot(tbl, aes_string(text = deparse(quote(name)))) +
        ggplot2::geom_point(aes_string(x = x_coord, y = y_coord), alpha = 0.4) +
        ggplot2::xlab(x_coord) + ggplot2::ylab(y_coord) +
        ggplot2::theme_classic() + ggplot2::theme(legend.position = "none")

    plotly::ggplotly(g, tooltip = "name")
}
