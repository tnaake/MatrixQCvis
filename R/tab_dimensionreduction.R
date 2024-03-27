#' @name dimensionReduction
#'
#' @title Dimensionality reduction with dimensionReduction methods PCA,
#' PCoA, NMDS, UMAP and tSNE
#'
#' @description 
#' The function \code{dimensionReduction} creates a \code{data.frame} 
#' with the coordinates of the projected data (first entry of returned output). 
#' The function allows for the 
#' following projections:
#' Principal Component Analysis (PCA), Principal Coordinates 
#' Analysis/Multidimensional Scaling (PCoA), Non-metric Multidimensional 
#' scaling (NMDS), t-distributed stochastic neighbor embedding (tSNE), and 
#' Uniform Manifold Approximation and Projection (UMAP).
#' 
#' The second list entry will contains the object returned from 
#' \code{prcomp} (PCA), \code{cmdscale} (PCoA), \code{isoMDS} (NMDS), 
#' \code{Rtsne} (tSNE), or \code{umap} (UMAP).
#' 
#' @details 
#' The function \code{dimensionReduction} is a wrapper around the following 
#' functions \code{stats::prcomp} (PCA), \code{stats::cmdscale} (PCoA), 
#' \code{MASS::isoMDS} (NMDS), \code{Rtsne::Rtsne} (tSNE), and 
#' \code{umap::umap} (UMAP). For the function \code{umap::umap} 
#' the method is set to \code{naive}. 
#' 
#' @param x \code{matrix}, containing no missing values, samples are in columns 
#' and features are in rows
#' @param type \code{character}, specifying the type/method to use for 
#' dimensionality reduction. One of \code{PCA}, \code{PCoA}, \code{NMDS}, 
#' \code{tSNE}, or \code{UMAP}. 
#' @param params \code{list}, arguments/parameters given to the functions 
#' \code{stats::prcomp}, \code{stats::dist}, \code{Rtsne::Rtsne}, 
#' \code{umap::umap}
#' 
#' @examples 
#' x <- matrix(rnorm(seq_len(10000)), ncol = 100)
#' rownames(x) <- paste("feature", seq_len(nrow(x)))
#' colnames(x) <- paste("sample", seq_len(ncol(x)))
#' params <- list(method = "euclidean", ## dist
#'     initial_dims = 10, max_iter = 100, dims = 3, perplexity = 3, ## tSNE
#'     min_dist = 0.1, n_neighbors = 15, spread = 1) ## UMAP
#' dimensionReduction(x, type = "PCA", params = params)
#' dimensionReduction(x, type = "PCoA", params = params)
#' dimensionReduction(x, type = "NMDS", params = params)
#' dimensionReduction(x, type = "tSNE", params = params)
#' dimensionReduction(x, type = "UMAP", params = params)
#'
#' @author Thomas Naake
#'
#' @importFrom stats prcomp dist cmdscale
#' @importFrom MASS isoMDS
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom methods formalArgs
#' 
#' @return list, first entry contains a \code{tbl}, second entry contains
#' the object returned from \code{prcomp} (PCA), \code{cmdscale} (PCoA),
#' \code{isoMDS} (NMDS), \code{Rtsne} (tSNE), or \code{umap} (UMAP)
#' 
#' @export
dimensionReduction <- function(x, 
    type = c("PCA", "PCoA", "NMDS", "tSNE", "UMAP"), params = list()) {
    
    type <- match.arg(type)
    
    if (type == "PCA") { ## scale, center
        params <- append(params, list(x = t(x)))
        d <- do.call(what = stats::prcomp, params)
        tbl <- d[["x"]] |>
            tibble::as_tibble()
    }
    if (type == "PCoA") {
        params <- append(params, list(x = t(x)))
        ## truncate params
        params <- params[names(params) %in% methods::formalArgs(stats::dist)] 
        d <- do.call(what = stats::dist, args = params)
        d <- stats::cmdscale(d, k = ncol(x) - 1, eig = TRUE)
        tbl <- d$points |>
            tibble::as_tibble()
        colnames(tbl) <- paste("Axis.", seq_len(ncol(tbl)), sep = "")
    }
    if (type == "NMDS") {
        params <- append(params, list(x = t(x)))
        ## truncate params
        params <- params[names(params) %in% formalArgs(dist)] 
        d <- do.call(what = stats::dist, args = params)
        d <- MASS::isoMDS(d, k = 2)
        tbl <- d$points |>
            tibble::as_tibble()
        colnames(tbl) <- paste("MDS", seq_len(ncol(tbl)), sep = "")
    }
    if (type == "tSNE") { 
        ## hyperparameters for tSNE: perplexity, max_iter, initial_dims, dims
        params <- append(params, list(X = t(x)))
        d <- do.call(what = Rtsne::Rtsne, args = params) 
        tbl <- d$Y |>
            tibble::as_tibble()
        colnames(tbl) <- paste("X", seq_len(ncol(tbl)), sep = "")
    }
    if (type == "UMAP") { 
        ## hyperparameters for UMAP: n_neighbors, min_dist, spread
        ## remove first the method from the dist function then add "naive"
        if ("method" %in% names(params)) 
            params <- params[names(params) != "method"]
        params <- append(params, list(d = t(x), method = "naive"))
        d <- do.call(what = umap::umap, args = params)
        tbl <- d$layout |> 
            tibble::as_tibble()
        colnames(tbl) <- paste("X", seq_len(ncol(tbl)), sep = "")
    }
    
    tbl[["name"]] <- colnames(x)
    
    list(tbl, d)
}

#' @name dimensionReductionPlot
#' 
#' @title Plot the coordinates from \code{dimensionReduction} values
#' 
#' @description 
#' The function \code{dimensionReductionPlot} creates a dimension reduction plot. 
#' The function takes as input the \code{tbl} object obtained 
#' from the \code{dimensionReduction} function. The \code{tbl} contains 
#' transformed values by one of the dimension reduction methods. 
#' 
#' @details 
#' The function \code{dimensionReductionPlot} is a wrapper for a
#' \code{ggplot/ggplotly} expression. 
#'
#' @param tbl \code{tbl} as obtained by the function \code{dimensionReduction}
#' @param se \code{SummarizedExperiment}
#' @param color \code{character}, one of \code{"none"} or 
#' \code{colnames(se@@colData)}
#' @param size \code{character}, one of \code{"none"} or 
#' \code{colnames(se@@colData)}
#' @param explainedVar NULL or named \code{numeric}, if \code{numeric} 
#' \code{explainedVar} contains the explained variance per principal component 
#' (names of \code{explainedVar} corresponds to the principal components)
#' @param x_coord \code{character}, column name of \code{tbl} that stores 
#' x coordinates
#' @param y_coord \code{character}, column name of \code{tbl} that stores 
#' y coordinates
#' @param height \code{numeric}, specifying the height of the plot (in pixels)
#' @param interactive \code{logical(1)}, if \code{TRUE} 
#' \code{dimensionReductionPlot} will return a \code{plotly} object, 
#' if \code{FALSE} \code{dimensionReductionPlot} will return a \code{gg} object
#' 
#' @examples
#' library(SummarizedExperiment)
#' 
#' ## create se
#' a <- matrix(seq_len(100), nrow = 10, ncol = 10, byrow = TRUE,
#'             dimnames = list(seq_len(10), paste("sample", seq_len(10))))
#' set.seed(1)
#' a <- a + rnorm(100)
#' cD <- data.frame(name = colnames(a), type = c(rep("1", 5), rep("2", 5)))
#' rD <- data.frame(spectra = rownames(a))
#' se <- SummarizedExperiment(assay = a, rowData = rD, colData = cD)
#' 
#' pca <- dimensionReduction(x = assay(se), type = "PCA", params = list())[[1]]
#' 
#' dimensionReductionPlot(tbl = pca, se = se, color = "type", size = "none",
#'     x_coord = "PC1", y_coord = "PC2")
#'
#' @author Thomas Naake
#'
#' @importFrom plotly ggplotly
#' @importFrom dplyr left_join
#' @importFrom ggplot2 ggplot aes sym xlab ylab geom_point coord_fixed
#' @importFrom ggplot2 theme_classic theme
#' 
#' @return \code{plotly} or \code{gg}
#' 
#' @export
dimensionReductionPlot <- function(tbl, se, 
    color = c("none", colnames(se@colData)), 
    size = c("none", colnames(se@colData)), 
    explainedVar = NULL, x_coord, y_coord, height = 600, interactive = TRUE) {
    
    color <- match.arg(color)
    color <- make.names(color)
    size <- match.arg(size)
    size <- make.names(size)
    
    ## access the colData slot and add the rownames as a new column to cD
    ## (will add the column "rowname")
    cD <- se@colData |> as.data.frame()
    colnames(cD) <- make.names(colnames(cD))
    cD[["name"]] <- rownames(cD)
    
    if (color == "none") {
        tT <- c("text")
    } else {
        cD_cut <- data.frame(name = cD[["name"]], color = cD[[color]])
        tbl <- dplyr::left_join(tbl, cD_cut, by = "name", copy = TRUE)
        tT <- c("text", "color")
    }
    
    ## do the actual plotting
    g <- ggplot2::ggplot(tbl, ggplot2::aes(x = !!ggplot2::sym(x_coord), 
        y = !!ggplot2::sym(y_coord), 
        text = !!ggplot2::sym("name")))
    
    if (!is.null(explainedVar)) {
        g <- g + ggplot2::xlab(paste(x_coord, " (", 
            round(explainedVar[[x_coord]] * 100, 2), "%)", sep = ""))
        g <- g + ggplot2::ylab(paste(y_coord, " (", 
            round(explainedVar[[y_coord]] * 100, 2), "%)", sep = ""))
    } else {
        g <- g + ggplot2::xlab(x_coord) + ggplot2::ylab(y_coord) 
    }
    
    if (color == "none") {
        g <- g + ggplot2::geom_point()
    } else {
        g <- g + ggplot2::geom_point(
            ggplot2::aes(color = !!ggplot2::sym("color")))
    }
    
    g <- g + ggplot2::theme_classic()

    if (interactive) {
        g <- g + ggplot2::theme(legend.position = "none") 
        plotly::ggplotly(g, tooltip = tT, height = height, width = height)
    } else {
        g + ggplot2::theme(aspect.ratio = 1)
    }
}



#' @name explVar
#'
#' @title Retrieve the explained variance for each principal component (PCA) or
#' axis (PCoA)
#'
#' @description
#' The function \code{explVar} calculates the proportion of explained variance 
#' for each principal component (PC, \code{type = "PCA"}) and axis 
#' (\code{type = "PCoA"}). 
#' 
#' @details 
#' \code{explVar} uses the function \code{prcomp} from the \code{stats} package 
#' to retrieve the explained standard deviation per PC 
#' (\code{type = "PCA"}) and the function \code{cmdscale} from the \code{stats} 
#' package to retrieve the explained variation based on eigenvalues per 
#' Axis (\code{type = "PCoA"}).
#' 
#' @param d \code{prcomp} or \code{list} from \code{cmdscale}
#' @param type \code{character}, one of \code{"PCA"} or \code{"PCoA"}
#' 
#' @return \code{numeric} vector with the proportion of explained variance 
#' for each PC or Axis
#' 
#' @examples
#' x <- matrix(seq_len(100), nrow = 10, ncol = 10, 
#'     dimnames = list(seq_len(10), paste("sample", seq_len(10))))
#' set.seed(1)
#' x <- x + rnorm(100)
#'
#' ## run for PCA
#' pca <- dimensionReduction(x = x, 
#'     params = list(center = TRUE, scale = TRUE), type = "PCA")[[2]]
#' explVar(d = pca, type = "PCA")
#' 
#' ## run for PCoA
#' pcoa <- dimensionReduction(x = x, 
#'     params = list(method = "euclidean"), type = "PCoA")[[2]]
#' explVar(d = pcoa, type = "PCoA")
#' 
#' @author Thomas Naake
#'
#' @export
explVar <- function(d, type = c("PCA", "PCoA")) {

    type <- match.arg(type)

    if (type == "PCA") {
        ## PCA (by prcomp) and retrieve the exlained variance for each PC
        sdev_2 <- d[["sdev"]] ^ 2
        explVar <- sdev_2 / sum(sdev_2)
        names(explVar) <- paste("PC", seq_along(explVar), sep = "")    
    }

    if (type == "PCoA") {
        eig <- d[["eig"]]
        explVar <- eig / sum(eig)
        explVar <- explVar[-length(explVar)]
        names(explVar) <- paste("Axis.", seq_along(explVar), sep = "")
    }

    explVar
}

#' @name permuteExplVar
#' 
#' @title Permute the expression values and retrieve the explained variance
#' 
#' @description 
#' The function \code{permuteExplVar} determines the explained variance of the 
#' permuted expression matrix (\code{x}). It is used to determine the optimal 
#' number of PCs for tSNE. 
#' 
#' @details 
#' For the input of tSNE, typically, we want to reduce the initial number of 
#' dimensions linearly with PCA (used as the \code{initial_dims} arguments in 
#' the \code{Rtsne} funtion). The reduced data set is used for feeding 
#' into tSNE. By plotting the percentage of variance explained by the Princical
#' Components (PCs) we can estimate how many PCs we keep as input into tSNE.
#' However, if we select too many PCs, noise will be included as input to tSNE;
#' if we select too few PCs we might loose the important data structures. 
#' To get a better understanding how many PCs to include, randomization will
#' be employed and the observed variance will be compared to the permuted 
#' variance.
#' 
#' @param x \code{matrix} or \code{data.frame}, samples in columns and features 
#' in rows
#' @param n \code{numeric}, number of permutation rounds
#' @param center \code{logical}, passed to the function \code{explVar}
#' @param scale \code{logical}, passed to the function \code{explVar}
#' @param sample_n \code{numeric(1)}, number of features (subset) to be 
#' taken for calculation of permuted explained variance, the top 
#' \code{sample_n} varying values based on their standard deviation will be 
#' taken
#' 
#' @examples  
#' x <- matrix(seq_len(100), nrow = 10, ncol = 10,
#'     dimnames = list(seq_len(10), paste("sample", seq_len(10))))
#' permuteExplVar(x = x, n = 10, center = TRUE, scale = TRUE, sample_n = NULL)
#'
#' @return matrix with explained variance
#'
#' @author Thomas Naake
#'
#' @export
permuteExplVar <- function(x, n = 10, center = TRUE, scale = TRUE, 
    sample_n = NULL) {
    
    if (n < 1) stop("n has to be greater than 0")

    ## in case of large data sets the permutation might take a long time,
    ## limit if there are more features (nrow(x)) than sample_n features the 
    ## features to sample_n features
    if (!is.null(sample_n)) {
        if (sample_n < nrow(x)) {
            sds <- apply(x, 1, sd, na.rm = TRUE)
            sds_feat <- order(sds, decreasing = TRUE)[seq_len(sample_n)]
            x <- x[sds_feat, ] 
        }
    }

    var_perm <- lapply(seq_len(n), function(i) {
        ## permute features across the samples
        x_perm <- apply(x, 1, sample)
        
        ## PCA and retrieve the explained variance for each PC
        pca <- dimensionReduction(x = x_perm,  type = "PCA", 
            params = list(center = center, scale = scale))[[2]]
        explVar(d = pca, type = "PCA")
    })

    ## create a matrix (rbind the list elements)
    do.call(rbind, var_perm)
}


#' @name plotPCAVar
#' 
#' @title Plot of explained variance against the principal components
#' 
#' @description
#' The function \code{plotPCAVar} plots the explained variance (in %) on the 
#' y-axis against the principal components for the measured and permuted values.  
#' 
#' @details
#' The argument \code{var_perm} is optional and visualization of permuted values 
#' can be omitted by setting \code{var_perm = NULL}.
#' 
#' @param var_x \code{numeric} (named \code{numeric} vector)
#' @param var_perm \code{matrix} with the explained variance obtained by 
#' permutation (function \code{permuteExplVar})
#' 
#' @examples 
#' x <- matrix(seq_len(100), ncol = 10)
#' pca <- dimensionReduction(x = x, params = list(center = TRUE, scale = TRUE),
#'     type = "PCA")[[2]]
#' var_x <- explVar(d = pca, type = "PCA")
#' var_perm <- permuteExplVar(x = x, n = 100, center = TRUE, scale = TRUE)
#' plotPCAVar(var_x = var_x, var_perm = var_perm)
#'
#' @return 
#' \code{gg} object from \code{ggplot}
#' 
#' @author Thomas Naake
#' 
#' @importFrom ggplot2 ggplot aes sym geom_point geom_line theme_bw
#' @importFrom ggplot2 ylab xlab theme element_text element_blank
#'
#' @export
plotPCAVar <- function(var_x, var_perm = NULL) {
    
    df <- data.frame(PC = names(var_x), values = var_x, group = "measured")
    
    if (!is.null(var_perm)) {
        df_perm <- data.frame(PC = colnames(var_perm), 
            values = colMeans(var_perm), group = "permuted")
        df <- rbind(df, df_perm)
    }
    
    df[["PC"]] <- factor(df[["PC"]], levels = names(var_x))
    df[["values"]] <- df[["values"]] * 100
    
    ## plotting
    g <- ggplot2::ggplot(df, ggplot2::aes(x = !!ggplot2::sym("PC"), 
            y = !!ggplot2::sym("values")))  + 
        ggplot2::geom_point(ggplot2::aes(color = !!ggplot2::sym("group"))) + 
        ggplot2::geom_line(ggplot2::aes(color = !!ggplot2::sym("group"), 
            group = !!ggplot2::sym("group"))) +
        ggplot2::theme_bw() + ggplot2::ylab("explained variance (%)") + 
        ggplot2::xlab("principal components") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    
    if (is.null(var_perm)) g <- g + 
        ggplot2::theme(legend.title = ggplot2::element_blank(), 
            legend.position = "none")
    
    g
}

#' @name plotPCAVarPvalue
#'
#' @title Plot p-values for the significance of principal components
#' 
#' @description 
#' The function \code{plotPCAVarPvalue} plots the p-values of significances of 
#' principal components. Using the visual output, the optimal number of 
#' principal components can be selected. 
#' 
#' @details
#' Internal usage in \code{shinyQC}.
#' 
#' @param var_x \code{numeric}, measured variances
#' @param var_perm \code{matrix}, variances obtained by permutation
#' 
#' @examples
#' x <- matrix(seq_len(100), ncol = 10)
#' pca <- dimensionReduction(x = x, params = list(center = TRUE, scale = TRUE), 
#'     type = "PCA")[[2]]
#' var_x <- explVar(d = pca, type = "PCA")
#' var_perm <- permuteExplVar(x = x, n = 100, center = TRUE, scale = TRUE)
#' plotPCAVarPvalue(var_x = var_x, var_perm = var_perm)
#' 
#' @return \code{gg} object from \code{ggplot}
#' 
#' @importFrom ggplot2 ggplot aes sym geom_point geom_line geom_hline aes
#' @importFrom ggplot2 ylab xlab theme_bw theme element_text
#'
#' @author Thomas Naake
#' 
#' @export
plotPCAVarPvalue <- function(var_x, var_perm) {
    p_val <- apply(t(var_perm) >= var_x, 1, sum) / nrow(var_perm)
    
    df <- data.frame(PC = names(p_val), values = p_val, group = "pvalue")
    df[["PC"]] <- factor(df[["PC"]], levels = df[["PC"]])
    
    ## plotting
    ggplot2::ggplot(df, ggplot2::aes(x = !!ggplot2::sym("PC"), 
            y = !!ggplot2::sym("values"))) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line(ggplot2::aes(group = !!ggplot2::sym("group"))) + 
        ggplot2::geom_hline(ggplot2::aes(yintercept = 0.05), color = "red") +
        ggplot2::ylab("p-value") + ggplot2::xlab("principal components") + 
        ggplot2::theme_bw() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
}

#' @name tblPCALoadings
#' 
#' @title Return tibble with PCA loadings for features
#' 
#' @description 
#' The function \code{tblPCALoadings} returns a \code{tibble} with loadings 
#' values for the features (row entries) in \code{x}.
#' 
#' @details 
#' The function \code{tblPCALoadings} acccesses the list entry \code{rotation} 
#' of the \code{prcomp} object. 
#' 
#' @param x \code{matrix}, containing no missing values
#' @param params \code{list}, arguments/parameters given to the function 
#' \code{stats::prcomp}
#' 
#' @examples 
#' set.seed(1)
#' x <- matrix(rnorm(seq_len(10000)), ncol = 100)
#' rownames(x) <- paste("feature", seq_len(nrow(x)))
#' colnames(x) <- paste("sample", seq_len(ncol(x)))
#' params <- list(method = "euclidean", ## dist
#'     initial_dims = 10, max_iter = 100, dims = 3, perplexity = 3, ## tSNE
#'     min_dist = 0.1, n_neighbors = 15, spread = 1) ## UMAP
#' tblPCALoadings(x, params)
#'
#' @return \code{tbl}
#' 
#' @author Thomas Naake
#' 
#' @export
tblPCALoadings <- function(x, params) {
    params <- append(params, list(x = t(x)))
    d <- do.call(what = stats::prcomp, params)$rotation
    tbl <- tibble::as_tibble(d) 
    tbl[["name"]] <- rownames(d)
    tbl
}

#' @name plotPCALoadings
#' 
#' @title Plot for PCA loadings of features
#' 
#' @description 
#' The function \code{plotPCALoadings} creates a loadings plot of the features.
#' 
#' @details 
#' The function takes as input the output of the function 
#' \code{tblPlotPCALoadings}. It uses the \code{ggplotly} function from 
#' \code{plotly} to create an interactive \code{plotly} plot.
#' 
#' 
#' @param tbl \code{tbl} as obtained by the function \code{dimensionReduction}
#' @param x_coord \code{character}, column name of \code{tbl} that stores x 
#' coordinates
#' @param y_coord \code{character}, column name of \code{tbl} that stores y 
#' coordinates
#' 
#' @examples 
#' x <- matrix(rnorm(seq_len(10000)), ncol = 100)
#' rownames(x) <- paste("feature", seq_len(nrow(x)))
#' colnames(x) <- paste("sample", seq_len(ncol(x)))
#' params <- list(method = "euclidean", ## dist
#'     initial_dims = 10, max_iter = 100, dims = 3, perplexity = 3, ## tSNE
#'     min_dist = 0.1, n_neighbors = 15, spread = 1) ## UMAP
#' tbl <- tblPCALoadings(x, params)
#' plotPCALoadings(tbl, x_coord = "PC1", y_coord = "PC2")
#'
#' @return \code{plotly}
#' 
#' @author Thomas Naake
#' 
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 ggplot aes sym geom_point xlab ylab theme_classic
#' @importFrom ggplot2 theme
#' 
#' @export
plotPCALoadings <- function(tbl, x_coord, y_coord) {
    
    g <- ggplot2::ggplot(tbl, 
            ggplot2::aes(text = !!ggplot2::sym("name"))) +
        ggplot2::geom_point(ggplot2::aes(x = !!ggplot2::sym(x_coord), 
            y = !!ggplot2::sym(y_coord)), alpha = 0.4) +
        ggplot2::xlab(x_coord) + ggplot2::ylab(y_coord) +
        ggplot2::theme_classic() + ggplot2::theme(legend.position = "none")

    plotly::ggplotly(g, tooltip = "name")
}
