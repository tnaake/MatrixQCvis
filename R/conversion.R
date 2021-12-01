#' @name biocrates
#' 
#' @title Convert Biocrates xlsx output to `SummarizedExperiment` object
#' 
#' @description 
#' The function `biocrates` will create a `SummarizedExperiment` from a
#' Biocrates xlsx file. 
#' The function `biocrates` takes as input the path to a .xlsx file 
#' (Biocrates output) and additional parameters given to the `read.xlsx` 
#' function from the `openxlsx` package (e.g. specifying the sheet name or index
#' by `sheet`).
#' 
#' @details 
#' The column "Sample Identification" has to contain unique identifiers
#' (no duplications).
#' 
#' @param file `character`
#' @param sheet `character` or `numeric`, the name or index of the sheet to 
#' read data from
#' @param ... additional parameters given to `read.xslx`
#' 
#' @examples
#' file <- "path/to/biocrates/object"
#' \donttest{biocrates(file = file, sheet = 1)}
#' 
#' @usage biocrates(file, sheet, ...)
#'
#' @return 
#' `SummarizedExperiment` object
#' 
#' @export
#' 
#' @importFrom openxlsx read.xlsx
#' @importFrom dplyr select
#' @importFrom SummarizedExperiment SummarizedExperiment
biocrates <- function(file, sheet, ...) {
    
    xls <- openxlsx::read.xlsx(file, sheet = sheet, ...)
    
    ## colnames is in the first row, assign and remove the first row
    colnames(xls) <- xls[1, ]
    xls <- xls[-1, ]
    
    ## find the columns that contain the metabolites, row 1 contains class,
    ## row 2 contains LOD (is NA for columns not containing the metabolites)
    inds_met <- !is.na(xls[2, ])
    ## set the first TRUE value to FALSE since it contains the label of the row
    inds_met[which(inds_met)[1]] <- FALSE
    
    ## create rowData 
    rD <- data.frame(feature = colnames(xls)[inds_met], 
                class = as.character(xls[1, inds_met]))
    rownames(rD) <- rD[["feature"]]
    
    ## find the rows that contain the samples
    inds_name <- !is.na(xls[, 1])
    
    ## create colData
    ## rename column "Sample Identification" to "name" and move to the beginning
    ## of cD
    cD <- xls[inds_name, seq_len(min(which(inds_met)) - 1)]
    cD <- data.frame(name = cD[, "Sample Identification"], cD)
    cD <- dplyr::select(cD, -c("Sample.Identification"))
    rownames(cD) <- cD[["name"]]
    
    ## create assay, set values of 0 to NA
    a <- xls[inds_name, inds_met]
    a <- as.matrix(a)
    mode(a) <- "numeric"
    a[a == 0] <- NA
    rownames(a) <- cD[["name"]]

    ## create SummarizedExperiment and return
    SummarizedExperiment::SummarizedExperiment(assays = t(a), 
        rowData = rD, colData = cD)
}


#' @name maxQuant
#' 
#' @title Convert MaxQuant xlsx output to `SummarizedExperiment` object
#' 
#' @description 
#' The function `maxQuant` will create a `SummarizedExperiment` from a
#' MaxQuant xlsx file. 
#' The function `maxQuant` takes as input the path to a .xlsx file 
#' (MaxQuant output) and additional parameters given to the `read.xlsx` 
#' function from the `openxlsx` package (e.g. specifying the sheet name or index 
#' by `sheet`).
#' 
#' @details 
#' The argument `type` will specify if the `iBAQ` or `LFQ` values are taken. 
#' 
#' @param file `character`
#' @param type `character`, either `iBAQ` or `LFQ`
#' @param sheet `character` or `numeric`, the name or index of the sheet to 
#' read data from
#' @param ... additional parameters given to `read.xslx`
#'
#' @examples
#' file <- "path/to/maxQuant/object"
#' \donttest{maxQuant(file = file, type = "iBAQ", sheet = 1)}
#'
#' @usage maxQuant(file, type = c("iBAQ", "LFQ"), sheet, ...)
#'
#' @return 
#' `SummarizedExperiment` object
#'
#' @export
#' 
#' @importFrom openxlsx read.xlsx
#' @importFrom SummarizedExperiment SummarizedExperiment
maxQuant <- function(file, type = c("iBAQ", "LFQ"), sheet, ...) {
    
    type <- match.arg(type)
    
    xls <- openxlsx::read.xlsx(file, sheet = sheet, ...)
    
    ## names of proteins is in the first col, assign and remove the first col
    rownames(xls) <- xls[, 1]
    xls <- xls[, -1]
    
    ## find the columns that contain the features
    inds_samp <- grep(pattern = type, colnames(xls))
    cols_samp <- colnames(xls)[inds_samp]

    ## remove the column that only contains type
    inds_samp <- inds_samp[cols_samp != type]
    cols_samp <- cols_samp[cols_samp != type]

    ## create rowData
    rD <- data.frame(feature = rownames(xls))
    if ("Majority.protein.IDs" %in% colnames(xls))
        rD$Majority_protein_ids <- xls[, "Majority.protein.IDs"]
    if ("Peptide.counts..all." %in% colnames(xls))
        rD$Peptide_counts_all <- xls[, "Peptide.counts..all."]
    if ("Peptide.counts..razor.unique." %in% colnames(xls))
        rD$Peptide_counts_razor_unique <- xls[, "Peptide.counts..razor.unique."]
    if ("Peptide.counts..unique." %in% colnames(xls))
        rD$Peptide_counts_unique <- xls[, "Peptide.counts..unique."]
    if ("Protein.names" %in% colnames(xls)) 
        rD$Protein_names <- xls[, "Protein.names"]
    if ("Gene.names" %in% colnames(xls)) rD$Gene_name <- xls[, "Gene.names"]
    if ("Fasta.headers" %in% colnames(xls)) 
        rD$Fasta_header <- xls[, "Fasta.headers"]
    if ("Count" %in% colnames(xls)) rD$Count <- xls[, "Count"]
    if ("Number.of.proteins" %in% colnames(xls)) 
        rD$Number_of_proteins <- xls[, "Number.of.proteins"]
    if ("Peptides"  %in% colnames(xls)) rD$Peptides <- xls[, "Peptides"]
    if ("Razor...unique.peptides" %in% colnames(xls))
        rD$Razor_unique_peptides <- xls[, "Razor...unique.peptides"]
    if ("Unique.peptides" %in% colnames(xls))
        rD$Unique_peptides <- xls[, "Unique.peptides"]
    if ("Sequence.coverage...." %in% colnames(xls))
        rD$Sequence_coverage <- xls[, "Sequence.coverage...."]
    if ("Unique...razor.sequence.coverage...." %in% colnames(xls))
        rD$Unique_razor_sequence_coverage <- xls[, "Unique...razor.sequence.coverage...."]
    if ("Unique.sequence.coverage...." %in% colnames(xls))
        rD$Unique_sequence_coverage <- xls[, "Unique.sequence.coverage...."]
    if ("Mol..weight..kDa." %in% colnames(xls))
        rD$Mol_weight_kDa <- xls[, "Mol..weight..kDa."]
    if ("Sequence.length" %in% colnames(xls))
        rD$Sequence_length <- xls[, "Sequence.length"]
    if ("Sequence.lengths" %in% colnames(xls))
        rD$Sequence_lengths <- xls[, "Sequence.lengths"]
    if ("Q.value" %in% colnames(xls)) rD$Q_value <- xls[, "Q.value"]
    if ("Only.identified.by.site" %in% colnames(xls))
        rD$Only_identified_by_site <- xls[, "Only.identified.by.site"]
    if ("Reverse" %in% colnames(xls)) rD$Reverse <- xls[, "Reverse"]
    if ("Potential.contaminant" %in% colnames(xls))
        rD$Potential_contaminant <- xls[, "Potential.contaminant"]
    if ("id" %in% colnames(xls)) rD$id <- xls[, "id"]
    if ("Peptide.IDs" %in% colnames(xls)) rD$Peptide_IDs <- xls[, "Peptide.IDs"]
    if ("Peptide.is.razor" %in% colnames(xls))
        rD$Peptide_is_razor <- xls[, "Peptide.is.razor"]
    if ("Mod..peptide.IDs" %in% colnames(xls))
        rD$Mod_peptide_IDs <- xls[, "Mod..peptide.IDs"]
    if ("Evidence.IDs" %in% colnames(xls))
        rD$Evidence_IDs <- xls[, "Evidence.IDs"]
    if ("MS.MS.IDs" %in% colnames(xls)) rD$MS_MS_IDs <- xls[, "MS.MS.IDs"]
    if ("Best.MS.MS" %in% colnames(xls)) rD$Best_MS_MS <- xls[, "Best.MS.MS"]
    if ("Oxidation..M..site.IDs" %in% colnames(xls))
        rD$Oxidation_M_site_IDs <- xls[, "Oxidation..M..site.IDs"]
    if ("Oxidation..M..site.positions" %in% colnames(xls))
        rD$Oxidation_M_site_positions <- xls[, "Oxidation..M..site.positions"]
    rownames(rD) <- rD[["feature"]]

    ## create colData
    cD <- data.frame(name = cols_samp)
    name_cut <- gsub(paste0("^", type), "", cD$name)
    name_cut <- gsub("^[. _]", "", name_cut)
    cD$name_cut <- name_cut
    rownames(cD) <- cD[["name"]]
    
    ## create assay, set values of 0 to NA
    a <- xls[, cols_samp]
    a <- as.matrix(a)
    mode(a) <- "numeric"
    a[a == 0] <- NA
    
    ## create SummarizedExperiment and return
    SummarizedExperiment::SummarizedExperiment(assays = a, 
        rowData = rD, colData = cD)
}

#' @name spectronaut
#' 
#' @title Convert Spectronaut xlsx output to `SummarizedExperiment` object
#' 
#' @description 
#' The function `spectronaut` will create a `SummarizedExperiment` from a
#' Spectronaut xlsx file. 
#' The function `spectronaut` takes as input the path to a .xlsx file 
#' (Spectronaut output).
#' 
#' @details 
#' The function requires that the intensity values are stored in the sheet
#' `sheetIntensities` and the sample annotations in the sheet
#' `sheetAnnotation`.
#' 
#' The sample names are taken from the column `"SAMPLE_IDs"` from the 
#' sheet `sheetAnnotation`.
#' 
#' @param file `character`
#' @param sheetIntensities `character` or `numeric`, name or index of the 
#' sheet where the intensities are stored
#' @param sheetAnnotation `character` or `numeric`, name or index of the 
#' sheet where the annotations are stored 
#' @param ... additional parameters given to `read.xslx`
#'
#' @examples
#' file <- "path/to/spectronaut/object"
#' \donttest{spectronaut(file = file, sheetIntensitities = 1, 
#'     sheetAnnotation = 2, ...)}
#' 
#' @usage spectronaut(file, sheetIntensities, sheetAnnotation, ...)
#'
#' @return 
#' `SummarizedExperiment` object
#'
#' @export
#' 
#' @importFrom openxlsx read.xlsx
#' @importFrom SummarizedExperiment SummarizedExperiment
spectronaut <- function(file, sheetIntensities = 1, sheetAnnotation = 2, ...) {
    
    xls <- openxlsx::read.xlsx(file, sheet = sheetIntensities, ...)
    cD <- openxlsx::read.xlsx(file, sheet = sheetAnnotation, ...)
    
    ## get the name of the samples
    samps <- cD[, "Sample_IDs"]
    samps <- make.names(samps)
    
    ## names of proteins is in the first col, assign and remove the first col
    if ("PG.ProteinGroups" %in% colnames(xls))
        rownames(xls) <- xls[, "PG.ProteinGroups"]
    else 
        stop("column 'PG ProteinGroups' not present in file")

    ## find the columns that contain the metabolites
    colnames(xls) <- make.names(colnames(xls))
    a <- dplyr::select(xls, samps)

    ## create rowData
    rD <- data.frame(feature = rownames(xls))
    if ("PG.Molecularweight" %in% colnames(xls))
        rD$PG_Molecularweight <- xls[, "PG.Molecularweight"]
    if ("PG.Genes" %in% colnames(xls))
        rD$PG_Genes <- xls[, "PG.Genes"]
    if ("PG.CellularComponent"  %in% colnames(xls))
        rD$PG_CellularComponent <- xls[, "PG.CellularComponent"]
    if ("PG.BiologicalProcess" %in% colnames(xls)) 
        rD$PG_BiologicalProcess <- xls[, "PG.BiologicalProcess"]
    if ("PG.MolecularFunction" %in% colnames(xls)) 
        rD$PG_MolecularFunction <- xls[, "PG.MolecularFunction"]
    rownames(rD) <- rD[["feature"]]

    ## create colData
    colnames(cD)[colnames(cD) == "Sample_IDs"] <- "name"
    cD$name <- samps
    rownames(cD) <- cD[["name"]]

    ## create assay, set values of 0 to NA
    a <- as.matrix(a)
    mode(a) <- "numeric"
    a[a == 0] <- NA

    ## create SummarizedExperiment
    SummarizedExperiment::SummarizedExperiment(assays = a, 
        rowData = rD, colData = cD)
}
