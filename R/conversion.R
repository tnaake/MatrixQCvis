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
    colnames(xls) <- make.names(xls[1, ])
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
    cD <- data.frame(name = cD[, "Sample.Identification"], cD)
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
#' The argument `intensity` will specify if the `iBAQ` or `LFQ` values are taken. 
#' 
#' The argument `type` will specify if the data is loaded from `txt` or `xlsx`
#' files.
#' 
#' @param file `character`
#' @param intensity `character`, either `"iBAQ"` or `"LFQ"`
#' @param sheet `character` or `numeric`, the name or index of the sheet to 
#' read data from
#' @param type `character`, either `"txt"` or `"xlsx"`
#' @param ... additional parameters given to `read.xslx` (for `type = "xlsx"`)
#'
#' @examples
#' file <- "path/to/maxQuant/object.txt"
#' \donttest{maxQuant(file = file, intensity = "iBAQ", type = "txt")}
#'
#' @return 
#' `SummarizedExperiment` object
#'
#' @export
#' 
#' @importFrom openxlsx read.xlsx
#' @importFrom utils read.table
#' @importFrom SummarizedExperiment SummarizedExperiment
maxQuant <- function(file, intensity = c("iBAQ", "LFQ"), sheet, 
    type = c("txt", "xlsx"), ...) {
    
    intensity <- match.arg(intensity)
    type <- match.arg(type)
    
    if (type == "xlsx")
        f <- openxlsx::read.xlsx(file, sheet = sheet, ...)
    if (type == "txt")
        f <- utils::read.table(file, sep = "\t", dec = ".", header = TRUE)
    
    ## names of proteins is in the first col, assign and remove the first col
    rownames(f) <- f[, 1]
    f <- f[, -1]
    
    ## find the columns that contain the features
    cols <- colnames(f)
    inds_samp <- grep(pattern = intensity, cols)
    cols_samp <- cols[inds_samp]

    ## remove the column that only contains "intensity"
    inds_samp <- inds_samp[cols_samp != intensity]
    cols_samp <- cols_samp[cols_samp != intensity]

    ## create rowData
    rD <- data.frame(feature = rownames(f))
    if ("Best.MS.MS" %in% cols) rD$Best_MS_MS <- f[, "Best.MS.MS"]
    if ("Charges" %in% cols) rD$Charges <- f[, "Charges"]
    if ("Count" %in% cols) rD$Count <- f[, "Count"]
    if ("Evidence.IDs" %in% cols)
        rD$Evidence_IDs <- f[, "Evidence.IDs"]
    if ("Fasta.headers" %in% cols) rD$Fasta_header <- f[, "Fasta.headers"]
    if ("Gene.names" %in% cols) rD$Gene_name <- f[, "Gene.names"]
    if ("id" %in% cols) rD$id <- f[, "id"]
    if ("Length" %in% cols) rD$Length <- f[, "Length"]
    if ("Majority.protein.IDs" %in% cols)
        rD$Majority_protein_ids <- f[, "Majority.protein.IDs"]
    if ("Mass" %in% cols) rD$Mass <- f[, "Mass"]
    if ("Missed.cleavages" %in% cols) 
        rD$Missed_cleavages <- f[, "Missed.cleavages"]
    if ("Mod..peptide.IDs" %in% cols)
        rD$Mod_peptide_IDs <- f[, "Mod..peptide.IDs"]
    if ("Mol..weight..kDa." %in% cols)
        rD$Mol_weight_kDa <- f[, "Mol..weight..kDa."]
    if ("MS.MS.IDs" %in% cols) rD$MS_MS_IDs <- f[, "MS.MS.IDs"]
    if ("Number.of.proteins" %in% cols) 
        rD$Number_of_proteins <- f[, "Number.of.proteins"]
    if ("Only.identified.by.site" %in% cols)
        rD$Only_identified_by_site <- f[, "Only.identified.by.site"]
    if ("Oxidation..M..site.IDs" %in% cols)
        rD$Oxidation_M_site_IDs <- f[, "Oxidation..M..site.IDs"]
    if ("Oxidation..M..site.positions" %in% cols)
        rD$Oxidation_M_site_positions <- f[, "Oxidation..M..site.positions"]
    if ("Peptide.counts..all." %in% cols)
        rD$Peptide_counts_all <- f[, "Peptide.counts..all."]
    if ("Peptide.counts..razor.unique." %in% cols)
        rD$Peptide_counts_razor_unique <- f[, "Peptide.counts..razor.unique."]
    if ("Peptide.counts..unique." %in% cols)
        rD$Peptide_counts_unique <- f[, "Peptide.counts..unique."]
    if ("Peptides"  %in% cols) rD$Peptides <- f[, "Peptides"]
    if ("Peptide.IDs" %in% cols) rD$Peptide_IDs <- f[, "Peptide.IDs"]
    if ("Peptide.is.razor" %in% cols)
        rD$Peptide_is_razor <- f[, "Peptide.is.razor"]
    if ("Potential.contaminant" %in% cols)
        rD$Potential_contaminant <- f[, "Potential.contaminant"]
    if ("Protein.names" %in% cols) rD$Protein_names <- f[, "Protein.names"]
    if ("Proteins" %in% cols) rD$Proteins <- f[, "Proteins"]
    if ("Q.value" %in% cols) rD$Q_value <- f[, "Q.value"]
    if ("Razor...unique.peptides" %in% cols)
        rD$Razor_unique_peptides <- f[, "Razor...unique.peptides"]
    if ("Reverse" %in% cols) rD$Reverse <- f[, "Reverse"]
    if ("Sequence" %in% cols) rD$Sequence <- f[, "Sequence"]
    if ("Sequence.coverage...." %in% cols)
        rD$Sequence_coverage <- f[, "Sequence.coverage...."]
    if ("Sequence.length" %in% cols)
        rD$Sequence_length <- f[, "Sequence.length"]
    if ("Sequence.lengths" %in% cols)
        rD$Sequence_lengths <- f[, "Sequence.lengths"]
    if ("Unique.peptides" %in% cols) 
        rD$Unique_peptides <- f[, "Unique.peptides"]
    if ("Unique..Proteins." %in% cols) 
        rD$Unique_Proteins <- f[, "Unique..Proteins."]
    if ("Unique...razor.sequence.coverage...." %in% cols)
        rD$Unique_razor_sequence_coverage <- f[, "Unique...razor.sequence.coverage...."]
    if ("Unique.sequence.coverage...." %in% cols)
        rD$Unique_sequence_coverage <- f[, "Unique.sequence.coverage...."]
    rownames(rD) <- rD[["feature"]]

    ## create colData
    cD <- data.frame(name = cols_samp)
    name_cut <- gsub(paste0("^", intensity), "", cD$name)
    name_cut <- gsub("^[. _]", "", name_cut)
    cD$name_cut <- name_cut
    rownames(cD) <- cD[["name"]]
    
    ## create assay, set values of 0 to NA
    a <- f[, cols_samp]
    a <- as.matrix(a)
    mode(a) <- "numeric"
    a[a == 0] <- NA
    colnames(a) <- cD[["name"]]
    
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
