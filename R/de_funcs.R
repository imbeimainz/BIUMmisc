#' Title
#'
#' @param resuSet A list, provided in the format expected by the resuSet
#' specifications.
#' This can be an empty list, if the object was never created beforehand,
#' otherwise this function can simply add all the set of the results and output
#' into a new element of an existing list.
#' Defaults to `NULL`, which would lead to the creation of a new list object. If
#' this is not the desired behavior, please provide an existing object
#' structured as a `resuSet`.
#' @param dds_obj A DESeqDataset object, meaningfully related/matched to the
#' resuSet object.
#' @param contrast_name Character string, specifying the name of the contrast to be
#' extracted
#' @param FDR Numeric value, specifying the false discovery rate value to be
#' used
#' @param lfc_threshold Numeric value, corresponding to the logFC theshold to be
#' used to extract the results - it is relevant in the call to `results()` and
#' it does differ (a lot, conceptually) from the logFC filtering that commonly
#' is performed post-hoc, after testing against the default null hypothesis
#' (logFC = 0), which might be too liberal in some cases.
#' @param extended_anno_df An annotation data.frame with the following columns:
#' `gene_id` and `gene_name` (mandatory), and `description` (optional, can be
#' retrieved via biomaRt)
#' @param species Character string, specifying the species for the dataset under
#' investigation. Defaults to "Homo_sapiens", could also be "Mus_musculus" and
#' other similarly assembled character strings. This is relevant only when
#' creating the buttons for the interactive versions of the results tables.
#' @param name_res_entry Character string
#' @param verbose Logical
#'
#' @return A `resuSet` list object.
#'
#' @export
#'
#' @importFrom DESeq2 results lfcShrink
#' @importFrom mosdef deresult_to_df create_link_ENSEMBL create_link_NCBI
#' buttonifier
#' @importFrom DT datatable
#'
#' @examples
#' # TODO
create_DEresults <- function(resuSet = NULL,
                             dds_obj,
                             contrast_name,
                             FDR = 0.05,
                             lfc_threshold = 0,
                             extended_anno_df,
                             species = "Homo_sapiens",
                             name_res_entry = contrast_name,
                             verbose = TRUE) {

  if (is.null(resuSet)) {
    # initializing to an empty list
    resuSet <- list()
  }

  #### id_contrast <- paste0(contrast[2],"_vs_",contrast[3])
  message("Storing set of results in the element `", name_res_entry, "`...")

  resuSet[[name_res_entry]] <- list()

  message("Extracting results...")
  resuSet[[name_res_entry]][["res_DESeq"]] <-
    results(dds_obj, name = contrast_name, lfcThreshold = lfc_threshold, alpha = FDR)

  message("Performing LFC shrinkage...")
  resuSet[[name_res_entry]][["res_DESeq"]] <-
    lfcShrink(dds_obj,
              coef = contrast_name,
              res = resuSet[[name_res_entry]][["res_DESeq"]],
              type = "apeglm")
  resuSet[[name_res_entry]][["res_DESeq"]]$SYMBOL <-
    extended_anno_df$gene_name[match(rownames(resuSet[[name_res_entry]][["res_DESeq"]]),
                                     extended_anno_df$gene_id)]

  message("Summary MAplot...")
  summary(resuSet[[name_res_entry]][["res_DESeq"]])
  resuSet[[name_res_entry]][["maplot_res"]] <-
    mosdef::plot_ma(resuSet[[name_res_entry]][["res_DESeq"]],
                    ylim = c(-2,2),
                    title = name_res_entry)
  # TODO: control more on the options? or leave them by default

  message("Extracting tables...")
  resuSet[[name_res_entry]][["tbl_res_all"]] <-
    deresult_to_df(resuSet[[name_res_entry]][["res_DESeq"]])
  resuSet[[name_res_entry]][["tbl_res_all"]]$gene_name <-
    extended_anno_df$gene_name[match(resuSet[[name_res_entry]][["tbl_res_all"]]$id,
                                     extended_anno_df$gene_id)]
  resuSet[[name_res_entry]][["tbl_res_all"]]$description <-
    extended_anno_df$description[match(resuSet[[name_res_entry]][["tbl_res_all"]]$id,
                                       extended_anno_df$gene_id)]

  message("Extracting DEtables...")
  resuSet[[name_res_entry]][["tbl_res_DE"]] <-
    deresult_to_df(resuSet[[name_res_entry]][["res_DESeq"]], FDR = FDR)
  resuSet[[name_res_entry]][["tbl_res_DE"]]$gene_name <-
    extended_anno_df$gene_name[match(resuSet[[name_res_entry]][["tbl_res_DE"]]$id,
                                     extended_anno_df$gene_id)]
  resuSet[[name_res_entry]][["tbl_res_DE"]]$description <-
    extended_anno_df$description[match(resuSet[[name_res_entry]][["tbl_res_DE"]]$id,
                                       extended_anno_df$gene_id)]
  # resuSet[[name_res_entry]][["tbl_res_DE"]]$chromosome_name <- anns$chromosome_name[match(resuSet[[name_res_entry]][["tbl_res_DE"]]$id, anns$ensembl_gene_id)]

  if (nrow(resuSet[[name_res_entry]][["tbl_res_DE"]]) > 0) {
    message("Generating interactive DEtable...")
    resuSet[[name_res_entry]][["etbl_res_DE"]] <- resuSet[[name_res_entry]][["tbl_res_DE"]]
    resuSet[[name_res_entry]][["etbl_res_DE"]]$id <-
      create_link_ENSEMBL(resuSet[[name_res_entry]][["etbl_res_DE"]]$id, species = species)
    resuSet[[name_res_entry]][["etbl_res_DE"]]$gene_name <-
      create_link_NCBI(resuSet[[name_res_entry]][["etbl_res_DE"]]$gene_name)
  }

  mybuttons <- c('copy', 'csv', 'excel', 'pdf', 'print')

  DT::datatable(resuSet[[name_res_entry]][["etbl_res_DE"]],
            caption = paste0(name_res_entry,", DE genes"),
            escape = FALSE,
            extensions = 'Buttons',
            options = list(dom = 'Bfrtip',
                           buttons = mybuttons)
  )

  return(resuSet)
}


#' Plot total number of counts
#'
#' @param dds A DESeqDataset object
#' @param group Character string, indicating one of the names of the columns in
#' the `colData` slot of `dds`
#'
#' @return A ggplot object
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_bar theme theme_bw coord_flip
#' element_text labs
#' @importFrom DESeq2 counts
#'
#' @examples
#' # TODO
plot_totcounts <- function(dds, group) {

  myd <- data.frame(
    counts = colSums(counts(dds)),
    group = dds[[group]],
    sample = colnames(dds))

  p <- ggplot(myd,
              aes(x = sample,
                  weight = counts,
                  fill = group)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      x = "Sample",
      y = "Total sum of raw counts",
      title = "Raw counts distribution over samples",
      subtitle = paste0("grouped by `", group, "`")
    ) +
    theme_bw() +
    coord_flip()

  return(p)
}


overview_detected_genes <- function(dds) {
  # anka's nice one with different thresholds


}



dds_gencode_to_ensembl <- function(dds) {

}



#' Fortify different set of annotations
#'
#' @param anno_df_1 A data.frame object, corresponding to the first annotation
#' table
#' @param anno_df_2 A data.frame object, corresponding to the second annotation
#' table
#' @param id_1 Character string, specifying which column in the first table is
#' to be used as identifier (defaults to `gene_id`)
#' @param id_2 Character string, specifying which column in the second table is
#' to be used as identifier (defaults to `ensembl_gene_id`, for biomaRt-like
#' objects)
#' @param gene_name_1 Character string, specifying which column in the first
#' table is to be used as gene name (defaults to `gene_name`)
#' @param gene_name_2 Character string, specifying which column in the second
#' table is to be used as identifier (defaults to `external_gene_name`, again
#' as could be taken out e.g. from biomaRt)
#' @param dds A DESeqDataset object
#' @param verbose Logical, controlling the verbosity level of the function
#'
#' @return A list, including the fortified annotation data frame (in the
#' `anno_df` element), and (if provided as input) a dds object (in the `dds`
#' object) with the information stored in the `rowData` slot.
#'
#' @export
#'
#' @importFrom SummarizedExperiment rowData rowData<-
#'
#' @examples
#' # TODO
fortify_annotations <- function(anno_df_1,
                                anno_df_2,
                                id_1 = "gene_id",
                                id_2 = "ensembl_gene_id",
                                gene_name_1 = "gene_name",
                                gene_name_2 = "external_gene_name",
                                dds = NULL,
                                verbose = TRUE) {
  stopifnot(id_1 %in% colnames(anno_df_1))
  stopifnot(id_2 %in% colnames(anno_df_2))
  stopifnot(gene_name_1 %in% colnames(anno_df_1))
  stopifnot(gene_name_2 %in% colnames(anno_df_2))

  # anno_df_1 - normally taken via orgDb annotations
  # anno_df_2 - usually taken via biomaRt
  merged_anno <- merge.data.frame(
    anno_df_1, anno_df_2,
    by.x = id_1, by.y = id_2,
    all.x = TRUE
  )
  rownames(merged_anno) <- anno_df_1[[id_1]]

  na_names_1 <- sum(is.na(merged_anno[[gene_name_1]]))
  na_names_2 <- sum(is.na(merged_anno[[gene_name_2]]))

  if (verbose) {
    message("Found ", na_names_1, " features with value NA in column ", gene_name_1, " (anno_df_1)")
    message("Found ", na_names_2, " features with value NA in column ", gene_name_2, " (anno_df_2)")
  }

  if (any(c(na_names_1, na_names_2) > 0)) {
    if (verbose)
      message("Trying to resolve missing info in ", gene_name_1,
              " (anno_df_1) with info from ", gene_name_2, " (anno_df_2)...")
    merged_anno$resolved_gene_name <- merged_anno[[gene_name_1]]
    ids_with_missing_names <- rownames(anno_df_1)[is.na(merged_anno[[gene_name_1]])]
    merged_anno[ids_with_missing_names, "resolved_gene_name"] <-
      merged_anno[ids_with_missing_names, gene_name_2]
    na_names_resolved <- sum(is.na(merged_anno[["resolved_gene_name"]]))
    if (verbose) {
      message("Found ", na_names_resolved, " features with value NA in the resolved gene name column")
      if (na_names_resolved > 0)
        message("You might want to add/edit additionally this annotation table...")
    }

    merged_anno[[gene_name_1]] <- merged_anno$resolved_gene_name
    merged_anno$resolved_gene_name <- NULL
  }

  # merged_anno

  if (!is.null(dds)) {
    # update the rowData in a matched dds dataset object
    anno_for_dds <- merged_anno
    # drop the "duplicated" gene name column, gene_name_2
    anno_for_dds[[gene_name_2]] <- NULL
    # re-sort rows if needed to match to the object to extend
    anno_for_dds <- anno_for_dds[match(rownames(dds), rownames(anno_for_dds)), ]
    colnames(anno_for_dds) <- paste0("anno_", colnames(anno_for_dds))
    rowData(dds) <- cbind(rowData(dds), anno_for_dds)

    message("Updated rowData slot of the dds object by adding columns: ",
            paste0(colnames(anno_for_dds), collapse = ", "))
  }

  return(
    list(
      anno_df = merged_anno,
      dds = dds
    )
  )
}


#' Export the components of a resuSet object
#'
#' Export the components of a resuSet object
#'
#' @param resuSet A list, provided in the format expected by the resuSet
#' specifications
#' @param out_file_prefix String character, to be used to prepend all exported files.
#' Defaults to "out_SET", but can be meaningfully chosen to match the set of
#' results it refers to.
#' @param out_folder String character, specifying in which folder such tabular
#' outputs will be generated. Defaults to "_output", but can also be called "."
#' to specify the current directory.
#'
#' @return Invisible NULL if no errors are encountered
#'
#' @export
#'
#' @examples
#' # TODO
exportr <- function(resuSet,
                    out_file_prefix = "out_SET",
                    out_folder = "_output") {
  if (!dir.exists(out_folder))
    dir.create(out_folder)

  message("Exporting the whole set for ", out_file_prefix)
  for(i in names(resuSet)) {
    message("    ", out_file_prefix, "  ---  " ,i)
    curset <- resuSet[[i]]

    if(!is.null(curset$tbl_res_all)) {
      message("        Exporting results from DESeq... ")
      export_deseq_file <- paste0(out_file_prefix, "_", i, "_tbl_res_DESeq.xlsx")
      export_deseq_fullpath <- file.path(out_folder, export_deseq_file)
      writexl::write_xlsx(curset$tbl_res_all, export_deseq_fullpath)
      message("          ", export_deseq_fullpath)
    }
    if(!is.null(curset$topGO_tbl)) {
      message("        Exporting results from topGO... ")
      export_topgo_file <- paste0(out_file_prefix, "_", i, "_tbl_topGOres.xlsx")
      export_topgo_fullpath <- file.path(out_folder, export_topgo_file)
      writexl::write_xlsx(curset$topGO_tbl, export_topgo_fullpath)
      message("          ", export_topgo_fullpath)
    }
    if(!is.null(curset$clupro_tbl)) {
      message("        Exporting results from clusterProfiler... ")
      export_clupro_file <- paste0(out_file_prefix, "_", i, "_tbl_cluPro.xlsx")
      export_clupro_fullpath <- file.path(out_folder, export_clupro_file)
      writexl::write_xlsx(curset$clupro_tbl@result, export_clupro_fullpath)
      message("          ", export_clupro_fullpath)
    }
    if(!is.null(curset$reactome_tbl)) {
      message("        Exporting results from reactomePA ")
      export_reactome_file <- paste0(out_file_prefix, "_", i, "_tbl_reactome.xlsx")
      export_reactome_fullpath <- file.path(out_folder, export_reactome_file)
      writexl::write_xlsx(curset$reactome_tbl@result, export_reactome_fullpath)
      message("          ", export_reactome_fullpath)
    }
  }

  invisible(NULL)
}



#' Convert a resuSet into a GeneTonicList
#'
#' Convert a resuSet into a GeneTonicList object
#'
#' @param resuSet A list, provided in the format expected by the resuSet
#' specifications
#' @param result_name A character, specifying for which result name in the resuSet
#' object the conversion should be performed
#' @param which_enrich Character string, specifying which enrichment results to
#' use. Currently defaults to "topGO_tbl", will be extended to other formats -
#' TODO
#' @param dds A DESeqDataset object, meaningfully related/matched to the
#' resuSet object.
#' @param anno_df A data.frame as specified in the requirements expected by
#' GeneTonic, i.e. where at least the columns `gene_id` and `gene_name` are
#' specified
#'
#' @return A GeneTonicList object, ready to be provided to the GeneTonic
#' functions
#'
#' @export
#'
#' @importFrom GeneTonic GeneTonicList get_aggrscores shake_topGOtableResult
#'
#' @examples
#' # TODO
#'
resuset_to_gtl <- function(resuSet,
                           result_name,
                           which_enrich = "topGO_tbl",
                           dds,
                           anno_df) {
  dds_gtl <- dds
  anno_df_gtl <- anno_df
  res_de_gtl <- resuSet[[result_name]][["res_DESeq"]]

  # TODO: optionally select which enrichment result to export

  res_enrich_gtl <- GeneTonic::get_aggrscores(
    GeneTonic::shake_topGOtableResult(resuSet[[result_name]][[which_enrich]]),
    resuSet[[result_name]][["res_DESeq"]],
    annotation_obj = anno_df_gtl)

  gtl_assembled <- GeneTonicList(
    dds = dds_gtl,
    res_de = res_de_gtl,
    res_enrich = res_enrich_gtl,
    annotation_obj = anno_df_gtl
  )

  return(gtl_assembled)
}


#' Normalized counts table, with extra info
#'
#' @param dds A DESeqDataSet object
#' @param extended_anno_df An annotation data.frame with the following columns:
#' `gene_id` and `gene_name` (mandatory), and `description` (optional, can be
#' retrieved via biomaRt)
#'
#' @return A data.frame object, with the normalized counts and some additional
#' info on the features at hand
#' @export
#'
#' @importFrom DESeq2 estimateSizeFactors counts
#'
#' @examples
#' # TODO
#' # ext_anno <- merge(anno_df, anns, by.x = "gene_id", by.y = "ensembl_gene_id")
#'
#' # ...
#' # writexl::write_xlsx(obj, "exceltable.xlsx")
deseq_normcounts_with_info <- function(dds,
                                       extended_anno_df) {
  norm_counts_tbl <- as.data.frame(
    counts(estimateSizeFactors(dds), normalized = TRUE)
  )
  norm_counts_tbl$gene_id <- rownames(norm_counts_tbl)
  norm_counts_tbl$gene_name <-
    extended_anno_df$gene_name[match(rownames(norm_counts_tbl),extended_anno_df$gene_id)]
  # anns2 <- anns[match(norm_counts_tbl$id, anns$ensembl_gene_id), ]

  if ("description" %in% colnames(extended_anno_df)) {
    norm_counts_tbl$description <-
      extended_anno_df$description[match(rownames(norm_counts_tbl),extended_anno_df$gene_id)]
  } else {
    message("No column named `description` provided in the extended_anno_df object...")
  }

  return(norm_counts_tbl)
}

# combineTogether <- function(normCounts,resuTable,anns) {
  # combinedCountsAndRes <- inner_join(resuTable,normCounts,by="id")
  # anns2 <- anns[match(combinedCountsAndRes$id, anns$ensembl_gene_id), ]
  # combinedCountsAndRes$Description <- anns2$description
  # return(combinedCountsAndRes)
# }




#' TPM table with extended information
#'
#' @inheritParams deseq_normcounts_with_info
#'
#' @return A data.frame object, with the TPM values and some additional
#' info on the features at hand
#'
#' @export
#'
#' @importFrom SummarizedExperiment assayNames assay
#'
#' @examples
#' # TODO
deseq_tpm_with_info <- function(dds,
                                extended_anno_df) {

  stopifnot("abundance" %in% assayNames(dds))

  tpm_tbl <- as.data.frame(assay(dds, "abundance"))

  tpm_tbl$gene_id <- rownames(tpm_tbl)
  tpm_tbl$gene_name <-
    extended_anno_df$gene_name[match(rownames(tpm_tbl),extended_anno_df$gene_id)]
  # anns2 <- anns[match(norm_counts_tbl$id, anns$ensembl_gene_id), ]

  if ("description" %in% colnames(extended_anno_df)) {
    tpm_tbl$description <-
      extended_anno_df$description[match(rownames(tpm_tbl),extended_anno_df$gene_id)]
  } else {
    message("No column named `description` provided in the extended_anno_df object...")
  }

  return(tpm_tbl)
}

