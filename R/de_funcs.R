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



