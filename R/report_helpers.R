#' M&Ms maker
#'
#' Helping you to draft the materials and methods section
#'
#' @param dds_object A `DESeqDataset` object. Necessary to read in the info on
#' how the quantifications were performed. Thanks `tximeta`!
#' @param template_text A (long, correctly structured) character string. It defaults
#' to NULL, which would then lead to using the "default" content.
#' TODO: To implement and possibly use "custom"/personalized ones, that could
#' ideally live in separated files (one for each? one for type?)
#' @param packages Character vector. Defaults to NULL. Specifies which packages
#' would need to be captured, alongside the "standard" ones.
#' @param FDRthreshold Numeric value, specifying a precise FDR value. If left
#' unspecified (NULL by default) it searches for the value in the environment of
#' the variable called `FDR`.
#' @param de_framework Character string, defaults to "DESeq2"- if implemented in
#' other frameworks we could use this to swap some behavior.
#' @param add_citations Logical value, TODO to be implemented. In its implementation
#' it should add after the tools the relevant DOIs and/or PMIDs to the articles
#' it should refer to. This should simplify our lives and the lives of our
#' collaborators to add the relevant references into the manuscripts.
#'
#' @returns A character fairly long string, which can be used to draft quickly
#' the M&M section
#'
#' @importFrom glue glue
#' @importFrom utils packageVersion
#'
#' @export
#'
#' @examples
#' # TODO
matmetmaker_RNAseqbulk <- function(dds_object,
                                   template_text = NULL,
                                   packages = NULL,
                                   FDRthreshold = NULL,
                                   de_framework = "DESeq2",
                                   add_citations = TRUE) {

  if(is.null(FDRthreshold)) {
    # try to pick it from the environment it is launched from
    if(exists("FDR")) {
      FDRthreshold <- FDR
    } else {
      stop("Variable `FDR` not defined in the current environment!")
    }
  }


  template_text <- paste0(
    "Quality control on the sequencing data was performed with the FastQC tool (version 0.XX.YY [[CHANGEME]], https://www.bioinformatics.babraham.ac.uk/projects/fastqc/). [[OPTIONAL:]] After preprocessing, the number of reads per sample ranged from XX million to YY million with a median of ZZ million.\n",
    "[[if using Salmon]] Transcript abundance estimates were computed with Salmon [ADDCIT] (version {version_salmon}), with a [transcriptome index|combined transcriptome and genome index] generated from the {txome_info_source} reference (version {txome_info_version}), and subsequently summarized to gene level with the tximeta R package (version {version_tximeta}) [ADDCIT].\n",
    "The exploration, modeling, and interpretation of the expression data follows the protocols defined by Ludt et al (2022), [cit: DOI:10.1002/cpz1.411]. [alt: All downstream analyses were performed in R... following ...[]].\n",
    "Exploratory data analysis was performed with the pcaExplorer package (version {version_pcaExplorer}, [ADDCIT]). Principal Component Analysis plots were performed including the top XXX most variable genes.[[CHANGEME]]\n",
    "Differential expression analysis was performed with DESeq2 package (version {version_DESeq2}) [ADDCIT], setting the false discovery rate (FDR) cutoff to {value_FDR}. Accurate estimation of the effect sizes (described as log2 fold change) was performed using the apeglm shrinkage estimator (version {version_apeglm}) [ADDCIT].\n",
    "Further analyses included Gene Ontology pathway enrichment by the topGO (version {version_topGO}) | goseq (version {version_goseq}) | clusterProfiler (version {version_clusterProfiler}) packages [ADDCIT], setting all detected|expressed genes as background dataset, and were performed using the mosdef package (version {version_mosdef}) [ADDCIT]. ",
    "The enrichment results were subsequently processed with the GeneTonic package (version {version_GeneTonic}) for visualization and summarizing [ADDCIT].\n",
    "Gene expression profiles were plotted as heatmaps, using color-coded [mean-centered|standardized Z scores for the] expression values, after [variance stabilizing transformation|regularized logarithm transformation]) to enable comparison across samples.",
    "",
    "",
    ""
  )

  version_salmon <- dds_object@metadata$quantInfo$salmon_version[1]
  txome_info_source <- dds_object@metadata$txomeInfo$source
  txome_info_version <- dds_object@metadata$txomeInfo$release

  usual_packages <- c(
    "DESeq2",
    "edgeR",
    "limma",
    "tximeta",
    "topGO",
    "goseq",
    "clusterProfiler",
    "ReactomePA",
    "pcaExplorer",
    "ideal",
    "GeneTonic",
    "mosdef",
    "apeglm"
  )

  all_packages <- c(usual_packages, packages)

  pkg_versions <- lapply(all_packages, packageVersion)
  names(pkg_versions) <- all_packages

  mnms <- glue::glue(template_text,
                     version_salmon = version_salmon,
                     txome_info_source = txome_info_source,
                     txome_info_version = txome_info_version,
                     version_tximeta = pkg_versions$tximeta,
                     version_pcaExplorer = pkg_versions$pcaExplorer,
                     version_DESeq2 = pkg_versions$DESeq2,
                     value_FDR = FDRthreshold,
                     version_apeglm = pkg_versions$apeglm,
                     version_topGO = pkg_versions$topGO,
                     version_goseq = pkg_versions$goseq,
                     version_clusterProfiler = pkg_versions$clusterProfiler,
                     version_mosdef = pkg_versions$mosdef,
                     version_ReactomePA = pkg_versions$ReactomePA,
                     version_GeneTonic = pkg_versions$GeneTonic
  )

  # TOOD: oooooor... return the list of named arguments to be glued, and simply print out the message?
  return(mnms)

}




#' Title TODO
#'
#' TODO
#'
#' @param sce_object TODO
#' @param template_text TODO
#' @param packages TODO
#' @param de_framework TODO
#' @param add_citations TODO
#'
#' @returns TODO
#'
#' @importFrom glue glue
#' @importFrom utils packageVersion
#'
#' @export
#'
#' @examples
#' # TODO
matmetmaker_RNAseqsinglecell <- function(sce_object,
                                         template_text = NULL,
                                         packages = NULL,
                                         de_framework = "muscat",
                                         add_citations = TRUE) {
  # TODO TODO TODO
}

