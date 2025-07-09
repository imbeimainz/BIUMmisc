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
mamma_bulk <- function(dds_object,
                       template_text = NULL,
                       packages = NULL,
                       FDRthreshold = NULL,
                       de_framework = "DESeq2",
                       add_citations = TRUE) {

  if(is.null(FDRthreshold)) {
    # try to pick it from the environment it is launched from
    if(exists("FDR")) {
      FDRthreshold <- get("FDR")
    } else {
      stop("Variable `FDR` not defined in the current environment!")
    }
  }

  template_text <- paste0(
    "Quality control on the sequencing data was performed with the FastQC tool (version 0.XX.YY [[CHANGEMEIFNEEDED]], https://www.bioinformatics.babraham.ac.uk/projects/fastqc/). [[OPTIONAL:]] After preprocessing, the number of reads per sample ranged from XX million to YY million with a median of ZZ million.\n",
    "[[if using Salmon]] Transcript abundance estimates were computed with Salmon [CIT:10.1038/nmeth.4197] (version {version_salmon}), with a [transcriptome index|combined transcriptome and genome index] generated from the {txome_info_source} reference (version {txome_info_version}), and subsequently summarized to gene level with the tximeta R package (version {version_tximeta}) [CIT:10.1371/journal.pcbi.1007664].\n",
    "The exploration, modeling, and interpretation of the expression data follows the protocols defined by Ludt et al (2022), [CIT:10.1002/cpz1.411]. [alt: All downstream analyses were performed in R... following ...[]].\n",
    "Exploratory data analysis was performed with the pcaExplorer package (version {version_pcaExplorer}, [CIT:10.1186/s12859-019-2879-1 ]). Principal Component Analysis plots were performed including the top XXX most variable genes.[[CHANGEMEIFNEEDED]]\n",
    "Differential expression analysis was performed with DESeq2 package (version {version_DESeq2}) [CIT:10.1186/s13059-014-0550-8 ], setting the false discovery rate (FDR) cutoff to {value_FDR}. Accurate estimation of the effect sizes (described as log2 fold change) was performed using the apeglm shrinkage estimator (version {version_apeglm}) [CIT:10.1093/bioinformatics/bty895 ].\n",
    "Further analyses included Gene Ontology pathway enrichment by the topGO (version {version_topGO}) | goseq (version {version_goseq}) | clusterProfiler (version {version_clusterProfiler}) packages [CIT:10.1093/bioinformatics/btl140][CIT:10.1186/gb-2010-11-2-r14 ][CIT:10.1038/s41596-024-01020-z], setting all detected|expressed genes as background dataset, and were performed using the mosdef package (version {version_mosdef}) [ADDCIT] and the ideal package (version {version_ideal}) [CIT:10.1186/s12859-020-03819-5].",
    "The enrichment results were subsequently processed with the GeneTonic package (version {version_GeneTonic}) for visualization and summarizing [CIT:10.1186/s12859-021-04461-5].\n",
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
                     version_ideal = pkg_versions$ideal,
                     version_ReactomePA = pkg_versions$ReactomePA,
                     version_GeneTonic = pkg_versions$GeneTonic
  )

  # TOOD: oooooor... return the list of named arguments to be glued, and simply print out the message?

  cli::cli_alert_success("Here's a template for your M&Ms, please review this carefully.")
  cli::cli_alert_info("Most citations are provided with their DOI!")
  return(mnms)

}




#' Title TODO
#'
#' TODO
#'
#' @param sce_object TODO
#' @param template_text TODO
#' @param packages TODO
#' @param FDRthreshold TODO
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
mamma_singlecell <- function(sce_object,
                             template_text = NULL,
                             packages = NULL,
                             FDRthreshold = NULL,
                             de_framework = "muscat",
                             add_citations = TRUE) {
  # TODO TODO TODO

  if(is.null(FDRthreshold)) {
    # try to pick it from the environment it is launched from
    if(exists("FDR")) {
      FDRthreshold <- get("FDR")
    } else {
      stop("Variable `FDR` not defined in the current environment!")
    }
  }


  template_text <- paste0(
    # matrix generation
    "Initial data processing, quality control, and integration followed the previously established protocol described by Nedwed et al. (2023) [REF:10.3389/fimmu.2023.1241283]. Raw sequencing reads were demultiplexed and aligned using Cell Ranger v7.1.0 [[CHANGEMEIFNEEDED]] (10x Genomics) [REF Cell Ranger], employing a transcriptome index generated from the [[CHANGEMEIFNEEDED]] Mus musculus genome build GRCm38. Gene annotation was based on [[CHANGEMEIFNEEDED]] ENSEMBL release 102 for Mus musculus.\n",
    # QC & normalization
    "Single-cell data were imported as a SingleCellExperiment object (version {version_SingleCellExperiment}) in R using Bioconductor version {version_bioc} [REF OSCA:10.1038/s41592-019-0654-x][[CHANGEMEIFNEEDED if using Seurat or scanpy]]. Quality control (QC) was performed independently on each dataset to remove poor-quality cells using scater version {version_scater} [REF scater:10.1093/bioinformatics/btw777]. Mitochondrial gene content served as a proxy for identifying damaged cells, using a threshold of three median absolute deviations above the median, in accordance with recommendations from the OSCA guidelines (https://bioconductor.org/books/release/OSCA/) [REF OSCA]. Doublet detection was performed with scDblFinder version {version_scDblFinder} [REF scDblFinder:10.12688/f1000research.73600.2]. After QC, normalization of cell-specific biases was conducted using the deconvolution-based method implemented in scran version {version_scran} [REF scran normalization:https://doi.org/]. Counts were divided by size factors, and normalized values were log-transformed after adding a pseudocount of one.\n",
    # integration
    "Integration of datasets from different biological samples was achieved using the Mutual Nearest Neighbors (MNN) method implemented in batchelor version {version_batchelor} [REF batchelor/MNN:10.1038/nbt.4091]. Subsequently, highly variable genes (HVGs) were identified by decomposing per-gene variability into technical and biological components based on the mean-variance trend.\n",
    # dim red & clustering
    "Dimensionality reduction was performed using Principal Component Analysis (PCA). PCA results were then used as input for t-distributed stochastic neighbor embedding (t-SNE) [REF tSNE:https://jmlr.csail.mit.edu/papers/v9/vandermaaten08a.html] and Uniform Manifold Approximation and Projection (UMAP) algorithms [REF UMAP:10.1038/nbt.4314] for visualization. Clustering analysis utilized the [[CHANGEMEIFNEEDED]]9,982 most highly variable genes to build a shared nearest neighbor graph (SNNG) [REF SNNG:10.1093/bioinformatics/btv088]. Clusters were determined using the Louvain community detection algorithm implemented via igraph (version {version_igraph}/version {version_leidenAlg})[REF igraph:10.5281/zenodo.7682609] [REF Louvain:10.48550/arXiv.0803.0476] [REF leiden:10.1038/s41598-019-41695-z]. The resolution parameter was set to [[CHANGEMEIFNEEDED]] 0.5.\n",
    # annotation and viz
    "Initial automated cell-type annotation was performed using SingleR (version {version_SingleR})[REF SingleR:10.1038/s41590-018-0276-y] with the [[CHANGEMEIFNEEDED]]ImmGenData reference from celldex (version {version_celldex}) [REF celldex]. These annotations were manually refined using established cell-type marker genes retrieved from the literature. Visualization and manual refinement of cell clusters were carried out using iSEE version {version_iSEE} [REF iSEE:10.12688/f1000research.14966.1] and iSEEfier version {version_iSEEfier} [REF iSEEfier:TODO]. Most visualizations for single-cell data were generated using iSEE. Marker gene detection among the identified clusters and groups of cells has been performed via the findMarkers function included in scran, which combines the multiple pairwise comparisons.\n",
    # testing framework, enrichment, & more
    "Pseudobulk differential gene expression analysis was conducted using muscat version {version_muscat} [REF muscat:10.1038/s41467-020-19894-4] with the default modeling approach leveraging the edgeR package (version {version_edgeR})[[CHANGEMEIFNEEDED]]. Multiple testing corrections were applied using the Benjamini-Hochberg (BH) method, and genes with adjusted p-values < {value_FDR} were considered differentially expressed genes (DEGs).\n",
    "Functional enrichment analysis of DEGs was performed with mosdef version {version_muscat} [REF mosdef], leveraging the topGO package (version {version_topGO})[REF topGO:10.1093/bioinformatics/btl140] and the clusterProfiler package (version {version_clusterProfiler})[REF clusterProfiler:10.1038/s41596-024-01020-z]. Enrichment analysis used the 'elim' algorithm within the Biological Process ontology, employing DEGs as input against a background set comprising all detected genes. Finally, differential abundance analysis comparing cell type proportions across treatment conditions was performed using the propeller method in the speckle package version {version_speckle} [REF speckle:10.5281/zenodo.7009042]. Statistical significance was defined at a False Discovery Rate (FDR) < {value_FDR} [REF FDR:10.1111/j.2517-6161.1995.tb02031.x].\n",
    "",
    ""
  )

  usual_packages_singlecell <- c(
    "SingleCellExperiment",
    "scater",
    "scDblFinder",
    "scran",
    "batchelor",
    "igraph",
    "leidenAlg",
    "SingleR",
    "celldex",
    "iSEE",
    "iSEEfier",
    "muscat",
    "edgeR",
    "mosdef",
    "speckle",
    "topGO",
    "goseq",
    "clusterProfiler",
    "ReactomePA",
    "pcaExplorer",
    "ideal",
    "GeneTonic",
    "Seurat"
  )

  all_packages_singlecell <- c(usual_packages_singlecell, packages)

  bioc_version <- as.character(BiocManager::version())

  pkg_versions <- lapply(all_packages_singlecell, packageVersion)
  names(pkg_versions) <- all_packages_singlecell

  mnms <- glue::glue(template_text,
                     version_bioc = bioc_version,
                     version_SingleCellExperiment = pkg_versions$SingleCellExperiment,
                     version_scater = pkg_versions$scater,
                     version_scDblFinder = pkg_versions$scDblFinder,
                     version_scran = pkg_versions$scran,
                     version_batchelor = pkg_versions$batchelor,
                     version_igraph = pkg_versions$igraph,
                     version_leidenAlg = pkg_versions$leidenAlg,
                     version_SingleR = pkg_versions$SingleR,
                     version_celldex = pkg_versions$celldex,
                     version_iSEE = pkg_versions$iSEE,
                     version_iSEEfier = pkg_versions$iSEEfier,
                     version_muscat = pkg_versions$muscat,
                     version_edgeR = pkg_versions$edgeR,
                     version_mosdef = pkg_versions$mosdef,
                     version_topGO = pkg_versions$topGO,
                     version_clusterProfiler = pkg_versions$clusterProfiler,
                     version_speckle = pkg_versions$speckle,
                     version_ReactomePA = pkg_versions$ReactomePA,
                     version_pcaExplorer = pkg_versions$pcaExplorer,
                     version_ideal = pkg_versions$ideal,
                     version_GeneTonic = pkg_versions$GeneTonic,
                     version_Seurat = pkg_versions$Seurat,
                     value_FDR = FDRthreshold
  )

  cli::cli_alert_success("Here's a template for your M&Ms, please review this carefully.")
  cli::cli_alert_info("Most citations are provided with their DOI!")
  return(mnms)
}


#' @rdname mamma_bulk
#' @export
matmetmaker_RNAseqbulk <- mamma_bulk

#' @rdname mamma_singlecell
#' @export
matmetmaker_RNAseqsinglecell <- mamma_singlecell


