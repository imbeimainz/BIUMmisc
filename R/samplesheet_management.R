
#' Title
#'
#' @param n_samples Numeric value, corresponds to the number of samples included
#' @param ss_filename Character string, basename of the excel file that will
#' get generated
#' @param ss_dir Character string, folder where the sample sheet will be
#' generated into. Defaults to "." (the current working directory).
#' @param extra_fields Vector of character strings, specifying the names of the
#' columns for extra fields to be added.
#'
#' @return The character string corresponding to the path to the Excel sample
#' sheet created. The Excel sheet is created as a side effect.
#'
#' @export
#'
#' @examples
#' mysheet <- samplesheet_creator(ss_filename = "mysheet",
#'                                ss_dir = tempdir())
#' mysheet
samplesheet_creator <- function(n_samples = 4,
                                ss_filename,
                                ss_dir = ".",
                                extra_fields = NULL) {

  mandatory_fields <- c("id", "fastq_file1", "fastq_file2", "group",
                        "project_scenario", "sample_id", "quants_files", "notes")

  ss_df <- data.frame(matrix(nrow = n_samples,
                             ncol = length(mandatory_fields),
                             ""))

  colnames(ss_df) <- mandatory_fields
  ss_df$id <- paste0("id", seq_len(n_samples))

  ss_df$fastq_file1 <- "INFO: fill in with the location of the .fastq(.gz) files"
  ss_df$fastq_file2[1] <- "INFO: fill in (if paired end data is available)"
  ss_df$group[1] <- "INFO: fill in for a general definition of the experimental group"
  ss_df$project_scenario[1] <- "INFO: provide a name for the whole scenario (one single value should suffice)"
  ss_df$sample_id <- "INFO: to be filled in by the bioinformatics team"
  ss_df$quants_files <- "INFO: to be filled in by the bioinformatics team"
  ss_df$notes[1] <- "INFO: fill in to add any relevant freetext note about each sample"

  ss_df

  if (!is.null(extra_fields)) {
    # add those extra columns and populate them to empty char strings
    for (extra_col in extra_fields) {
      ss_df[[extra_col]] <- ""
      ss_df[[extra_col]][1] <- "INFO: fill in with specific info for the extra field"
    }
  }

  # ss_df

  ssheet_path <- file.path(ss_dir, paste0(ss_filename, ".xlsx"))

  ssheet_file <- writexl::write_xlsx(ss_df, path = ssheet_path)

  all_cols <- colnames(ss_df)
  message("Created a new sample sheet for ", n_samples,
          " samples with the following fields:\n",
          paste(all_cols, collapse = ", "))
  message("------\nPlease make sure to fill in the respective info fields, \n",
          "following the indications in the pre-filled cells")

  # return(ss_df)
  invisible(ssheet_file)
}

