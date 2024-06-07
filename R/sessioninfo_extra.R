#' Title
#'
#' @param to_file
#' @param out_file
#'
#' @return
#' @export
#'
#' @examples
sessioninfo_extra <- function(to_file = TRUE, out_file = NULL) {
  # run sessioninfo::session_info
  si <- sessioninfo::session_info()

  currfile <- rstudioapi::getSourceEditorContext()$path
  currwd <- dirname(currfile)
  currfile_noext <- tools::file_path_sans_ext(basename(currfile))

  # print its content into a file, named in a clever meaningful way
  if (to_file) {
    if (is.null(out_file)) {
      dest_file <- file.path(
        currwd, paste0("sessioninfo_", Sys.Date(), "_", currfile_noext, ".txt")
      )
    } else {
      stopifnot(is.character(out_file))
      dest_file <- out_file
    }

    message("Writing session info to file ", dest_file)
    writeLines(text = as.character(si),
               con = dest_file)
  }

  # print out sessioninfo, just like that
  return(si)
}
