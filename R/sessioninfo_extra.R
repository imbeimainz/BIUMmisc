#' Title
#'
#' @param to_file todo
#' @param out_file todo
#' @param add_date todo
#'
#' @return todo
#'
#' @importFrom sessioninfo session_info
#' @importFrom rstudioapi getSourceEditorContext isAvailable
#' @importFrom tools file_path_sans_ext
#' @export
#'
#' @examples
#' # todo
sessioninfo_extra <- function(to_file = TRUE, out_file = NULL, add_date = FALSE) {
  # run sessioninfo::session_info
  si <- sessioninfo::session_info()

  if(rstudioapi::isAvailable()) {
    currfile <- rstudioapi::getSourceEditorContext()$path
    currwd <- dirname(currfile)
  } else {
    currfile <- tempfile()
    currwd <- tempdir()
  }

  currfile_noext <- tools::file_path_sans_ext(basename(currfile))

  # print its content into a file, named in a clever meaningful way
  if (to_file) {
    if (is.null(out_file)) {
      if (add_date) {
        dest_file <- file.path(
          currwd, paste0("sessioninfoextra___", Sys.Date(), "_", currfile_noext, ".txt")
        )
      } else {
        dest_file <- file.path(
          currwd, paste0("sessioninfoextra___", currfile_noext, ".txt")
        )
      }

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
