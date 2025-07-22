#' Title
#'
#' @param file_name
#' @param report_folder
#' @param ext_name
#'
#' @returns
#' @export
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#'
#' @examples
create_bioinfo_html <- function(file_name = NULL,
                                report_folder = NULL,
                                ext_name = "bioinfo-html",
                                expose_aux_files = TRUE) {

  if (is.null(file_name)) {
    stop("You must provide a valid file_name")
  }

  if (is.null(report_folder)) {
    cli::cli_alert_info("Defaulting to create the report and its associated files in the current directory")
    report_folder <- "."
  } else {
    if(!dir.exists(report_folder)) {
      dir.create(report_folder)
      cli::cli_alert_info("Created report folder into ", report_folder)
    }
  }

  # check for available extensions
  stopifnot("Extension not in package" = ext_name %in% c("bioinfo-html"))

  # check for existing _extensions directory
  if(!dir.exists(file.path(report_folder, "_extensions"))) {
    dir.create(file.path(report_folder, "_extensions"))
    cli::cli_alert_info("Created '_extensions' folder into ", report_folder)
  }

  # create folder for the specific extension
  if(!dir.exists(file.path(report_folder, "_extensions/", ext_name)))
    dir.create(file.path(report_folder, "_extensions/", ext_name))

  # copy from internals
  file.copy(
    from = system.file("extdata", "_extensions/", ext_name, package = "BIUMmisc"),
    to = file.path(report_folder, "_extensions/"),
    overwrite = TRUE,
    recursive = TRUE,
    copy.mode = TRUE
  )

  # logic check to make sure extension files were moved
  n_files <- length(list.files(file.path(report_folder, "_extensions/", ext_name)))

  if(n_files >= 3){
    cli::cli_alert_info(
      paste(ext_name, "was installed to _extensions folder in the specified report directory.")
    )
  } else {
    cli::cli_alert_warning("Extension appears not to have been created")
  }

  # create new qmd report based on skeleton
  file.copy(file.path(report_folder, "_extensions", "bioinfo-html", "bioinfo-html-report-template.qmd"),
            file.path(report_folder, paste0(file_name, ".qmd")))

  if (expose_aux_files) {
    file.copy(file.path(report_folder, "_extensions", "bioinfo-html", "references.bib"),
              file.path(report_folder, "references.bib"))
    file.copy(file.path(report_folder, "_extensions", "bioinfo-html", "ummz_theme.scss"),
              file.path(report_folder, "ummz_theme.scss"))
    cli::cli_alert_info("Copied auxiliary files into the toplevel report folder",
                        "You might want to edit the corresponding lines in the yaml section")
  }


  # open the new file in the editor
  file.edit(file.path(report_folder, paste0(file_name, ".qmd")))


  cli::cli_alert_success("Have fun with your report - available inside the folder", report_folder)
}



