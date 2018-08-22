#' Create folder structure
#'
#' @param rootname Character string, corresponding to the name of the main folder
#' to be created. Should ideally follow this schema:
#' [person_identifier]_[cooperation_partner_name]_[project_related_keyword]
#'
#' @return Creates the designed folder structure in the desired location
#' @export
#'
#' @examples # An example
create_folder_structure <- function(rootname=paste0(Sys.Date(),"_newproject_RENAME_ME"))
{
  dir.create(rootname)
  dir.create(file.path(rootname,"_fastqFiles"))
  dir.create(file.path(rootname,"_alignedData"))
  dir.create(file.path(rootname,"_logs"))
  dir.create(file.path(rootname,"_report"))
  dir.create(file.path(rootname,"_qc"))
  dir.create(file.path(rootname,"_counts"))
  dir.create(file.path(rootname,"_output"))
  dir.create(file.path(rootname,"_tempFiles"))

  message("Created folder structure for",rootname,". \nPlease rename the folder if necessary\n")
}
