#' Generate commands to run salmon
#'
#' Generate commands to run salmon on the files from a standardized sample sheet
#' as the ones we exchange with collaboration partners
#'
#' @param samples_info_sheet Character string,
#' @param salmon_bin Character, specifying the name of the executable to be run
#' as `salmon`. Defaults to "salmon", which is perfectly fine if the tool is
#' available in the `PATH`, be that system wide or (even better) with a conda
#' environment
#' @param salmon_index_dir Character, defines the folder where the salmon index
#' is located. Needs to be specified by the user.
#' @param salmon_output_dir Character, specifies the folder where the outputs of
#' salmon will be written into. Sensibly defaults to "_quants", which matches
#' the folder structure we are familiar with
#' @param quants_prefix Character - what string should be prepended to the values
#' of the `id` field in the `samples_info_sheet`
#' @param salmon_ncores Numeric, number of cores to be used by salmon
#' @param salmon_options Character string, contains all additional salmon
#' parameters. Defaults to `--validateMappings`, but could also be an empty
#' string or set to `NULL`
#' @param create_script Logical value, whether to create or not a script - its
#' name is specified by `script_name`
#' @param script_name Character string, name of the script to write commands
#' into. Defaults to "salmonrunner.sh"
#' @param display_commands Logical, whether to print out to the console the
#' commands as message. Defaults to TRUE
#' @param force Logical value, whether to overwrite the file in `script_name` if
#' already existing. Defaults to FALSE
#'
#' @return Invisible `NULL`, the function prints out the commands as messages
#' *and* as a file, optionally
#'
#' @export
#'
#' @examples
#' mysheet <- samplesheet_creator(ss_filename = "mysheet", ss_dir = tempdir())
#' run_salmon(readxl::read_excel(mysheet), salmon_index_dir = "/myindex/dir",
#'            create_script = FALSE)
run_salmon <- function(samples_info_sheet, # contains the locations of each file/file pair
                       salmon_bin = "salmon",
                       salmon_index_dir,
                       salmon_output_dir = "_quants",
                       quants_prefix = "sample_scenarioid_sampleid_",
                       salmon_ncores = 12,
                       salmon_options = "--validateMappings",
                       create_script = TRUE,
                       script_name = "salmonrunner.sh",
                       display_commands = TRUE,
                       force = FALSE
) {
  # need the following columns:

  samples_info_sheet$id
  samples_info_sheet$fastq_file1
  samples_info_sheet$fastq_file2
  samples_info_sheet$quants_files

  message("Running salmon with index located at ", salmon_index_dir)

  message("Creating output quantifications at ", salmon_output_dir)


  # salmon quant -i /Users/fede/Projects/NGS/refs/salmon_index_gentrome_GENCODE_44 -l A -1 _fastqFiles/706_S39_R1_001.fastq.gz  -2 _fastqFiles/706_S39_R2_001.fastq.gz  -o _quants/sample_schwarting_id_706 --threads 8 --validateMappings


  # if(!all(file.exists(unlist(samples_info_sheet$fastq_file1)))) {
  #   warning("Not all fastq files of the samplesinfo object are actually existing!")
  #   return(samples_info_sheet)
  # }


  # check existence of indices
  # if(!dir.exists(salmon_index_dir))
  #   stop("salmon index not found!")

  N_samples <- nrow(samples_info_sheet)
  salmon_calls <- lapply(seq_len(N_samples), function (i){

    this_fastq_r1 <- samples_info_sheet$fastq_file1[[i]]
    this_fastq_r2 <- samples_info_sheet$fastq_file2[[i]]

    input_reads_salmon_param <- if(is.na(this_fastq_r2)) {
      # single end
      paste("-r", this_fastq_r1)
    } else {
      # paired end
      paste("-1", this_fastq_r1, "-2", this_fastq_r2)
    }

    salmon_call <- paste(
      salmon_bin, "quant",
      "--index", salmon_index_dir,
      "--libType A",

      input_reads_salmon_param,


      "-o", file.path(salmon_output_dir, paste0(quants_prefix, samples_info_sheet$id[i])),
      "--threads", salmon_ncores,
      ifelse(is.null(salmon_options), "", salmon_options)
    )

    return(salmon_call)
  })


  names(salmon_calls) <- paste0("Sample_", samples_info_sheet$id)
  # TODO: check if quant.sf already exist?


  all_cmds <- unlist(salmon_calls)


  out_bashscript <- script_name

  if(create_script) {
    if(file.exists(out_bashscript) & !force) {
      message("Bash script for running salmon already available at ", out_bashscript)
    } else {
      writeLines(all_cmds, con = out_bashscript)
      message("Bash script for running salmon generated at ", out_bashscript)
    }
  }

  if (display_commands)
    writeLines(all_cmds)

  invisible(NULL)
}


## TODO: option to not generate the command if it already finds the corresponding quant.sf files?

