#' Prints out the timestamp
#'
#' Prints out the timestamp to console and to a file. Useful for diagnosing
#' potential failures of a terminal server session.
#'
#' @param file_out Character, path to the filename to print out the series of
#' timestamps
#' @param extrainfo Character, specifies the extra information to print out
#' before the timestamp is recorded. Defaults to
#' @param secs_interval Numeric value, specifying the interval of seconds after
#' which to repeat the timestamp operation
#' @param max_lines Integer, number of max lines to record - the time interval
#' covered will depend on this *and* on the `secs_interval`. This is mainly to
#' avoid having a really endless loop and not being able to stop the process.
#' @param verbose Logical, defaults to TRUE
#'
#' @return Invisible NULL - the output is very much the side effect of printing
#' the timestamp to file
#' @export
#'
#' @examples
#' # count_up_till_death("mytestfile.txt", extrainfo = "YourName")
count_up_till_death <- function(file_out,
                                extrainfo = NULL,
                                secs_interval = 60,
                                max_lines = 10000,
                                verbose = TRUE) {
  nr_lines <- 0

  while (nr_lines <= max_lines) {
    cur_timestamp <- Sys.time()
    cur_line <- paste0(
      ifelse(is.null(extrainfo), "", paste0(extrainfo, "___")),
      as.character(paste0(cur_timestamp, "\n"))
    )
    cat(cur_line, file = file_out, append = TRUE)

    nr_lines <- nr_lines + 1

    if (verbose)
      message("done printing out line ", nr_lines, " - ", cur_line)

    Sys.sleep(secs_interval)
  }

  invisible(NULL)
}
