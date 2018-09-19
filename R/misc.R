# sep guesser
#' Make an educated guess on the separator character
#'
#' This function tries to guess which separator was used in a text delimited file
#'
#' @param file The name of the file which the data are to be read from
#' @param sep_list A vector containing the candidates for being identified as
#' separators. Defaults to \code{c(",", "\t", ";"," ")}
#'
#' @return A character value, corresponding to the guessed separator. One of ","
#' (comma), "\\t" (tab), ";" (semicolon)," " (whitespace)
#' @export
#'
#' @examples
#' sepguesser(system.file("extdata/design_commas.txt",package = "ideal"))
#' sepguesser(system.file("extdata/design_semicolons.txt",package = "ideal"))
#' sepguesser(system.file("extdata/design_spaces.txt",package = "ideal"))
#' mysep <- sepguesser(system.file("extdata/design_tabs.txt",package = "ideal"))
#'
#' # to be used for reading in the same file, without having to specify the sep
#'
sepguesser <- function(file, sep_list = c(",", "\t", ";"," ")) {
  separators_list = sep_list
  rl = readLines(file, warn = FALSE)
  rl = rl[rl != ""] # allow last line to be empty
  sephits_min = sapply(separators_list, function(x) min(stringr::str_count(rl, x))) #minimal number of separators on all lines
  sep = separators_list[which.max(sephits_min)]
  sep
}


