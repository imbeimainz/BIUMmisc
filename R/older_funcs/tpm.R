#' Title
#'
#' @param counts
#' @param lengths
#'
#' @return
#' @export
#'
#' @examples
tpm_diy <- function(counts, lengths) {
  tpm_fun <- function(counts, lengths) {
    rpk <- counts/(lengths/1000)
    coef <- sum(rpk) / 1e6
    rpk/coef
  }
  tpms <- apply(counts, 2, function(x) tpm_fun(x, lengths) )
  colnames(tpms) <- colnames(counts)
  rownames(tpms) <- rownames(counts)
  return(as.data.frame(tpms))
}
