% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/de_funcs.R
\name{beautify_GO_table}
\alias{beautify_GO_table}
\title{Beautifying an enrichment table with GO identifiers in it}
\usage{
beautify_GO_table(enrich_tbl, GO_id_column = "GO.ID")
}
\arguments{
\item{enrich_tbl}{A data frame tabular representation of the enrichment results}

\item{GO_id_column}{Character string, specifying which column contains the
GO term identifier}
}
\value{
A data.frame, ready to be passed on to
\code{DT::datatable(..., escape = FALSE)}, to render the buttons correctly
}
\description{
Beautifying an enrichment table with GO identifiers in it
}
\examples{
## TODO
}
