% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-docs.R
\docType{data}
\name{bec_zone_table}
\alias{bec_zone_table}
\title{BEC zone lookup table}
\format{
A data.table with columns:
\describe{
\item{nfi_plot}{Plot identifier}
\item{loc_id}{Location identifier}
\item{BEC_ZONE_CODE}{Assigned BEC zone code}
}
}
\source{
Computed using \code{bcdata::bcdc_get_data()} and saved with \code{precompute_bec_zone()}.
}
\usage{
bec_zone_table
}
\description{
This dataset provides the Biogeoclimatic Ecosystem Classification (BEC) zone code
assigned to each NFI plot location based on latitude and longitude.
}
\keyword{datasets}
