% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_data.R
\name{get_datapoints}
\alias{get_datapoints}
\title{Get datapoints}
\usage{
get_datapoints(meta)
}
\arguments{
\item{meta}{dataframe row with the table metadata, with columns `code`,
`partial_file_url`, `extension`, `excelling_function`}
}
\value{
a dataframe with the period_id, value and id values for all the vintages in the table.
}
\description{
Get the datapoints by downloading and parsing the appropriate excel file
}
