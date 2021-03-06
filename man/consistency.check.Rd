% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covid19_checks.R
\name{consistency.check}
\alias{consistency.check}
\title{function that determines whether there are consistency issues within the data, such as, anomalies in the cumulative quantities of the data as reported by JHU/CCSEGIS}
\usage{
consistency.check(
  data,
  n0 = 5,
  nf = ncol(data),
  datasetName = "",
  disclose = FALSE,
  details = TRUE
)
}
\arguments{
\item{data}{dataset to analyze}

\item{n0}{column where the cumulative data begins}

\item{nf}{column where the cumulative data ends}

\item{datasetName}{optional argument to display the name of the dataset}

\item{disclose}{boolean flag to indicate whether index of problematic entries are returned}

\item{details}{optional argument to specify whether to show details about the records where inconsistencies were detected}
}
\description{
function that determines whether there are consistency issues within the data, such as, anomalies in the cumulative quantities of the data as reported by JHU/CCSEGIS
}
