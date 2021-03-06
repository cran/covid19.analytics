% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covid19.R
\name{covid19.US.data}
\alias{covid19.US.data}
\title{function to read the TimeSeries US detailed data}
\usage{
covid19.US.data(local.data = FALSE, debrief = FALSE, acknowledge = FALSE)
}
\arguments{
\item{local.data}{boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity}

\item{debrief}{boolean specifying whether information about the read data is going to be displayed in screen}

\item{acknowledge}{boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.}
}
\value{
TimeSeries dataframe with data for the US
}
\description{
function to read the TimeSeries US detailed data
}
