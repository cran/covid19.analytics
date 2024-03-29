% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covid19_Canada.R
\name{covid19.Toronto_city.data}
\alias{covid19.Toronto_city.data}
\title{function to import data from the city of Toronto, ON - Canada
as reported by the City of Toronto
https://www.toronto.ca/home/covid-19/covid-19-pandemic-data/}
\usage{
covid19.Toronto_city.data(
  data.fmt = "TS",
  local.data = FALSE,
  debrief = FALSE,
  OLD.fmt = FALSE,
  acknowledge = FALSE
)
}
\arguments{
\item{data.fmt}{"TS" for TimeSeries of cumulative cases or "original" for the data as reported in the google-document with multiple sheets}

\item{local.data}{boolean flag to indicate whether the data will be read from the local repo, in case of connectivity issues or data integrity}

\item{debrief}{boolean specifying whether information about the read data is going to be displayed in screen}

\item{OLD.fmt}{boolean flag to specify if the data is being read in an old format}

\item{acknowledge}{boolean flag to indicate that the user acknowledges where the data is coming from.  If FALSE, display data acquisition messages.}
}
\value{
a dataframe (or a list in the case of "original") with the latest data reported for the city of Toronto, ON - Canada
}
\description{
function to import data from the city of Toronto, ON - Canada
as reported by the City of Toronto
https://www.toronto.ca/home/covid-19/covid-19-pandemic-data/
}
