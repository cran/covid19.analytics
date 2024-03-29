% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tor_utils.R
\name{report.Tor}
\alias{report.Tor}
\title{function to obtain main indicators from Toronto data}
\usage{
report.Tor(
  colTgts = c("Source.of.Infection", "Age.Group", "Client.Gender", "Outcome",
    "Neighbourhood.Name"),
  report = TRUE,
  staticPlt = TRUE,
  horiz.plts = 4,
  vert.plts = 3,
  same.Yaxis = TRUE,
  interactiveFig = FALSE,
  interactive.display = TRUE
)
}
\arguments{
\item{colTgts}{optional argument to indicate which columns from the Toronto data to process}

\item{report}{optional argument indicating whether a report will be printed to the screen}

\item{staticPlt}{optional argument to indicate whether the 'static' graphical output is wanted or not}

\item{horiz.plts}{number of plots in the horizontal direction}

\item{vert.plts}{number of plots in the vertical direction}

\item{same.Yaxis}{graphical argument to indicate if plots will use same y-axis}

\item{interactiveFig}{boolean flag to indicate whether interactice figures would be generated or not}

\item{interactive.display}{boolean flag to indicate whether the interactive plot will be displayed (pushed) to your browser}
}
\value{
list with statistics by selected as indicated in colTgts

#@export
}
\description{
function to obtain main indicators from Toronto data
}
