% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covid19_models.R
\name{plt.SIR.model}
\alias{plt.SIR.model}
\title{function to plot the results from the SIR model fn}
\usage{
plt.SIR.model(
  SIR.model,
  geo.loc = "",
  interactiveFig = FALSE,
  fileName = NULL,
  interactive.display = TRUE,
  add.extras = TRUE
)
}
\arguments{
\item{SIR.model}{model resulting from the generate.SIR.model() fn}

\item{geo.loc}{optional string to specify geographical location}

\item{interactiveFig}{optional flag to activate interactive plot}

\item{fileName}{file where to save the HTML version of the interactive figure}

\item{interactive.display}{boolean flag to indicate whether the interactive plot will be displayed (pushed) to your browser}

\item{add.extras}{boolean flag to add extra indicators, such as, the "force of infection" and time derivatives}
}
\description{
function to plot the results from the SIR model fn
}
