% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aux_stats.R
\name{movingFn}
\alias{movingFn}
\title{generic fn that computes the "fn" on a moving window}
\usage{
movingFn(x, fn = mean, period = length(x), direction = "forward")
}
\arguments{
\item{x}{a numeric vector}

\item{fn}{a function to be applied/computed, default is set to mean()}

\item{period}{size of the "moving window", default set to the lenght of the vector}

\item{direction}{type of moving avergage to consider: "forward", "centered", "backward"; ie. whether the window computation is ( "centered" / "forward" / "backward" ) wrt the data series}
}
\value{
a vector with the 'moving operation' applied to the x vector
}
\description{
generic fn that computes the "fn" on a moving window
}
