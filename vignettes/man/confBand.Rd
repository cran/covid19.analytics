% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aux_stats.R
\name{confBand}
\alias{confBand}
\title{function to draw confidence bands, using generalized moving averages/sds}
\usage{
confBand(
  x,
  y,
  x0,
  x1,
  y0,
  y1,
  windowsNbr = 10,
  period = ceiling(length(y)/windowsNbr),
  lcolour = "gray",
  ltype = 4,
  lwidth = 2,
  filling = TRUE
)
}
\description{
importFrom  grDevices  rgb
importFrom  graphics  lines polygon
importFrom  stats  sd
}
\keyword{internal}
