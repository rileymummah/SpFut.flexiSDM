% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DND_filter.R
\name{DND_filter}
\alias{DND_filter}
\title{Get species ranges for reference}
\usage{
DND_filter(
  data,
  covs.mean,
  covs.sum,
  sp.code,
  DND.maybe,
  age.use = c("adult", "metamorph", "juvenile", "egg mass", "NR", NA),
  req.cols = c("unique.id", "site.id", "lat", "lon", "day", "month", "year",
    "survey.conducted", "survey.id", "data.type", "species", "time.to.detect", "count")
)
}
\arguments{
\item{range.path}{A vector of paths leading to species ranges}

\item{range.name}{A vector of names for each range}

\item{crs}{EPSG code for desired crs of output}
}
\value{
A list containing ranges as sf objects
}
\description{
Get species ranges for reference
}
