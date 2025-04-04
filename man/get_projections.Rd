% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_projections.R
\name{get_projections}
\alias{get_projections}
\title{Extract and calculate derived quantities for projections}
\usage{
get_projections(i, data, beta, spat, lambda0)
}
\arguments{
\item{i}{(numeric) projection number}

\item{data}{(NIMBLE data) data created and formatted for NIMBLE}

\item{beta}{(matrix) beta estimates extracted from MCMC samples}

\item{spat}{(matrix) spatial effect estimates extracted from MCMC samples}

\item{lambda0}{(matrix) lambda estimates extracted from MCMC samples}
}
\value{
A data.frame of lambda, psi, and XB estimates for projection i
}
\description{
Extract and calculate derived quantities for projections
}
