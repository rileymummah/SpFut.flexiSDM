% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_effects.R
\name{plot_effects}
\alias{plot_effects}
\title{Plot marginal effects of process parameters}
\usage{
plot_effects(data, out, breaks = 0.01)
}
\arguments{
\item{data}{(list) output from data_for_nimble()}

\item{out}{(list) output from summarize_chains()}

\item{breaks}{(numeric) distance between covariate values to simulate}
}
\value{
plot of marginal effects
}
\description{
Plot marginal effects of process parameters
}
