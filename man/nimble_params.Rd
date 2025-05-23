% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nimble_params.R
\name{nimble_params}
\alias{nimble_params}
\title{Set up nimble parameters}
\usage{
nimble_params(data, constants, sp.auto, lambda = T, XB = T)
}
\arguments{
\item{data}{(list) data formatted for nimble}

\item{constants}{(list) constants formatted for nimble}

\item{sp.auto}{(logical) whether to trace parameters associated with spatial model (T) or not (F)}

\item{lambda}{(logical) whether to trace lambda (T) or not (F)}

\item{XB}{(logical) whether to trace XB (T) or not (F)}
}
\value{
A vector of parameters to trace
}
\description{
Sets up vector of parameters to trace
}
\examples{
\dontrun{

params <- nimble_params(data,
                       constants,
                       lambda = T,
                       XB = T,
                       sp.auto = T)

}
}
