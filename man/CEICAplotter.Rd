% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CEICAplotter.R
\name{CEICAplotter}
\alias{CEICAplotter}
\title{Plots cost-effectiveness and incremental cost analysis}
\usage{
CEICAplotter(altnames, benefit, cost, CE, BB, figure.name)
}
\arguments{
\item{altnames}{vector of numerics or characters as unique restoration action identifiers.}

\item{benefit}{a vector of restoration benefits. Typically, these are time-averaged
ecological outcomes (e.g., average annual habitat units). Often project benefits
are best presented as the "lift" associated with a restoration action
(i.e., the benefits of an alternative minus the benefits of a "no action" plan).}

\item{cost}{a vector of restoration costs. Typically, these are monetary costs
associated with a given restoration action such as project first cost or
annualized economic cost. Notably, these functions are agnostic to units, so costs
could also be non-monetary such as lost political capital or social costs
of each alternative.}

\item{CE}{numeric vector of 0's and 1's indicating whether a plan is cost-effective (1)
or non-cost-effective (0). Can be derived from ecorest::CEfinder.}

\item{BB}{numeric vector of 0's and 1's indicating whether a plan is a best buy (1)
or not (0). Can be derived from ecorest::BBfinder.}

\item{figure.name}{output figure file name structured as "filename.jpeg".}
}
\value{
A multi-panel *.jpeg figure summarizing cost-effectiveness and incremental
 cost analyses.
}
\description{
\code{CEICAplotter} Plots Cost-effective Incremental Cost Analysis (CEICA) in *.jpeg format.
}
\examples{

#Identify cost-effective actions based on random vectors of benefits and costs
altnames<- paste("Alt",seq(1,50), sep="")
benefit <- runif(50,min=0,max=10)
cost <- runif(50, min=0,max=1000)
CE <- CEfinder(benefit, cost)
BB <- BBfinder(benefit, cost, CE)[[1]][,4]
CEICAplotter(altnames, benefit, cost, CE, BB, tempfile("CEICAexample",fileext=".jpeg"))

}
\references{
Robinson R., Hansen W., and Orth K. 1995. Evaluation of environmental investments
procedures manual interim: Cost effectiveness and incremental cost analyses.
IWR Report 95-R-1. Institute for Water Resources, U.S. Army Corps of Engineers, Alexandria, Virginia
}
