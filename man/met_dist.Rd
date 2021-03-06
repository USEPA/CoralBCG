% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/met_dist.R
\name{met_dist}
\alias{met_dist}
\title{Get distribution estimates}
\usage{
met_dist(met_in, scr_in)
}
\arguments{
\item{met_in}{input \code{data.frame} of coral metrics}

\item{scr_in}{input \code{data.frame} of site BCG scores, see details}
}
\value{
A two-element list named \code{met_in} and \code{pars}, where the former is the joined input data with BCG scores in \code{scr_in} and the latter is a \code{data.frame} with parameter (mean and standard deviation) estimates for each metric at each BCG level.
}
\description{
Get distribution estimates for BCG metrics by level
}
\details{
The input \code{scr_in} data has two columns labelled \code{station_code} and \code{scr}. The station codes should match those in \code{met_in}. The \code{scr} values for each station can be numeric or character string BCG levels that represent qualitative rankings. 

BCG levels for each metric are based on the raw metric distributions for the a priori expert rankings of each site. Distributions for each metric at each level are recreated from normal distributions, where the mean and standard deviation of each distribution are based on maximum likelihoods estimates from the raw data.
}
\examples{
\dontrun{
# coral metrics
met_in <- get_stony_mets(crl_dem)

# bcg scores for each station
station_code <- c(1:5)
scr <- c(2, 5, 3, 2, 4)
scr_in <- data.frame(station_code, scr) 

# get levels
met_dist(met_in, scr_in)
} 
}
