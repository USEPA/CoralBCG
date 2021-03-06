% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_3d.R
\name{est_3d}
\alias{est_3d}
\title{Estimate 3d surface area of a coral colony,}
\usage{
est_3d(spp, h, d1, d2 = d1)
}
\arguments{
\item{spp}{chr string of coral species to estimate}

\item{h}{numeric of colony height (cm)}

\item{d1}{numeric of colony diameter (cm), diameter of major axis if ellipse}

\item{d2}{numeric of colony diameter (dm), diameter of minor axis if ellipse}
}
\value{
Vector of values with estimated surface area as cm2 for each species, -1 is juvenile
}
\description{
Estimate 3d surface area of a coral colony,
}
\details{
If both d1 and d2 are provided, they are converted to a single value for the diameter of a circle with the same area as the ellipse

NA values will be all values that do not match species in \code{\link{conv}}, juveniles marked as -1
 
Uses size conversion equations from appendix J
}
\examples{
with(crl_dem, est_3d(species_name, Height, MaxDiam, PerpDiam)) 
}
