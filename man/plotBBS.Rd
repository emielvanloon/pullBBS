% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BBSLoader.R
\name{plotBBS}
\alias{plotBBS}
\title{Plot data from the North American Breeding Bird Survey}
\usage{
plotBBS(routes, year, covariates, colorScheme = c("#FF5555", "#FFFF55",
  "#55FF55"), markerSize = 7, AOU)
}
\arguments{
\item{routes}{A data.frame returned from the function WideToLongBBS.}

\item{covariates}{An optional raster or raster sobject to be used as a layer containing covariate information.}

\item{colorScheme}{An optional argument to specify the color map of the covariate raster layers.}

\item{markerSize}{An optional argument specifyig the radius of the plotted circle markers.}

\item{AOU}{Optional vector of AOU codes to plot  If unused, all species will be plotted.}
}
\value{
A interactive map which can be used to compare observation data between species.
}
\description{
Plot data from the North American Breeding Bird Survey
}
\details{
Each site in the North American Breeding Bird Survey consists of 50 stops along a route.  This function will return the summed observations for each species accross all 50 stops for each site.
}

