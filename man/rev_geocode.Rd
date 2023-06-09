% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insectable.R
\name{rev_geocode}
\alias{rev_geocode}
\title{This function attaches desired data from a shapefile to a Symbiota formatted
data frame.}
\usage{
rev_geocode(df, poly, field_name, new_name)
}
\arguments{
\item{df}{a Symbiota formatted database}

\item{poly}{a simple feature formatted shapefile of polygons}

\item{field_name}{The name of the single field in the shapefile you want to extract}

\item{new_name}{User-defined new name for the newly attached field in the dataframe}
}
\value{
returns a database with desired column from the shapefile attached
}
\description{
This function attaches desired data from a shapefile to a Symbiota formatted
data frame.
}
