% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insectable.R
\name{read_csv_char}
\alias{read_csv_char}
\title{wrapper for readr::read_csv that forces all cols to character
which also avoids the erroneous auto-guessing errors stemming from
fields which have too many NA's in their records}
\usage{
read_csv_char(csv_path)
}
\arguments{
\item{csv_path}{a path to a csv file}
}
\description{
wrapper for readr::read_csv that forces all cols to character
which also avoids the erroneous auto-guessing errors stemming from
fields which have too many NA's in their records
}
