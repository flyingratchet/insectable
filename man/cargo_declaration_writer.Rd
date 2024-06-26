% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insectable.R
\name{cargo_declaration_writer}
\alias{cargo_declaration_writer}
\title{Generate MediaWiki Cargo Declarations and Template Structures}
\usage{
cargo_declaration_writer(
  df,
  cargo_table_name,
  field_formats,
  wiki_data_structure,
  infobox_template_name = NA,
  output_folder
)
}
\arguments{
\item{df}{A dataframe where the first column is considered an ID variable and is not included in the output.}

\item{cargo_table_name}{The name of the Cargo table to be declared and used in the MediaWiki environment.}

\item{field_formats}{A vector indicating the desired data types or formats for each column in `df`, except the ID.}

\item{wiki_data_structure}{A character string specifying how the data is structured relative to wiki pages.
Valid values are "one_to_one" or "collapsed". This determines the inclusion of certain template lines.}

\item{infobox_template_name}{Optional; the name of the infobox template to be included. If NA, no infobox is included.
Defaults to NA.}

\item{output_folder}{The directory path where the output text file should be saved.}
}
\description{
This function automates the creation of MediaWiki Cargo declarations and template structures based on a given dataframe.
It formats field names to comply with MediaWiki conventions and constructs different template components depending on the data structure specified.
The output is saved as a text file containing the necessary MediaWiki syntax for Cargo table declaration and related template usage.
}
