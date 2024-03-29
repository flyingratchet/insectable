% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insectable.R
\name{wiki_writer}
\alias{wiki_writer}
\title{Function that takes a data frame with two columns, one with the MediaWiki
page name and one with the text to be uploaded to overwrite the text on that page.}
\usage{
wiki_writer(data, mw_site_url, bot_name, bot_password, summary_message)
}
\arguments{
\item{mw_site_url}{a string representing a mediawiki site url}

\item{bot_name}{a string representing a mediawiki bot account name}

\item{bot_password}{a string representing a mediawiki bot passord}

\item{summary_message}{a string representing a summary message to log with the page edit}

\item{df}{a data frame with columns named "page_name" and "wiki_text"}
}
\description{
Function that takes a data frame with two columns, one with the MediaWiki
page name and one with the text to be uploaded to overwrite the text on that page.
}
