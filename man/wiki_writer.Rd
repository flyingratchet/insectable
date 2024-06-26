% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insectable.R
\name{wiki_writer}
\alias{wiki_writer}
\title{Function to update MediaWiki pages from a data frame
This function takes a data frame with page names and corresponding text and updates
the specified pages on a MediaWiki site using a specified bot account.}
\usage{
wiki_writer(
  data,
  site_url,
  bot_name,
  bot_password,
  summary_message,
  char_limit = 1e+06,
  retries = 3
)
}
\arguments{
\item{data}{data frame containing columns 'page_name' and 'wiki_text'}

\item{bot_name}{Username of the bot account used for editing pages}

\item{bot_password}{Password for the bot account}

\item{summary_message}{Summary message to accompany edits}

\item{char_limit}{Character limit for splitting wiki pages (default 1,000,000)}

\item{retries}{Number of retry attempts for failed requests (default 3)}

\item{mw_site_url}{URL of the MediaWiki site}
}
\description{
Function to update MediaWiki pages from a data frame
This function takes a data frame with page names and corresponding text and updates
the specified pages on a MediaWiki site using a specified bot account.
}
