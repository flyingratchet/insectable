% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insectable.R
\name{wiki_page_chunk_splitter}
\alias{wiki_page_chunk_splitter}
\title{Split Wiki Page Content into Manageable Chunks}
\usage{
wiki_page_chunk_splitter(data, char_limit = 1e+06)
}
\arguments{
\item{data}{A dataframe with at least two columns: `page_name` and `wiki_text`, where `page_name` is the name of the wiki page and `wiki_text` contains the text content of the wiki page.}

\item{char_limit}{An integer value specifying the maximum number of characters allowed per chunk. Default is 1,000,000 characters.}
}
\description{
This function splits a dataframe containing wiki page text into smaller chunks
if the total text exceeds a character limit. It ensures that all content comes from the same
page before splitting. If the text from multiple pages is mixed, the function will halt
and return an error.
}
