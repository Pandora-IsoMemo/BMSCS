% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-bindAllModelResults.R
\name{bindAllResults}
\alias{bindAllResults}
\title{Bind All results}
\usage{
bindAllResults(listOfDataframes, addEmptyRow = TRUE)
}
\arguments{
\item{listOfDataframes}{(list) list of data.frames that should be bind with 
\code{dplyr::bind_rows}}

\item{addEmptyRow}{(logical) if TRUE, add an empty row with NA values after each element of
\code{listOfDataframes}}
}
\description{
Bind All results
}
