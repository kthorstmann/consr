% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/consr.R
\name{prep_item_df}
\alias{prep_item_df}
\title{Prepare an dataframe for consensus analysis}
\usage{
prep_item_df(data, range = NULL)
}
\arguments{
\item{data}{The dataframe to be recoded.}

\item{range}{The range of the Likert-scale for all items in that dataframe. Must be a vector of at least two characters.}
}
\value{
The dataframe itself, where all entries outside of \code{range} are recoded as \code{NA}.
}
\description{
Prepare an dataframe for consensus analysis
}
\seealso{
\code{\link[consr]{prep_item}} for running this function on one item.
}

