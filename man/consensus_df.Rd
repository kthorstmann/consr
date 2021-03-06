% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/consr.R
\name{consensus_df}
\alias{consensus_df}
\title{Consensus for data frame}
\usage{
consensus_df(data, range = NULL, consensus.only = FALSE, round = 2,
  check.int = TRUE)
}
\arguments{
\item{range}{The range of the items indicating the valid values. Default is \code{NULL}.}

\item{consensus.only}{Logical, if only the consensus should be reported. Default is to \code{FALSE}.}

\item{round}{The number of digits to round the result to. Default is to 2.}

\item{check.int}{Whether the function should check for integers. Default is to \code{TRUE}. Only integers are allowd. If \code{FALSE}, things can go horribly wrong.}

\item{item}{The dataframe with the items, for which the conensus should be computed.}
}
\value{
Returns an data frame with the consensus and additional information for each item. See details for further explanation.
}
\description{
Compute the consensus for all items of a data frame, based on Tastle & Wiermann, (2006).
}
\details{
\describe{
 \item{n}{The number of valid answers on that item}
 \item{min}{The minimum answer on that item}
 \item{max}{The maximum answer on that item}
 \item{consensus}{The consensus}
 \item{dissensus}{The dissensus}
 \item{n_missing}{The absolute number of missing values on that item, defined as \code{NA}}
 \item{perc_missing}{The relative number of missing values on that item, defined as \code{NA}}
}
}
\examples{
n = 10 # (max = 26 for now)
var_names <- paste0("var_", letters[1:10])
entries <- matrix(sample(-2:2, n*10, replace = TRUE),
                 nrow = n, ncol = 10)
entries[sample(1:100, 15)] <- NA
data <- as.data.frame(entries)
colnames(data) <- var_names
subj_id <- paste0("id_", sample(letters, n, replace = FALSE))
data <- cbind(subj_id, data, stringsAsFactors = FALSE)
data["var_j"] <- data["var_j"] / 2.3
consensus_df(data, range = c(-2, 2), check.int = TRUE)
}

