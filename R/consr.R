#' Subset integers from data frame
#'
#' For computing the consensus, only integers should be included in the data frame.
#'
#' @param data a data frame or matrix.
#'
#' @return The same data frame as \code{data}, but the non-integers are dropped.
#' @export
#'
#' @examples
#' a <- sample(1:20, 10)
#' b <- rnorm(10)
#' c <- sample(-2:2, 10, replace = TRUE)
#' integer_subset_df(data.frame(a, b, c))
#' @seealso If you need to recode data into integers, see for example \code{\link[car]{recode}}.
integer_subset_df <- function(data){
  stopifnot(is.data.frame(data) | is.matrix(data))
  # select numerics
  num_only <- purrr::map(data, ~ is.numeric(.))
  num_data <- data[unlist(num_only)]
  # select integers
  int_only_i <- purrr::map(num_data, ~ .%%1==0)
  int_only <- purrr::map(int_only_i, ~ !any(!., na.rm = TRUE))
  int_data <- num_data[unlist(int_only)]
  int_variables <- names(int_data)
  if (length(int_variables) == 0) {
    stop("none of the variables in 'data' were integers. Make sure there are no decimal numbers (but negatie values are allowed)")
  } else {
    message(c("The following variables were selected: ",
              paste0(int_variables, collapse = ", ")))
  }
  return(int_data)
}

#' rbind data frames from lists
#'
#' @param list A list where each entry is a data frame with the same columns
#'
#' @return A data frame.
#' @export
rbind_map <- function(list){
  for (i in seq_along(list)) {
    if ( i == 1) {
      df <- list[[i]]
    } else {
      df <- rbind(df, list[[i]])
    }
  }
  df
}

## TESTS:
  # - return correct data frame
  # - return error message if no integers
  # - return correct message for correct data frame


#' Get frequencies from an item
#'
#' The return value contains two enries for each category used in the data. One that ends with \code{abs} shows the absolute frequencies, the other shows the relative frequencies, ending on \code{perc}. \code{min} before an entry means that this value was negative. Example: \code{C_min2_perc} with a value of 5 means that the -2 was present 5% on that item.
#'
#' @param item The item that will be analysed
#'
#' @return An overview for the frequencies of each entry in the item.
#' @export
#'
#' @examples
#' item <- c(rep(1, 19), rep(2, 8), rep(3, 26), rep(4, 29),
#'           rep(5, 5), rep(NA, 5), rep(-2, 4), rep(-3, 4))
#' freq_overview(item)
freq_overview <- function(item){

  entries_item <- length(item)
  original_missings <- sum(is.na(item))
  original_missings_perc <- original_missings/entries_item*100
  # get all entries:
  uniques <- unique(item)
  uniques <- uniques[!is.na(uniques)]
  cat_names <- as.character(uniques)

  freq_item_abs <- table(item)
  freq_item_perc <- freq_item_abs/entries_item*100

  cat_names <- names(freq_item_abs)
  freq_item_abs_names <- stringr::str_c("C_", cat_names, "_abs")
  freq_item_perc_names <- stringr::str_c("C_", cat_names, "_perc")
  return_names <- stringr::str_replace_all(c(freq_item_abs_names,
                                             freq_item_perc_names) , "-", "min")
  return <- t(data.frame(c(freq_item_abs, freq_item_perc)))
  colnames(return) <- return_names
  rownames(return) <- NULL
  return
}


#' Prepare an item for consensus analysis
#'
#' @param item The item to be recoded.
#' @param range The range of the Likert-scale for that item. Must be a vector of at least two characters.
#'
#' @return The item itself, where all entries outside of \code{range} are recoded as \code{NA}.
#' @export
#'
#' @examples
#' item <- c(rep(1, 19), rep(2, 8), rep(3, 26), rep(4, 29),
#'           rep(5, 5), rep(NA, 5), rep(-2, 4), rep(-3, 4))
#' prep_item(item, range = c(1, 5)
prep_item <- function(item, range = NULL) {
  # function prepars item, is basically a recode such that only the complete values in range remain.
  if (is.null(range)) {
    range_item <- range(item, na.rm = TRUE)
  } else {
    range_item <- range
  }

  # if a range is indicated, change all variables outside the range to NA
  if(!is.null(range)) {
    item_valid <- seq(from = min(range), to = max(range), by = 1)
    item[!is.element(item, item_valid)] <- NA
    return_item <- item
  } else {
    return_item <- item
  }
  return_item
}


#' Prepare an dataframe for consensus analysis
#'
#' @param data The dataframe to be recoded.
#' @param range The range of the Likert-scale for all items in that dataframe. Must be a vector of at least two characters.
#'
#' @return The dataframe itself, where all entries outside of \code{range} are recoded as \code{NA}.
#' @export
#'
#'@seealso \code{\link[consr]{prep_item}} for running this function on one item.
prep_item_df <- function(data, range = NULL){
  prep_df <- purrr::map(data, ~ prep_item(., range = range))
  prep_df <- as.data.frame(prep_df)
  prep_df
}


#' Get frequencies in an item and plot them
#'
#' @param item The item to be analysed. Must be a vector.
#' @param range The range of valid values.
#' @param plot logical, if a plot should be plotted. Defaultis to \code{FALSE}.
#' @param ... Further arguments passed on to \code{\link[graphics]{barplot}}.
#'
#' @return Returns a frequency table of the entries in that item. Optionally, also a barplot of these items.
#' @export
#' @examples
#' item <- c(rep(1, 19), rep(2, 8), rep(3, 26), rep(4, 29),
#'           rep(5, 5), rep(NA, 5), rep(-2, 4), rep(-3, 4))
#' freq_item(item, range = c(1,5), plot = TRUE, col = "#00376c")
freq_item <- function(item, range = NULL, plot = FALSE, ...){
  stopifnot(is.integer(abs(item)) | is.numeric(item))

  if (is.null(range)) {
    range_item <- range(item, na.rm = TRUE)
  } else {
    range_item <- range
  }

  # compute frequencies for each value in between the range
  item_categories <- seq(from = min(range_item),
                         to = max(range_item), by = 1)
  item_factor <- factor(item, levels = item_categories)
  item_frequencies <- table(item_factor)
  if (plot) {
    barplot(item_frequencies, ...)
  }
  item_frequencies
}


#' Consensus for an item
#'
#' Compute the consensus for an item, based on Tastle & Wiermann, (2006).
#' @details
#' \describe{
#'  \item{n}{The number of valid answers on that item}
#'  \item{min}{The minimum answer on that item}
#'  \item{max}{The maximum answer on that item}
#'  \item{consensus}{The consensus}
#'  \item{dissensus}{The dissensus}
#'  \item{n_missing}{The absolute number of missing values on that item, defined as \code{NA}}
#'  \item{perc_missing}{The relative number of missing values on that item, defined as \code{NA}}
#' }
#'
#' @param item The item that should be analyzed.
#' @param range The range of the item indicating the valid values. Default is \code{NULL}.
#' @param consensus.only Logical, if only the consensus should be reported. Default is to \code{FALSE}.
#' @param round The number of digits to round the result to. Default is to 2.
#'
#' @return Returns an data frame with the consensus and additional information for each item. See details for further explanation.
#' @export
#'
#' @examples
#' item <- c(rep(1, 19), rep(2, 8), rep(3, 26), rep(4, 29),
#'           rep(5, 5), rep(NA, 5), rep(-2, 4), rep(-3, 4))
#' consensus(item, range = c(1, 5), round = 4)
#' ## or the original example from the article:
#' item <- c(rep(1, 19), rep(2, 16), rep(3, 26), rep(4, 29), rep(5, 10))
#' consensus(item, range = c(1, 5), round = 3)
consensus <- function(item, range = NULL, consensus.only = FALSE, round = 2){
  item <- prep_item(item, range = range)
  item_frequencies <- freq_item(item, range = range, plot = FALSE)

  if (is.null(range)) {
    range_item <- range(item, na.rm = TRUE)
  } else {
    range_item <- range
  }

  # now all the parts of the equation (6)
  item_mean <- mean(item, na.rm = TRUE)
  nominator <- sum(item_frequencies) # this will be returned later
  if(nominator != sum(!is.na(item))) {stop("Something went wrong. Nonimator and number of non-missings not equal. Perhaps some values other than 'NA' not inlcuded in 'range'?")}

  item_width_d <- max(range_item) - min(range_item)

  # helper function (equation 5, for one category only)
  cns_x_c <- function(item_cat, item_freq_x){
    item_p <- (item_freq_x/nominator)
    cons_x <- item_p*(log2(1 - (abs(item_cat - item_mean))/item_width_d))
    cons_x
  }

  item_cat <- as.numeric(names(item_frequencies))
  item_freq_x <- c(item_frequencies)
  names(item_freq_x) <- NULL
  consensus_item <- 1 + sum(unlist(purrr::map2(item_cat, item_freq_x, cns_x_c)))

  if (consensus.only) {
    return <- consensus_item
    return(return)
  }

  item_missing <- sum(is.na(item))
  percent_missing <- item_missing/length(item)*100

  return.df <- data.frame(n = nominator,
                          min = min(range_item),
                          max = max(range_item),
                          consensus = consensus_item,
                          dissensus = 1 - consensus_item,
                          n_missing = item_missing,
                          perc_missing = percent_missing)
  round(return.df, round)
}


#' Consensus for data frame
#'
#' Compute the consensus for all items of a data frame, based on Tastle & Wiermann, (2006).
#' @details
#' \describe{
#'  \item{n}{The number of valid answers on that item}
#'  \item{min}{The minimum answer on that item}
#'  \item{max}{The maximum answer on that item}
#'  \item{consensus}{The consensus}
#'  \item{dissensus}{The dissensus}
#'  \item{n_missing}{The absolute number of missing values on that item, defined as \code{NA}}
#'  \item{perc_missing}{The relative number of missing values on that item, defined as \code{NA}}
#' }
#'
#' @param item The dataframe with the items, for which the conensus should be computed.
#' @param range The range of the items indicating the valid values. Default is \code{NULL}.
#' @param consensus.only Logical, if only the consensus should be reported. Default is to \code{FALSE}.
#' @param round The number of digits to round the result to. Default is to 2.
#' @param check.int Whether the function should check for integers. Default is to \code{TRUE}. Only integers are allowd. If \code{FALSE}, things can go horribly wrong.
#'
#' @return Returns an data frame with the consensus and additional information for each item. See details for further explanation.
#' @export
#'
#' @examples
#'n = 10 # (max = 26 for now)
#'var_names <- paste0("var_", letters[1:10])
#'entries <- matrix(sample(-2:2, n*10, replace = TRUE),
#'                  nrow = n, ncol = 10)
#'entries[sample(1:100, 15)] <- NA
#'data <- as.data.frame(entries)
#'colnames(data) <- var_names
#'subj_id <- paste0("id_", sample(letters, n, replace = FALSE))
#'data <- cbind(subj_id, data, stringsAsFactors = FALSE)
#'data["var_j"] <- data["var_j"] / 2.3
#'consensus_df(data, range = c(-2, 2), check.int = TRUE)
consensus_df <- function(data, range = NULL, consensus.only = FALSE,
                         round = 2, check.int = TRUE){
  if (check.int) {
    data_int <- integer_subset_df(data)
  } else {
    data_int <- data
  }
  prep_df <- prep_item_df(data_int, range = range)
  cons_list <- purrr::map(prep_df, ~ consensus(., range,
                          consensus.only = consensus.only, round = round))
  cons_df <- rbind_map(cons_list)
  variable <- names(cons_list)
  return <- cbind(variable, cons_df)
  return
}













