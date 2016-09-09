


# functions for the computation of consensus
# it is based on the following paper doi:10.1016/j.ijar.2006.06.024




# sample data --------------------------------------------------------

# input will be questionnaire data
# n = 10 # (max = 26 for now)
# var_names <- paste0("var_", letters[1:10])
# entries <- matrix(sample(-2:2, n*10, replace = TRUE),
#                   nrow = n, ncol = 10)
# entries[sample(1:100, 15)] <- NA
# data <- as.data.frame(entries)
# colnames(data) <- var_names
# subj_id <- paste0("id_", sample(letters, n, replace = FALSE))
# data <- cbind(subj_id, data, stringsAsFactors = FALSE)
# data["var_j"] <- data["var_j"] / 2.3
# str(data)



# functions overview -------------------------------------------------

# 1 select appropriate data
# 2 compute consensus for one variable
# 3 compute consensus for a data frame

# select integers from data ------------------------------------------

## input to the function
# we should only allow values that are integers

integer_subset_df <- function(data){
  stopifnot(is.data.frame(data) | is.matrix(data))
  # select numerics
  num_only <- purrr::map(data, ~ is.numeric(.))
  num_data <- data[unlist(num_only)]
  # select integers
  int_only <- purrr::map(num_data, ~ is.integer(abs(.)))
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


# helper function rbind ----------------------------------------------

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


# frequency_analyses -------------------------------------------------

# example
# item <- c(rep(1, 19), rep(2, 8), rep(3, 26), rep(4, 29), rep(5, 5), rep(NA, 5), rep(-2, 4), rep(-3, 4))
# in this item:
# 87 times vlaid entries
# 5 times NA
# 3 times -2
# 3 times -3


# now this function needs to change all values in the item to 1 and 5 or NA and give an overview

freq_overview <- function(item, range = NULL){

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

# freq_overview(item, range = c(1, 5))

# freq_overview_df <- function(data, range = NULL) {
#   data <- integer_subset_df(data) # das raus hier, hat hier nichts verloren
#   ov_df <- purrr::map(data, ~ freq_overview(., range = range))
#   rbind_map(ov_df)
# }

# freq_overview_df(prep_df, range = c(1, 5))


# prep data ----------------------------------------------------------

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

prep_item_df <- function(data, range = NULL){
  prep_df <- purrr::map(data, ~ prep_item(., range = range))
  prep_df <- as.data.frame(prep_df)
  prep_df
}

# prep_df <- prep_item_df(data)



# compute frequencies ------------------------------------------------

# data <- integer_subset(data)

# compute frequencies:

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

# item_paper <- c(rep(1, 19), rep(2, 16), rep(3, 26), rep(4, 29), rep(5, 10))
# freq_item(item_paper, plot = TRUE)

# consensus ----------------------------------------------------------


consensus <- function(item, range = NULL, consensus.only = FALSE, round = round){
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

  # n, min, max, consensus, dissensus, n-missing, % missing
  # categories
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


# consensus(item_paper)


# next
# turn all these into a function that makes the table John requires.

# i.e. function 'consensus_df'

consensus_df <- function(data, range = NULL, consensus.only = FALSE, round = 2){
  data_int <- integer_subset_df(data)
  prep_df <- prep_item_df(data_int, range = range)
  cons_list <- purrr::map(prep_df, ~ consensus(., range,
                          consensus.only = consensus.only, round = round))
  cons_df <- rbind_map(cons_list)
  variable <- names(cons_list)
  return <- cbind(variable, cons_df)
  return
}













