# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

adjust_lags <- function(tbl_in, max_lag){
  
  other_lags <- as.character(tbl_in$lag[tbl_in$lag >= max_lag])
  
  tbl_in |> 
    mutate(
      lag_factor = as_factor(lag),
      lag_tail = lag_factor |> 
        fct_other(drop = other_lags, other_level = "Tail"),
    )
  
}

prior_case <- function(tbl_in) {

  tbl_in %>% 
    group_by(accident_year, .add = TRUE) %>% 
    arrange(development_year, .by_group = TRUE) %>% 
    mutate(
      across('case',
             dplyr::lag,
             .names = "prior_{.col}"
      )) |> 
    ungroup()
  
}

form_priors <- function(tbl_in){
  tbl_in %>% 
    group_by(accident_year, .add = TRUE) %>% 
    arrange(development_year, .by_group = TRUE) %>% 
    mutate(
      across(starts_with("cumulative_"),
             dplyr::lag,
             .names = "prior_{.col}"
      )) |> 
    ungroup()
}

form_incrementals_ldfs <- function(tbl_in) {
  
  prior_columns <- names(tbl_in)[str_starts(names(tbl_in), 'prior_cumulative_')]
  cumulative_columns <- names(tbl_in)[str_starts(names(tbl_in), 'cumulative_')]
  
  column_stems <- intersect(
    prior_columns |> str_remove('prior_cumulative_'),
    cumulative_columns |> str_remove('cumulative_')
  )
  
  incremental_columns <- paste0("incremental_", column_stems)
  ldf_columns <- paste0("ldf_", column_stems)
  
  for (i_col in seq_along(column_stems)) {
    
    col_prior <- tbl_in[, prior_columns[i_col]]
    col_cumul <- tbl_in[, cumulative_columns[i_col]]
    
    tbl_in[, incremental_columns[i_col]] <- coalesce(
      col_cumul - col_prior
      , col_cumul
    )
    tbl_in[, ldf_columns[i_col]] = col_cumul / col_prior - 1
  }
  
  tbl_in
}

wrangle_triangle <- function(tbl_in, last_diagonal_year, max_lag = 7){
  
  tbl_in |> 
    janitor::clean_names() |> 
    mutate(
      upper = development_year <= last_diagonal_year,
      cumulative_incurred = cumulative_incurred - ibnr,
      case = cumulative_incurred - cumulative_paid
    ) |> 
    adjust_lags(max_lag) |> 
    form_priors() |> 
    form_incrementals_ldfs() |> 
    prior_case()
}
