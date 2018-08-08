#' Count events for a single case
#'
#' @param case_id ID of the case for which events will be counted
#' @inheritParams count_events
#'
#' @return data frame
#'
#' @export
#'

count_events_single <- function(x, case_id, case_id_col, event_date_col, 
                                end_date_col, event_codes_cols,
                                intervals, groupings) {

  x <- x[x[[case_id_col]] == case_id, ]
  x <- x[x[[event_date_col]] <= x[[end_date_col]], ]

  aggregates <- lapply(groupings, function(y) {
    single_grouping(x, event_codes_cols[y], case_id_col, event_date_col, end_date_col, intervals)
  })
  
  Reduce(function(...) merge(..., all = TRUE), aggregates)
  
}

#' Count events for all cases in the database
#'
#' @param x Data frame
#' @param case_id_col Character, name of the column in which case IDs are stored
#' @param event_date_col Character, name of the column where event dates are stored
#' @param end_date_col Character, name of the column in which end_date is stored
#' @param event_codes_cols Characters vector of names of column where action codes are stored
#' @param intervals Numeric vector of day intervals. Events that occur in the span of y days for
#'  each y in interval will be calculated
#' @param groupings list of vectors of combinations of positions in event_codes_cols by which
#'        grouping will be performed
#'
#' @return data frame
#'
#' @export
#'

count_events <- function(x, case_id_col = "spr_Id", 
                         event_date_col = "evl_EventDate",
                         end_date_col = "Data0", 
                         event_codes_cols = c("ert_Code", "ser_Code", "ier_Code"),
                         intervals = c(30, 90, 180, 360),
                         groupings = list(1, 2, 3, 1:2, 1:3, c(1, 3))) {

  if(!(check_conditions(x, case_id_col, event_date_col,
                        end_date_col, event_codes_cols, intervals, groupings))) {
    stop("Please check data validity")
  }

  x <- data.table::as.data.table(x)

  unique_cases <- unique(x[[case_id_col]])
  all_cases <- lapply(unique_cases, function(y) {
    count_events_single(x, y, case_id_col, event_date_col, end_date_col,
                        event_codes_cols, intervals, groupings)
  })
  
  all_cases <- lapply(seq_along(unique_cases), function(y) {
    all_cases[[y]]$client_id <- unique_cases[y]
    all_cases[[y]]
  })
  
  tmp <- data.table::rbindlist(all_cases, fill = T, use.names = T)
  tmp <- tmp[, dplyr::select_if(.SD, function(y) !all(is.na(y) | y == 0))]
  tmp[, dplyr::mutate_if(.SD, is.numeric, function(y) ifelse(is.na(y), 0, y))]
}



#' Count events for all clients in the database in parallel
#'
#' @param x Data frame
#' @param case_id_col Character, name of the column in which case IDs are stored
#' @param event_date_col Character, name of the column where event dates are stored
#' @param end_date_col Character, name of the column in which end_date is stored
#' @param event_codes_cols Characters vector of names of column where action codes are stored
#' @param intervals Numeric vector of day intervals. Events that occur in the span of y days for
#'  each y in interval will be calculated
#' @param groupings list of vectors of combinations of positions in event_codes_cols by which
#'        grouping will be performed
#'
#' @importFrom foreach %dopar%
#'
#' @return data frame
#'
#' @export
#'

count_events_parallel <- function(x, case_id_col = "spr_Id",
                         event_date_col = "evl_EventDate", end_date_col = "Data0",
                         event_codes_cols = c("ert_Code", "ser_Code", "ier_Code"),
                         intervals = c(30, 90, 180, 360),
                         groupings = list(1, 2, 3, 1:2, 1:3, c(1, 3))) {
  
  if(!(check_conditions(x, case_id_col, event_date_col,
                        end_date_col, event_codes_cols, intervals, groupings))) {
    stop("Please check data validity")
  }
  
  x <- data.table::as.data.table(x)
  
  unique_cases <- unique(x[[case_id_col]])
  all_cases <- foreach::foreach(y = seq_along(unique_cases),
                         .packages = c("data.table", "lubridate", "eventcounting2")) %dopar%
    count_events_single(x, unique_cases[y], case_id_col, event_date_col, end_date_col,
                        event_codes_cols, intervals, groupings)
  
  all_cases <- lapply(seq_along(unique_cases), function(y) {
    all_cases[[y]]$client_id <- unique_cases[y]
    all_cases[[y]]
  })
  
  tmp <- data.table::rbindlist(all_cases, fill = T, use.names = T)
  tmp[, dplyr::select_if(.SD, function(y) !all(is.na(y) | y == 0))]
  tmp[, dplyr::mutate_if(.SD, is.numeric, function(y) ifelse(is.na(y), 0, y))]
}

