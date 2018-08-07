#' Check data validity before counting events for a single client
#'
#' @inheritParams count_events
#'
#' @return logical
#'
#' @export
#'

check_conditions <- function(x, case_id_col, event_date_col,
                             end_date_col, event_codes_cols, intervals, groupings) {
  if((case_id_col %in% colnames(x)) &
    (end_date_col %in% colnames(x)) &
    (event_date_col %in% colnames(x)) &
    (all(event_codes_cols %in% colnames(x))) &
    (length(event_codes_cols) > 0) &
    (length(intervals) > 0) &
    (length(groupings) > 0) # &
    # (lubridate::is.Date(x[, event_date_col]) | lubridate::is.POSIXt(x[, event_date_col])) &
    # (lubridate::is.Date(x[, end_date_col]) | lubridate::is.POSIXt(x[, end_date_col]))
   ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Add columns indicating whether the event took plays in y days before given date
#'
#' @param x data frame
#' @param event_date_col Character, name of the column in which event dates are stored
#' @param end_date_col Character, name of the column in which end_date is stored
#' @param intervals Numeric vector of day intervals. Events that occur in the span of y days for
#'  each y in interval will be calculated
#'
#' @return data frame
#'
#' @export
#'

add_indicator_cols <- function(x, event_date_col, end_date_col, intervals) {
  interval_columns <- lapply(intervals, function(y) {
    x[[end_date_col]] - lubridate::days(y) <= x[[event_date_col]]
  })
  interval_columns <- data.table::as.data.table(interval_columns)
  interval_columns_names <- paste0("cnt__", intervals, "d")
  colnames(interval_columns) <- interval_columns_names
  x <- cbind(x, interval_columns)
  remove_cols <- which(colnames(x) %in% c(event_date_col, end_date_col))
  x[, -..remove_cols]
}


#' Transform data into wide form
#'
#' @param src_df data frame
#' @param grouping_variables Character, names of grouping variable
#' @param case_id_col Character, name of the column in which case IDs are stored
#'
#' @return data frame
#'
#' @export
#'

make_wide <- function(src_df, grouping_variables, case_id_col) {
  to_gather <- colnames(src_df)[!(colnames(src_df) %in% c(grouping_variables, case_id_col))]
  src_df <- data.table::melt(src_df, id.vars = c(grouping_variables, case_id_col), 
                             measure.vars = to_gather, variable.name = "statistic", value.name = "value")
  g_vars <- colnames(src_df)[which(colnames(src_df) %in% c(grouping_variables, "statistic"))]
  combined_names <- apply(src_df[, ..g_vars], 1, function(x) glue::glue_collapse(x, sep = "_"))
  y <- src_df[, -..g_vars]
  y$combined_names <- combined_names
  y <- y[!(is.na(y$combined_names)), ] 
  y <- unique(y)
  y <- data.table::dcast(y, spr_Id ~ combined_names)
  y[order(y[[case_id_col]])]
}


#' Find date of first and last occurence of the event
#'
#' @param to_count data frame
#' @param grouping_variables Character, names of columns to group by
#' @param case_id_col Character, name of the column in which case IDs are stored
#' @param event_date_col Character, name of the column that contains dates of events
#'  
#' @import data.table  
#'  
#' @return data frame
#'
#' @export
#'

find_first_last <- function(to_count, grouping_variables, case_id_col, event_date_col) {
  event_dates <- to_count[, .(first_event = min(.SD[[1]], na.rm = T),
                              last_event = max(.SD[[1]], na.rm = T)),
                          by = c(grouping_variables, case_id_col), 
                          .SDcols = c(event_date_col, end_date_col)]
  make_wide(event_dates, grouping_variables, case_id_col)
}


#' Find date of first and last occurence of the event
#'
#' @param to_count data frame
#' @param grouping_variables Character, names of columns to group by
#' @param case_id_col Character, name of the column in which case IDs are stored
#' @param event_date_col Character, name of the column that contains dates of events
#' @param end_date_col Character, name of the column that contains date zero
#'  
#' @import data.table  
#'  
#' @return data frame
#'
#' @export
#'

find_diffs <- function(to_count, grouping_variables, case_id_col, event_date_col, end_date_col) {
  event_diffs <- to_count[, .(first_diff = as.numeric(lubridate::ceiling_date(.SD[[2]], "days") - lubridate::ceiling_date(min(.SD[[1]], na.rm = T), "days")),
                              last_diff = as.numeric(lubridate::ceiling_date(.SD[[2]], "days") - lubridate::ceiling_date(max(.SD[[1]], na.rm = T), "days"))),
                          by = c(grouping_variables, case_id_col), 
                          .SDcols = c(event_date_col, end_date_col)]
  make_wide(event_diffs, grouping_variables, case_id_col)
}


#' Calculate numbers of event in time intervals
#'
#' @param to_count data frame
#' @param grouping_variables Character, names of variables to group by
#' @param case_id_col Character, name of the column in which case IDs are stored
#'
#' @return data frame
#'
#' @export
#'

find_counts <- function(to_count, grouping_variables, case_id_col) {
  to_count$cnt__all <- as.integer(rep(1, nrow(to_count)))
  to_summarise <- colnames(to_count)[!(colnames(to_count) %in% c(grouping_variables, case_id_col))]
  counts_in_intervals <- to_count[, lapply(.SD, sum, na.rm=TRUE), 
                                  by = c(grouping_variables, case_id_col), .SDcols = to_summarise]
  make_wide(counts_in_intervals, grouping_variables, case_id_col)
}


#' Find counts and first/last event date for a single set of grouping variables
#'
#' @param x Data frame
#' @param grouping_variables Character, names of columns to group by
#' @param case_id_col Character, name of the column in which case IDs are stored
#' @param event_date_col Character, name of the column where event dates are stored
#' @param end_date_col Character, name of the column in which end_date is stored
#' @param intervals Numeric vector of day intervals. Events that occur in the span of y days for
#'  each y in interval will be calculated
#'
#' @return data frame
#'
#' @export
#'

single_grouping <- function(x, grouping_variables, case_id_col, event_date_col, end_date_col, intervals) {
  to_count <- x[, c(grouping_variables, case_id_col, event_date_col, end_date_col), with = F]
  first_last <- find_first_last(to_count, grouping_variables, case_id_col, event_date_col)
  diffs <- find_diffs(to_count, grouping_variables, case_id_col, event_date_col, end_date_col)

  to_count <- add_indicator_cols(to_count, event_date_col, end_date_col, intervals)
  counts_in_intervals <- find_counts(to_count, grouping_variables, case_id_col)

  result <- merge(first_last, diffs)
  result <- merge(result, counts_in_intervals)
  result
}
