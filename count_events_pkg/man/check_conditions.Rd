% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{check_conditions}
\alias{check_conditions}
\title{Check data validity before counting events for a single client}
\usage{
check_conditions(x, case_id_col, event_date_col, end_date_col,
  event_codes_cols, intervals, groupings)
}
\arguments{
\item{x}{Data frame}

\item{case_id_col}{Character, name of the column in which case IDs are stored}

\item{event_date_col}{Character, name of the column where event dates are stored}

\item{end_date_col}{Character, name of the column in which end_date is stored}

\item{event_codes_cols}{Characters vector of names of column where action codes are stored}

\item{intervals}{Numeric vector of day intervals. Events that occur in the span of y days for
each y in interval will be calculated}

\item{groupings}{list of vectors of combinations of positions in event_codes_cols by which
grouping will be performed}
}
\value{
logical
}
\description{
Check data validity before counting events for a single client
}
