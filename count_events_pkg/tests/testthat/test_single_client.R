context("Tests for function that counts events for a single client")

data("events")

intervals <- c(30, 90, 180, 360)
groupings <- list(1, 2, 3, 1:2, 1:3, c(1, 3))
test_df <- count_events_single(data.table::as.data.table(events), 818, client_id_col = "cli_id", case_id_col = "spr_Id",
                               event_date_col = "evl_EventDate", end_date_col = "Data0",
                               event_codes_cols = c("ert_Code", "ser_Code", "ier_Code"),
                               intervals = intervals, groupings = groupings)
counts <- dplyr::select_if(test_df, is.numeric)


testthat::test_that("Counts are non-negative", {
  testthat::expect_true(all(counts >= 0, na.rm = T))
})

# testthat::test_that("Counts are integers", {
#   testthat::expect_true(dplyr::summarise_all(counts, function(x) all(is.integer(x))))
# })
