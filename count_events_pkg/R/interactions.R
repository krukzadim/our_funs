#' Get interactions terms for a set of categorical predictors
#' 
#' @param x data frame
#' @param exclude character or factor columns to exclude
#' 
#' @return model matrix
#' 
#' @export
#'

get_all_interactions <- function(x, exclude = NULL) {
  x <- dplyr::select_if(x, function(y) is.character(y) | is.factor(y))
  x <- data.table::as.data.table(x)
  if(!is.null(exclude)) {
    x <- x[, -..exclude]
  }
  model.matrix(y~.*., data = cbind(y = rep(1, nrow(x)), x))
}


#' Get summary of classes and unique values in factors or character variables
#' 
#' @param x data frame
#' 
#' @return data frame
#' 
#' @export
#' 

get_factors_summary <- function(x) {
  summary <- suppressWarnings(dplyr::summarise_all(x, 
                                                   .funs = c(zz_class_z = "class", 
                                                             zz_levels_z = "n_distinct")))
  summary <- tidyr::gather(summary, "stat", "value")
  summary <- dplyr::mutate(summary,
                           statistic = ifelse(grepl("zz_class_z", stat), "class", "unique_levels"),
                           variable = stringr::str_replace_all(stat, "_zz_levels_z", ""),
                           variable = stringr::str_replace_all(variable, "_zz_class_z", ""))
  summary <- dplyr::select(summary, variable, statistic, value)
  summary <- tidyr::spread(summary, statistic, value)
  dplyr::arrange(summary, variable)
}


#' Replace missing values with a special token
#' 
#' @param x Factor or character vector
#' @param replacement String which will replace missing values
#' 
#' @return data frame
#' 
#' @export
#' 

replace_na <- function(x, replacement = "brak_inf") {
  dplyr::mutate_all(x, function(y) {
    y[is.na(y)] <- replacement
    y
  })
} 


#' Lump together rare levels only if number of levels changed
#' 
#' @param fct factor of character vector
#' @param min_prop minimum proportion to leave level unchanged
#' 
#' @return factor vector
#' 
#' @export
#' 

lump_wisely <- function(fct, min_prop) {
  try <- forcats::fct_lump(fct, prop = min_prop, other_level = "Inny")
  if(dplyr::n_distinct(try) == dplyr::n_distinct(fct)) as.factor(fct)
  else as.factor(try)
}


#' Merge rare levels of all factors in data frame
#' 
#' @param x data frame
#' @param exclude character or factor columns to exclude
#' @param min_n Levels with less than n observations will be merged
#' @param min_prop Levels with observations that make up less than 100*min_prop% 
#' will be merged.
#' 
#' @return factor vector
#' 
#' @export
#' 

merge_levels_rare <- function(x, exclude = NULL, min_n = NULL, min_prop = NULL) {
  if((is.null(min_n) & is.null(min_prop)) | !is.null(min_n) & !is.null(min_prop)) 
    stop("Only one of min_n, min_prop should be non-NULL")
  x <- x[, -which(colnames(x) %in% exclude)]
  if(!is.null(min_n)) x <- dplyr::mutate_all(x, function(y) merge_less_than_n(y, min_n))
  else x <- dplyr::mutate_all(x, function(y) lump_wisely(y, min_prop))
  x
}

#' Merge levels of factors based on relationship to response variable
#' 
#' @param fct Factor to merge
#' @param response Response variable, logical vector
#' 
#' @importFrom factorMerger getOptimalPartitionDf mergeFactors
#' @importFrom dplyr left_join
#' 
#' @return factor vector
#' 
#' @export
#' 

merge_levels_response <- function(fct, response) {
  dictionary <- getOptimalPartitionDf(mergeFactors(response, fct, family = "binomial", abbreviate = F))
  as.factor(left_join(data.frame(x = fct), dictionary, by = c("x" = "orig"))$pred)
}

#' Find interacting variables by fitting random forest
#' 
#' @param x explanatory variables, all factors
#' @param y response variable
#' 
#' @return data frame
#' 
#' @export
#' 

explore_interactions_rf <- function(x, y) {
  model <- randomForest::randomForest(y ~ ., data = cbind(x, y = y), localImp = T)
  explainer <- randomForestExplainer::measure_importance(model, measures = "mean_min_depth")
  variables <- randomForestExplainer::important_variables(explainer, k = ncol(x))
  min_depth_interactions(variables)
}
