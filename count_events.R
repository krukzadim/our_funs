
count_events <- function(x, id, event_cols, date0, event_date, intervals = c(30, 90, 180, 360)){

library(tidyverse, lubridate)
  
comb_cols_names <- list()
comb_cols <- list()

#### generating all possible combinations of event's names ####

  for (i in 1:length(event_cols)) {
    if (i == length(event_cols))  ## zakombinaować coś z while
      break
    comb_cols_names[[i]] <- t(combn(event_cols, i+1))
    comb_cols[[i]] <- t(combn(event_cols, i+1))
      for (j in 1 : dim(comb_cols_names[[i]])[1]){
        comb_cols_names[[i]][j] <- paste(comb_cols_names[[i]][j,], collapse = "_")
      }
    comb_cols_names[[i]] <- comb_cols_names[[i]][,1]
  }

comb_cols_names <- unlist(unlist(comb_cols_names))

#### generating all possible combinations of columns ####

tmp2 <- list()

k <-1

  for (i in 1:length(comb_cols)){
    for (j in 1:dim(comb_cols[[i]])[1]){
       tmp2[[k]] <- comb_cols[[i]][j,]
       k <- k+1
    }
  }

#### merging and renaming combinations of columns ####

  for (i in 1:length(comb_cols_names)){
    x <- tidyr::unite(x, comb_cols_names[i], tmp2[[i]], remove = FALSE)
    names(x)[names(x) == 'comb_cols_names[i]'] <- comb_cols_names[i]
  }

#### roundind dates ####

x$Data0 <- lubridate::floor_date(x[, date0], unit = "day")  # zastanowic sie nad fukcja z base R
x$evv_EventDate <- lubridate::floor_date(x[, event_date], unit = "day")

#### creating interval's start date for each interval ####

interval_lenght <- c()

  for (i in seq_along(intervals)) {
    interval_lenght[i] <- paste0("D", intervals[i])
    interval_start <- x$Data0 - intervals[i]
    x <- cbind(x, interval_start)
    colnames(x)[colnames(x)=="interval_start"] <- interval_lenght[i]
  }

### counting each combination of events for each interval for each id ####

comb_cols_names <- c(event_cols, comb_cols_names)
z <- data.frame()
df <- data.frame()
all <- list()

  for (i in comb_cols_names) {
    count_name <- "N"
    count_interval <- 'n()'
    df<- x %>% 
      group_by_(id, i) %>%
      summarise_(.dots = setNames(count_interval, count_name))
        for (j in interval_lenght){
            count_interval <- 'sum(event_date > j)'
            renaming <- 'paste(.[, i], j, sep = "_")'
            z <- x %>%
                    mutate_(.dots = set_names(renaming, i)) %>%
                    group_by_(id, i) %>%
                    summarise_(.dots = setNames(count_interval, count_name))
            df <- dplyr::union(df, z)
        }
    all[[i]] <- df
  }

df <- data.frame(id = unique(x[,id]))
names(df)[names(df)=="id"] <- id
  
  for (i in all){
      tmp <- as.data.frame(i)
      tmp <- spread(tmp, key = names(tmp)[2], value = names(tmp)[3])
      tmp[is.na(tmp)] <- 0
      df <- left_join(df, tmp, by = id)
  }

funs <- c("max", "min")

for (i in comb_cols_names){
  for (j in funs){
    renaming <- 'paste(.[, i], j, sep = "_")'
    z <- x %>%
      mutate_(.dots = setNames(renaming, i)) %>%
      group_by_(id, i) %>%
      summarise_at(vars(event_date, date0), funs_(j)) %>%
      rename_(.dots = setNames(i, "code")) %>%
      mutate_(.dots = setNames(paste0("as.numeric(difftime(",date0,",",event_date,",units = \"days\"))"), "N")) %>%
      select_(id, "code", "N") %>%
      spread(., code, N)
    df <- left_join(df, z, by = id)
  }
}

return(df)

}
