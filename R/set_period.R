set_period <- function(x, period = NA, days = NA) {
  x$ResightDate <- mdy(x$ResightDate)
  if(period == "year") {

    resight_data <- x %>%
      mutate(ResightPeriod = year(ResightDate)) %>%
      select(ResightPeriod, FlagID)

  } else if
    (period == "month") {

      resight_data <- x %>%
        mutate(ResightPeriod = month(ResightDate)) %>%
        select(ResightPeriod, FlagID)

    } else if
      (period == "week") {
        resight_data <- x %>%
          mutate(ResightPeriod = week(ResightDate)) %>%
          select(ResightPeriod, FlagID)

      } else
        NA

  assign("resight_data", resight_data, envir = globalenv())

}
