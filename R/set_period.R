set_period <- function(x, period = NA, rsint = y) {

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
        resight_data <- x %>%
          mutate(ResightPeriod = lubridate::round_date(ResightDate, rsint)) %>%
          select(ResightPeriod, FlagID)


  assign("resight_data", resight_data, envir = globalenv())

}
