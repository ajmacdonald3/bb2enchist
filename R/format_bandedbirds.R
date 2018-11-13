#' Tidy BandedBirds.org data
#'
#' @param bandedbirds A dataframe formatted as submitted to BandedBirds.org.
#' @param cert Was resight certainty consistently recorded? Defaults to TRUE.
#' @param species Which species do you want to create encounter histories for? Use the four-letter AOU code.
#' @return Dataframe with columns for ResightDate and FlagID (flag colour and
#'         code).
#' @examples
#' format_bandedbirds(bandedbirds, cert = TRUE)

format_bandedbirds <- function(bandedbirds, species = "species", cert = TRUE) {
  if (cert == TRUE) {

  # manipulate the BandedBirds data into a format we can work with
  resight_data <- bandedbirds %>%
    filter(Species == "species") %>%
    filter(ResightCertainty == "C") %>% # if resight certainty was consistently recorded!
    select(ResightDate, FlagColor, FlagCode) %>%
    fill(ResightDate) %>%
    mutate(FlagID = paste(FlagColor, FlagCode, sep = "(")) %>%
    select(ResightDate, FlagID)

  resight_data$FlagID <- paste0(resight_data$FlagID, ")")

  assign("resight_data", resight_data, envir = globalenv())
  }
  else {

    # manipulate the BandedBirds data into a format we can work with
    resight_data <- bandedbirds %>%
      filter(Species == "REKN") %>%
      filter(is.na(ResightCertainty) | ResightCertainty > 94) %>% # if resight certainly wasn't consistently
                                                                  # recorded :(
      filter(!str_detect(FlagCode, "Q")) %>% # if resight certainly wasn't consistently recorded :(
      select(ResightDate, FlagColor, FlagCode) %>%
      fill(ResightDate) %>%
      mutate(FlagID = paste(FlagColor, FlagCode, sep = "(")) %>%
      select(ResightDate, FlagID)

    resight_data$FlagID <- paste0(resight_data$FlagID, ")")

    assign("resight_data", resight_data, envir = globalenv())
    }
}