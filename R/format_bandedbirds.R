#' Tidy BandedBirds.org data
#'
#' @param x A dataframe formatted as submitted to BandedBirds.org.
#' @param cert Was resight certainty consistently recorded? Defaults to TRUE.
#' @param sp Which species do you want to create encounter histories for? Use the four-letter AOU codes.
#' @return Dataframe with columns for ResightDate and FlagID (flag colour and
#'         code).
#' @examples
#' format_bandedbirds(bandedbirds, sp = "REKN", cert = TRUE)

format_bandedbirds <- function(x, sp = NA, cert = TRUE) {
  if (cert == TRUE) {

  # manipulate the BandedBirds data into a format we can work with
  resight_data <- x %>%
    filter(Species %in% sp) %>% # keeps only species of interest
    filter(ResightCertainty == "C") %>% # if resight certainty was consistently recorded!
    select(ResightDate, FlagColor, FlagCode) %>% # keeps only these columns
    fill(ResightDate) %>% # fills in date for every observation
    mutate(FlagID = paste(FlagColor, FlagCode, sep = "(")) %>% # creates new column that combines flag colour and code
    select(ResightDate, FlagID) # keeps only these columns

  resight_data$FlagID <- paste0(resight_data$FlagID, ")") # flag colour and code in PASP format

  assign("resight_data", resight_data, envir = globalenv()) # sends dataframe back to R environment
  }
  else {

    # manipulate the BandedBirds data into a format we can work with
    resight_data <- x %>%
      filter(Species %in% sp) %>% # keeps only species of interest
      filter(is.na(ResightCertainty) | ResightCertainty > 94) %>% # if resight certainly wasn't consistently
                                                                  # recorded :(
      filter(!str_detect(FlagCode, "Q")) %>% # if resight certainly wasn't consistently recorded :(
      select(ResightDate, FlagColor, FlagCode) %>% # keeps only these columns
      fill(ResightDate) %>% # fills in date for every observation
      mutate(FlagID = paste(FlagColor, FlagCode, sep = "(")) %>% # creates new column that combines flag colour and code
      select(ResightDate, FlagID) # keeps only these columns

    resight_data$FlagID <- paste0(resight_data$FlagID, ")") # flag colour and code in PASP format

    assign("resight_data", resight_data, envir = globalenv()) # sends dataframe back to R environment
    }
}
