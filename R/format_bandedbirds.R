#' Tidy BandedBirds.org data
#'
#' @param x A dataframe formatted as submitted to BandedBirds.org.
#' @param new_df A name for the new dataframe that will be created.
#' @param cert Was resight certainty consistently recorded? Defaults to TRUE.
#' @param sp Which species do you want to create encounter histories for? Use the four-letter AOU codes.
#' @return Dataframe with columns for ResightDate and FlagID (flag colour and
#'         code).
#' @examples
#' format_bandedbirds(bandedbirds, new_df = resight_data, sp = "REKN", cert = TRUE)

format_bandedbirds <- function(x, new_df = y, sp = NA, cert = TRUE) {
  if (cert == TRUE) {

    # manipulate the BandedBirds data into a format we can work with
    df_name <- deparse(substitute(new_df))

    new_df <- x %>%
      filter(Species %in% sp) %>% # keeps only species of interest
      filter(ResightCertainty == "C") %>% # if resight certainty was consistently recorded!
      select(ResightDate, FlagColor, FlagCode) %>% # keeps only these columns
      fill(ResightDate) %>% # fills in date for every observation
      mutate(FlagID = paste(FlagColor, FlagCode, sep = "(")) %>% # creates new column that combines flag colour and code
      select(ResightDate, FlagID) # keeps only these columns

    new_df$FlagID <- paste0(new_df$FlagID, ")") # flag colour and code in PASP format

    assign(df_name, new_df, envir = globalenv()) # sends dataframe back to R environment
  }
  else {

    # manipulate the BandedBirds data into a format we can work with
    df_name <- deparse(substitute(new_df))

    new_df <- x %>%
      filter(Species %in% sp) %>% # keeps only species of interest
      filter(!str_detect(FlagCode, "Q")) %>% # if resight certainly wasn't consistently recorded :(
      select(ResightDate, FlagColor, FlagCode) %>% # keeps only these columns
      fill(ResightDate) %>% # fills in date for every observation
      mutate(FlagID = paste(FlagColor, FlagCode, sep = "(")) %>% # creates new column that combines flag colour and code
      select(ResightDate, FlagID) # keeps only these columns

    new_df$FlagID <- paste0(new_df$FlagID, ")") # flag colour and code in PASP format

    assign(df_name, new_df, envir = globalenv()) # sends dataframe back to R environment
  }
}
