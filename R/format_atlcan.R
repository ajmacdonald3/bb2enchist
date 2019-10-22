#' Format Atlantic Canada Shorebirds banding data for BandedBirds.org
#'
#' @param x A dataframe of Atlantic Canada Shorebirds banding data
#' @param y The ProjectID number assigned by BandedBirds.org
#' @return A dataframe of banding data formatted for submission to BandedBirds.org
#' @examples
#' format_atlcan(banding_data, project = 79)

format_atlcan <- function(x, project = y){

  bandedbirds_data <- x %>%
    filter(!is.na(FlagColour)) %>%
    mutate(ProjectID = project) %>%
    mutate(CaptureMethodID = case_when(Site == "JM" ~ "FundyPT",
                                       Site == "PCAP" ~ "Mist")) %>%
    mutate(LocationID = case_when(Site == "JM" ~ "JOHNSONSMILL",
                                  Site == "PCAP" ~ "PETIT_CAP")) %>%
    mutate(Latitude = case_when(Site == "JM" ~ 45.772264,
                                Site == "PCAP" ~ 46.178008)) %>%
    mutate(Longitude = case_when(Site == "JM" ~ -64.511386,
                                 Site == "PCAP" ~ -64.140922)) %>%
    mutate(CaptureDate = ymd(Date)) %>%
    mutate(CAPTURES = "") %>%
    rename(SpeciesID = Species) %>%
    mutate(MetalOut = paste(Prefix, BandNum, sep = "-")) %>%
    mutate(FlagColorOut = FlagColour) %>%
    mutate(FlagCodeOut = FlagCode) %>%
    rename(AgeID = Age) %>%
    mutate(SexID = "U") %>%
    rename(Culmen = Bill_mm) %>%
    mutate(Head = "") %>%
    rename(Wing = Wing_mm) %>%
    rename(Weight = Weight_g) %>%
    mutate(WeightTime = "", LapsedMinutes = "", BMIMolts = "", BMIExtentID = "", BMIStage = "",
           PlumageID = 0, PrimaryMolts = "", P1 = "", P2 = "", P3 = "", P4 = "", P5 = "", P6 = "",
           P7 = "", P8 = "", P9 = "", P10 = "", Fats = "", FatScore = "", SizeAdjMass = "",
           FatGram = "", FatAdjGram = "", FatMassRatio = "", WingSpan = "", Blood = "",
           SampleNum = "", TrackerLogs = "") %>%
    rename(TrackerID = NanoID) %>%
    mutate(TrackerTypeID = case_when(TrackerID > 1 ~ "NANOTAG"),
           Status = case_when(TrackerID > 1 & Recap %in% c("F", "R") ~ "Retained",
                              TrackerID > 1 ~ "New")) %>%
    mutate(BloodTaken = case_when(Plasma == "1" & Isotope == "1" & FTA == "1" ~ "Blood sample collected (isotopes, plasma triglycerides, FTA card)",
                                  Plasma == "1" & Isotope == "1" & is.na(FTA) ~ "Blood sample collected (isotopes, plasma triglycerides)",
                                  Plasma == "1" & is.na(Isotope) & FTA == "1" ~ "Blood sample collected (plasma triglycerides, FTA card)",
                                  Plasma == "1" & is.na(Isotope) & is.na(FTA) ~ "Blood sample collected (plasma triglycerides)",
                                  is.na(Plasma) & Isotope == "1" & FTA == "1" ~ "Blood sample collected (isotopes, FTA card)",
                                  is.na(Plasma) & Isotope == "1" & is.na(FTA) ~ "Blood sample collected (isotopes)",
                                  is.na(Plasma) & is.na(Isotope) & FTA == "1" ~ "Blood sample collected (FTA card)")) %>%
    mutate(`N/R` = case_when(Recap %in% c("F", "R") ~ "R",
                             is.na(Recap) ~ "N")) %>%
    mutate(DispID = case_when(Recap == "F" ~ "F",
                              Recap == "R" ~ "R",
                              is.na(Recap) ~ "1")) %>%
    mutate(StatusID = case_when(is.na(Plasma) & is.na(Isotope) & is.na(FTA) & is.na(TrackerID) ~ 369,
                                is.na(Plasma) & is.na(Isotope) & is.na(FTA) & TrackerID > 1 ~ 325,
                                Plasma == "1" | Isotope == "1" | FTA == "1" ~ 319)) %>%
    mutate(MetalIn = case_when(`N/R` == "R" ~ paste(Prefix, BandNum, sep = "-")),
           FlagColorIn = case_when(`N/R` == "R" ~ FlagColour),
           FlagCodeIn = case_when(`N/R` == "R" ~ FlagCode),
           UpperLeftIn = case_when(`N/R` == "R" & BandLeg == "L" ~ "M",
                                   `N/R` == "R" & BandLeg == "R" ~ paste0(paste(FlagColour, FlagCode, sep = "("), ")")),
           LowerLeftIn = case_when(`N/R` == "R" ~ "-"),
           UpperRightIn = case_when(`N/R` == "R" & BandLeg == "R" ~ "M",
                                    `N/R` == "R" & BandLeg == "L" ~ paste0(paste(FlagColour, FlagCode, sep = "("), ")")),
           LowerRightIn = case_when(`N/R` == "R" ~ "-")) %>%
    mutate(UpperLeftOut = case_when(BandLeg == "L" ~ "M",
                                    BandLeg == "R" ~ paste0(paste(FlagColour, FlagCode, sep = "("), ")")),
           LowerLeftOut = "-",
           UpperRightOut = case_when(BandLeg == "R" ~ "M",
                                     BandLeg == "L" ~ paste0(paste(FlagColour, FlagCode, sep = "("), ")")),
           LowerRightOut = "-") %>%
    mutate(Observations = paste(BloodTaken, Notes, sep = "; ")) %>%
    mutate(Observations = str_replace_all(Observations, "NA; NA", "")) %>%
    mutate(Observations = str_replace_all(Observations, "NA; ", "")) %>%
    mutate(Observations = str_replace_all(Observations, "; NA", "")) %>%
    select(ProjectID, CaptureMethodID, LocationID, Latitude, Longitude, CaptureDate, CAPTURES, SpeciesID,
           MetalIn, FlagColorIn, FlagCodeIn, UpperLeftIn, LowerLeftIn, UpperRightIn, LowerRightIn,
           MetalOut, FlagColorOut, FlagCodeOut, UpperLeftOut, LowerLeftOut, UpperRightOut, LowerRightOut,
           DispID, StatusID, AgeID, SexID, Culmen, Head, Wing, Weight, CaptureTime, WeightTime,
           LapsedMinutes, Observations, `N/R`, BMIMolts, BMIExtentID, BMIStage, PlumageID, PrimaryMolts,
           P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, Fats, FatScore, SizeAdjMass, FatGram, FatAdjGram,
           FatMassRatio, WingSpan, Blood, SampleNum, TrackerLogs, TrackerTypeID, TrackerID, Status) %>%
    arrange(CaptureDate) %>%
    group_by(CaptureDate) %>%
    mutate_at(vars(1:5), list(~replace(., duplicated(.), NA))) %>%
    ungroup() %>%
    mutate_at(vars(CaptureDate), list(~replace(., duplicated(.), NA)))


  bandedbirds_data$CaptureDate <- format(as.Date(bandedbirds_data$CaptureDate), "%d/%m/%y")
  bandedbirds_data$CaptureTime <- ymd_hms(bandedbirds_data$CaptureTime)
  bandedbirds_data$CaptureTime <- format(bandedbirds_data$CaptureTime, "%H:%M")

  assign("bandedbirds_data", bandedbirds_data, envir = globalenv())

}
