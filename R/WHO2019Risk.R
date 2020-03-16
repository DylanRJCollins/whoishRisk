#' Calculating 2019 WHO 10-year CVD risk scores
#'
#' This function calculates 2019 WHO 10-year CVD risk scores for any epidemiolgical subregion of the world.
#'
#' @param age age (years)
#' @param gdr gender (1 = male, 0 = female)
#' @param smk smoking status (1 = current smoker, 0 = non-smoker)
#' @param sbp systolic blood pressure (mmHg)
#' @param dm diabetes status (1 = diabetic, 0 = non-diabetic)
#' @param chl total cholesterol (mmol/L)
#' @param subregion epidemiolgical subregion (character value, one of: "N_AFR_ME", "CSS_AFR", "ESS_AFR", "SSS_AFR", "WSS_AFR", "SN_LAT_AME", "HI_N_AME", "CAR", "AND_LAT_AME", "CEN_LAT_AME", "TRO_LAT_AME", "EAS_ASI", "SOU_ASI", "SE_ASI", "CEN_ASI", "HI_ASI_PAC", "WES_EUR", "CEN_EUR", "EAS_EUR", "OCE", "AUS")
#' @return A vector of WHO risk scores.
#' @author Rodrigo Citton P. dos Reis, Dylan R.J. Collins, Joseph Lee, Niklas Bobrovtiz, Constantinos Koshiaris, Alison Ward, Carl Heneghan
#' @details
#' This function calculates the updated 2019 WHO 10-year CVD risk scores for any epidemiological subregion based on the values of six paramters passed to it (age, gender, smoking status, systolic blood pressure, diabetes status, and total cholesterol).
#'
#' Subregions:
#'
#'   "N_AFR_ME": North Africa and Middle East
#'
#'   "CSS_AFR": Central Sub-Saharan Africa
#'
#'   "ESS_AFR": Eastern Sub-Saharan Africa
#'
#'   "SSS_AFR": Southern Sub-Saharan Africa
#'
#'   "WSS_AFR": Western Sub-Saharan Africa
#'
#'   "SN_LAT_AME": Southern Latin America
#'
#'   "HI_N_AME": High-income North America
#'
#'   "CAR": Caribbean
#'
#'   "AND_LAT_AME": Andean Latin America
#'
#'   "CEN_LAT_AME": Central Latin America
#'
#'   "TRO_LAT_AME": Tropical Latin America
#'
#'   "EAS_ASI": East Asia
#'
#'   "SOU_ASI": South Asia
#'
#'   "SE_ASI": Southeast Asia
#'
#'   "CEN_ASI": Central Asia
#'
#'   "HI_ASI_PAC": High-income Asia Pacific
#'
#'   "WES_EUR": Western Europe
#'
#'   "CEN_EUR": Central Europe
#'
#'   "EAS_EUR": Eastern Europe
#'
#'   "OCE": Oceania
#'
#'   "AUS": Australasia
#'
#' @references
#' The WHO CVD Risk Chart Working Group. (2019). World Health
#' Organization cardiovascular disease risk charts: revised models to
#' estimate risk in 21 global regions.
#' *Lancet Glob Health* **(published online Sept 2.)**.
#' @export

WHO_CVD_Risk_2019 <- function(age, gdr, smk, sbp, dm, chl, subregion) {
  #Load required data
  df <- data.frame(age, gdr, smk, sbp, dm, chl)
  ref <- read.csv(file = system.file("extdata", "WHO_2019_Scores.csv", package = "whoishRisk"))

  #Warning Messages
  # if (any(df$age < 19))
  #   warning("At least one age is 18 or younger")
  # if (any(df$age > 100))
  #   warning("At least one age is greater than 100")
  # if (any(df$gdr > 1))
  #   warning("Gender must be equal to 0 or 1")
  # if (any(df$smk > 1))
  #   warning("Smoking must be equal to 0 or 1")
  # if (any(df$sbp < 90))
  #   warning("At least one systolic blood pressure is below 90 mmHg")
  # if (any(df$sbp > 250))
  #   warning("At least one systolic blood pressure is over 250 mmHg")
  # if (any(df$dm > 1))
  #   warning("Diabetes status must be equal to 0 or 1")
  # if (any(df$chl > 10))
  #   warning(
  #     "At least one total cholesterol is greater than 10 mmol/L. Ensure all values are in units of mmol/L"
  #   )


  #Age
  df$age_cat <- NULL
  df$age_cat <- ifelse(df$age > 17 & df$age <= 44, "40-44",
                       ifelse(df$age > 44 & df$age <= 49, "45-49",
                              ifelse(df$age > 49 & df$age <= 54, "50-54",
                                     ifelse(df$age > 54 & df$age <= 59, "55-59",
                                            ifelse(df$age > 59 & df$age <= 64, "60-64",
                                                   ifelse(df$age > 64 & df$age <= 69, "65-69", "70-74"))))))

  #Cholesterol
  df$chl_cat <- NULL
  df$chl_cat <- ifelse(df$chl > 0 & df$chl < 4, "<4",
                       ifelse(df$chl > 4 & df$chl < 5, "4-4.9",
                              ifelse(df$chl > 5 & df$chl < 6, "5-5.9",
                                     ifelse(df$chl > 6 & df$chl < 7, "6-6.9", ">=7"))))

  #Systolic Blood Pressure
  df$sbp_cat <- NULL
  df$sbp_cat <- ifelse(df$sbp > 0 & df$sbp < 120, "<120",
                       ifelse(df$sbp > 120 & df$sbp < 140, "120-139",
                              ifelse(df$sbp > 140 & df$sbp < 160, "140-159",
                                     ifelse(df$sbp > 160 & df$sbp < 180, "160-179", ">=180"))))

  #Create a new variable called luv (look up value)
  df$luv <- paste(df$age_cat, df$gdr, df$dm, df$smk, df$sbp_cat, df$chl_cat, sep = "_")

  #Create a new variable called refv (reference value)
  ref$refv <-
    paste(ref$age, ref$gdr, ref$dm, ref$smk, ref$sbp, ref$chl, sep = "_")

  #Match the look up value with the reference value
  if (subregion == "N_AFR_ME") {
    df$N_AFR_ME <- ref$N_AFR_ME[match(df$luv, ref$refv)]
    return(df$N_AFR_ME)
  }

  if (subregion == "CSS_AFR") {
    df$CSS_AFR <- ref$CSS_AFR[match(df$luv, ref$refv)]
    return(df$CSS_AFR)
  }

  if (subregion == "ESS_AFR") {
    df$ESS_AFR <- ref$ESS_AFR[match(df$luv, ref$refv)]
    return(df$ESS_AFR)
  }

  if (subregion == "SSS_AFR") {
    df$SSS_AFR <- ref$SSS_AFR[match(df$luv, ref$refv)]
    return(df$SSS_AFR)
  }

  if (subregion == "WSS_AFR") {
    df$WSS_AFR <- ref$WSS_AFR[match(df$luv, ref$refv)]
    return(df$WSS_AFR)
  }

  if (subregion == "SN_LAT_AME") {
    df$SN_LAT_AME <- ref$SN_LAT_AME[match(df$luv, ref$refv)]
    return(df$SN_LAT_AME)
  }

  if (subregion == "HI_N_AME") {
    df$HI_N_AME <- ref$HI_N_AME[match(df$luv, ref$refv)]
    return(df$HI_N_AME)
  }

  if (subregion == "CAR") {
    df$CAR <- ref$CAR[match(df$luv, ref$refv)]
    return(df$CAR)
  }

  if (subregion == "AND_LAT_AME") {
    df$AND_LAT_AME <- ref$AND_LAT_AME[match(df$luv, ref$refv)]
    return(df$AND_LAT_AME)
  }

  if (subregion == "CEN_LAT_AME") {
    df$CEN_LAT_AME <- ref$CEN_LAT_AME[match(df$luv, ref$refv)]
    return(df$CEN_LAT_AME)
  }

  if (subregion == "TRO_LAT_AME") {
    df$TRO_LAT_AME <- ref$TRO_LAT_AME[match(df$luv, ref$refv)]
    return(df$TRO_LAT_AME)
  }

  if (subregion == "EAS_ASI") {
    df$EAS_ASI <- ref$EAS_ASI[match(df$luv, ref$refv)]
    return(df$EAS_ASI)
  }

  if (subregion == "SOU_ASI") {
    df$SOU_ASI <- ref$SOU_ASI[match(df$luv, ref$refv)]
    return(df$SOU_ASI)
  }

  if (subregion == "SE_ASI") {
    df$SE_ASI <- ref$SE_ASI[match(df$luv, ref$refv)]
    return(df$SE_ASI)
  }

  if (subregion == "CEN_ASI") {
    df$CEN_ASI <- ref$CEN_ASI[match(df$luv, ref$refv)]
    return(df$CEN_ASI)
  }

  if (subregion == "HI_ASI_PAC") {
    df$HI_ASI_PAC <- ref$HI_ASI_PAC[match(df$luv, ref$refv)]
    return(df$HI_ASI_PAC)
  }

  if (subregion == "WES_EUR") {
    df$WES_EUR <- ref$WES_EUR[match(df$luv, ref$refv)]
    return(df$WES_EUR)
  }

  if (subregion == "CEN_EUR") {
    df$CEN_EUR <- ref$CEN_EUR[match(df$luv, ref$refv)]
    return(df$CEN_EUR)
  }

  if (subregion == "EAS_EUR") {
    df$EAS_EUR <- ref$EAS_EUR[match(df$luv, ref$refv)]
    return(df$EAS_EUR)
  }

  if (subregion == "OCE") {
    df$OCE <- ref$OCE[match(df$luv, ref$refv)]
    return(df$OCE)
  }

  if (subregion == "AUS") {
    df$AUS <- ref$AUS[match(df$luv, ref$refv)]
    return(df$AUS)
  }

}
