library(readxl)
library(RHRV)
library(tidyverse)

all_valleys <- readxl::read_xlsx("data.xlsx", sheet = "all_valleys")
all_valleys$Time <- as.numeric(all_valleys$Time)
all_valleys$Value <- as.numeric(all_valleys$Value)

extract_HRV_metrics <- function(hrv_obj, ID) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]]
    RR_ms <- hrv_obj$Beat
    data.frame(
      ID = ID,
      MeanHR = mean(hrv_obj$HR, na.rm = TRUE),
      SDNN = ta$SDNN,
      RMSSD = ta$rMSSD,
      pNN50 = ta$pNN50,
      NNi = mean(RR_ms$RR),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = ID,
      MeanHR = NA,
      SDNN = NA,
      RMSSD = NA,
      pNN50 = NA,
      NNi = NA,
      stringsAsFactors = FALSE
    )
  }
}
grouped <- all_valleys %>% group_by(Treatment, ID)
data_by_snail <- grouped %>% group_split()
Real_IDs <- grouped %>% group_keys()
results_HRV <- lapply(data_by_snail, function(df_snail) {
  HRV <- CreateHRVData()
  HRV <- LoadBeatVector(HRV, df_snail$Time)
  HRV <- BuildNIHR(HRV)
  HRV <- InterpolateNIHR(HRV)
  HRV <- CreateTimeAnalysis(HRV, size = 60, interval = 7.8125)
  return(HRV)
})

names(results_HRV) <- names(results_HRV) <- paste0(
  Real_IDs$Treatment,
  "_",
  Real_IDs$ID
)

HRV_metrics_total <- imap_dfr(results_HRV, ~ extract_HRV_metrics(.x, .y))

HRV_metrics_total <- HRV_metrics_total %>%
  tidyr::separate(ID, into = c("Treatment", "ID"), sep = "_")


extract_RR <- function(hrv_obj, ID) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]]
    RR_ms <- hrv_obj$Beat
    data.frame(
      ID = ID,
      RRms = (RR_ms$RR),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = ID,
      RRms = NA,
      stringsAsFactors = FALSE
    )
  }
}

RR_data <- imap_dfr(results_HRV, ~ extract_RR(.x, .y))

RR_data <- RR_data %>%
  tidyr::separate(ID, into = c("Treatment", "ID"), sep = "_")

pNN100_function <- function(rr, x = 100) {
  diff_rr <- abs(diff(rr)) # Succesive differences
  nnx <- sum(diff_rr > x) # Number of differences > 100 ms
  pnnx <- nnx / length(diff_rr) * 100
  return(pnnx)
}

cv_function <- function(rr) {
  mean <- mean(rr)
  sd <- sd(rr)
  CV <- (sd / mean) * 100
  return(CV)
}


CV_data <- RR_data %>%
  group_by(Treatment, ID) %>%
  summarise(CV = cv_function(RRms), .groups = "drop")


pnn100_data <- RR_data %>%
  group_by(Treatment, ID) %>%
  summarise(pNN100 = pNN100_function(RRms, x = 100), .groups = "drop")

HRV_metrics_total <- HRV_metrics_total %>%
  left_join(pnn100_data, by = c("Treatment", "ID")) %>%
  left_join(CV_data, by = c("Treatment", "ID"))


HRV_metrics_total <- HRV_metrics_total %>%
  mutate(
    Temperature = case_when(
      Treatment == "1C" ~ 1,
      Treatment == "8C" ~ 8,
      Treatment == "15C" ~ 15,
      Treatment == "29C" ~ 29,
      Treatment == "36C" ~ 36,
      Treatment == "WATHEAT" ~ 21,
      Treatment == "ENVHEAT" ~ 20,
      Treatment == "WATCOLD" ~ 22,
      Treatment == "ENVCOLD" ~ 22
    )
  ) %>%
  dplyr::filter(!is.na(Temperature))


HRV_metrics_total <- HRV_metrics_total %>%
  mutate(
    Block = case_when(
      Treatment == "1C" ~ "cold",
      Treatment == "8C" ~ "cold",
      Treatment == "15C" ~ "cold",
      Treatment == "29C" ~ "warm",
      Treatment == "36C" ~ "warm",
      Treatment == "WATHEAT" ~ "warm",
      Treatment == "ENVHEAT" ~ "warm",
      Treatment == "WATCOLD" ~ "cold",
      Treatment == "ENVCOLD" ~ "cold"
    )
  ) %>%
  dplyr::filter(!is.na(Block))

view(HRV_metrics_total)
