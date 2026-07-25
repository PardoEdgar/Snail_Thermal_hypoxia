library(readxl)
library(RHRV)
all_valleys <- readxl::read_xlsx("C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_JP_HRV_data/Data/data_extraction/S1_File_Supplementary_data_Pardo_Sarmiento.xlsx", sheet = "all_valleys")
all_valleys$Time <- as.numeric(all_valleys$Time)
all_valleys$Value <- as.numeric(all_valleys$Value)

extract_HRV_metrics <- function(hrv_obj, id) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]] 
    RR_ms <- hrv_obj$Beat 
      data.frame(
      ID = id,
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
results_HRV<- lapply(data_by_snail, function(df_snail) {
  HRV <- CreateHRVData()
  HRV <- LoadBeatVector(HRV, df_snail$Time)
  HRV <- BuildNIHR(HRV)
  HRV <- InterpolateNIHR(HRV)
  HRV <- CreateTimeAnalysis(HRV, size = 60, interval = 7.8125)
  return(HRV)
})

names(results_HRV) <- names(results_HRV) <- paste0(Real_IDs$Treatment, "_", Real_IDs$ID)

HRV_metrics_total<- imap_dfr(results_HRV, ~ extract_HRV_metrics(.x, .y))

HRV_metrics_total <- HRV_metrics_total %>%
  tidyr::separate(ID, into = c("Treatment", "ID"), sep = "_")

View(HRV_metrics_total)




extract_RR <- function(hrv_obj, ID) {
  if (!is.null(hrv_obj$TimeAnalysis[[1]])) {
    ta <- hrv_obj$TimeAnalysis[[1]]  
    RR_ms <- hrv_obj$Beat
    data.frame(
      ID = ID,
      RRms= (RR_ms$RR),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      ID = ID,
      RRms= NA,
      stringsAsFactors = FALSE
    )
  }
}

RR_data <- imap_dfr(results_HRV, ~ extract_RR(.x, .y))

RR_data <- RR_data %>%
  tidyr::separate(ID, into = c("Treatment", "ID"), sep = "_")

pNN100_function <- function(rr, x = 100) {
  diff_rr <- abs(diff(rr))   # Succesive differences
  nnx <- sum(diff_rr > x)    # Number of differences > 100 ms
  pnnx <- nnx / length(diff_rr) * 100
  return(pnnx)
}

pnn100_data <- RR_data %>%
  group_by(ID) %>%
  summarise(pNN100= pNN100_function(RRms, x = 100)) 

View(pnn100_data)

HRV_metrics_total <- HRV_metrics_total %>%
  left_join(pnn100_data, by = "ID") 
