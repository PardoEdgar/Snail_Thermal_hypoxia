all_valleys <- read.csv("all_valleys_data.csv")

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
      ID = id,
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
  HRV <- LoadBeatVector(HRV, df_snail$time)
  HRV <- BuildNIHR(HRV)
  HRV <- InterpolateNIHR(HRV)
  HRV <- CreateTimeAnalysis(HRV, size = 60, interval = 7.8125)
  return(HRV)
})

names(results_HRV) <- names(results_HRV) <- paste0(Real_IDs$Treatment, "_", Real_IDs$ID)

HRV_metrics_total<- imap_dfr(results_HRV, ~ extract_HRV_metrics(.x, .y))

HRV_metrics_total <- HRV_metrics_total %>%
  tidyr::separate(ID, into = c("Treatment", "ID"), sep = "_")

