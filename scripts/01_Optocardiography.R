###############################################################################
# Optocardiography
# Author: Edgar Alejandro Pardo-Sarmiento
###############################################################################

library(tidyverse)
library(signal)

folder_path <- "" #folder_with_pixel_intensity_data_from_individuals
Available_IDs <- c() #all numbers of available IDs
All_results_experiment <- list() 
for (id in Available_IDs) {
filename <- file.path(folder_path, paste0(id, "_ENVCOLD_Ven_Edited.csv"))
    cat("Searching:", filename, "\n")
  if (!file.exists(filename)) {
    cat("File not found for ID", id, "\n")
    next
  }

  data <- read_csv(filename)

Duration_seconds <- 60  
data_long <- data %>%   
  select(contains("Mean")) %>% 
  mutate(t = seq(0, Duration_seconds, length.out = n())) %>% 
  pivot_longer(-t, names_to = "Mean", values_to = "value")
  results <- data_long %>% arrange(t) %>% 
    mutate(smoothed = sgolayfilt(value, p = 5, n = 25), trend = predict(loess(smoothed ~ t, span = 0.08), newdata = data.frame(t = t))) 

  derivative <- c(NA, diff(results$smoothed))  
  changes <- sign(derivative)
  significant_changes <- c(NA, diff(changes))

  valley_indices <- which(significant_changes == 2)
  valley_values <- results$smoothed[valley_indices]

  trend_fit <- loess(smoothed ~ t, data = results, span = 0.08)
  trend <- predict(trend_fit, newdata = data.frame(t = results$t))
  threshold <- 1 * trend
  valid_valleys <- valley_indices[valley_values < threshold[valley_indices]]
  min_dist <- 18
  values <- results$smoothed[valid_valleys]
  filtered_valleys <- c()

  if (length(valid_valleys) == 1) {
    filtered_valleys <- valid_valleys
  } else if (length(valid_valleys) > 1) {
    current_group <- c(valid_valleys[1])
    filtered_valleys <- c()
    
    for (i in 2:length(valid_valleys)) {
      if ((valid_valleys[i] - valid_valleys[i - 1]) <= min_dist) {
        current_group <- c(current_group, valid_valleys[i])
      } else {
        min_valley <- current_group[which.min(results$smoothed[current_group])]
        filtered_valleys <- c(filtered_valleys, min_valley)
        current_group <- c(valid_valleys[i])
      }
    }
    
    if (length(current_group) > 0) {
      min_valley <- current_group[which.min(results$smoothed[current_group])]
      filtered_valleys <- c(filtered_valleys, min_valley)
    }
  }

  local_valley_threshold <- trend[filtered_valleys]
  filtered_valley_values <- results$smoothed[filtered_valleys]
  valley_trend_filter <- filtered_valley_values < local_valley_threshold
  filtered_valleys <- filtered_valleys[valley_trend_filter]

  if (length(filtered_valleys) > 0) {
    final_valleys <- tibble(
      time = results$t[filtered_valleys],
      value = results$smoothed[filtered_valleys]
    )
  } else {
    final_valleys <- NULL
  }

  derivative <- c(NA, diff(results$smoothed))
  changes <- sign(derivative)
  significant_changes <- c(NA, diff(changes))

  peak_indices <- which(significant_changes == -2)
  peak_values <- results$smoothed[peak_indices]

  trend_fit <- loess(smoothed ~ t, data = results, span = 0.08)
  trend <- predict(trend_fit, newdata = data.frame(t = results$t))

  threshold <- 1.01 * trend
  local_threshold <- threshold[peak_indices]

  valid_peaks <- peak_indices[peak_values > local_threshold]
  min_dist <- 13
  values <- results$smoothed[valid_peaks]

  filtered_peaks <- c()

  if (length(valid_peaks) == 1) {
    filtered_peaks <- valid_peaks
  } else if (length(valid_peaks) > 1) {
    current_group <- c(valid_peaks[1])
    filtered_peaks <- c()
    
    for (i in 2:length(valid_peaks)) {
      if ((valid_peaks[i] - valid_peaks[i - 1]) <= min_dist) {
        current_group <- c(current_group, valid_peaks[i])
      } else {
        max_peak <- current_group[which.max(results$smoothed[current_group])]
        filtered_peaks <- c(filtered_peaks, max_peak)
        current_group <- c(valid_peaks[i])
      }
    }
    
    if (length(current_group) > 0) {
      max_peak <- current_group[which.max(results$smoothed[current_group])]
      filtered_peaks <- c(filtered_peaks, max_peak)
    }
  }

  local_threshold_filtered <- trend[filtered_peaks]
  filtered_values <- results$smoothed[filtered_peaks]
  trend_filter <- filtered_values > local_threshold_filtered
  filtered_peaks <- filtered_peaks[trend_filter]

  if (length(filtered_peaks) > 0) {
    final_peaks <- tibble(
      time = results$t[filtered_peaks],
      value = results$smoothed[filtered_peaks]
    )
  } else {
    final_peaks <- NULL
  }

  data_smoothed <- bind_rows(results)

  cat("Processed ID", id, "\n")

  plot_individual <- ggplot(data_smoothed, aes(x = t)) +
    geom_line(aes(y = value, color = "Raw signal"), size = 0.8) +
    geom_line(aes(y = smoothed, color = "Smoothed signal"), size = 0.9) +
    geom_line(aes(y = trend, color = "Trend", linetype = "Trend"), size = 1) +
    geom_point(data = final_valleys, aes(x = time, y = value, color = "Valleys", shape = "Valleys"), size = 2) +
    geom_point(data = final_peaks, aes(x = time, y = value, color = "Peaks", shape = "Peaks"), size = 2) +

    labs(title = paste("Signals and detected peaks - ID", id),
         x = "Time (s)", y = "Intensity", color = "", linetype = "", shape = "") +

    scale_color_manual(values = c(
      "Raw signal" = "gray70",
      "Smoothed signal" = "blue",
      "Trend" = "green",
      "Valleys" = "orange",
      "Peaks" = "red"
    )) +
    scale_linetype_manual(values = c("Trend" = "dashed")) +
    scale_shape_manual(values = c("Valleys" = 16, "Peaks" = 16)) +

    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
      axis.title = element_text(size = 14, color = "black"),
      axis.text = element_text(size = 12, color = "black"),
      strip.text = element_text(color = "black"),
      legend.text = element_text(color = "black")
    )

  ggsave(filename = file.path(folder_path, "Graphs", paste0("Graph", id, ".png")),
         plot = plot_individual, width = 12, height = 8, dpi = 300)

  All_results_experiment[[as.character(id)]] <- list(
    Smoothed_data = data_smoothed %>% mutate(ID = id),
    final_valleys = final_valleys %>% mutate(ID = id),
    final_peaks = final_peaks %>% mutate(ID = id)
  )
  }

Smoothed_data_all <- map_dfr(All_results_experiment, "Smoothed_data")
all_valleys <- map_dfr(All_results_experiment, "final_valleys")
picos_ENVCOLD_rep <- map_dfr(All_results_experiment, "final_peaks")

#Repeat for each treatment
all_valleys <- all_valleys %>% 
  mutate(Treatment = "") #Type treatment
