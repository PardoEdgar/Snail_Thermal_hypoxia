###############################################################################
# Cardiac parameters analysis (Comparison before and after water immersion)
# Author: Edgar Alejandro Pardo-Sarmiento
###############################################################################
library(tidyverse)
library(readxl)
library(rstatix)
library(ggpubr)

body_mass <- readxl::read_xlsx(
  "C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_JP_HRV_data/Pardo_Sarmiento _et_al_2/S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
  sheet = "Body_mass"
)
body_mass$ID <- as.character(body_mass$ID)
HRV_metrics_total <- readxl::read_xlsx(
  "C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_JP_HRV_data/Pardo_Sarmiento _et_al_2/S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
  sheet = "HRV_metrics_total"
)
HRV_metrics_total <- HRV_metrics_total %>%
  mutate(across(
    c("Temperature", "MeanHR", "SDNN", "RMSSD", "pNN50", "NNi", "CV"),
    as.numeric
  ))
HRV_metrics_total$ID <- as.character(HRV_metrics_total$ID)

combined_data <- left_join(
  HRV_metrics_total,
  body_mass,
  by = c("ID", "Treatment", "Block")
)
combined_data$Temperature <- factor(combined_data$Temperature)

#Results: HR & HRV baseline

HRV_metrics_baseline <- combined_data %>%
  dplyr::filter(
    Treatment %in% c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
  ) %>%
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
    )
  )

HRV_metrics_baseline_cold <- combined_data %>%
  dplyr::filter(Treatment %in% c("ENVCOLD", "WATCOLD")) %>%
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("ENVCOLD", "WATCOLD")
    )
  )

HRV_metrics_baseline_heat <- combined_data %>%
  dplyr::filter(Treatment %in% c("ENVHEAT", "WATHEAT")) %>%
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("ENVHEAT", "WATHEAT")
    )
  )


paired_cold <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, MeanHR) |>
  pivot_wider(
    names_from = Treatment,
    values_from = MeanHR
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold$WATCOLD - paired_cold$ENVCOLD
shapiro.test(diff_cold)


t.test(
  paired_cold$ENVCOLD,
  paired_cold$WATCOLD,
  paired = TRUE
)


paired_heat <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, MeanHR) |>
  pivot_wider(
    names_from = Treatment,
    values_from = MeanHR
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat$WATHEAT - paired_heat$ENVHEAT
shapiro.test(diff_heat)

t.test(
  paired_heat$ENVHEAT,
  paired_heat$WATHEAT,
  paired = TRUE,
)

mean_summary <- paired_cold |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    Mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary

mean_summary <- paired_heat |>
  summarise(
    Mean_ENVHEAT = mean(ENVHEAT, na.rm = TRUE),
    SD_ENVHEAT = sd(ENVHEAT, na.rm = TRUE),
    Mean_WATHEAT = mean(WATHEAT, na.rm = TRUE),
    SD_WATHEAT = sd(WATHEAT, na.rm = TRUE),
    n = n()
  )
mean_summary

#CV

paired_cold <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, CV) |>
  pivot_wider(
    names_from = Treatment,
    values_from = CV
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold$WATCOLD - paired_cold$ENVCOLD
shapiro.test(diff_cold)

t.test(
  paired_cold$ENVCOLD,
  paired_cold$WATCOLD,
  paired = TRUE
)


paired_heat <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, CV) |>
  pivot_wider(
    names_from = Treatment,
    values_from = CV
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat$WATHEAT - paired_heat$ENVHEAT
shapiro.test(diff_heat)

wilcox.test(
  paired_heat$ENVHEAT,
  paired_heat$WATHEAT,
  paired = TRUE,
)

mean_summary <- paired_cold |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary

median_summary <- paired_heat |>
  summarise(
    Median_ENVHEAT = median(ENVHEAT, na.rm = TRUE),
    IQR_ENVHEAT = IQR(ENVHEAT, na.rm = TRUE),
    Median_WATHEAT = median(WATHEAT, na.rm = TRUE),
    IQR_WATHEAT = IQR(WATHEAT, na.rm = TRUE),
    n = n()
  )
median_summary


HR_1 <- ggplot(
  data = HRV_metrics_baseline,
  aes(x = Treatment, y = MeanHR, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5) +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4) +
  scale_color_manual(
    values = c(
      "Cold" = "black",
      "Heat" = "black"
    )
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5,
    position = position_dodge(width = 0.4)
  ) +
  labs(x = "Baseline", y = "Heart rate (BPM)") +
  theme_classic2() +
  theme(legend.position = "Top")

CV_1 <- ggplot(
  data = HRV_metrics_baseline,
  aes(x = Treatment, y = CV, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.6, size = 0.5) +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4) +
  scale_color_manual(
    values = c(
      "Cold" = "black",
      "Heat" = "black"
    )
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5,
    position = position_dodge(width = 0.4)
  ) +
  labs(x = "Baseline", y = "Heart rate variability (CV%)") +
  coord_cartesian(ylim = c(0, 20)) +
  theme_classic2() +
  theme(legend.position = "Top")

#Time domain metrics

ggplot(
  data = HRV_metrics_baseline,
  aes(x = Treatment, y = NNi, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5) +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4) +
  scale_color_manual(
    values = c(
      "Cold" = "black",
      "Heat" = "black"
    )
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5,
    position = position_dodge(width = 0.4)
  ) +
  labs(x = "Baseline", y = "NN intervals (ms)") +
  theme_classic2() +
  theme(legend.position = "Top")


ggplot(
  data = HRV_metrics_baseline,
  aes(x = Treatment, y = pNN50, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5) +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4) +
  scale_color_manual(
    values = c(
      "Cold" = "black",
      "Heat" = "black"
    )
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5,
    position = position_dodge(width = 0.4)
  ) +
  labs(x = "Baseline", y = "pNN50 (%)") +
  theme_classic2() +
  theme(legend.position = "Top")


ggplot(
  data = HRV_metrics_baseline,
  aes(x = Treatment, y = RMSSD, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5) +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4) +
  scale_color_manual(
    values = c(
      "Cold" = "black",
      "Heat" = "black"
    )
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5,
    position = position_dodge(width = 0.4)
  ) +
  labs(x = "Baseline", y = "RMSSD (ms)") +
  coord_cartesian(ylim = c(0, 300)) +
  theme_classic2() +
  theme(legend.position = "Top")


ggplot(
  data = HRV_metrics_baseline,
  aes(x = Treatment, y = SDNN, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5) +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4) +
  scale_color_manual(
    values = c(
      "Cold" = "black",
      "Heat" = "black"
    )
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5,
    position = position_dodge(width = 0.4)
  ) +
  labs(x = "Baseline", y = "SDNN (ms)") +
  coord_cartesian(ylim = c(0, 300)) +
  theme_classic2() +
  theme(legend.position = "Top")


paired_cold <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, NNi) |>
  pivot_wider(
    names_from = Treatment,
    values_from = NNi
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold$WATCOLD - paired_cold$ENVCOLD
shapiro.test(diff_cold)


wilcox.test(
  paired_cold$ENVCOLD,
  paired_cold$WATCOLD,
  paired = TRUE
)


paired_heat <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, NNi) |>
  pivot_wider(
    names_from = Treatment,
    values_from = NNi
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat$WATHEAT - paired_heat$ENVHEAT
shapiro.test(diff_heat)

t.test(
  paired_heat$ENVHEAT,
  paired_heat$WATHEAT,
  paired = TRUE,
)


median_summary <- paired_cold |>
  summarise(
    Median_ENVCOLD = median(ENVCOLD, na.rm = TRUE),
    IQR_ENVCOLD = IQR(ENVCOLD, na.rm = TRUE),
    Median_WATCOLD = median(WATCOLD, na.rm = TRUE),
    IQR_WATCOLD = IQR(WATCOLD, na.rm = TRUE),
    n = n()
  )

median_summary

mean_summary <- paired_heat |>
  summarise(
    Mean_ENVHEAT = mean(ENVHEAT, na.rm = TRUE),
    SD_ENVHEAT = sd(ENVHEAT, na.rm = TRUE),
    Mean_WATHEAT = mean(WATHEAT, na.rm = TRUE),
    SD_WATHEAT = sd(WATHEAT, na.rm = TRUE),
    n = n()
  )
mean_summary


paired_cold <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, pNN50) |>
  pivot_wider(
    names_from = Treatment,
    values_from = pNN50
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold$WATCOLD - paired_cold$ENVCOLD
shapiro.test(diff_cold)


t.test(
  paired_cold$ENVCOLD,
  paired_cold$WATCOLD,
  paired = TRUE
)


paired_heat <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, pNN50) |>
  pivot_wider(
    names_from = Treatment,
    values_from = pNN50
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat$WATHEAT - paired_heat$ENVHEAT
shapiro.test(diff_heat)

t.test(
  paired_heat$ENVHEAT,
  paired_heat$WATHEAT,
  paired = TRUE,
)


mean_summary <- paired_cold |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    Mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary

mean_summary <- paired_heat |>
  summarise(
    Mean_ENVHEAT = mean(ENVHEAT, na.rm = TRUE),
    SD_ENVHEAT = sd(ENVHEAT, na.rm = TRUE),
    Mean_WATHEAT = mean(WATHEAT, na.rm = TRUE),
    SD_WATHEAT = sd(WATHEAT, na.rm = TRUE),
    n = n()
  )
mean_summary


paired_cold <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, SDNN) |>
  pivot_wider(
    names_from = Treatment,
    values_from = SDNN
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold$WATCOLD - paired_cold$ENVCOLD
shapiro.test(diff_cold)

t.test(
  paired_cold$ENVCOLD,
  paired_cold$WATCOLD,
  paired = TRUE
)


paired_heat <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, SDNN) |>
  pivot_wider(
    names_from = Treatment,
    values_from = SDNN
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat$WATHEAT - paired_heat$ENVHEAT
shapiro.test(diff_heat)

wilcox.test(
  paired_heat$ENVHEAT,
  paired_heat$WATHEAT,
  paired = TRUE,
)


mean_summary <- paired_cold |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    Mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary


median_summary <- paired_heat |>
  summarise(
    Median_ENVHEAT = median(ENVHEAT, na.rm = TRUE),
    IQR_ENVHEAT = IQR(ENVHEAT, na.rm = TRUE),
    Median_WATHEAT = median(WATHEAT, na.rm = TRUE),
    IQR_WATHEAT = IQR(WATHEAT, na.rm = TRUE),
    n = n()
  )
median_summary

paired_cold <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, RMSSD) |>
  pivot_wider(
    names_from = Treatment,
    values_from = RMSSD
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold$WATCOLD - paired_cold$ENVCOLD
shapiro.test(diff_cold)


t.test(
  paired_cold$ENVCOLD,
  paired_cold$WATCOLD,
  paired = TRUE
)


paired_heat <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, RMSSD) |>
  pivot_wider(
    names_from = Treatment,
    values_from = RMSSD
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat$WATHEAT - paired_heat$ENVHEAT
shapiro.test(diff_heat)

wilcox.test(
  paired_heat$ENVHEAT,
  paired_heat$WATHEAT,
  paired = TRUE
)


mean_summary <- paired_cold |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    Mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary


median_summary <- paired_heat |>
  summarise(
    Median_ENVHEAT = median(ENVHEAT, na.rm = TRUE),
    IQR_ENVHEAT = IQR(ENVHEAT, na.rm = TRUE),
    Median_WATHEAT = median(WATHEAT, na.rm = TRUE),
    IQR_WATHEAT = IQR(WATHEAT, na.rm = TRUE),
    n = n()
  )
median_summary
