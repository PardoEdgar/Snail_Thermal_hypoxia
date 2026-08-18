###############################################################################
# Cardiac parameters analysis (Comparison before and after water immersion)
# Author: Edgar Alejandro Pardo-Sarmiento
###############################################################################
library(tidyverse)
library(readxl)
library(rstatix)
library(ggpubr)

body_mass <- readxl::read_xlsx(
  "S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
  sheet = "Body_mass"
)
body_mass$ID <- as.character(body_mass$ID)
HRV_metrics_total <- readxl::read_xlsx(
  "S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
  sheet = "HRV_metrics_total"
)
HRV_metrics_total <- HRV_metrics_total %>%
  mutate(across(
    c("Temperature", "MeanHR", "SDNN", "RMSSD", "pNN50", "NNi", "CV"),
    as.numeric
  ))
HRV_metrics_total$ID <- as.character(HRV_metrics_total$ID)
nrow(HRV_metrics_total)
HRV_metrics_total |>
  group_by(Treatment) |>
  summarise(n = n())
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


paired_cold_HR <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, MeanHR) |>
  pivot_wider(
    names_from = Treatment,
    values_from = MeanHR
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold_HR$WATCOLD - paired_cold_HR$ENVCOLD
shapiro.test(diff_cold)


t.test(
  paired_cold_HR$ENVCOLD,
  paired_cold_HR$WATCOLD,
  paired = TRUE
)


paired_heat_HR <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, MeanHR) |>
  pivot_wider(
    names_from = Treatment,
    values_from = MeanHR
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat_HR$WATHEAT - paired_heat_HR$ENVHEAT
shapiro.test(diff_heat)

t.test(
  paired_heat_HR$ENVHEAT,
  paired_heat_HR$WATHEAT,
  paired = TRUE,
)

mean_summary <- paired_cold_HR |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    Mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary

mean_summary <- paired_heat_HR |>
  summarise(
    Mean_ENVHEAT = mean(ENVHEAT, na.rm = TRUE),
    SD_ENVHEAT = sd(ENVHEAT, na.rm = TRUE),
    Mean_WATHEAT = mean(WATHEAT, na.rm = TRUE),
    SD_WATHEAT = sd(WATHEAT, na.rm = TRUE),
    n = n()
  )
mean_summary


plot_cold_HR <- paired_cold_HR |>
  pivot_longer(
    cols = c(ENVCOLD, WATCOLD),
    names_to = "Treatment",
    values_to = "MeanHR"
  ) |>
  mutate(
    Block = "Low",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_heat_HR <- paired_heat_HR |>
  pivot_longer(
    cols = c(ENVHEAT, WATHEAT),
    names_to = "Treatment",
    values_to = "MeanHR"
  ) |>
  mutate(
    Block = "High",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_HR <- bind_rows(plot_cold_HR, plot_heat_HR)
plot_HR$Treatment <- factor(
  plot_HR$Treatment,
  levels = c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
)

HR_1 <- ggplot(
  data = plot_HR,
  aes(x = Treatment, y = MeanHR, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5, color="black") +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4, color="black") +
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


#CV

paired_cold_CV <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, CV) |>
  pivot_wider(
    names_from = Treatment,
    values_from = CV
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold_CV$WATCOLD - paired_cold_CV$ENVCOLD
shapiro.test(diff_cold)

t.test(
  paired_cold_CV$ENVCOLD,
  paired_cold_CV$WATCOLD,
  paired = TRUE
)


paired_heat_CV <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, CV) |>
  pivot_wider(
    names_from = Treatment,
    values_from = CV
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat_CV$WATHEAT - paired_heat_CV$ENVHEAT
shapiro.test(diff_heat)

wilcox.test(
  paired_heat_CV$ENVHEAT,
  paired_heat_CV$WATHEAT,
  paired = TRUE,
)

mean_summary <- paired_cold_CV |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary

median_summary <- paired_heat_CV |>
  summarise(
    Median_ENVHEAT = median(ENVHEAT, na.rm = TRUE),
    IQR_ENVHEAT = IQR(ENVHEAT, na.rm = TRUE),
    Median_WATHEAT = median(WATHEAT, na.rm = TRUE),
    IQR_WATHEAT = IQR(WATHEAT, na.rm = TRUE),
    n = n()
  )
median_summary


plot_cold_CV <- paired_cold_CV |>
  pivot_longer(
    cols = c(ENVCOLD, WATCOLD),
    names_to = "Treatment",
    values_to = "CV"
  ) |>
  mutate(
    Block = "Low",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_heat_CV <- paired_heat_CV |>
  pivot_longer(
    cols = c(ENVHEAT, WATHEAT),
    names_to = "Treatment",
    values_to = "CV"
  ) |>
  mutate(
    Block = "High",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_CV <- bind_rows(plot_cold_CV, plot_heat_CV)
plot_CV$Treatment <- factor(
  plot_CV$Treatment,
  levels = c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
)

CV_1 <- ggplot(
  data = plot_CV,
  aes(x = Treatment, y = CV, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.6, size = 0.5, color= "black") +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4,  color= "black") +
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


#NNi
paired_cold_NNi <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, NNi) |>
  pivot_wider(
    names_from = Treatment,
    values_from = NNi
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold_NNi$WATCOLD - paired_cold_NNi$ENVCOLD
shapiro.test(diff_cold)


wilcox.test(
  paired_cold_NNi$ENVCOLD,
  paired_cold_NNi$WATCOLD,
  paired = TRUE
)


paired_heat_NNi <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, NNi) |>
  pivot_wider(
    names_from = Treatment,
    values_from = NNi
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat_NNi$WATHEAT - paired_heat_NNi$ENVHEAT
shapiro.test(diff_heat)

t.test(
  paired_heat_NNi$ENVHEAT,
  paired_heat_NNi$WATHEAT,
  paired = TRUE,
)


median_summary <- paired_cold_NNi |>
  summarise(
    Median_ENVCOLD = median(ENVCOLD, na.rm = TRUE),
    IQR_ENVCOLD = IQR(ENVCOLD, na.rm = TRUE),
    Median_WATCOLD = median(WATCOLD, na.rm = TRUE),
    IQR_WATCOLD = IQR(WATCOLD, na.rm = TRUE),
    n = n()
  )

median_summary

mean_summary <- paired_heat_NNi |>
  summarise(
    Mean_ENVHEAT = mean(ENVHEAT, na.rm = TRUE),
    SD_ENVHEAT = sd(ENVHEAT, na.rm = TRUE),
    Mean_WATHEAT = mean(WATHEAT, na.rm = TRUE),
    SD_WATHEAT = sd(WATHEAT, na.rm = TRUE),
    n = n()
  )
mean_summary


plot_cold_NNi <- paired_cold_NNi |>
  pivot_longer(
    cols = c(ENVCOLD, WATCOLD),
    names_to = "Treatment",
    values_to = "NNi"
  ) |>
  mutate(
    Block = "Low",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_heat_NNi <- paired_heat_NNi |>
  pivot_longer(
    cols = c(ENVHEAT, WATHEAT),
    names_to = "Treatment",
    values_to = "NNi"
  ) |>
  mutate(
    Block = "High",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_NNi <- bind_rows(plot_cold_NNi, plot_heat_NNi)
plot_NNi$Treatment <- factor(
  plot_NNi$Treatment,
  levels = c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
)

ggplot(
  data = plot_NNi,
  aes(x = Treatment, y = NNi, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5, color="black") +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4, color="black") +
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




paired_cold_pNN50 <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, pNN50) |>
  pivot_wider(
    names_from = Treatment,
    values_from = pNN50
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold_pNN50$WATCOLD - paired_cold_pNN50$ENVCOLD
shapiro.test(diff_cold)


t.test(
  paired_cold_pNN50$ENVCOLD,
  paired_cold_pNN50$WATCOLD,
  paired = TRUE
)


paired_heat_pNN50 <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, pNN50) |>
  pivot_wider(
    names_from = Treatment,
    values_from = pNN50
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat_pNN50$WATHEAT - paired_heat_pNN50$ENVHEAT
shapiro.test(diff_heat)

t.test(
  paired_heat_pNN50$ENVHEAT,
  paired_heat_pNN50$WATHEAT,
  paired = TRUE,
)


mean_summary <- paired_cold_pNN50 |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    Mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary

mean_summary <- paired_heat_pNN50 |>
  summarise(
    Mean_ENVHEAT = mean(ENVHEAT, na.rm = TRUE),
    SD_ENVHEAT = sd(ENVHEAT, na.rm = TRUE),
    Mean_WATHEAT = mean(WATHEAT, na.rm = TRUE),
    SD_WATHEAT = sd(WATHEAT, na.rm = TRUE),
    n = n()
  )
mean_summary



plot_cold_pNN50 <- paired_cold_pNN50 |>
  pivot_longer(
    cols = c(ENVCOLD, WATCOLD),
    names_to = "Treatment",
    values_to = "pNN50"
  ) |>
  mutate(
    Block = "Low",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_heat_pNN50 <- paired_heat_pNN50 |>
  pivot_longer(
    cols = c(ENVHEAT, WATHEAT),
    names_to = "Treatment",
    values_to = "pNN50"
  ) |>
  mutate(
    Block = "High",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_pNN50 <- bind_rows(plot_cold_pNN50, plot_heat_pNN50)
plot_pNN50$Treatment <- factor(
  plot_pNN50$Treatment,
  levels = c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
)

ggplot(
  data = plot_pNN50,
  aes(x = Treatment, y = pNN50, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5, color="black") +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4, color= "black") +
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
#SDNN
paired_cold_SDNN <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, SDNN) |>
  pivot_wider(
    names_from = Treatment,
    values_from = SDNN
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold_SDNN$WATCOLD - paired_cold_SDNN$ENVCOLD
shapiro.test(diff_cold)

t.test(
  paired_cold_SDNN$ENVCOLD,
  paired_cold_SDNN$WATCOLD,
  paired = TRUE
)


paired_heat_SDNN <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, SDNN) |>
  pivot_wider(
    names_from = Treatment,
    values_from = SDNN
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat_SDNN$WATHEAT - paired_heat_SDNN$ENVHEAT
shapiro.test(diff_heat)

wilcox.test(
  paired_heat_SDNN$ENVHEAT,
  paired_heat_SDNN$WATHEAT,
  paired = TRUE,
)


mean_summary <- paired_cold_SDNN |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    Mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary


median_summary <- paired_heat_SDNN |>
  summarise(
    Median_ENVHEAT = median(ENVHEAT, na.rm = TRUE),
    IQR_ENVHEAT = IQR(ENVHEAT, na.rm = TRUE),
    Median_WATHEAT = median(WATHEAT, na.rm = TRUE),
    IQR_WATHEAT = IQR(WATHEAT, na.rm = TRUE),
    n = n()
  )
median_summary



plot_cold_SDNN <- paired_cold_SDNN |>
  pivot_longer(
    cols = c(ENVCOLD, WATCOLD),
    names_to = "Treatment",
    values_to = "SDNN"
  ) |>
  mutate(
    Block = "Low",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_heat_SDNN <- paired_heat_SDNN |>
  pivot_longer(
    cols = c(ENVHEAT, WATHEAT),
    names_to = "Treatment",
    values_to = "SDNN"
  ) |>
  mutate(
    Block = "High",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_SDNN <- bind_rows(plot_cold_SDNN, plot_heat_SDNN)
plot_SDNN$Treatment <- factor(
  plot_SDNN$Treatment,
  levels = c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
)

ggplot(
  data = plot_SDNN,
  aes(x = Treatment, y = SDNN, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5, color= "black") +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4, color= "black") +
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

#RMSSD
paired_cold_RMSSD <- HRV_metrics_baseline_cold |>
  select(`Mass_(g)`, Treatment, RMSSD) |>
  pivot_wider(
    names_from = Treatment,
    values_from = RMSSD
  ) |>
  drop_na(ENVCOLD, WATCOLD)


diff_cold <- paired_cold_RMSSD$WATCOLD - paired_cold_RMSSD$ENVCOLD
shapiro.test(diff_cold)


t.test(
  paired_cold_RMSSD$ENVCOLD,
  paired_cold_RMSSD$WATCOLD,
  paired = TRUE
)


paired_heat_RMSSD <- HRV_metrics_baseline_heat |>
  select(`Mass_(g)`, Treatment, RMSSD) |>
  pivot_wider(
    names_from = Treatment,
    values_from = RMSSD
  ) |>
  drop_na(ENVHEAT, WATHEAT)

diff_heat <- paired_heat_RMSSD$WATHEAT - paired_heat_RMSSD$ENVHEAT
shapiro.test(diff_heat)

wilcox.test(
  paired_heat_RMSSD$ENVHEAT,
  paired_heat_RMSSD$WATHEAT,
  paired = TRUE
)


mean_summary <- paired_cold_RMSSD |>
  summarise(
    Mean_ENVCOLD = mean(ENVCOLD, na.rm = TRUE),
    SD_ENVCOLD = sd(ENVCOLD, na.rm = TRUE),
    Mean_WATCOLD = mean(WATCOLD, na.rm = TRUE),
    SD_WATCOLD = sd(WATCOLD, na.rm = TRUE),
    n = n()
  )

mean_summary


median_summary <- paired_heat_RMSSD |>
  summarise(
    Median_ENVHEAT = median(ENVHEAT, na.rm = TRUE),
    IQR_ENVHEAT = IQR(ENVHEAT, na.rm = TRUE),
    Median_WATHEAT = median(WATHEAT, na.rm = TRUE),
    IQR_WATHEAT = IQR(WATHEAT, na.rm = TRUE),
    n = n()
  )
median_summary



plot_cold_RMSSD <- paired_cold_RMSSD |>
  pivot_longer(
    cols = c(ENVCOLD, WATCOLD),
    names_to = "Treatment",
    values_to = "RMSSD"
  ) |>
  mutate(
    Block = "Low",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_heat_RMSSD <- paired_heat_RMSSD |>
  pivot_longer(
    cols = c(ENVHEAT, WATHEAT),
    names_to = "Treatment",
    values_to = "RMSSD"
  ) |>
  mutate(
    Block = "High",
    Pair_ID = as.factor(`Mass_(g)`)
  )

plot_RMSSD <- bind_rows(plot_cold_RMSSD, plot_heat_RMSSD)
plot_RMSSD$Treatment <- factor(
  plot_RMSSD$Treatment,
  levels = c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
)
ggplot(
  data = plot_RMSSD,
  aes(x = Treatment, y = RMSSD, color = Block)
) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8, size = 0.5, color="black") +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.4, color= "black") +
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


