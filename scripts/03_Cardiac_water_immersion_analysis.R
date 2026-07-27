library(tidyverse)
library(readxl)
library(rstatix)
library(ggpubr)

#Results: HR & HRV baseline
HRV_metrics_total <- readxl::read_xlsx(
  "S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
  sheet = "HRV_metrics_total"
)
HRV_metrics_total <- HRV_metrics_total %>%
  mutate(across(
    c(Temperature, MeanHR, SDNN, RMSSD, pNN50, NNi, pNN100, CV),
    as.numeric
  ))


HRV_metrics_baseline <- HRV_metrics_total %>%
  dplyr::filter(
    Treatment %in% c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
  ) %>%
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")
    )
  )


HRV_metrics_baseline_cold <- HRV_metrics_total %>%
  dplyr::filter(Treatment %in% c("ENVCOLD", "WATCOLD")) %>%
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("ENVCOLD", "WATCOLD")
    )
  )

HRV_metrics_baseline_heat <- HRV_metrics_total %>%
  dplyr::filter(Treatment %in% c("ENVHEAT", "WATHEAT")) %>%
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("ENVHEAT", "WATHEAT")
    )
  )

n <- HRV_metrics_baseline %>% group_by(Treatment) %>% shapiro_test(MeanHR)
n
levene_test(HRV_metrics_baseline_heat, MeanHR ~ Treatment)
t_test(HRV_metrics_baseline_heat, MeanHR ~ Treatment)
levene_test(HRV_metrics_baseline_cold, MeanHR ~ Treatment)
t_test(HRV_metrics_baseline_cold, MeanHR ~ Treatment)

mean_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(mean_HR = mean(MeanHR), sd_HR = sd(MeanHR))
mean_baseline

shapiro_test(HRV_metrics_baseline_heat$CV)
n <- HRV_metrics_baseline %>% group_by(Treatment) %>% shapiro_test(CV)
n 
levene_test(HRV_metrics_baseline_cold, CV ~ Treatment)
wilcox_test(HRV_metrics_baseline_cold, CV ~ Treatment)
levene_test(HRV_metrics_baseline_heat, CV ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, CV ~ Treatment) #CV no normal para esta comparación


median_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(Median_CV = median(CV), IQR_CV = IQR(CV))
median_baseline

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
  aes(x = Treatment, y = pNN100, color = Block)
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
  labs(x = "Baseline", y = "pNN100 (%)") +
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


shapiro_test(HRV_metrics_baseline_heat$NNi)
n <- HRV_metrics_baseline %>% group_by(Treatment) %>% shapiro_test(NNi)
n

shapiro_test(HRV_metrics_baseline_heat$pNN50)
n <- HRV_metrics_baseline %>% group_by(Treatment) %>% shapiro_test(pNN50)
n

shapiro_test(HRV_metrics_baseline_heat$pNN100)
n <- HRV_metrics_baseline %>% group_by(Treatment) %>% shapiro_test(pNN100)
n

shapiro_test(HRV_metrics_baseline_heat$SDNN)
n <- HRV_metrics_baseline %>% group_by(Treatment) %>% shapiro_test(SDNN)
n

shapiro_test(HRV_metrics_baseline_heat$RMSSD)
n <- HRV_metrics_baseline %>% group_by(Treatment) %>% shapiro_test(RMSSD)
n

mean_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(mean_NN = mean(NNi), sd_NN = sd(NNi))
mean_baseline

median_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(Median_pNN50 = median(pNN50), IQR_pNN50 = IQR(pNN50))
median_baseline

median_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(Median_pNN100 = median(pNN100), IQR_pNN100 = IQR(pNN100))
median_baseline

median_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(Median_SDNN = median(SDNN), IQR_SDNN = IQR(SDNN))
median_baseline


median_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(Median_RMSSD = median(RMSSD), IQR_RMSSD = IQR(RMSSD))
median_baseline

t_test(HRV_metrics_baseline_cold, NNi ~ Treatment)
t_test(HRV_metrics_baseline_heat, NNi ~ Treatment)

wilcox_test(HRV_metrics_baseline_cold, NNi ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, NNi ~ Treatment)


wilcox_test(HRV_metrics_baseline_cold, pNN50 ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, pNN50 ~ Treatment)

wilcox_test(HRV_metrics_baseline_cold, pNN100 ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, pNN100 ~ Treatment)


wilcox_test(HRV_metrics_baseline_cold, SDNN ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, SDNN ~ Treatment)

wilcox_test(HRV_metrics_baseline_cold, RMSSD ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, RMSSD ~ Treatment)
