###############################################################################
# Cardiac parameters analysis (Comparing responses across different temperatures)
# Author: Edgar Alejandro Pardo-Sarmiento
###############################################################################

library(tidyverse)
library(rstatix)
library(ggpubr)
library(readxl)
library(FSA)

HRV_metrics_total <- readxl::read_xlsx(
  "C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_JP_HRV_data/Data/data_extraction/S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
  sheet = "HRV_metrics_total"
)

HRV_metrics_total <- HRV_metrics_total %>%
  mutate(across(
    c(
      "Temperature",
      "MeanHR",
      "SDNN",
      "RMSSD",
      "pNN50",
      "NNi",
      "pNN100",
      "CV"
    ),
    as.numeric
  ))

HRV_metrics_total_tem <- HRV_metrics_total %>%
  dplyr::filter(
    Treatment %in% c("1C", "8C", "15C", "WATHEAT", "29C", "36C")
  ) %>%
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("1C", "8C", "15C", "WATHEAT", "29C", "36C")
    )
  )

#HR

HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(MeanHR)
levene_test(MeanHR ~ Treatment, data = HRV_metrics_total_tem)
hist(log(HRV_metrics_total_tem$MeanHR))
qqnorm(HRV_metrics_total_tem$MeanHR)
qqline(HRV_metrics_total_tem$MeanHR)

anovaHR <- aov(MeanHR ~ Treatment, HRV_metrics_total_tem)
summary(anovaHR)
TukeyHSD(anovaHR)

#Heart rate variability

HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(CV)
levene_test(CV ~ Treatment, data = HRV_metrics_total_tem)
hist(log(HRV_metrics_total_tem$CV))
qqnorm(HRV_metrics_total_tem$CV)
qqline(HRV_metrics_total_tem$CV)


kruskal.test(CV ~ Treatment, data = HRV_metrics_total_tem)

dun_test_tem_HRV <- dunnTest(
  CV ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV

#Figures
HR_2 <- ggplot(HRV_metrics_total_tem, aes(x = Temperature, y = MeanHR)) +
  geom_boxplot(
    aes(group = Temperature),
    width = 1.5,
    outlier.shape = NA,
    alpha = 0.5,
    size = 0.4
  ) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) +
  geom_smooth(
    se = TRUE,
    color = "black",
    alpha = 0.3,
    size = 0.8,
    fullrange = T
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5
  ) +
  labs(x = "Temperature (°C)", y = "Heart rate (BPM)") +
  scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  ) +
  theme_classic2()

lineal_model <- lm(MeanHR ~ log(Temperature), data = HRV_metrics_total_tem)
summary(lineal_model)

#Heart rate variability

CV_2 <- ggplot(HRV_metrics_total_tem, aes(x = Temperature, y = CV)) +
  geom_boxplot(
    aes(group = Temperature),
    width = 1.5,
    outlier.shape = NA,
    alpha = 0.5,
    size = 0.4
  ) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) +
  geom_smooth(
    se = TRUE,
    color = "black",
    alpha = 0.3,
    size = 0.8,
    fullrange = T
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
  labs(
    x = "Temperature (°C)",
    y = "Heart rate variability (CV%)"
  ) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_x_continuous(limits = c(0, 50), expand = c(0, 0)) +
  scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  ) +
  theme_classic2()


lineal_model <- lm(
  CV ~ Temperature + I(Temperature^2),
  data = HRV_metrics_total_tem
)
summary(lineal_model)


#Time domain HRV metrics

ggplot(HRV_metrics_total_tem, aes(x = Temperature, y = NNi)) +
  geom_boxplot(
    aes(group = Temperature),
    width = 1.5,
    outlier.shape = NA,
    alpha = 0.5,
    size = 0.4
  ) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) +
  geom_smooth(
    se = TRUE,
    color = "black",
    alpha = 0.3,
    size = 0.8,
    fullrange = T
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5
  ) +
  labs(x = "Temperature (°C)", y = "NN intervals (ms)") +
  scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  ) +
  theme_classic2()

ggplot(HRV_metrics_total_tem, aes(x = Temperature, y = pNN50)) +
  geom_boxplot(
    aes(group = Temperature),
    width = 1.5,
    outlier.shape = NA,
    alpha = 0.5,
    size = 0.4
  ) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) +
  geom_smooth(
    se = TRUE,
    color = "black",
    alpha = 0.3,
    size = 0.8,
    fullrange = T
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5
  ) +
  labs(x = "Temperature (°C)", y = "pNN50 (%)") +
  scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  ) +
  theme_classic2()

ggplot(HRV_metrics_total_tem, aes(x = Temperature, y = SDNN)) +
  geom_boxplot(
    aes(group = Temperature),
    width = 1.5,
    outlier.shape = NA,
    alpha = 0.5,
    size = 0.4
  ) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) +
  geom_smooth(
    se = TRUE,
    color = "black",
    alpha = 0.3,
    size = 0.8,
    fullrange = T
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5
  ) +
  labs(x = "Temperature (°C)", y = "SDNN (ms)") +
  scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  ) +
  coord_cartesian(ylim = c(0, 1000)) +
  theme_classic2()

ggplot(HRV_metrics_total_tem, aes(x = Temperature, y = RMSSD)) +
  geom_boxplot(
    aes(group = Temperature),
    width = 1.5,
    outlier.shape = NA,
    alpha = 0.5,
    size = 0.4
  ) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) +
  geom_smooth(
    se = TRUE,
    color = "black",
    alpha = 0.3,
    size = 0.8,
    fullrange = T
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "darkred",
    size = 1.5
  ) +
  labs(x = "Temperature (°C)", y = "RMSSD (ms)") +
  scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  ) +
  coord_cartesian(ylim = c(0, 1000)) +
  theme_classic2()

kruskal.test(NNi ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_HRV <- dunnTest(
  NNi ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV


kruskal.test(pNN50 ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_HRV <- dunnTest(
  pNN50 ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV


kruskal.test(pNN100 ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_HRV <- dunnTest(
  pNN100 ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV


kruskal.test(SDNN ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_HRV <- dunnTest(
  SDNN ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV


kruskal.test(RMSSD ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_HRV <- dunnTest(
  RMSSD ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV


mean_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(mean_HR = mean(MeanHR), sd_HR = sd(MeanHR))
mean_baseline

median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_CV = median(CV), IQR_CV = IQR(CV))
median_baseline


HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(NNi)
HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(pNN50)
HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(pNN100)
HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(SDNN)
HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(RMSSD)


median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_NNi = median(NNi), IQR_NNi = IQR(NNi))
median_baseline

median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_pNN50 = median(pNN50), IQR_pNN50 = IQR(pNN50))
median_baseline


median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_pNN100 = median(pNN100), IQR_pNN100 = IQR(pNN100))
median_baseline

median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_SDNN = median(SDNN), IQR_SDNN = IQR(SDNN))
median_baseline

median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_RMSSD = median(RMSSD), IQR_RMSSD = IQR(RMSSD))
median_baseline
