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
  "C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_JP_HRV_data/Pardo_Sarmiento _et_al_2/S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
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

HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(MeanHR)
levene_test(
  HRV_metrics_total_tem,
  MeanHR ~ Treatment
)
qqnorm(HRV_metrics_total_tem$MeanHR)
qqline(HRV_metrics_total_tem$MeanHR)

anovaHR <- aov(MeanHR ~ Treatment, HRV_metrics_total_tem)
shapiro.test(residuals(anovaHR))

summary(anovaHR)
TukeyHSD(anovaHR)

#HRV

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

log_model <- lm(MeanHR ~ log(Temperature), data = HRV_metrics_total_tem)
summary(log_model)

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


quadratic_model <- lm(
  CV ~ Temperature + I(Temperature^2),
  data = HRV_metrics_total_tem
)
summary(quadratic_model)


#NN intervals and Time-domain HRV metrics

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


mean_summary <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(mean_HR = mean(MeanHR), sd_HR = sd(MeanHR))
mean_summary


median_summary <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_CV = median(CV), IQR_CV = IQR(CV))
median_summary


HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(NNi)
levene_test(
  HRV_metrics_total_tem,
  NNi ~ Treatment
)


HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(pNN50)
levene_test(
  HRV_metrics_total_tem,
  NNi ~ Treatment
)
anovaHR <- aov(pNN50 ~ Treatment, HRV_metrics_total_tem)
shapiro.test(residuals(anovaHR))

HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(SDNN)
HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(RMSSD)


kruskal.test(NNi ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_NNi <- dunnTest(
  NNi ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_NNi <- dun_test_tem_NNi$res
res_tem_NNi


kruskal.test(pNN50 ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_pNN50 <- dunnTest(
  pNN50 ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_pNN50 <- dun_test_tem_pNN50$res
res_tem_pNN50

kruskal.test(SDNN ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_SDNN <- dunnTest(
  SDNN ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_SDNN <- dun_test_tem_SDNN$res
res_tem_SDNN


kruskal.test(RMSSD ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_RMSSD <- dunnTest(
  RMSSD ~ Treatment,
  data = HRV_metrics_total_tem,
  method = "holm"
)
res_tem_RMSSD <- dun_test_tem_RMSSD$res
res_tem_RMSSD

median_summary <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_NNi = median(NNi), IQR_NNi = IQR(NNi))
median_summary

median_summary <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_pNN50 = median(pNN50), IQR_pNN50 = IQR(pNN50))
median_summary

median_summary <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_SDNN = median(SDNN), IQR_SDNN = IQR(SDNN))
median_summary

median_summary <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_RMSSD = median(RMSSD), IQR_RMSSD = IQR(RMSSD))
median_summary
View(median_summary)
