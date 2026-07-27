###############################################################################
# Cardiac recovery analysis (Comparing two Time windows after water immersion across different temperatures)
# Author: Edgar Alejandro Pardo-Sarmiento
###############################################################################
library(dplyr)
library(tidyverse)
library(readxl)

all_valleys <- readxl::read_xlsx(
  "C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_JP_HRV_data/Data/data_extraction/S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
  sheet = "all_valleys"
)
all_valleys$Time <- as.numeric(all_valleys$Time)
Heart_data_30s <- all_valleys %>%
  group_by(ID, Treatment) %>%
  arrange(Time) %>%
  mutate(
    delta_t = Time - lag(Time),
    HR = 60 / delta_t 
  ) %>%
  dplyr::filter(!is.na(HR)) %>%
  mutate(period = ifelse(Time <= 30, "First_30s", "Last_30s")) %>%
  group_by(ID, Treatment, period) %>%
  summarise(MeanHR = mean(HR, na.rm = TRUE), .groups = "drop")

Heart_data_30s <- Heart_data_30s %>%
  dplyr::filter(
    Treatment %in% c("1C", "8C", "15C", "WATHEAT", "29C", "36C")
  ) %>%
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("1C", "8C", "15C", "WATHEAT", "29C", "36C")
    )
  ) %>%
  dplyr::mutate(
    Temperature = case_when(
      Treatment == "1C" ~ 1,
      Treatment == "8C" ~ 8,
      Treatment == "15C" ~ 15,
      Treatment == "29C" ~ 29,
      Treatment == "36C" ~ 36,
      Treatment == "WATHEAT" ~ 21,
    )
  )

MeanHR_30 <- ggplot(
  Heart_data_30s,
  aes(x = Temperature, y = MeanHR, fill = period)
) +
  geom_boxplot(
    aes(group = interaction(Temperature, period), linetype = period),
    fill = "white",
    color = "black",
    alpha = 0.3,
    size = 0.4,
    width = 4,
    outlier.shape = NA,
    position = position_dodge(width = 5)
  ) +
  geom_point(
    position = position_jitterdodge(
      jitter.width = 1.5,
      dodge.width = 5
    ),
    size = 0.3,
    alpha = 0.4
  ) +
  geom_smooth(
    aes(linetype = period),
    se = TRUE,
    alpha = 0.3,
    color = "black",
    fill = "grey70",
    size = 0.8,
    fullrange = T
  ) +
  stat_summary(
    aes(group = period),
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 5)
  ) +
  labs(x = "Temperature (°C)", y = "Heart rate (BPM)") +
  coord_cartesian(ylim = c(15, 80)) +
  scale_x_continuous(breaks = sort(unique(Heart_data_30s$Temperature))) +
  theme_classic2()

by(Heart_data_30s, Heart_data_30s$period, function(Heart_data_30s) {
  summary(lm(MeanHR ~ log(Temperature), data = Heart_data_30s))
})

Heart_data_30s %>% group_by(Treatment, period) %>% shapiro_test(MeanHR)

levene_test(MeanHR ~ Treatment, data = Heart_data_30s)
hist(Heart_data_30s$MeanHR)
qqnorm(Heart_data_30s$MeanHR)
qqline(Heart_data_30s$MeanHR)

stats_results <- Heart_data_30s %>%
  group_by(Treatment) %>%
  wilcox_test(MeanHR ~ period, paired = TRUE)
stats_results


CV_data_30s <- all_valleys %>%
  arrange(ID, Treatment, Time) %>%
  group_by(ID, Treatment) %>%
  mutate(
    period = ifelse(Time <= 30, "First_30s", "Last_30s")
  ) %>%
  group_by(ID, Treatment, period) %>%
  mutate(
    delta_t = (Time * 1000) - lag(Time * 1000),
  ) %>%
  dplyr::filter(!is.na(delta_t)) %>%
  summarise(
    CV = (sd(delta_t) / mean(delta_t)) * 100,
    .groups = "drop"
  )

CV_data_30s <- CV_data_30s %>%
  dplyr::filter(
    Treatment %in% c("1C", "8C", "15C", "29C", "36C", "WATHEAT")
  ) %>%
  dplyr::mutate(
    Temperature = case_when(
      Treatment == "1C" ~ 1,
      Treatment == "8C" ~ 8,
      Treatment == "15C" ~ 15,
      Treatment == "29C" ~ 29,
      Treatment == "36C" ~ 36,
      Treatment == "WATHEAT" ~ 21,
    )
  )

CV_30S <- ggplot(
  CV_data_30s,
  aes(x = Temperature, y = CV, fill = period)
) +
  geom_boxplot(
    aes(group = interaction(Temperature, period), linetype = period),
    fill = "white",
    color = "black",
    alpha = 0.3,
    size = 0.4,
    width = 4,
    outlier.shape = NA,
    position = position_dodge(width = 5)
  ) +
  geom_point(
    position = position_jitterdodge(
      jitter.width = 1.5,
      dodge.width = 5
    ),
    size = 0.3,
    alpha = 0.4
  ) +
  geom_smooth(
    aes(linetype = period),
    se = TRUE,
    alpha = 0.3,
    color = "black",
    fill = "grey70",
    size = 0.8,
    fullrange = T
  ) +
  stat_summary(
    aes(group = period),
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 5)
  ) +
  labs(x = "Temperature (°C)", y = "Heart rate variability (CV%)") +
  coord_cartesian(ylim = c(0, 40)) +
  scale_x_continuous(breaks = sort(unique(CV_data_30s$Temperature))) +
  theme_classic2()

by(CV_data_30s, CV_data_30s$period, function(CV_data_30s) {
  summary(lm(CV ~ Temperature + I(Temperature^2), data = CV_data_30s))
})


CV_data_30s %>% group_by(Treatment, period) %>% shapiro_test(CV)
levene_test(CV ~ Treatment, data = CV_data_30s)
hist(CV_data_30s$CV)
qqnorm(CV_data_30s$CV)
qqline(CV_data_30s$CV)

stats_results <- CV_data_30s %>%
  group_by(Treatment) %>%
  wilcox_test(CV ~ period, paired = TRUE)
stats_results
