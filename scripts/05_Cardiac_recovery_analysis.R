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
Cardiac_time_windows <- all_valleys %>%
  group_by(ID, Treatment) %>%
  arrange(Time) %>%
  mutate(
    delta_t = Time - lag(Time),
    HR = 60 / delta_t
  ) %>%
  dplyr::filter(!is.na(HR)) %>%
  mutate(period = ifelse(Time <= 30, "First_30s", "Last_30s")) %>%
  group_by(ID, Treatment, period) %>%
  summarise(
    MeanHR = mean(HR, na.rm = TRUE),
    CV = (sd(delta_t) / mean(delta_t)) * 100,
    .groups = "drop"
  )

Cardiac_time_windows <- Cardiac_time_windows %>%
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
  Cardiac_time_windows,
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
  scale_x_continuous(breaks = sort(unique(Cardiac_time_windows$Temperature))) +
  theme_classic2()


by(
  Cardiac_time_windows,
  Cardiac_time_windows$period,
  function(Cardiac_time_windows) {
    summary(lm(MeanHR ~ log(Temperature), data = Cardiac_time_windows))
  }
)

Cardiac_time_windows %>% group_by(Treatment, period) %>% shapiro_test(MeanHR)

levene_test(MeanHR ~ Treatment, data = Cardiac_time_windows)
hist(Cardiac_time_windows$MeanHR)
qqnorm(Cardiac_time_windows$MeanHR)
qqline(Cardiac_time_windows$MeanHR)

stats_results <- Cardiac_time_windows %>%
  group_by(Treatment) %>% dplyr::filter(
    Treatment %in% c("1C", "8C", "15C", "29C")
  ) %>% 
  t_test(MeanHR ~ period, paired = TRUE)
stats_results


stats_results <- Cardiac_time_windows %>%
  group_by(Treatment) %>% dplyr::filter(
    Treatment %in% c("WATHEAT", "36C")
  ) %>% 
  wilcox_test(MeanHR ~ period, paired = TRUE)
stats_results

Cardiac_time_windows |>
  group_by(Treatment, period) %>% dplyr::filter(
    Treatment %in% c("1C", "8C", "15C", "29C")
  ) %>% 
  summarise(Mean_HR = mean(MeanHR), sd_HR = sd(MeanHR))


Cardiac_time_windows |>
  group_by(Treatment, period) %>% dplyr::filter(
    Treatment %in% c("WATHEAT", "36C")
  ) %>% 
  summarise(Median_HR = median(MeanHR), IQR_HR = IQR(MeanHR))


CV_30S <- ggplot(
  Cardiac_time_windows,
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
  scale_x_continuous(breaks = sort(unique(Cardiac_time_windows$Temperature))) +
  theme_classic2()

by(
  Cardiac_time_windows,
  Cardiac_time_windows$period,
  function(Cardiac_time_windows) {
    summary(lm(
      CV ~ Temperature + I(Temperature^2),
      data = Cardiac_time_windows
    ))
  }
)


Cardiac_time_windows %>% group_by(Treatment, period) %>% shapiro_test(CV)
levene_test(CV ~ Treatment, data = Cardiac_time_windows)
hist(Cardiac_time_windows$CV)
qqnorm(Cardiac_time_windows$CV)
qqline(Cardiac_time_windows$CV)

stats_results <- Cardiac_time_windows %>%
  group_by(Treatment) %>%
  wilcox_test(CV ~ period, paired = TRUE)
stats_results


Cardiac_time_windows |>
  group_by(Treatment, period) |>
  summarise(Median_CV = median(CV), IQR_CV = IQR(CV))
