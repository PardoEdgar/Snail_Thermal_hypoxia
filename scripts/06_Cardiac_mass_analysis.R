###############################################################################
# Mass effect in HR and HRV across different temperatures
# Author: Edgar Alejandro Pardo-Sarmiento
###############################################################################
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggpubr)
library(broom)
library(performance)
library(emmeans)
library(ggpubr)
library(readxl)

body_mass <- readxl::read_xlsx(
  "C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_JP_HRV_data/Data/data_extraction/S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
  sheet = "Body_mass"
)
body_mass$ID <- as.character(body_mass$ID)
HRV_metrics_total <- readxl::read_xlsx(
  "C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_JP_HRV_data/Data/data_extraction/S1_File_Supplementary_data_Pardo_Sarmiento.xlsx",
  sheet = "HRV_metrics_total"
)
View(combined_data)
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
HRV_metrics_total$ID <- as.character(HRV_metrics_total$ID)

combined_data <- left_join(
  HRV_metrics_total,
  body_mass,
  by = c("ID", "Treatment", "Block")
) %>%
  dplyr::filter(
    Temperature %in% c(1, 8, 15, 21, 29, 36)
  ) %>%
  dplyr::mutate(
    Temperature = factor(
      Temperature,
      levels = c(1, 8, 15, 21, 29, 36)
    )
  )
combined_data$Temperature <- factor(combined_data$Temperature)

mass_HR <- ggplot(
  combined_data,
  aes(x = `Mass_(g)`, y = MeanHR, color = Temperature)
) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(
    method = "lm",
    se = T,
    linewidth = 1,
    alpha = 0.10,
    fullrange = T
  ) +
  labs(
    x = "Mass (g)",
    y = "Heart rate (BPM)"
  ) +
  scale_color_manual(
    values = c(
      "darkblue",
      "lightblue",
      "yellow",
      "darkorange",
      "red",
      "darkred"
    )
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(0, 80)) +
  scale_x_continuous(limits = c(0, 8), expand = c(0, 0)) +
  theme_classic2()

model_lmm <- lmer(
  MeanHR ~ Temperature * `Mass_(g)` + (1 | ID),
  data = combined_data
)
summary(model_lmm)


R2_by_temp_HR <- combined_data %>%
  group_by(Temperature) %>%
  summarise(
    model = list(lm(MeanHR ~ `Mass_(g)`, data = cur_data()))
  ) %>%
  mutate(stats = lapply(model, glance)) %>%
  tidyr::unnest_wider(stats) %>%
  dplyr::select(Temperature, r.squared, adj.r.squared, p.value)
R2_by_temp_HR

slopes_by_temp_HR <- combined_data %>%
  group_by(Temperature) %>%
  summarise(
    model = list(lm(MeanHR ~ `Mass_(g)`, data = cur_data()))
  ) %>%
  mutate(coefs = lapply(model, tidy)) %>%
  unnest(coefs) %>%
  dplyr::filter(term == "`Mass_(g)`") %>%
  dplyr::select(
    Temperature,
    slope = estimate,
    std_error = std.error,
    p_value = p.value
  )
slopes_by_temp_HR


#CV
model_lmm <- lmer(
  CV ~ Temperature * `Mass_(g)` + (1 | ID),
  data = combined_data
)
summary(model_lmm)


R2_by_temp_CV <- combined_data %>%
  group_by(Temperature) %>%
  summarise(
    model = list(lm(CV ~ `Mass_(g)`, data = cur_data()))
  ) %>%
  mutate(stats = lapply(model, glance)) %>%
  tidyr::unnest_wider(stats) %>%
  dplyr::select(Temperature, r.squared, adj.r.squared, p.value)
R2_by_temp_CV

slopes_by_temp_CV <- combined_data %>%
  group_by(Temperature) %>%
  summarise(
    model = list(lm(CV ~ `Mass_(g)`, data = cur_data()))
  ) %>%
  mutate(coefs = lapply(model, tidy)) %>%
  unnest(coefs) %>%
  dplyr::filter(term == "`Mass_(g)`") %>%
  dplyr::select(
    Temperature,
    slope = estimate,
    std_error = std.error,
    p_value = p.value
  )
slopes_by_temp_CV
