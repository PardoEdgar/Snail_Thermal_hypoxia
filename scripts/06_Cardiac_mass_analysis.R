###############################################################################
# Mass effect in HR and HRV after water immersion across different temperatures 
# Author: Edgar Alejandro Pardo-Sarmiento
###############################################################################
library(tidyverse)
library(ggpubr)
library(broom)
library(readxl)
library(car)

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
HRV_metrics_total$ID <- as.character(HRV_metrics_total$ID)

combined_data <- left_join(
  HRV_metrics_total,
  body_mass,
  by = c("ID", "Treatment", "Block")
) %>%
  dplyr::filter(
    Temperature %in% c(1.5, 8, 15, 21, 29, 36)
  ) %>%
  dplyr::mutate(
    Temperature = factor(
      Temperature,
      levels = c(1.5, 8, 15, 21, 29, 36)
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


combined_data$Mass_centered <- combined_data$`Mass_(g)` -
  mean(combined_data$`Mass_(g)`, na.rm = TRUE)
model_lm_centered <- lm(
  MeanHR ~ Temperature * Mass_centered,
  data = combined_data
)
summary(model_lm_centered)
car::Anova(model_lm_centered, type = 3)


R2_by_temp_HR <- combined_data %>%
  group_by(Temperature) %>%
  summarise(
    model = list(lm(MeanHR ~ `Mass_(g)`, data = cur_data()))
  ) %>%
  mutate(stats = lapply(model, glance)) %>%
  tidyr::unnest_wider(stats) %>%
  dplyr::select(Temperature, adj.r.squared, p.value)
R2_by_temp_HR

slopes_by_temp_HR <- combined_data %>%
  group_by(Temperature) %>%
  summarise(
    model = list(lm(MeanHR ~ `Mass_(g)`, data = cur_data()))
  ) %>%
  mutate(coefs = lapply(model, tidy)) %>%
  unnest(coefs) %>%
  dplyr::select(
    Temperature,
    term,
    slope = estimate,
  )
slopes_by_temp_HR


#CV

mass_cv <- ggplot(
  combined_data,
  aes(x = `Mass_(g)`, y = CV, color = Temperature)
) +
  geom_point(size = 0.9, alpha = 0.4) +
  geom_smooth(
    method = "lm",
    se = T,
    linewidth = 1.3,
    alpha = 0.10,
    fullrange = T
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
  labs(
    x = "Mass (g)",
    y = "Heart Rate Variability (CV%)"
  ) +
  coord_cartesian(ylim = c(0, 40)) +
  scale_x_continuous(limits = c(0, 8), expand = c(0, 0)) +
  theme_classic2()

model_lm_centered <- lm(CV ~ Temperature * Mass_centered, data = combined_data)
summary(model_lm_centered)
car::Anova(model_lm_centered, type = 3)

R2_by_temp_CV <- combined_data %>%
  group_by(Temperature) %>%
  summarise(
    model = list(lm(CV ~ `Mass_(g)`, data = cur_data()))
  ) %>%
  mutate(stats = lapply(model, glance)) %>%
  tidyr::unnest_wider(stats) %>%
  dplyr::select(Temperature, adj.r.squared, p.value)
R2_by_temp_CV


slopes_by_temp_CV <- combined_data %>%
  group_by(Temperature) %>%
  summarise(
    model = list(lm(CV ~ `Mass_(g)`, data = cur_data()))
  ) %>%
  mutate(coefs = lapply(model, tidy)) %>%
  unnest(coefs) %>%
  dplyr::select(
    Temperature,
    term,
    slope = estimate,
  )
slopes_by_temp_CV


