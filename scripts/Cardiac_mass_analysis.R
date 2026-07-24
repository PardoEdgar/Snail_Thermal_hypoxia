

#Efecto de la masa en el cambio de HR y HRV por temperatura
library(lme4)
library(lmerTest) 
library(ggpubr)
library(broom)
library(performance)
library(emmeans)
library(ggpubr)

setwd("C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_Pressure_HRV/Data/body_size_data")
list.files()
body_mass <- read_xlsx("body_mass.xlsx")
body_mass$Snail <- as.character(body_mass$Snail)
HRV_metrics_total$Snail <- as.character(HRV_metrics_total$Snail)
combined_data <- left_join(HRV_metrics_total, body_mass, by = c("Snail", "Treatment","Block"))
combined_data_filtered <- combined_data %>% dplyr::filter(Treatment %in% c("1C","8C","15C","WATHEAT","29C","36C")) %>% dplyr::mutate(Treatment = factor(Treatment, levels=c("1C","8C","15C","WATHEAT","29C","36C")))
  

mass_HR <- ggplot(combined_data_filtered, aes(x = `Mass(g)`, y = MeanHR_mov, color = Treatment)) +
  geom_point(size=0.8,alpha = 0.4) +
  geom_smooth(method = "lm", se = T, linewidth = 1, alpha=0.10, fullrange=T) + labs(
    x = "Mass (g)",
    y = "Heart rate (BPM)"
  )  + scale_color_manual(values= c("darkblue","lightblue", "yellow","darkorange","red","darkred")) + theme(
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + coord_cartesian(ylim = c(0, 80))+scale_x_continuous(limits = c(0, 8), expand = c(0, 0)) + theme_classic2()

model_lmm <- lmer(MeanHR_mov ~ Treatment * `Mass(g)` + (1 | Snail), data = combined_data_filtered)
summary(model_lmm)


R2_by_temp_HR <- combined_data_filtered %>%
  group_by(Treatment) %>%
  summarise(
    model = list(lm(MeanHR_mov ~ `Mass(g)`, data = cur_data()))
  ) %>%
  mutate(stats = lapply(model, glance)) %>%
  tidyr::unnest_wider(stats) %>%
  dplyr::select(Treatment, r.squared, adj.r.squared, p.value)
R2_by_temp_HR

slopes_by_temp_HR <- combined_data_filtered %>%
  group_by(Treatment) %>%
  summarise(
    model = list(lm(MeanHR_mov ~ `Mass(g)`, data = cur_data()))
  ) %>%
  mutate(coefs = lapply(model, tidy)) %>%
  unnest(coefs) %>%
  dplyr::filter(term == "`Mass(g)`") %>%
  dplyr::select(Treatment, slope = estimate, std_error = std.error, p_value = p.value)

slopes_by_temp_HR


slopes_plot_HR <- slopes_by_temp_HR %>%
  mutate(
    Temp_numeric = case_when(
      Treatment == "1C" ~ 1,
      Treatment == "8C" ~ 8,
      Treatment == "15C" ~ 15,
      Treatment == "29C" ~ 29,
      Treatment == "36C" ~ 36,
      Treatment == "WATHEAT" ~ 21
    )
  ) %>%
  dplyr::filter(!is.na(Temp_numeric))


ggplot(slopes_plot_HR, aes(x = Temp_numeric, y = slope)) +
  geom_point(aes(color = p_value < 0.05), size = 3)+
  geom_line() +
  geom_errorbar(aes(ymin = slope - std_error,
                    ymax = slope + std_error),
                width = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Temperature (°C)",
    y = "Slope (Mass effect on Heart rate)"
  ) +
  theme_classic()

mass_cv <- ggplot(combined_data_filtered,
       aes(x = `Mass(g)`, y = CV, color = Treatment)) +
  geom_point(size = 0.9, alpha = 0.4) +
   geom_smooth(method = "lm", se = T, linewidth = 1.3, alpha=0.10, fullrange=T)+
  scale_color_manual(values = c("darkblue","lightblue","yellow","darkorange","red","darkred"))+
  labs(
    x = "Mass (g)",
    y = "Heart Rate Variability (CV%)"
  ) + coord_cartesian(ylim = c(0, 40))+scale_x_continuous(limits = c(0, 8), expand = c(0, 0))+
   theme_classic2()


model_lmm <- lmer(CV ~ Treatment * `Mass(g)` + (1 | Snail), data = combined_data_filtered)
summary(model_lmm)


R2_by_temp_CV <- combined_data_filtered %>%
  group_by(Treatment) %>%
  summarise(
    model = list(lm(CV ~ `Mass(g)`, data = cur_data()))
  ) %>%
  mutate(stats = lapply(model, glance)) %>%
  tidyr::unnest_wider(stats) %>%
  dplyr::select(Treatment, r.squared, adj.r.squared, p.value)
R2_by_temp_CV

slopes_by_temp_CV <- combined_data_filtered %>%
  group_by(Treatment) %>%
  summarise(
    model = list(lm(CV ~ `Mass(g)`, data = cur_data()))
  ) %>%
  mutate(coefs = lapply(model, tidy)) %>%
  unnest(coefs) %>%
  dplyr::filter(term == "`Mass(g)`") %>%
  dplyr::select(Treatment, slope = estimate, std_error = std.error, p_value = p.value)
slopes_by_temp_CV


slopes_plot_CV <- slopes_by_temp_CV %>%
  mutate(
    Temp_numeric = case_when(
      Treatment == "1C" ~ 1,
      Treatment == "8C" ~ 8,
      Treatment == "15C" ~ 15,
      Treatment == "29C" ~ 29,
      Treatment == "36C" ~ 36,
      Treatment == "WATHEAT" ~ 21
    )
  ) %>%
  dplyr::filter(!is.na(Temp_numeric))


ggplot(slopes_plot_CV, aes(x = Temp_numeric, y = slope)) +
  geom_point(aes(color = p_value < 0.08), size = 3)+
  geom_line() +
  geom_errorbar(aes(ymin = slope - std_error,
                    ymax = slope + std_error),
                width = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Temperature (°C)",
    y = "Slope (Mass effect on CV)"
  ) +
  theme_classic()
