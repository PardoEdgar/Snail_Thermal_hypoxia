
library(dplyr)
all_valleys_complete_HEAT
all_valleys_complete_cold

HR_data_heat_30s <- all_valleys_complete_HEAT %>%
  group_by(ID, Treatment) %>%
  arrange(time) %>%
  mutate(
    delta_t = time - lag(time),
    HR = 60 / delta_t  # HR instantánea entre latidos consecutivos
  ) %>%
  dplyr::filter(!is.na(HR)) %>%
  mutate(period = ifelse(time <= 30, "First_30s", "Last_30s")) %>%
  group_by(ID, Treatment, period) %>%
  summarise(mean_HR = mean(HR, na.rm = TRUE), .groups = "drop")

all_valleys_complete_cold %>%
  group_by(ID, Treatment) %>%
  arrange(time) %>%
  mutate(delta_t = time - lag(time)) %>%
  dplyr::filter(delta_t == 0)

HR_data_cold_30s <- all_valleys_complete_cold %>%
  group_by(ID, Treatment) %>%
  arrange(time) %>%
  mutate(
    delta_t = time - lag(time),
    HR = 60 / delta_t  # HR instantánea entre latidos consecutivos
  ) %>%
  dplyr::filter(!is.na(HR)) %>%
  mutate(period = ifelse(time <= 30, "First_30s", "Last_30s")) %>%
  group_by(ID, Treatment, period) %>%
  summarise(mean_HR = mean(HR, na.rm = TRUE), .groups = "drop")
view(all_valleys_complete_cold)
view(HR_data_cold_30s)
view(HR_data_heat_30s)
Heart_data_30s <- dplyr::bind_rows(
  HR_data_cold_30s %>% dplyr::mutate(condition = "cold"),
  HR_data_heat_30s %>% dplyr::mutate(condition = "heat")
)
view(Heart_data_30s)
##write_csv(Heart_data_30s, "C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_Pressure_HRV/Data/data_extraction/Heart_data_1st_2nd_30s.csv")

view(Heart_data_30s_F)
Heart_data_30s_F <- Heart_data_30s %>% dplyr::filter(Treatment %in% c("1C","8C","15C","WATHEAT","29C","36C")) %>% 
  dplyr::mutate(Treatment = factor(Treatment,
levels=c("1C","8C","15C","WATHEAT","29C","36C"))) %>% dplyr::mutate(
    Temperature = case_when(
      Treatment == "WATHEAT" ~ 21,
      TRUE ~ parse_number((as.character(Treatment)))
    )
  )

MeanHR_30 <- ggplot(Heart_data_30s_F, aes(x=Temperature, y = mean_HR, fill = period)) +
  geom_boxplot(aes(group = interaction(Temperature, period), linetype = period),
    fill = "white",
    color = "black",
    alpha=0.3, size=0.4, width = 4,
    outlier.shape = NA,
    position = position_dodge(width = 5)
  ) + geom_point(
    position = position_jitterdodge(
      jitter.width = 1.5,
      dodge.width = 5), size = 0.3, alpha=0.4) + 
  geom_smooth(aes(linetype = period),
              se = TRUE,
              alpha=0.3, color = "black", fill = "grey70",size=0.8, fullrange=T)+ stat_summary(
    aes(group = period),
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 5)
  )+
  labs(x = "Temperature (°C)", y = "Heart rate (BPM)") + coord_cartesian(ylim = c(15, 80)) + scale_x_continuous(breaks= sort(unique(Heart_data_30s_F$Temperature))) +
  theme_classic2()

by(Heart_data_30s_F,
   Heart_data_30s_F$period,
   function(Heart_data_30s_F) summary(lm(mean_HR ~ log(Temperature), data = Heart_data_30s_F)))

shapiro_test(Heart_data_30s_F, mean_HR)
Heart_data_30s_F %>% group_by(Treatment, period) %>% shapiro_test(mean_HR)
leveneTest(MeanHR_mov ~ Treatment, data = Heart_data_30s_F)
hist(Heart_data_30s_F$MeanHR_mov)
qqnorm(Heart_data_30s_F$MeanHR_mov); qqline(Heart_data_30s_F$MeanHR_mov)


##No normalidad
stats_results <- Heart_data_30s_F %>%
  group_by(Treatment) %>%
  wilcox_test(mean_HR ~ period, paired = TRUE) 


all_vall_complete <-read_csv("C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_Pressure_HRV/Data/data_extraction/all_vall_complete.csv")
view(all_vall_complete)
CV_data_30s <- all_vall_complete %>%
  arrange(ID, Treatment, time) %>%
  group_by(ID, Treatment) %>%
  mutate(
    period = ifelse(time <= 30, "First_30s", "Last_30s")
  ) %>%
  group_by(ID, Treatment, period) %>%
  mutate(
    delta_t = (time * 1000) - lag(time * 1000), 
  ) %>%
  dplyr::filter(!is.na(delta_t)) %>%
  summarise(
    CV = (sd(delta_t)/mean(delta_t))*100,
    .groups = "drop"
  )
##write_csv(CV_data_30s, "C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_Pressure_HRV/Data/data_extraction/CV_data_30s_temp.csv")

CV_data_30s_temp.csv <-read_csv("C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_Pressure_HRV/Data/data_extraction/CV_data_30s_temp.csv")

CV_data_30s_temp <- CV_data_30s %>%
  dplyr::filter(Treatment %in% c("1C","8C","15C","29C","36C","WATHEAT")) %>%
  mutate(
    Temperature = case_when(
      Treatment == "WATHEAT" ~ 21,
      TRUE ~ parse_number(Treatment)
    )
  )

CV_30S <- ggplot(CV_data_30s_temp, aes(x=Temperature, y = CV, fill = period)) +
  geom_boxplot(aes(group = interaction(Temperature, period), linetype = period),
    fill = "white",
    color = "black",
     alpha=0.3, size=0.4, width = 4,
    outlier.shape = NA,
    position = position_dodge(width = 5)
  ) + geom_point(
    position = position_jitterdodge(
      jitter.width = 1.5,
      dodge.width = 5), size = 0.3, alpha=0.4) + 
  geom_smooth(aes(linetype = period),
                            se = TRUE,
              alpha=0.3, color = "black", fill = "grey70",size=0.8, fullrange=T)+ stat_summary(
    aes(group = period),
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 5)
  ) +
  labs(x = "Temperature (°C)", y = "Heart rate variability (CV%)") + coord_cartesian(ylim = c(0, 40)) + scale_x_continuous(breaks = sort(unique(CV_data_30s_temp$Temperature)))+
  theme_classic2()

by(CV_data_30s_temp,
   CV_data_30s_temp$period,
   function(CV_data_30s_temp) summary(lm(CV ~  Temperature + I(Temperature^2), data = CV_data_30s_temp)))


shapiro_test(CV_data_30s_temp, CV)
CV_data_30s_temp %>% group_by(Treatment, period) %>% shapiro_test(CV)
leveneTest(MeanHR_mov ~ Treatment, data = CV_data_30s_temp)
hist(CV_data_30s_temp$MeanHR_mov)
qqnorm(CV_data_30s_temp$MeanHR_mov); qqline(CV_data_30s_temp$MeanHR_mov)


stats_results <- CV_data_30s_temp %>% 
  group_by(Treatment) %>%
  wilcox_test(CV ~ period, paired = TRUE)


file <- "C:/Users/jandr/OneDrive - Universidad del rosario/Heart_rate_speed_HRV/Escritura/Supporting information/S1_File_Pardo_Sarmiento_et_al.xlsx"
file_1 <- read_xlsx(file, sheet = "T_RH")

mean(file_1$`Temperature_°C`)
sd(file_1$`Temperature_°C`)

mean(file_1$`Relative_humidity_%`)
sd(file_1$`Relative_humidity_%`)


