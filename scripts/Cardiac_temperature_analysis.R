
##Cambio en HR y HRV por temperatura
HRV_metrics_total_tem <- HRV_metrics_total %>% dplyr::filter(Treatment %in% c("1C","8C","15C","WATHEAT","29C","36C")) %>% 
  dplyr::mutate(Treatment = factor(Treatment,
      levels = c("1C","8C","15C","WATHEAT","29C","36C")))


##Estadística
  #Heart rate 

HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(MeanHR_mov)
leveneTest(MeanHR_mov ~ Treatment, data = HRV_metrics_total_tem)
hist(log(HRV_metrics_total_tem$MeanHR_mov))
qqnorm(HRV_metrics_total_tem$MeanHR_mov); qqline(data_sin_outliers$MeanHR_mov)

anovaHR <- aov(MeanHR_mov ~ Treatment,HRV_metrics_total_tem )
summary(anovaHR)
TukeyHSD(anovaHR)

  #Heart rate variability

HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(CV)
levene_test(CV ~ Treatment, data = HRV_metrics_total_tem)
hist(log(HRV_metrics_total_tem$CV))
qqnorm(HRV_metrics_total_tem$CV); qqline(data_sin_outliers$CV)


kruskal.test(CV ~ Treatment, data = HRV_metrics_total_tem)

dun_test_tem_HRV  <- dunnTest(CV ~ Treatment, data = HRV_metrics_total_tem, method = "holm")
dun_test_tem_HRV$res
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV
view(res_tem_HRV)

#Figuras
  #Heart rate 

HR_2 <- ggplot(HRV_metrics_total_tem,
      aes(x = Temperature, y = MeanHR_mov)) +
  geom_boxplot(aes(group = Temperature),
               width = 1.5,
               outlier.shape = NA,
               alpha = 0.5,
               size = 0.4) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) + 
  geom_smooth(se = TRUE,
              color = "black", alpha=0.3, size=0.8, fullrange=T) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 21,
               fill = "darkred",
               color = "darkred",
               size = 1.5)+
  labs(x = "Temperature (°C)",
       y = "Heart rate (BPM)") + scale_x_continuous(
    breaks = sort(unique(data_sin_outliers_tem$Temperature))
  )+
  theme_classic2()

modelo_lin <- lm(MeanHR_mov ~ log(Temperature),
                 data = HRV_metrics_total_tem)
summary(modelo_lin)
 
 #Heart rate variability

CV_2 <- ggplot(HRV_metrics_total_tem,
       aes(x = Temperature, y = CV)) + geom_boxplot(aes(group= Temperature),
               width = 1.5,
               outlier.shape = NA,
               alpha = 0.5,
               size = 0.4) +
   geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) + geom_smooth(
            se = TRUE,
            color = "black",alpha=0.3, size=0.8, fullrange=T) + stat_summary(fun = mean, geom = "point", shape = 21, fill = "darkred", 
    color = "darkred", size = 1.5,
    position = position_dodge(width = 0.4))  +labs(
    x = "Temperature (°C)", y = "Heart rate variability (CV%)") +  coord_cartesian(ylim = c(0, 50)) + scale_x_continuous(limits = c(0, 50), expand = c(0, 0))+
  scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature)))+ theme_classic2()


modelo_lin <- lm(CV ~ Temperature + I(Temperature^2),
                 data = HRV_metrics_total_tem)
summary(modelo_lin)


modelo_lin <- lm(MeanHR_mov ~ log(Temperature),
                 data = HRV_metrics_total_tem)
summary(modelo_lin)


#Time domain heart rate variability metrics

 ggplot(HRV_metrics_total_tem,
      aes(x = Temperature, y = NNi_mov)) +
  geom_boxplot(aes(group = Temperature),
               width = 1.5,
               outlier.shape = NA,
               alpha = 0.5,
               size = 0.4) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) + 
  geom_smooth(se = TRUE,
              color = "black", alpha=0.3, size=0.8, fullrange=T) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 21,
               fill = "darkred",
               color = "darkred",
               size = 1.5)+
  labs(x = "Temperature (°C)",
       y = "NN intervals (ms)") + scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  )+
  theme_classic2()
 
ggplot(HRV_metrics_total_tem,
      aes(x = Temperature, y = pNN50_mov)) +
  geom_boxplot(aes(group = Temperature),
               width = 1.5,
               outlier.shape = NA,
               alpha = 0.5,
               size = 0.4) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) + 
  geom_smooth(se = TRUE,
              color = "black", alpha=0.3, size=0.8, fullrange=T) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 21,
               fill = "darkred",
               color = "darkred",
               size = 1.5)+
  labs(x = "Temperature (°C)",
       y = "pNN50 (%)") + scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  )+
  theme_classic2()

ggplot(HRV_metrics_total_tem,
      aes(x = Temperature, y = SDNN_mov)) +
  geom_boxplot(aes(group = Temperature),
               width = 1.5,
               outlier.shape = NA,
               alpha = 0.5,
               size = 0.4) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) + 
  geom_smooth(se = TRUE,
              color = "black", alpha=0.3, size=0.8, fullrange=T) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 21,
               fill = "darkred",
               color = "darkred",
               size = 1.5)+
  labs(x = "Temperature (°C)",
       y = "SDNN (ms)") + scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  )+coord_cartesian(ylim = c(0, 1000))+
  theme_classic2()

ggplot(HRV_metrics_total_tem,
      aes(x = Temperature, y = RMSSD_mov)) +
  geom_boxplot(aes(group = Temperature),
               width = 1.5,
               outlier.shape = NA,
               alpha = 0.5,
               size = 0.4) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) + 
  geom_smooth(se = TRUE,
              color = "black", alpha=0.3, size=0.8, fullrange=T) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 21,
               fill = "darkred",
               color = "darkred",
               size = 1.5)+
  labs(x = "Temperature (°C)",
       y = "RMSSD (ms)") + scale_x_continuous(
    breaks = sort(unique(HRV_metrics_total_tem$Temperature))
  )+coord_cartesian(ylim = c(0, 1000))+
  theme_classic2()

kruskal.test(NNi_mov ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_HRV  <- dunnTest(NNi_mov ~ Treatment, data = HRV_metrics_total_tem, method = "holm")
dun_test_tem_HRV$res
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV
view(res_tem_HRV)


kruskal.test(pNN50_mov ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_HRV  <- dunnTest(pNN50_mov ~ Treatment, data = HRV_metrics_total_tem, method = "holm")
dun_test_tem_HRV$res
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV
view(res_tem_HRV)


kruskal.test(SDNN_mov ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_HRV  <- dunnTest(SDNN_mov ~ Treatment, data = HRV_metrics_total_tem, method = "holm")
dun_test_tem_HRV$res
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV
view(res_tem_HRV)


kruskal.test(RMSSD_mov ~ Treatment, data = HRV_metrics_total_tem)
dun_test_tem_HRV  <- dunnTest(RMSSD_mov ~ Treatment, data = HRV_metrics_total_tem, method = "holm")
dun_test_tem_HRV$res
res_tem_HRV <- dun_test_tem_HRV$res
res_tem_HRV
view(res_tem_HRV)


mean_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(mean_HR = mean(MeanHR_mov), sd_HR = sd(MeanHR_mov))
mean_baseline

median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_CV = median(CV), IQR_CV = IQR(CV))
median_baseline


HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(NNi_mov)
HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(pNN50_mov)
HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(SDNN_mov)
HRV_metrics_total_tem %>% group_by(Treatment) %>% shapiro_test(RMSSD_mov)


median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_NNi = median(NNi_mov), IQR_NNi = IQR(NNi_mov))
median_baseline

median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_pNN50 = median(pNN50_mov), IQR_pNN50 = IQR(pNN50_mov))
median_baseline

median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_SDNN = median(SDNN_mov), IQR_SDNN= IQR(SDNN_mov))
median_baseline

median_baseline <- HRV_metrics_total_tem %>%
  group_by(Treatment) %>%
  summarise(Median_RMSSD = median(RMSSD_mov), IQR_RMSSD = IQR(RMSSD_mov))
median_baseline

