#Results: HRV baseline
HRV_metrics_total <- read.csv("C:/Users/jandr/OneDrive - Universidad del rosario/Temperature_Pressure_HRV/Data/data_extraction/HRV_metrics_total_T_exp.csv")

HRV_metrics_baseline <-  HRV_metrics_total %>%
      dplyr::filter(Treatment %in%  c("ENVCOLD","WATCOLD", "ENVHEAT", "WATHEAT")) %>% 
      dplyr::mutate(
        Treatment = factor(
          Treatment,
          levels = c("ENVCOLD", "WATCOLD", "ENVHEAT", "WATHEAT")))

HRV_metrics_baseline_cold <-  HRV_metrics_total %>%
  dplyr::filter(Treatment %in% c("ENVCOLD", "WATCOLD")) %>% 
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("ENVCOLD", "WATCOLD") ))

HRV_metrics_baseline_heat <-  HRV_metrics_total %>%
  dplyr::filter(Treatment %in% c("ENVHEAT", "WATHEAT")) %>% 
  dplyr::mutate(
    Treatment = factor(
      Treatment,
      levels = c("ENVHEAT", "WATHEAT")))

n <- HRV_metrics_baseline %>%  group_by(Treatment) %>%  shapiro_test(MeanHR_mov)
n #Normales MeanHR
levene_test(HRV_metrics_baseline_heat, MeanHR_mov ~ Treatment)
t_test(HRV_metrics_baseline_heat, MeanHR_mov ~ Treatment)
levene_test(HRV_metrics_baseline_cold, MeanHR_mov ~ Treatment)
t_test(HRV_metrics_baseline_cold, MeanHR_mov ~ Treatment)

mean_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(mean_HR = mean(MeanHR_mov), sd_HR = sd(MeanHR_mov))
mean_baseline

shapiro_test(HRV_metrics_baseline_heat$CV)
n <- HRV_metrics_baseline %>%  group_by(Treatment) %>%  shapiro_test(CV)
n #Normales MeanHR
levene_test(HRV_metrics_baseline_cold, CV ~ Treatment)
wilcox_test(HRV_metrics_baseline_cold, CV ~ Treatment)
levene_test(HRV_metrics_baseline_heat, CV ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, CV ~ Treatment)  #CV no normal para esta comparación


median_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(Median_CV = median(CV), IQR_CV = IQR(CV))
median_baseline

HR_1<- ggplot(data= HRV_metrics_baseline, aes(x= Treatment, y= MeanHR_mov,color=Block)) + geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8,
    size =0.5) + geom_jitter(width= 0.1,size = 0.8, 
    alpha = 0.4)  +
    scale_color_manual(
    values = c(
    "Cold" = "black",
    "Heat" = "black"
    )) + stat_summary(fun = mean, geom = "point", shape = 21, fill = "darkred", 
    color = "darkred", size = 1.5,
    position = position_dodge(width = 0.4)) + labs(x= "Baseline", y="Heart rate (BPM)")+ theme_classic2() + theme(legend.position = "Top")

CV_1 <- ggplot(data= HRV_metrics_baseline, aes(x= Treatment, y= CV,color=Block)) + geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.6,
    size = 0.5) + geom_jitter(width= 0.1, size = 0.8, 
    alpha = 0.4)  +
    scale_color_manual(
    values = c(
    "Cold" = "black",
    "Heat" = "black"
    )) + stat_summary(fun = mean, geom = "point", shape = 21, fill = "darkred", 
    color = "darkred", size = 1.5,
    position = position_dodge(width = 0.4)) + labs(x= "Baseline", y="Heart rate variability (CV%)")+  coord_cartesian(ylim = c(0, 20)) +
 theme_classic2()+ theme(legend.position = "Top")

#Time domain metrics

ggplot(data= HRV_metrics_baseline, aes(x= Treatment, y= NNi_mov,color=Block)) + geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8,
    size =0.5) + geom_jitter(width= 0.1,size = 0.8, 
    alpha = 0.4)  +
    scale_color_manual(
    values = c(
    "Cold" = "black",
    "Heat" = "black"
    )) + stat_summary(fun = mean, geom = "point", shape = 21, fill = "darkred", 
    color = "darkred", size = 1.5,
    position = position_dodge(width = 0.4)) + labs(x= "Baseline", y="NN intervals (ms)") + theme_classic2() + theme(legend.position = "Top")



ggplot(data= HRV_metrics_baseline, aes(x= Treatment, y= pNN50_mov,color=Block)) + geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8,
    size =0.5) + geom_jitter(width= 0.1,size = 0.8, 
    alpha = 0.4)  +
    scale_color_manual(
    values = c(
    "Cold" = "black",
    "Heat" = "black"
    )) + stat_summary(fun = mean, geom = "point", shape = 21, fill = "darkred", 
    color = "darkred", size = 1.5,
    position = position_dodge(width = 0.4)) + labs(x= "Baseline", y="pNN50 (%)") + theme_classic2() + theme(legend.position = "Top")


ggplot(data= HRV_metrics_baseline, aes(x= Treatment, y= RMSSD_mov,color=Block)) + geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8,
    size =0.5) + geom_jitter(width= 0.1,size = 0.8, 
    alpha = 0.4)  +
    scale_color_manual(
    values = c(
    "Cold" = "black",
    "Heat" = "black"
    )) + stat_summary(fun = mean, geom = "point", shape = 21, fill = "darkred", 
    color = "darkred", size = 1.5,
    position = position_dodge(width = 0.4)) + labs(x= "Baseline", y="RMSSD (ms)")+ coord_cartesian(ylim = c(0, 300)) + theme_classic2() + theme(legend.position = "Top")


ggplot(data= HRV_metrics_baseline, aes(x= Treatment, y= SDNN_mov,color=Block)) + geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8,
    size =0.5) + geom_jitter(width= 0.1,size = 0.8, 
    alpha = 0.4)  +
    scale_color_manual(
    values = c(
    "Cold" = "black",
    "Heat" = "black"
    )) + stat_summary(fun = mean, geom = "point", shape = 21, fill = "darkred", 
    color = "darkred", size = 1.5,
    position = position_dodge(width = 0.4)) + labs(x= "Baseline", y="SDNN (ms)") + coord_cartesian(ylim = c(0, 300))+theme_classic2() + theme(legend.position = "Top")

wilcox_test(HRV_metrics_baseline_cold, NNi_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, NNi_mov ~ Treatment)

wilcox_test(HRV_metrics_baseline_cold, pNN50_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, pNN50_mov ~ Treatment)

wilcox_test(HRV_metrics_baseline_cold, SDNN_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, SDNN_mov ~ Treatment)

wilcox_test(HRV_metrics_baseline_cold, RMSSD_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, RMSSD_mov ~ Treatment)


shapiro_test(HRV_metrics_baseline_heat$NNi_mov)
n <- HRV_metrics_baseline %>%  group_by(Treatment) %>%  shapiro_test(NNi_mov)
n #Normales MeanHR

shapiro_test(HRV_metrics_baseline_heat$pNN50_mov)
n <- HRV_metrics_baseline %>%  group_by(Treatment) %>%  shapiro_test(pNN50_mov)
n #Normales MeanHR

shapiro_test(HRV_metrics_baseline_heat$SDNN_mov)
n <- HRV_metrics_baseline %>%  group_by(Treatment) %>%  shapiro_test(SDNN_mov)
n #Normales MeanHR

shapiro_test(HRV_metrics_baseline_heat$RMSSD_mov)
n <- HRV_metrics_baseline %>%  group_by(Treatment) %>%  shapiro_test(RMSSD_mov)
n #Normales MeanHR


mean_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(mean_NN = mean(NNi_mov), sd_NN = sd(NNi_mov))
mean_baseline

median_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(Median_pNN50 = median(pNN50_mov), IQR_pNN50 = IQR(pNN50_mov))
median_baseline

median_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(Median_SDNN = median(SDNN_mov), IQR_SDNN = IQR(SDNN_mov))
median_baseline


median_baseline <- HRV_metrics_baseline %>%
  group_by(Treatment) %>%
  summarise(Median_RMSSD = median(RMSSD_mov), IQR_RMSSD = IQR(RMSSD_mov))
median_baseline

t_test(HRV_metrics_baseline_cold, NNi_mov ~ Treatment)
t_test(HRV_metrics_baseline_heat, NNi_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_cold, pNN50_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, pNN50_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_cold, SDNN_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, SDNN_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_cold, RMSSD_mov ~ Treatment)
wilcox_test(HRV_metrics_baseline_heat, RMSSD_mov ~ Treatment)


