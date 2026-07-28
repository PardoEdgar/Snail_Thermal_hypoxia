# Snail Thermal hypoxia (2026), Universidad del Rosario

This repository contains all data and code used in the study:

**"Short-term oxygen deprivation during high and low temperature water immersion influences heart dynamics function of the invasive snail *Cornu aspersum*"**



<img width="975" height="379" alt="image" src="https://github.com/user-attachments/assets/d48a725e-c2e6-456a-a952-b08fa12b6645" />



## Overview

This repository has the content to quantify cardiac activity during short-term water immersion across different tempeeratures in the land snail *Cornu aspersum*. 

Our methodology uses optocardiography based on pixel intensity changes.

Signal processing and heart rate variability (HRV) analyses are implemented in R to investigate.


## Author

**Edgar Alejandro Pardo-Sarmiento**

## Requirements
 - R (V.4.4.2)
 - Required packages
     - Tidyverse
     - readxl
     - rstatix
     - FSA
     - ggpubr
     - RHRV
       
## Reproducibility
 - `01_Optocardiography.R`
 - `02_HRV_metrics_extraction.R`
 - `03_Cardiac_water_immersion_analysis.R`
 - `04_Cardiac_temperature_analysis.R`
 - `05_Cardiac_recovery_analysis.R`
 - `06_Cardiac_mass_analysis.R`
   
## Contents
 - `Data/` Raw and processed datasets
    - Optocadiographic_data
 - `Images/` Optocardiogram images from Optocardiography
    - Optocardiograms
 - `Scripts/` R scripts for data extraction and analysis

## Data Availability

All data and code required to reproduce the analyses are included in this repository.

## Licenses
 - The source code in this repository is licensed under the MIT License.  
 - All data and figures are licensed under the Creative Commons Attribution 4.0 International License (CC BY 4.0).

## Collaborators
**Juan Pablo Hernández, Universidad del Rosario, Escuela de Ciencias e Ingenería, Colombia**
