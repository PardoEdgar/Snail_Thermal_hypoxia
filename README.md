# Snail thermal oxygen deprivation (2026)

**"Short-term oxygen deprivation during water immersion at high and low temperatures influences heart function of the invasive snail *Cornu aspersum*"**




## Overview
This repository and the associated Google Drive folder contain the data and code used to quantify heart function during short-term water immersion across different temperatures in the invasive snail *Cornu aspersum*. 

Our methodology uses optocardiography and a computational workflow based on pixel intensity changes.

Signal processing and heart rate variability (HRV) analyses are performed in R.


## Author
**Edgar Alejandro Pardo-Sarmiento**

- Escuela de Ciencias e Ingeniería, Universidad del Rosario, Colombia
- ORCID: 0009-0004-0623-8455


## Requirements
 - R (v4.4.2)
 - Required packages
     - tidyverse
     - readxl
     - rstatix
     - FSA
     - ggplot2
     - ggpubr
     - RHRV
     - car
     - signal
     - broom
       
## Reproducibility
The following treatment codes are used throughout the datasets and R scripts:

- `ENVCOLD`: Environmental reference treatment before water immersion (M₁) in the Low temperature group. Ambient air temperature was 21.64±0.09°C.

- `WATCOLD`: Post-immersion environmental reference treatment (M₂) in the Low temperature group. The temperature of the water used for immersion was 22.06±0.05°C.

- `ENVHEAT`: Environmental reference treatment before water immersion (M₁) in the High temperature group. Ambient air temperature was 19.63±0.25°C.

- `WATHEAT`: Post-immersion environmental reference treatment (M₂) in the High temperature group. The temperature of the water used for immersion was 20.63±0.12°C. Data from this treatment are also used as the environmental reference temperature (aprox 21°C) in the analyses comparing water-immersion temperatures, post-immersion temporal changes, and the relationships between cardiac parameters and body mass.

- `1C`: Refers to the 1.50°C water-immersion treatment and usually is abbreviated as 1C throughout the datasets and analyses.

The following are the R scripts used for data extraction and analysis and are provided in sequential order:
 - `01_Optocardiography.R`
 - `02_HRV_metrics_extraction.R`
 - `03_Cardiac_water_immersion_analysis.R`
 - `04_Cardiac_temperature_analysis.R`
 - `05_Cardiac_post_immersion_temporal_changes_analysis.R`
 - `06_Cardiac_mass_analysis.R`
   
## Contents
 - `Data/` Raw and processed datasets
 - `Scripts/` R scripts for data extraction and analysis
 - `Google Drive folder` Contains the optocardiograms and the pixel-intensity data over time for all experimental treatments and recordings. Link: https://drive.google.com/drive/folders/1fX7OBxKt-VaYgu_Iyaah_0rEejzOyl6W?usp=drive_link

## Data Availability

All data and code required to reproduce the workflow and analyses are included in this repository and the associated Google Drive folder

## Licenses
 - The source code in this repository is licensed under the MIT License.  
 - All data and figures are licensed under the Creative Commons Attribution 4.0 International License (CC BY 4.0).

## Collaborators
**Juan Pablo Hernández Benavides**

- Escuela de Ciencias e Ingeniería, Universidad del Rosario, Colombia
- ORCID: 0000-0002-6689-0830


**Andre J. Riveros**

- Escuela de Ciencias e Ingeniería, Universidad del Rosario, Bogotá. Colombia
- Department of Neuroscience, School of Science, University of Arizona, Tucson, Arizona. United States of America
- ORCID: 0000-0001-7928-1885
