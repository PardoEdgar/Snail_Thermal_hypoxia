# Snail Thermal Hypoxia (2026)

This repository contains all data and code used in the study:

**"Short-term oxygen deprivation during high and low temperature water immersion influences heart function of the invasive snail *Cornu aspersum*"**




## Overview
This repository contains the data and code used to quantify heart function during short-term water immersion across different temperatures in the invasive snail *Cornu aspersum*. 

Our methodology uses optocardiography and a computational workflow based on pixel intensity changes.

Signal processing and heart rate variability (HRV) analyses are performed in R.


## Author
**Edgar Alejandro Pardo-Sarmiento**

- Escuela de Ciencias e Ingeniería, Universidad del Rosario, Colombia
- ORCID: 0009-0004-0623-8455


## Requirements
 - R (V.4.4.2)
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
       
## Reproducibility
Conventions to consider: 

- ENVCOLD: Environmental temperature treatment recorded before water immersion (air) in the low-temperature group. The recorded temperature was 21.64±0.09°C
- WATCOLD: Environmental temperature treatment recorded after water immersion in the low-temperature group. The recorded temperature was 22.06±0.05°C
- ENVHEAT: Environmental temperature treatment recorded before water immersion (air) in the high-temperature group. The recorded temperature was 19.63±0.25°C
- WATHEAT: Environmental temperature treatment recorded after water immersion in the high-temperature group. The recorded temperature was 20.63±0.12 °C. Data from this treatment are used as the environmental reference temperature for the temperature comparison analysis, the post-immersion two time-interval analysis, and the heart function vs. body mass analyses.
- 1C: When mentioned is 1.50°C temperature treatment, but for abbreviation in analysis and data is usually noted as 1C.

The following are the R scripts used for data extraction and analysis and are provided in sequential order:
 - `01_Optocardiography.R`
 - `02_HRV_metrics_extraction.R`
 - `03_Cardiac_water_immersion_analysis.R`
 - `04_Cardiac_temperature_analysis.R`
 - `05_Cardiac_recovery_analysis.R`
 - `06_Cardiac_mass_analysis.R`
   
## Contents
 - `Data/` Raw and processed datasets
    - Optocardiographic_data
 - `Scripts/` R scripts for data extraction and analysis
 - `Google Drive folder` Presents the optocardiograms with the pixel-intensity data over time for all experimental treatments and recordings. Link: https://drive.google.com/drive/folders/1fX7OBxKt-VaYgu_Iyaah_0rEejzOyl6W?usp=drive_link

## Data Availability

All data and code required to reproduce the workflow and analyses are included in this repository and the associated Google Drive folder https://drive.google.com/drive/folders/1fX7OBxKt-VaYgu_Iyaah_0rEejzOyl6W?usp=drive_link

## Licenses
 - The source code in this repository is licensed under the MIT License.  
 - All data and figures are licensed under the Creative Commons Attribution 4.0 International License (CC BY 4.0).

## Collaborators
**Juan Pablo Hernández Benavides**

- Escuela de Ciencias e Ingeniería, Universidad del Rosario, Colombia
- ORCID: 0000-0002-6689-0830


**Andre J. Riveros**

- Escuela de Ciencias e Ingeniería, Universidad del Rosario, Bogota. Colombia
- Department of Neuroscience, School of Science, University of Arizona, Tucson, Arizona. United States of America
- ORCID: 0000-0001-7928-1885
