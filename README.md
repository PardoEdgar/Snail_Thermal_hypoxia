# Snail Thermal Hypoxia (2026)

This repository contains all data and code used in the study:

**"Short-term oxygen deprivation during high and low temperature water immersion influences heart function of the invasive snail *Cornu aspersum*"**




## Overview

This repository has the content to quantify cardiac activity during short-term water immersion across different temperatures in the invasive snail *Cornu aspersum*. 

Our methodology uses optocardiography and a computational workflow based on pixel intensity changes.

Signal processing and heart rate variability (HRV) analyses are perform with R.


## Author
**Edgar Alejandro Pardo-Sarmiento**

- Escuela de Ciencias e Ingeniería, Universidad del Rosario, Colombia
- ORCID: 0009-0004-0623-8455

## Requirements
 - R (V.4.4.2)
 - Required packages
     - Tidyverse
     - readxl
     - rstatix
     - FSA
     - ggplot2
     - ggpubr
     - RHRV
     - Car
     - Signal
       
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

All data and code required to reproduce the workflow and analyses are included in this repository.

## Licenses
 - The source code in this repository is licensed under the MIT License.  
 - All data and figures are licensed under the Creative Commons Attribution 4.0 International License (CC BY 4.0).

## Collaborators
**Juan Pablo Hernández Benavides**

- Escuela de Ciencias e Ingenería, Universidad del Rosario, Colombia
- ORCID: 0000-0002-6689-0830
**Andre J. Riveros**

- Escuela de Ciencias e Ingeniería, Universidad del Rosario, Bogota. Colombia
- Department of Neuroscience, School of Science, University of Arizona, Tucson, Arizona. United States of America
- ORCID: 0000-0001-7928-1885
