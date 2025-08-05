# Project Title
Zhu. et al. (In Review). Corporate ESG scores fail to capture actual deforestation exposure.
This repository contains the code and analysis for analyzing the effects of corporate deforestation exposure and deforestation-related news on the companies’ Environment, Social and Governance (ESG) score  
It used R and several R packages for statistical modeling and visualization.

# System Requirements
- **Operating System**: Windows 11  
- **R version**: R 4.3.0
Software Dependencies - The following R packages are required to run the code

#Installation Guide
Install R - Download and install R (version 4.3.0 or later) from: https://cran.r-project.org
RStudio version (2023.06.1+524 or later) from: https://posit.co/download/rstudio-desktop/
#Install R packages
install.packages(c("lme4", "lmtest", "lmerTest", "visreg", "sjPlot", "ggplot2", 
  "dplyr", "tidyr", "lattice", "usdm", "Hmisc", "DHARMa"))

#Demo
Instructions to run on data – apply the codes to the simulated datasets for demo step-by-step

Expected output: 
The result of the linear mixed-effects model on the effects of deforestation-related news counts and deforestation exposure on the overall Analyst View of Sustainalytics
The result of the linear mixed-effects model on the effects of deforestation-related news counts and deforestation exposure on specific issue scores from MSCI
The result of the linear mixed-effects model on the effects of deforestation-related news counts and deforestation exposure on the five ESG scores from MSCI
The result of the ordinal logistic model on the effects of deforestation-related news counts and deforestation exposure on Climate Change Score from CDP

The demo is expected to complete within a few minutes on a standard desktop or laptop computer
#Instructions for use

Please refer to the methods section in the manuscript

Please refer to the manuscript for more information on the methods and data availability of ESG scores from five commercial ESG providers, i.e., Morgan Stanley Capital International (MSCI), Sustainalytics, Refinitiv, S&P Global and Carbon Disclosure Project (CDP), and firm-level deforestation exposure measures from Transparency for Sustainable Economies (TRASE) platform
