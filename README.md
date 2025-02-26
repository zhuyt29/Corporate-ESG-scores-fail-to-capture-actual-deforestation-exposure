Code for analysing ESG scores from two commercial ESG providers, Morgan Stanley Capital International (MSCI) and Sustainalytics, and actual deforestation exposure of companies  using R.

Zhu. et al. (In Review). Corporate ESG scores fail to capture actual deforestation exposure

Please find one analysis script which aims to investiagte the relationship between corporate ESG scores and corporate actual deforestation exposure using R. 
For each data point, we used the company issuer ISIN to extract the market capitalisation in the same month and year as the rating date for MSCI and the extraction date of Sustainalytics. The datapoints with missing market capitalisation were removed. For MSCI, IAS, WAKIS, E score, S score and G score were fitted as the response variable in separate models. For Sustainalytics, the response was ESG risk score, managed risk and overall risk exposure in separate models. The explanatory variables were market capitalisation (in billions), deforestation exposure, commodity (beef as the reference), the interaction of deforestation exposure and market capitalisation and the interaction of deforestation exposure and commodity. Z-score standardization was used for all the continuous variables. For MSCI, the rating year, company and industry were fitted as crossed random intercepts. For Sustainalytics, company nested within subindustry and extraction year were fitted as cross random intercepts.

Please reference the manuscript for more information on the method and data availability.
