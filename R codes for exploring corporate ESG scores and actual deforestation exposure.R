#Corporate ESG scores fail to capture actual deforestation exposure#

#The MSCI and Sustainalytics datasets were organized and merged with TRASE deforestation dataset before the analysis below

library(ggplot2)
library(dplyr)
library(tidyr)

#Include market capitalisation in the dataset
#market capitalisation was avergaed by month in each year for each company
market_capital_0<- read.csv("eikon_mcap_yingtong.csv")

market_capital_0$year <- substr(market_capital_0$Date, 1, 4)
market_capital_0$month <- substr(market_capital_0$Date, 6, 7)
market_capital_0$date <- substr(market_capital_0$Date, 9, 10)

colnames(market_capital_0)[1]<- "Issuer_ISIN" 
colnames(market_capital_0)[3]<- "Market_Capital"
colnames(market_capital_0)[4]<- "MC_year"
colnames(market_capital_0)[5]<- "MC_month"
colnames(market_capital_0)[6]<- "MC_date"

MSCI$Rating_year<- substr(MSCI$Rating_Date, 1, 4)
MSCI$Rating_month<- substr(MSCI$Rating_Date, 5, 6)
MSCI$Rating_date<- substr(MSCI$Rating_Date, 7, 8)

MSCI_0<- MSCI %>% left_join(market_capital_0, by = c("Issuer_ISIN" = "Issuer_ISIN", "Rating_year" = "MC_year", "Rating_month" = "MC_month"))
#Similar process was appied to Sustainalytics

#Morgan Stanley Capital International (MSCI)============================================
#ESG scores are:Industry-Adjusted Score (IAS), Weighted Average Key Issue Score (WAKIS), Environmental Pillar score (E score), Social Pillar Score (S score), Governance Pillar Score (G score)

MSCI_0<- read.csv("E:/PhD Research/Chapter 2 - Reanalysis/MSCI data 2/MSCI_TRASE_same year (rating year).csv")
length(unique(MSCI_0$Common_Name))  #202 companies initially

#Exluded companies missing market capitalisation
MSCI1.1<- MSCI_0[!(is.na(MSCI_0$Market_Capital)),]

#market capitalisation in the unit of billion
MSCI1.1$Market_Capital_B<- MSCI1.1$Market_Capital/(10^9)
length(unique(MSCI1.1$Common_Name)) #154

#Deforestation exposure in thousands
MSCI1.1$DF_E<- MSCI1.1$DF_E/1000
MSCI1.1<- MSCI1.1 %>% rename(Deforestation = DF_E) %>% rename(Market_Capitalisation = Market_Capital_B)

#The dataset MSCI1.1 had a sample size of 520 firm-years, including 154 unique companies from 37 industries

#LMM Models
require(lme4)  
require(lmtest)
require(lmerTest)

lmer_model2 <- function(data, response, market_cap) {
  
  model_formula <- as.formula(paste0("scale(",response, ") ~  scale(Market_Capitalisation)*scale(Deforestation) + scale(Deforestation)*Commodity + (1|Rating_year) + (1|Common_Name)+ (1|Industry)"))
  model <- lmer(model_formula, data = data)
  
  return(model)
}

IAS_cap.con1<- lmer_model2(MSCI1.1, "Industry_Adjusted_Score")
WAS_cap.con1<- lmer_model2(MSCI1.1, "Weighted_Average_Score") 
E_cap.con1<- lmer_model2(MSCI1.1, "E_score")
S_cap.con1<- lmer_model2(MSCI1.1, "S_score")
G_cap.con1<-  lmer(scale(G_score) ~  scale(Market_Capitalisation)*scale(Deforestation) + scale(Deforestation)*Commodity + (1|Rating_year) + (1|Common_Name), data = MSCI1.1)

#Interaction plot
library(visreg)
visreg(G_cap.con1, "Deforestation", by="Market_Capitalisation", breaks=3)
visreg(G_cap.con1, "Deforestation", by="Commodity", overlay=TRUE)
#also applied to other responses variables to create Interaction plots


extract_coefficients <- function(model, response, market_cap) {
  coef_summary <- summary(model)$coefficients
  coef_df <- data.frame(
    term = rownames(coef_summary),
    estimate = coef_summary[, "Estimate"],
    std_Error= coef_summary[, "Std. Error"],
    p_value = coef_summary[, "Pr(>|t|)"],
    response = response,
    market_cap = market_cap
  )
  return(coef_df)
}

# Define the function
create_coefficient_plot <- function(model, response_var, method_name) {
  # Extract coefficients
  coef_data <- extract_coefficients(model, response_var, method_name)
  coef_data <- data.frame(coef_data)
  
  # Add additional columns
  coef_data$model <- paste0(coef_data$response)
  coef_data$significance <- ifelse(coef_data$p_value < 0.05, "significant", "not significant")
  
  coef_data <- coef_data %>%
    mutate(p_value_category = case_when(
      p_value >= 0.05 ~ "p >= 0.05",
      p_value < 0.05 ~ "p < 0.05"
    ))
  
  coef_data$direction <- ifelse(coef_data$estimate > 0, "positive", "negative")
  
  # Convert columns to factors
  coef_data$p_value_category <- as.factor(coef_data$p_value_category)
  coef_data$significance <- as.factor(coef_data$significance)
  coef_data$direction <- as.factor(coef_data$direction)
  
  # Define the levels for the term factor
  coef_data$term <- factor(coef_data$term, levels = c(
    "scale(DF_E):Commoditywood pulp", "scale(DF_E):Commoditysoy", 
    "scale(DF_E):Commoditypalm oil", "scale(DF_E):Commoditypork", 
    "scale(DF_E):Commoditychicken", "scale(Market_Capital_B):scale(DF_E)", 
    "Commoditywood pulp", "Commoditysoy", "Commoditypalm oil", 
    "Commoditypork", "Commoditychicken", "scale(DF_E)", 
    "scale(Market_Capital_B)", "(Intercept)"
  ))
  
  # Create the plot
  plot <- ggplot(coef_data, aes(x = estimate, y = term, color = direction)) +
    geom_point(aes(shape = p_value_category), size = 3) +
    geom_errorbarh(aes(xmin = estimate - std_Error, xmax = estimate + std_Error, 
                       linetype = p_value_category), height = 0.2) +
    theme_classic() +
    xlab("Coefficient estimates") +
    ylab("") +
    scale_color_manual(values = c("positive" = "#CC3333", "negative" = "#3366CC")) +
    scale_shape_manual(values = c(16, 1)) +
    scale_linetype_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 3)) +
    guides(
      colour = guide_legend("Direction"),
      linetype = guide_legend("p-value category"),
      shape = guide_legend("p-value category")
    ) + 
    geom_vline(xintercept = 0, colour = "grey90", linetype = 1, size = 0.5) +
    theme(
      axis.text.y = element_text(size = 12),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.justification = c(1, 1),
      legend.background = element_rect(colour = "grey80")
    )
  
  return(plot)
}

plot_WAS1 <- create_coefficient_plot(WAS_cap.con1, "Weighted_Average_Score", "MSCI")
plot_IAS1 <- create_coefficient_plot(IAS_cap.con1, "Industry_Adjusted_Score", "MSCI")
plot_E <- create_coefficient_plot(E_cap.con1, "E_score", "MSCI")
plot_S <- create_coefficient_plot(S_cap.con1, "S_score", "MSCI")
plot_G <- create_coefficient_plot(G_cap.con1, "G_score", "MSCI")


#Figure 1
extract_coefficients <- function(model, response, market_cap) {
  coef_summary <- summary(model)$coefficients
  coef_df <- data.frame(
    term = rownames(coef_summary),
    estimate = coef_summary[, "Estimate"],
    std_Error= coef_summary[, "Std. Error"],
    p_value = coef_summary[, "Pr(>|t|)"],
    response = response,
    market_cap = market_cap
  )
  return(coef_df)
}

coef_IAS1 <- extract_coefficients(IAS_cap.con1, "Industry_Adjusted_Score", "MSCI")
coef_WAS1 <- extract_coefficients(WAS_cap.con1, "Weighted_Average_Score", "MSCI")
coef_E1<- extract_coefficients(E_cap.con1, "E_score", "MSCI")
coef_S1<- extract_coefficients(S_cap.con1, "S_score", "MSCI")
coef_G1 <- extract_coefficients(G_cap.con1, "G_score", "MSCI")

coef_all <- rbind(coef_IAS1, coef_WAS1, coef_E1, coef_S1, coef_G1)

coef_all$model<- paste0(coef_all$response)

coef_all$significance <- ifelse(coef_all$p_value < 0.05, "significant", "not significant")

coef_all2 <- coef_all %>%
  mutate(p_value_category = case_when(
    p_value > 0.05 ~ "p > 0.05",
    p_value > 0.01 & p_value <= 0.05 ~ "0.01 < p ≤ 0.05",
    p_value <= 0.01 ~ "p ≤ 0.01"
  ))

coef_all2 <- coef_all %>%
  mutate(p_value_category = case_when(
    p_value >= 0.05 ~ "p >= 0.05",
    p_value < 0.05 ~ "p < 0.05"
  ))

coef_all3<- coef_all2
coef_all3$DE<- "t"
coef_all3<- coef_all3[,c(7,5,1,2,3,4,9,8,6,10)]
setwd("E:/PhD Research/Chapter 2 - Reanalysis/R codes 2")
write.csv(coef_all3,"LMM_results_MSCI_rating year_t2.csv")


library(tidyverse)
library(dotwhisker)
library(gapminder)


coef_all3 <- coef_all3 %>%
  mutate(
    conf.low = estimate - (1.96 * std_Error),
    conf.high = estimate + (1.96 * std_Error)
  )


#coef_all3<- coef_all3 %>% rename(model = label)

setwd("E:/PhD Research/Chapter 2 - Reanalysis/R codes 2")
write.csv(coef_all3,"LMM_results_MSCI_rating year_t.csv")

coef_all3$p_value_category<- as.factor(coef_all3$p_value_category)
coef_all4<-coef_all3[!(coef_all3$term  %in% c("Deforestation:pork","pork")),]

dwplot(coef_all4, vline = ggplot2::geom_vline(xintercept = 0,
                                              colour = "grey90",
                                              linetype = 1,
                                              size = 0.5),
       dot_args = list(aes(shape = p_value_category), size = 1.8),
       whisker_args = list(aes(col= model, linetype = p_value_category))
) + 
  theme_classic() + 
  xlab("Coefficient estimates") + 
  scale_color_manual(values = c("red3","orange3","dodgerblue1","#009E73", "darkorchid1")) +
  scale_shape_manual(values = c(16, 1, 4)) +
  scale_linetype_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 3)) +
  guides(
    colour = guide_legend("Model"),
    linetype = guide_legend("Significance"),
    shape = guide_legend("Significance")
  ) + # Combine the legends for shape and color 
  theme(
    axis.text.y = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    legend.position = c(0.8, 1),
    legend.justification = c(1, 1),
    legend.key.size = unit(0.8, "cm"),  # Increase legend key size
    legend.text = element_text(size = 10),  # Increase text size
    legend.title = element_text(size = 12, face = "bold"),
    legend.background = element_rect(colour = "grey80"),
  )


#Sustainalytics==============================
#The ESG scores are: ESG risk score, managed risk score and overall risk exposure

Sustainalytics_0<- read.csv("Sustain_TRASE_same year2.csv")
length(unique(Sustainalytics_0$Common_name))  #139 companies initially

##Exluded companies missing market capitalisation
Sustainalytics1.1<- Sustainalytics_0[!(is.na(Sustainalytics$Market_Capital)),]
length(unique(Sustainalytics1.1$Common_name)) #114

#market capitalisation in the unit of billion
Sustainalytics1.1$Market_Capital_B<- Sustainalytics1.1$Market_Capital/(10^9)
length(unique(Sustainalytics1.1$Common_name)) #107 

Sustainalytics1.1$DF_E<- Sustainalytics1.1$DF_E/1000

Sustainalytics1.1<- Sustainalytics1.1 %>% rename(Deforestation = DF_E) %>% rename(Market_Capitalisation = Market_Capital_B)

#The dataset Sustainalytics1.1 had a sample size of 221 firm-years, including 107 distinct companies from 33 subindustries and 9 sectors

#Model
lmer_model2 <- function(data, response) {
  
  model_formula <- as.formula(paste0("scale(",response, ") ~  scale(Market_Capitalisation)*scale(Deforestation) + scale(Deforestation)*Commodity + (1|Year_Sustain)  + (1|Subindustry/Common_name)"))
  model <- lmer(model_formula, data = data)
  
  return(model)
}

ESG_risk.con1<- lmer_model2(Sustainalytics1.1, "ESG_risk")
Managed_risk.con1<- lmer_model2(Sustainalytics1.1, "Managed_Risk")
Overall_exposure.con1<-lmer_model2(Sustainalytics1.1, "Overall_Exposure")

visreg(Overall_exposure.con1, "Deforestation", by="Market_Capitalisation", breaks=3)
visreg(Overall_exposure.con1, "Deforestation", by="Commodity", overlay=TRUE)
#also applied to other responses variables to create Interaction plots


extract_coefficients <- function(model, response) {
  coef_summary <- summary(model)$coefficients
  coef_df <- data.frame(
    term = rownames(coef_summary),
    estimate = coef_summary[, "Estimate"],
    std_Error= coef_summary[, "Std. Error"],
    p_value = coef_summary[, "Pr(>|t|)"],
    response = response
  )
  return(coef_df)
}


# Define the function
create_coefficient_plot <- function(model, response_var) {
  # Extract coefficients
  coef_data <- extract_coefficients(model, response_var)
  coef_data <- data.frame(coef_data)
  
  # Add additional columns
  coef_data$model <- paste0(coef_data$response)
  coef_data$significance <- ifelse(coef_data$p_value < 0.05, "significant", "not significant")
  
  coef_data <- coef_data %>%
    mutate(p_value_category = case_when(
      p_value >= 0.05 ~ "p >= 0.05",
      p_value < 0.05 ~ "p < 0.05"
    ))
  
  coef_data$direction <- ifelse(coef_data$estimate > 0, "positive", "negative")
  
  # Convert columns to factors
  coef_data$p_value_category <- as.factor(coef_data$p_value_category)
  coef_data$significance <- as.factor(coef_data$significance)
  coef_data$direction <- as.factor(coef_data$direction)
  
  # Define the levels for the term factor
  coef_data$term <- factor(coef_data$term, levels = c(
    "scale(DF_E):Commoditywood pulp", "scale(DF_E):Commoditysoy", 
    "scale(DF_E):Commoditypalm oil", "scale(DF_E):Commoditypork", 
    "scale(DF_E):Commoditychicken", "scale(Market_Capital_B):scale(DF_E)", 
    "Commoditywood pulp", "Commoditysoy", "Commoditypalm oil", 
    "Commoditypork", "Commoditychicken", "scale(DF_E)", 
    "scale(Market_Capital_B)", "(Intercept)"
  ))
  
  # Create the plot
  plot <- ggplot(coef_data, aes(x = estimate, y = term, color = direction)) +
    geom_point(aes(shape = p_value_category), size = 3) +
    geom_errorbarh(aes(xmin = estimate - std_Error, xmax = estimate + std_Error, 
                       linetype = p_value_category), height = 0.2) +
    theme_classic() +
    xlab("Coefficient estimates") +
    ylab("") +
    scale_color_manual(values = c("positive" = "#CC3333", "negative" = "#3366CC")) +
    scale_shape_manual(values = c("p < 0.05" = 16, "p >= 0.05" = 1)) +
    scale_linetype_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 3)) +
    guides(
      colour = guide_legend("Direction"),
      linetype = guide_legend("p-value category"),
      shape = guide_legend("p-value category")
    ) + 
    geom_vline(xintercept = 0, colour = "grey90", linetype = 1, size = 0.5) +
    theme(
      axis.text.y = element_text(size = 12),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.justification = c(1, 1),
      legend.background = element_rect(colour = "grey80")
    )
  
  return(plot)
}

plot_ESG_risk <- create_coefficient_plot(ESG_risk.con1, "ESG_risk")
plot_Managed_risk <- create_coefficient_plot(Managed_risk.con1, "Managed_Risk")
plot_Overall_exposure <- create_coefficient_plot(Overall_exposure.con1, "Overall_Exposure")


#Figure 2
extract_coefficients <- function(model, response) {
  coef_summary <- summary(model)$coefficients
  coef_df <- data.frame(
    term = rownames(coef_summary),
    estimate = coef_summary[, "Estimate"],
    std_Error= coef_summary[, "Std. Error"],
    p_value = coef_summary[, "Pr(>|t|)"],
    response = response
  )
  return(coef_df)
}

coef_ESG_risk1 <- extract_coefficients(ESG_risk.con1, "ESG_risk")
coef_managed_risk1 <- extract_coefficients(Managed_risk.con1, "Managed_Risk")
coef_overall_exposure1<- extract_coefficients(Overall_exposure.con1, "Overall_Exposure")

coef_all <- rbind(coef_ESG_risk1, coef_managed_risk1, coef_overall_exposure1)

coef_all$label <- paste0(coef_all$response)

coef_all$significance <- ifelse(coef_all$p_value < 0.05, "significant", "not significant")

coef_all2 <- coef_all %>%
  mutate(p_value_category = case_when(
    p_value >= 0.05 ~ "p >= 0.05",
    p_value < 0.05 ~ "p < 0.05"
  ))

coef_all3<- coef_all2
coef_all3$DE<- "t"
coef_all3<- coef_all3[,c(7,5,1,2,3,4,9,8,6,10)]
setwd("E:/PhD Research/Chapter 2 - Reanalysis/R codes 2")
write.csv(coef_all3,"LMM_results_Sustain_rating year_t.csv")

coef_all3 <- coef_all3 %>%
  mutate(
    conf.low = estimate - (1.96 * std_Error),
    conf.high = estimate + (1.96 * std_Error)
  )


coef_all3$p_value_category<- as.factor(coef_all3$p_value_category)

coef_all4<-coef_all3[!(coef_all3$term  %in% c("Deforestation:pork","pork")),]

dwplot(coef_all4, vline = ggplot2::geom_vline(xintercept = 0,
                                              colour = "grey90",
                                              linetype = 1,
                                              size = 0.5),
       dot_args = list(aes(shape = p_value_category), size = 2),
       whisker_args = list(aes(col= model, linetype = p_value_category))
) + 
  theme_classic() + 
  xlab("Coefficient estimates") + 
  scale_color_manual(values = c("violetred","orange3","deepskyblue")) +
  scale_shape_manual(values = c(16, 1, 4)) +
  scale_linetype_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 3)) +
  guides(
    colour = guide_legend("Model"),
    linetype = guide_legend("Significance"),
    shape = guide_legend("Significance")
  ) + # Combine the legends for shape and color 
  theme(
    axis.text.y = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    legend.position = c(0.94,0.03),
    legend.justification = c(0.8, 0),
    legend.key.size = unit(0.5, "cm"),  # Increase legend key size
    legend.text = element_text(size = 11),  # Increase text size
    legend.title = element_text(size = 11, face = "bold"),
    legend.background = element_rect(colour = "grey80"),
  )

