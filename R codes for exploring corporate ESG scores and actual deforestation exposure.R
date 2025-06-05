#Load the R package
library(dplyr)
library(tidyr)
require(usdm)
require(lme4)  
require(lmtest)
require(lmerTest)
library(visreg)
require(ggplot2)
require(MASS)
library(DHARMa)
library(performance)
library(MuMIn)
library(marginaleffects)


setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Consolidated data/Overall Analyst View")
DEview_news_financial<- read.csv("All_matched_company_info_DE_view_merge news_financial.csv")

DEview_news_financial$DEView_binary<- ifelse(DEview_news_financial$Deforestation_statement %in% c("negative","positive"),1,0)

DEview_news_financial$Common_Name<- as.factor(DEview_news_financial$Common_Nam)
DEview_news_financial$Country<- as.factor(DEview_news_financial$Country)
DEview_news_financial$Sector<- as.factor(DEview_news_financial$Sector)

# Create a dataframe with just the explanatory variables
explanatory_vars <- DEview_news_financial[, c("mean_news_count", "mean_DF_E", 
                                              "assets", "sales", "book_equity",
                                              "net_income", "enterprise_value", "ni_be",
                                              "ope_be", "debt_bev", "cash_lt",
                                              "lt_ppen", "debt_at", "age","me")]

require(usdm)
usdm::vif(explanatory_vars)

#exclude variable interatively
explanatory_vars <- DEview_news_financial[, c("mean_news_count", "mean_DF_E", 
                                              "assets", "sales", "ni_be", "debt_bev", "cash_lt", "age")]

glmm_model1 <- glmer(DEView_binary ~ mean_news_count + mean_DF_E +
                       # standardized control variables
                       assets + sales + ni_be + # replace with actual names
                       debt_bev + cash_lt + age +
                       (1|Country) + (1|Sector),
                     family = binomial(link = "logit"),
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 100000)),
                     data = DEview_news_financial3)

sim_residuals <- simulateResiduals(glmm_model1)
plot(sim_residuals)

# Check for zero-inflation
testZeroInflation(sim_residuals)

# Check for dispersion
testDispersion(sim_residuals)

# Check for outliers
testOutliers(sim_residuals)

#Effects of deforestation exposure and deforestation-related news on ESG scores=============================

#MSCI----------------------------
setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Consolidated data/EX_IM merge_merge JKP financial updated")
MSCI_TRASE<- read.csv("MSCI_TRASE4_news.csv")
names(MSCI_TRASE)

MSCI_TRASE2 <- dplyr::select(MSCI_TRASE, -Issuer_Name, -Issuer_ISIN, -Industry, -Mark.Cap_type,-Rating_month, -Market_Capital)

#Check multilinearity
explanatory_vars <- MSCI_TRASE2[, c( "News_count","DF_E", "assets", "sales", "book_equity","net_income", "enterprise_value", "ni_be", "ope_be", "debt_bev", "cash_lt","lt_ppen", "debt_at", "age","me")]

vif(explanatory_vars)

#interatively to get the variables below
explanatory_vars <- MSCI_TRASE2[, c( "News_count","DF_E", "sales", "ni_be", "ope_be", "debt_bev", "cash_lt","lt_ppen", "age")] #all below 3

#Z Standardise the continuous variables
MSCI_TRASE4<- MSCI_TRASE2 %>% mutate(across(c(19,22:36), 
                                            ~ as.numeric(scale(.x)), 
                                            .names = "{.col}"))

MSCI_TRASE5<- MSCI_TRASE4[MSCI_TRASE4$Sector == "Consumer Staples",]  # repeat the analysis for this dataset

analyze_lmm <- function(model) {
  # Output objects for interactive use
  model_summary <- summary(model)
  residual_plot <- plot(model)
  qq_plot <- qqmath(model, lty = 2)
  visreg_plot <- visreg(model, "DF_E", by = "Commodity", overlay = TRUE)
  r2_results <- r2_nakagawa(model) #conditional and marginal R²
  icc_result <- icc(model)
  vc <- VarCorr(model)
  
  # Return all components as a list
  return(list(
    summary = model_summary,
    residual_plot = residual_plot,
    qq_plot = qq_plot,
    visreg = visreg_plot,
    r2 = r2_results,
    icc = icc_result,
    varcorr = vc
  ))
}

#Industry_Adjusted_Score (IAS)
lmm_model1 <- lmer(Industry_Adjusted_Score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                     sales + ni_be + ope_be + debt_bev+ cash_lt + lt_ppen + age +
                     (1|Sector),data = MSCI_TRASE4)
summary(lmm_model1)

analyze_lmm(lmm_model1)

avg_slopes(lmm_model1,
           variables = "DF_E",
           by = "Commodity")

#Weighted Average Key Issue Score (WAKIS)
lmm_model2 <- lmer(Weighted_Average_Score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                     sales  + ni_be + ope_be + debt_bev+ cash_lt + lt_ppen + age +
                     (1|Sector),data = MSCI_TRASE4)
summary(lmm_model2)

#E score
lmm_model3 <- lmer(E_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                     sales  + ni_be + ope_be + debt_bev+ cash_lt + lt_ppen + age +
                     (1|Sector),data = MSCI_TRASE4)
summary(lmm_model3)

#S score
lmm_model4 <- lmer(S_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                     sales   + ni_be + ope_be + debt_bev+ cash_lt + lt_ppen + age +
                     (1|Sector),data = MSCI_TRASE4)
summary(lmm_model4)

#G score
lmm_model5 <- lmer(G_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                     sales + ni_be + ope_be + debt_bev+ cash_lt + lt_ppen + age +
                     (1|Rating_year),data = MSCI_TRASE4)
summary(lmm_model5)

setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Analysis/Full Model - Effects of deforestation on ESG scores/MSCI")
saveRDS(lmm_model1, "IAS score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model2, "WAKIS score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model3, "E score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model4, "S score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model5, "G score - DE + news + financial_2013-2020.rds")

#Sustainalytics---------------------------------
setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Consolidated data/EX_IM merge_merge JKP financial updated")
Sustain_TRASE<- read.csv("Sustain_TRASE4_news.csv")


Sustain_TRASE2 <- dplyr::select(Sustain_TRASE, -Entity_ID, -Entity_name,-Month_Sustain, -Issuer_ISIN, -Region,-Industry, -Market_Capital,-MC_type)

#Check multilinearity
explanatory_vars <- Sustain_TRASE2[, c( "News_count","DF_E", "assets", "sales", "book_equity","net_income", "enterprise_value", "ni_be", "ope_be", "debt_bev", "cash_lt","lt_ppen", "debt_at", "age","me")]

vif(explanatory_vars)

#The updated one
explanatory_vars <- Sustain_TRASE2[, c( "News_count","DF_E", "assets", "sales", "ni_be", "ope_be", "cash_lt", "debt_at", "age","me")]

Sustain_TRASE4<- Sustain_TRASE2 %>%   mutate(across(c(19,22:36), 
                                                    ~ as.numeric(scale(.x)), 
                                                    .names = "{.col}"))

#Unmanged risk score
lmm_model6 <- lmer(ESG_risk~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                       assets+ sales + ni_be + ope_be + cash_lt + debt_at+ age + me+
                       (1|Sector)+(1|Country_trade),data = Sustain_TRASE4)
summary(lmm_model6)


shapiro.test(Sustain_TRASE4$Managed_Risk) #Not normal
Sustain_TRASE4$sqrt_Managed_Risk<- sqrt(Sustain_TRASE4$Managed_Risk)
shapiro.test(Sustain_TRASE4$sqrt_Managed_Risk)

#Manged risk score
lmm_model7 <- lmer(sqrt_Managed_Risk~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                       assets+ sales + ni_be + ope_be + cash_lt + debt_at+ age + me+
                       (1|Sector),data = Sustain_TRASE4)
summary(lmm_model7)

#Overall risk exposure
lmm_model8 <- lmer(Overall_Exposure~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                       assets+ sales + ni_be + ope_be + cash_lt + debt_at+ age + me+
                       (1|Sector),data = Sustain_TRASE4)
summary(lmm_model8)

setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Analysis/Full Model - Effects of deforestation on ESG scores/Full model_updated with ADM and me")
saveRDS(lmm_model6, "Unmanaged risk - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model7, "Managed risk - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model8, "Risk exposure - DE + news + financial_2013-2020.rds")

#Refinitiv-----------------------------
setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Consolidated data/EX_IM merge_merge JKP financial updated")
Refinitiv_TRASE<- read.csv("Refinitiv_TRASE4_news.csv")
Refinitiv_TRASE2 <- dplyr::select(Refinitiv_TRASE, -Company_name, -Issuer_ISIN,-Industry)

explanatory_vars <- Refinitiv_TRASE2[, c( "News_count","DF_E", "assets", "sales", "book_equity","net_income", "enterprise_value", "ni_be", "ope_be", "debt_bev", "cash_lt","lt_ppen", "debt_at", "age","me")]

vif(explanatory_vars)

explanatory_vars <- Refinitiv_TRASE2[, c( "News_count","DF_E", "sales","net_income", "ni_be", "ope_be", "debt_bev", "cash_lt","lt_ppen", "debt_at", "age")]  # all below 3, updated

Refinitiv_TRASE4<- Refinitiv_TRASE2 %>%   mutate(across(c(9, 18:32), 
                                                        ~ as.numeric(scale(.x)), 
                                                        .names = "{.col}"))

#ESGC score
lmm_model9 <- lmer(ESGC_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                       sales + net_income+ni_be+ope_be+debt_bev+cash_lt+lt_ppen+debt_at+age+
                       (1|Sector),data = Refinitiv_TRASE4)
summary(lmm_model9)

#ESG score
lmm_model10 <- lmer(ESG_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                       sales + net_income+ni_be+ope_be+debt_bev+cash_lt+lt_ppen+debt_at+age+
                       (1|Sector),data = Refinitiv_TRASE4)
summary(lmm_model10)

#E score
lmm_model11<- lmer(E_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                       sales + net_income+ni_be+ope_be+debt_bev+cash_lt+lt_ppen+debt_at+age+
                       (1|Sector),data = Refinitiv_TRASE4)
summary(lmm_model11)

#S score
lmm_model12 <- lmer(S_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                       sales + net_income+ni_be+ope_be+debt_bev+cash_lt+lt_ppen+debt_at+age+
                       (1|Sector),data = Refinitiv_TRASE4)
summary(lmm_mode12)

#G score
lmm_model13 <- lmer(G_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                       sales + net_income+ni_be+ope_be+debt_bev+cash_lt+lt_ppen+debt_at+age+
                       (1|Sector),data = Refinitiv_TRASE4)
summary(lmm_model13)

setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Analysis/Full Model - Effects of deforestation on ESG scores/Full model_updated with ADM and me")
saveRDS(lmm_model9, "Refinitiv_ESGC score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model10, "Refinitiv_ESG score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model11, "Refinitiv_E score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model12, "Refinitiv_S score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model13, "Refinitiv_G score - DE + news + financial_2013-2020.rds")


#S&P global---------------------
setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Consolidated data/EX_IM merge_merge JKP financial updated")
SP_TRASE<- read.csv("SP_TRASE4_news.csv")

explanatory_vars <- SP_TRASE2[, c( "News_count","DF_E", "assets", "sales", "book_equity","net_income", "enterprise_value", "ni_be", "ope_be", "debt_bev", "cash_lt","lt_ppen", "debt_at", "age","me")]

vif(explanatory_vars)

explanatory_vars <- SP_TRASE2[, c( "News_count","DF_E", "sales","net_income", "ni_be", "ope_be", "debt_bev", "cash_lt","lt_ppen", "debt_at", "age")] # all below 3

SP_TRASE4<- SP_TRASE2 %>%   mutate(across(c(13,16:30), 
                                          ~ as.numeric(scale(.x)), 
                                          .names = "{.col}"))

shapiro.test(SP_TRASE4$ESG_score) 
SP_TRASE4$sqrt_ESG_score<- sqrt(SP_TRASE4$ESG_score)
lmm_model14<- lmer(sqrt_ESG_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                     sales + net_income+ ni_be + ope_be + debt_bev+ cash_lt + lt_ppen + debt_at+ age +
                     (1|Sector),data = SP_TRASE4)
summary(lmm_model14)

shapiro.test(SP_TRASE4$E_score) 
SP_TRASE4$sqrt_E_score<- sqrt(SP_TRASE4$E_score)
lmm_model115 <- lmer(sqrt_E_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                     sales + net_income+ ni_be + ope_be + debt_bev+ cash_lt + lt_ppen + debt_at+ age +
                     (1|Sector),data = SP_TRASE4)
summary(lmm_model15)

shapiro.test(SP_TRASE4$S_score) 
SP_TRASE4$sqrt_S_score<- sqrt(SP_TRASE4$S_score)
lmm_model16 <- lmer(sqrt_S_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                     sales + net_income+ ni_be + ope_be + debt_bev+ cash_lt + lt_ppen + debt_at+ age +
                     (1|Sector),data = SP_TRASE4)
summary(lmm_model16)


shapiro.test(SP_TRASE4$G_score) 
SP_TRASE4$sqrt_G_score<- sqrt(SP_TRASE4$G_score)
lmm_model17<- lmer(sqrt_G_score~ News_count + DF_E+ Commodity + DF_E: Commodity+ 
                     sales + net_income+ ni_be + ope_be + debt_bev+ cash_lt + lt_ppen + debt_at+ age +
                     (1|Sector),data = SP_TRASE4)
summary(lmm_model17)


setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Analysis/Full Model - Effects of deforestation on ESG scores/Full model_updated with ADM and me")
saveRDS(lmm_model14, "SP_ESG score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model15, "SP_E score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model16, "SP_S score - DE + news + financial_2013-2020.rds")
saveRDS(lmm_model17, "SP_G score - DE + news + financial_2013-2020.rds")

#CDP--------------------------------
setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Consolidated data/EX_IM merge_merge JKP financial updated")
CDP_TRASE<- read.csv("CDP_TRASE4_news.csv")

CDP_TRASE2 <- dplyr::select(CDP_TRASE, -Organisation.name, -Issuer_ISIN,-Ticker,-Primary.activity,-Primary.sector, -Primary.industry)

#Check multilinearity
explanatory_vars <- CDP_TRASE2[, c( "News_count","DF_E", "assets", "sales", "book_equity","net_income", "enterprise_value", "ni_be", "ope_be", "debt_bev", "cash_lt","lt_ppen", "debt_at", "age","me")]

vif(explanatory_vars)

explanatory_vars <- CDP_TRASE2[, c( "News_count","DF_E", "sales","net_income", "ni_be", "ope_be", "debt_bev", "cash_lt","lt_ppen", "age")] # all below 3

CDP_TRASE4<- CDP_TRASE2 %>%   mutate(across(c(11,14:28), 
                                            ~ as.numeric(scale(.x)), 
                                            .names = "{.col}"))
CDP_TRASE4$Score <- factor(
  CDP_TRASE4$Score,
  levels = c("E" ,"D-", "D","C", "B-", "B", "A-","A"),
  ordered = TRUE
)

mod <- polr(Score~ News_count + DF_E + Commodity+ DF_E:Commodity+ sales+ net_income+ ni_be + ope_be+ debt_bev+ cash_lt+ lt_ppen+ age, data = CDP_TRASE4, Hess = TRUE)
summary(mod)

#p value
(ctable <- coef(summary(mod)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

#95%CI
(ctable <- cbind(ctable, "p value" = p))
confint.default(mod)


lmm_model18 <- lmer(Score2~ News_count + DF_E + Commodity+ DF_E:Commodity+ sales+ net_income+ ni_be + ope_be+ debt_bev+ cash_lt+ lt_ppen+ age +(1|Sector),data = CDP_TRASE4)
summary(lmm_model18)


# Getting Specific issue scores=============================
#MSCI
setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Data/MSCI_2013-2020")
MSCI_ISSUE<- read.csv("MSCI_key_issue_score.csv")

# Function to check if a value is missing (handles multiple "NA" cases)
is_missing <- function(x) {
  is.null(x) || is.na(x) || is.nan(x)
}

#Whole MSCI issue dataset
# Calculate proportion of non-missing responses for each question
result <- MSCI_ISSUE %>%
  summarise(across(9:23, ~ mean(!sapply(.x, is_missing), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "question", values_to = "Not_NA_proportion")

setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Consolidated data/EX_IM merge")
MSCI_TRASE3<- read.csv("MSCI_TRASE3.csv")

#merge with MSCI-TRASE
MSCI_ISSUE2<- MSCI_ISSUE[,-c(2,3,4,5,7)]
MSCI_ISSUE2$Rating_year<- substr(MSCI_ISSUE2$AS_OF_DATE,1,4)
MSCI_ISSUE2$Rating_year<- as.integer(MSCI_ISSUE2$Rating_year)

colnames(MSCI_ISSUE2)[1]<-  "Issuer_Name"
colnames(MSCI_ISSUE2)[2]<-  "Issuer_ISIN"

MSCI_ISSUE2$Issuer_ISIN <- trimws(MSCI_ISSUE2$Issuer_ISIN)
MSCI_TRASE3$Issuer_ISIN <- trimws(MSCI_TRASE3$Issuer_ISIN)

unique(MSCI_ISSUE2$AS_OF_DATE)

MSCI_ISSUE3 <- MSCI_ISSUE2 %>%
  group_by(Issuer_Name) %>% 
  summarise(across(3:17, ~mean(.x, na.rm = TRUE), .names = "Mean_{.col}"))

MSCI_TRASE4<- left_join(MSCI_TRASE3,MSCI_ISSUE3, by = "Issuer_Name")

#S&P Global
setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Data/S & P Global")
SP_question<-read.csv("SP_question_score.csv")

#assessment year is different
SP_question2<- SP_question %>% group_by(institutionid, questionname,assessmentyear) %>%
  summarise(mean.questionscore = mean(questionscore, na.rm=T))

SP_question2_wide<- pivot_wider(SP_question2, names_from = "questionname", values_from =  "mean.questionscore")
colnames(SP_question2_wide)[2]<- "Rating_year"

# Calculate proportion of non-missing responses for each question
SP_result_q <- SP_question2_wide %>%
  ungroup() %>%  # Remove any grouping
  summarise(across(3:18, ~ mean(!sapply(.x, is_missing), na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "question", values_to = "Not_NA_proportion")

#merge with S&P_TRASE
setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Consolidated data/EX_IM merge")
SP_TRASE3<- read.csv("SP_TRASE3.csv")

SP_question2_wide$institutionid <- trimws(SP_question2_wide$institutionid )
SP_TRASE3$institutionid <- trimws(SP_TRASE3$institutionid)

SP_TRASE4<- left_join(SP_TRASE3,SP_question2_wide, by = c("institutionid","Rating_year"))


#Sustaianlytics
setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Data/Sustainalytics_2018-2023")
Sus_bio <- read_parquet("sus_bio.parquet.gzip")
unique(Sus_bio$FieldId)
Sus_bio$FieldId<- as.character(Sus_bio$FieldId)

Sustain_issue_score_all<- read.csv("Sustain_issue_score_all.csv" )
Sustain_issue_score_all<- Sustain_issue_score_all[!(duplicated(Sustain_issue_score_all)),]
Sustain_issue_score_all$FieldId<- as.character(Sustain_issue_score_all$FieldId)

Sus_bio<- Sus_bio%>% left_join(Sustain_issue_score_all,  by = "FieldId")

Sus_bio_score<- Sus_bio[Sus_bio$FieldName %in% c("E.1.2.1 Biodiversity Programmes-Raw Score-RR" , "E.1.2.9 Deforestation Policy-Raw Score-RR","E.1.2.10 Deforestation Programme-Raw Score-RR" ,"E.1.2.1 Biodiversity Programmes-Weight-Consolidated-RR" ,"E.1.2.1 Biodiversity Programmes-Weighted Score-Consolidated-RR","E.1.2.9 Deforestation Policy-Weight-Consolidated-RR" , "E.1.2.10 Deforestation Programme-Weight-Consolidated-RR","E.1.2.9 Deforestation Policy-Weighted Score-Consolidated-RR","E.1.2.10 Deforestation Programme-Weighted Score-Consolidated-RR"),]

Sus_bio_score4<- Sus_bio_score %>% group_by(EntityId, FieldName)  %>%
  summarise(FieldValue = mean(FieldValue))

Sus_bio_score5<- Sus_bio_score4 %>% pivot_wider(names_from = FieldName, values_from = FieldValue)

#setwd("E:/PhD Research/Chapter 2 - Reanalysis for Nat Sustain/Consolidated data/EX_IM merge")
Sustain_TRASE3<- read.csv("Sustain_TRASE3.csv")

Sus_bio_score5$EntityId <- trimws(Sus_bio_score5$EntityId)
Sustain_TRASE3$EntityId <- trimws(Sustain_TRASE3$Entity_ID)

Sus_bio_score5$EntityId<- as.character(Sus_bio_score5$EntityId)
Sustain_TRASE3$EntityId<- as.character(Sustain_TRASE3$EntityId)


Sustain_TRASE4.0<- left_join(Sustain_TRASE3,Sus_bio_score5, by = c("EntityId"))

#The regression analyses for these datasets are similar as above
