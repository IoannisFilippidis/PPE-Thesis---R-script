
#### Load all Libraries ####

library(modelsummary)
library(fixest)
library(readr)
library(dplyr)
library(plm)
library(purrr)
library(broom)
library(ggplot2)
library(tidyr)
library(scales)
library(knitr)
library(lmtest)
library(sandwich)
library(writexl)

energy_data <- read.csv("Energy_Supply.csv")

energy_data <- energy_data %>%
  mutate(
    Year = as.integer(Year),
    TES_TJ = as.numeric(TES_TJ),
    COAL_TJ = as.numeric(COAL_TJ),
    NG_TJ = as.numeric(NG_TJ),
    HDR_TJ = as.numeric(HDR_TJ),
    NUC_TJ = as.numeric(NUC_TJ),
    REN_TJ = as.numeric(REN_TJ),
    BW_TJ = as.numeric(BW_TJ),
    OIL_TJ = as.numeric(OIL_TJ),
    CO2_MT = as.numeric(CO2_MT),
    Population = as.numeric(Population)
  )


energy_data <- energy_data %>%
  mutate(
    ln_Coal = log(COAL_TJ + 1),
    ln_Oil = log(OIL_TJ + 1),
    ln_Gas = log(NG_TJ + 1),
    ln_Renewables = log(REN_TJ + 1),
    ln_Hydro = log(HDR_TJ + 1),
    ln_Nuclear = log(NUC_TJ + 1),
    ln_Biofuels = log(BW_TJ + 1),
    ln_TES = log(TES_TJ),
    ln_TES_sq = (log(TES_TJ))^2,
    ln_Co2 = log(CO2_MT),
    ln_Population = log(Population),
    share_COAL = 100 * COAL_TJ/ TES_TJ,
    share_OIL  = 100 * OIL_TJ/ TES_TJ,
    share_NG   = 100 * NG_TJ/ TES_TJ,
    share_NUC  = 100 * NUC_TJ/ TES_TJ,
    share_HDR  = 100 * HDR_TJ/ TES_TJ,
    share_REN  = 100 * REN_TJ/ TES_TJ,
    share_BW   = 100 * BW_TJ/ TES_TJ,
    COAL_PJ = COAL_TJ / 1e3,
    OIL_PJ  = OIL_TJ  / 1e3,
    NG_PJ   = NG_TJ   / 1e3,
    NUC_PJ  = NUC_TJ  / 1e3,
    HDR_PJ  = HDR_TJ  / 1e3,
    REN_PJ  = REN_TJ  / 1e3,
    BW_PJ   = BW_TJ   / 1e3,
    TES_PJ    = TES_TJ / 1e3,      
    ln_TES_PJ = log(TES_PJ), 
    COAL_PJ2  = COAL_PJ^2,
    OIL_PJ2   = OIL_PJ^2,
    NG_PJ2    = NG_PJ^2,
    NUC_PJ2   = NUC_PJ^2,
    HDR_PJ2   = HDR_PJ^2,
    REN_PJ2   = REN_PJ^2,
    BW_PJ2    = BW_PJ^2
  )

energy_data <- energy_data %>%
  mutate(
    TES_PJ    = TES_TJ / 1e3,      
    ln_TES_PJ = log(TES_PJ) 
  )

# Join income level
static_income_level <- energy_data %>%
  filter(Year == 2022) %>%
  select(Country, Income_Level) %>%
  distinct()

pdata <- energy_data %>%
  left_join(static_income_level, by = "Country") %>%
  rename(
    Income_Level_dynamic = Income_Level.x,
    Income_Level_2022 = Income_Level.y
  ) %>%
  mutate(
    Income_Level_2022 = as.factor(Income_Level_2022),
    Year = as.numeric(as.character(Year)),
    Time = Year - min(Year) + 1,
    ln_Time = log(Time)
  )

# Convert to pdata.frame
pdata <- pdata.frame(pdata, index = c("Country", "Year"))

# Create grouping and split
pdata$Group_HighIncome <- ifelse(pdata$Income_Level_2022 == "H", "High", "NonHigh")
pdata$Group_HighIncome <- as.factor(pdata$Group_HighIncome)
pdata_split_binary <- split(pdata, pdata$Group_HighIncome)


pdata <- pdata %>%
  mutate(
    sum_shares = share_COAL + share_OIL + share_NG +
      share_NUC  + share_HDR + share_REN + share_BW
  )

pdata <- pdata %>%
  rowwise() %>%
  mutate(
    total_share = sum(c_across(share_COAL:share_BW)),
    share_COAL = share_COAL / total_share * 100,
    share_OIL  = share_OIL  / total_share * 100,
    share_NG   = share_NG   / total_share * 100,
    share_NUC  = share_NUC  / total_share * 100,
    share_HDR  = share_HDR  / total_share * 100,
    share_REN  = share_REN  / total_share * 100,
    share_BW   = share_BW   / total_share * 100,
    sum_shares = 100
  ) %>%
  ungroup()


#### List of Sample Countries per Income Group ####

country_list <- pdata %>%
  select(Country, Income_Level_2022) %>%
  distinct() %>%
  mutate(Income_Group = ifelse(Income_Level_2022 == "H", "High-Income", "Non-High-Income")) %>%
  arrange(Income_Group, Country)

print(country_list)

writexl::write_xlsx(country_list, "Country_Income_Group_List.xlsx")


####  Aggregate TES_PJ by Year and Income Group - Graph ####

tes_grouped_h <- pdata %>%
  group_by(Year, Group_HighIncome) %>%
  summarise(Total_TES_PJ = sum(TES_PJ, na.rm = TRUE), .groups = "drop")

ggplot(tes_grouped_h, aes(x = Year, y = Total_TES_PJ, color = Group_HighIncome)) +
  geom_line(size = 1.4) +
  labs(
    title = "Aggregate Total Energy Supply (TES) by Income Group (2000–2022)",
    x = "Year",
    y = "Total TES (Petajoules)",
    color = "Income Group"
  ) +
  theme_minimal(base_size = 15)


#### Average TES_PJ by Year and Income Group - Graph ####

tes_avg_grouped_h <- pdata %>%
  group_by(Year, Group_HighIncome) %>%
  summarise(Avg_TES_PJ = mean(TES_PJ, na.rm = TRUE), .groups = "drop")

ggplot(tes_avg_grouped_h, aes(x = Year, y = Avg_TES_PJ, color = Group_HighIncome)) +
  geom_line(size = 1.4) +
  labs(
    title = "Average Total Energy Supply (TES) by Income Group (2000–2022)",
    x = "Year",
    y = "Average TES per Country (Petajoules)",
    color = "Income Group"
  ) +
  theme_minimal(base_size = 15)

#### Primary Variables Descriptive Statistics - Table ####

vars1 <- c("TES_TJ", "CO2_MT", "COAL_TJ", "OIL_TJ", "NG_TJ", "NUC_TJ",
          "HDR_TJ", "REN_TJ", "BW_TJ", "Population" )

desc_stats_overall_1 <- energy_data %>%
  summarise(across(all_of(vars1), list(
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

desc_stats_tidy_1 <- desc_stats_overall_1 %>%
  pivot_longer(everything(), names_to = c("Variable", "Stat"),
               names_pattern = "(.*)_(min|max|mean|median|sd)") %>%
  pivot_wider(names_from = Stat, values_from = value)

# Show as table
kable(desc_stats_tidy_1, digits = 2, caption = "Overall Descriptive Statistics for Primary Variables (All Years)")

writexl::write_xlsx(
  list("Primary Vars" = desc_stats_tidy_1),
  path = "Descriptive_Stats_Primary_Variables.xlsx"
)





#### Model Variables Descriptive Statistics - Table ####

vars2 <- c( "COAL_PJ", "OIL_PJ", "NG_PJ", "NUC_PJ", "HDR_PJ", "REN_PJ", "BW_PJ",
  "COAL_PJ2", "OIL_PJ2", "NG_PJ2", "NUC_PJ2", "HDR_PJ2", "REN_PJ2", "BW_PJ2",
  "ln_TES", "ln_TES_sq", "ln_Population",
  "CO2_MT", "ln_Co2",
  "share_COAL", "share_OIL", "share_NG", "share_NUC",  "share_HDR", "share_REN", "share_BW")

desc_stats_overall_2 <- energy_data %>%
  summarise(across(all_of(vars2), list(
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

desc_stats_tidy_2 <- desc_stats_overall_2 %>%
  pivot_longer(everything(), names_to = c("Variable", "Stat"),
               names_pattern = "(.*)_(min|max|mean|median|sd)") %>%
  pivot_wider(names_from = Stat, values_from = value)

kable(desc_stats_tidy_2, digits = 2, caption = "Descriptive Statistics for Model Variables (All Years)")

writexl::write_xlsx(
  list("Model Vars" = desc_stats_tidy_2),
  path = "Descriptive_Stats_Model_Variables.xlsx"
)

#### Model Variables Descriptive Statistics by Income Group - Table ####

desc_stats_by_group <- pdata %>%
  group_by(Group_HighIncome) %>%
  summarise(across(all_of(vars2), list(
    min    = ~min(.,   na.rm = TRUE),
    max    = ~max(.,   na.rm = TRUE),
    mean   = ~mean(.,  na.rm = TRUE),
    median = ~median(.,na.rm = TRUE),
    sd     = ~sd(.,    na.rm = TRUE)
  ), .names = "{.col}_{.fn}"),
  .groups = "drop")

desc_stats_tidy_by_group <- desc_stats_by_group %>%
  pivot_longer(
    -Group_HighIncome,
    names_to     = c("Variable","Stat"),
    names_pattern = "(.*)_(min|max|mean|median|sd)"
  ) %>%
  pivot_wider(
    names_from  = Stat,
    values_from = value
  ) %>%
  arrange(Variable, Group_HighIncome)

desc_stats_tidy_by_group %>%
  kable(
    digits  = 2,
    caption = "Descriptive Statistics for Model Variables by Income Group")

writexl::write_xlsx(
  list("Model Vars by Income Group" = desc_stats_tidy_by_group),
  path = "Descriptive_Stats_Model_Variables_byincomegroup.xlsx"
)


####  Model 0: EKC Baseline Model Year fixed effects - Panel Data #### 

Panel_basic_model <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + factor(Year),
  data = pdata,
  model = "within"
)

# Cluster by Country 

cov_fe_clustered1 <- vcovHC(Panel_basic_model, 
                           method = "arellano",
                           type   = "HC1",        
                           cluster = "group")

coeftest(Panel_basic_model, vcov = cov_fe_clustered1)


# Partial F-test for Year Dummies 

model_noyear <- update(Panel_basic_model,
                       . ~ . - factor(Year))

# b) classical (homoskedastic) F-test

waldtest(model_noyear, Panel_basic_model, test = "F")

#### Model 0: EKC Baseline Model - Linear Time ####

Panel_basic_model_ltime <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + Time,
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

# Random effects model - Hausman Test 

re_model <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + Time,
  data  = pdata,
  index = c("Country", "Year"),
  model = "random"
)

hausman_result <- phtest(Panel_basic_model_ltime, re_model)

# Serial Correlation test - Wooldridge‐style

pbgtest(Panel_basic_model_ltime)

# Heteroskedasticity Test - (Breusch–Pagan)

bptest(Panel_basic_model_ltime)

# Cluster by Country 

cov_fe_clustered <- vcovHC(Panel_basic_model_ltime, 
                           method = "arellano",
                           type   = "HC1",        
                           cluster = "group")

coeftest(Panel_basic_model_ltime, vcov = cov_fe_clustered)


#### Model 0 High-Income: EKC Baseline Model - Linear Time ####

Panel_basic_model_ltimehigh <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + Time,
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

# Cluster by Country 

cov_fe_clusteredhigh <- vcovHC(Panel_basic_model_ltimehigh, 
                           method = "arellano",
                           type   = "HC1",        
                           cluster = "group")

coeftest(Panel_basic_model_ltimehigh, vcov = cov_fe_clusteredhigh)

#### Model 0 High-Income: EKC Baseline Model - fixed year effects ####

Panel_basic_modelhigh <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + factor(Year),
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

# Cluster by Country 

cov_fe_clusteredhighf <- vcovHC(Panel_basic_modelhigh, 
                               method = "arellano",
                               type   = "HC1",        
                               cluster = "group")

coeftest(Panel_basic_modelhigh, vcov = cov_fe_clusteredhighf)


#### Fixed year effects Graph - EKC TES model Robustness - High-Income ####

coef_table <- tidy(Panel_basic_modelhigh, conf.int = TRUE)

year_effects_high <- coef_table %>%
  filter(grepl("^factor\\(Year\\)", term)) %>%
  mutate(Year = as.numeric(gsub("factor\\(Year\\)", "", term)))

ggplot(year_effects_high, aes(x = Year, y = estimate)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "grey50") +
  labs(title = "Estimated Year Fixed Effects (High-Income Countries)",
       x = "Year",
       y = "Year Effect on ln(CO2)") +
  theme_minimal(base_size = 14)


#### Model 0 Developing: EKC Baseline Model - Linear Time ####

Panel_basic_model_ltimenonhigh <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + Time,
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

# Cluster by Country 

cov_fe_clusterednonhigh <- vcovHC(Panel_basic_model_ltimenonhigh, 
                               method = "arellano",
                               type   = "HC1",        
                               cluster = "group")

coeftest(Panel_basic_model_ltimenonhigh, vcov = cov_fe_clusterednonhigh)

#### Turning point: model 0 Developing / linear time #### 

coef_valsld <- coef(Panel_basic_model_ltimenonhigh)
beta1nh <- coef_valsld["ln_TES"]
beta2nh <- coef_valsld["ln_TES_sq"]

ln_turning_pointd <- -beta1nh / (2 * beta2nh)

TES_turning_pointd <- exp(ln_turning_pointd)

ln_turning_pointd
TES_turning_pointd


#### Model 0 Developing: EKC Baseline Model - fixed year effects  ####

Panel_basic_model_nonhighf <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + factor(Year),
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

# Cluster by Country 

cov_fe_clusterednonhighf <- vcovHC(Panel_basic_model_nonhighf, 
                                  method = "arellano",
                                  type   = "HC1",        
                                  cluster = "group")

coeftest(Panel_basic_model_nonhighf, vcov = cov_fe_clusterednonhighf)


#### Fixed Year Effects Graph - EKC TES Model Robustness - Developing ####

coef_table <- tidy(Panel_basic_model_nonhighf, conf.int = TRUE)

year_effects_nonhigh <- coef_table %>%
  filter(grepl("^factor\\(Year\\)", term)) %>%
  mutate(Year = as.numeric(gsub("factor\\(Year\\)", "", term)))

ggplot(year_effects_nonhigh, aes(x = Year, y = estimate)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "grey50") +
  labs(title = "Estimated Year Fixed Effects (Non High-Income Countries)",
       x = "Year",
       y = "Year Effect on ln(CO2)") +
  theme_minimal(base_size = 14)



#### Turning Point: model 0 developing / fixed year effects ####

coef_valsdf <- coef(Panel_basic_model_nonhighf)
beta1nhf <- coef_valsdf["ln_TES"]
beta2nhf <- coef_valsdf["ln_TES_sq"]

ln_turning_pointdf <- -beta1nhf / (2 * beta2nhf)

TES_turning_pointdf <- exp(ln_turning_pointdf)

ln_turning_pointdf
TES_turning_pointdf

#### Country Fixed Effects Model 0 - Panel Data two-way fixed effects and Income Groups ####

fe_full <- plm::fixef(Panel_basic_model)

high_list <- names(plm::fixef(Panel_basic_modelhigh))

fe_df_full <- tibble(
  Country     = names(fe_full),
  FixedEffect = as.numeric(fe_full)
) %>%
  mutate(
    Group = if_else(Country %in% high_list, "High", "NonHigh"))

fe_summary <- fe_df_full %>%
  group_by(Group) %>%
  summarise(Mean_FE = mean(FixedEffect))

ggplot(fe_summary, aes(Group, Mean_FE, fill = Group)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Income Group", y = "Mean Fixed Effect") +
  theme_minimal()

ggplot(fe_df_full, aes(reorder(Country, FixedEffect), FixedEffect, fill = Group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Group, scales="free_y") +
  coord_flip() +
  labs(title = "Country-Level Fixed Effects by Income Group",x = "Country", y = "Country Fixed Effect") +
  theme_minimal(base_size = 12)

# Panel Data countries no income groups 

fe_df_all <- data.frame(
  Country     = names(fe_full),
  FixedEffect = as.numeric(fe_full),
  stringsAsFactors = FALSE
)

#Plot all country fixed effects
ggplot(fe_df_all, aes(x = reorder(Country, FixedEffect), y = FixedEffect)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Country Fixed Effects from Pooled Two-Way FE Model",
    x     = "Country",
    y     = "Fixed Effect (log CO₂)"
  ) +
  theme_minimal(base_size = 10)



#### Robustness with population Fixed year effects ####  

Panel_basic_modelRob <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + ln_Population +  factor(Year),
  data = pdata,
  model = "within"
)

# Cluster by Country 

cov_fe_clusteredPf <- vcovHC(Panel_basic_modelRob, 
                            method = "arellano",
                            type   = "HC1",        
                            cluster = "group")

coeftest(Panel_basic_modelRob, vcov = cov_fe_clustered)


#### Robustness with population linear time ####

Panel_basic_model_ltime_Rob <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + ln_Population + Time,
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

# Cluster by Country 

cov_fe_clusteredP <- vcovHC(Panel_basic_model_ltime_Rob, 
                           method = "arellano",
                           type   = "HC1",        
                           cluster = "group")

coeftest(Panel_basic_model_ltime_Rob, vcov = cov_fe_clustered)

#### Robustness with population linear time - High Income  ####

Panel_basic_model_ltime_Robhigh <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + ln_Population + Time,
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

# Cluster by Country 

cov_fe_clusteredPh <- vcovHC(Panel_basic_model_ltime_Robhigh, 
                            method = "arellano",
                            type   = "HC1",        
                            cluster = "group")

coeftest(Panel_basic_model_ltime_Robhigh, vcov = cov_fe_clusteredPh)


#### Robustness with population linear time - Developing  ####

Panel_basic_model_ltime_Robnonhigh <- plm(
  ln_Co2 ~ ln_TES + ln_TES_sq + ln_Population + Time,
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

# Cluster by Country 

cov_fe_clusteredPnh <- vcovHC(Panel_basic_model_ltime_Robnonhigh, 
                             method = "arellano",
                             type   = "HC1",        
                             cluster = "group")

coeftest(Panel_basic_model_ltime_Robnonhigh, vcov = cov_fe_clusteredPnh)

#### Turning Point Rob model Population linear time / Developing ####

coef_valsrobpd <- coef(Panel_basic_model_ltime_Robnonhigh)
beta1bnh <- coef_valsrobpd["ln_TES"]
beta2bnh <- coef_valsrobpd["ln_TES_sq"]

ln_turning_pointrobpd <- -beta1bnh / (2 * beta2bnh)

TES_turning_pointrobpd <- exp(ln_turning_pointrobpd)

ln_turning_pointrobpd
TES_turning_pointrobpd



#### Squared Disaggregated Energy Poisson Regression model PJ /  Panel Data  ####

model_quad_pj <- fepois(
  CO2_MT ~ COAL_PJ + COAL_PJ2 +
    OIL_PJ  + OIL_PJ2  +
    NG_PJ   + NG_PJ2   +
    NUC_PJ  + NUC_PJ2  +
    HDR_PJ  + HDR_PJ2  +
    REN_PJ  + REN_PJ2  +
    BW_PJ   + BW_PJ2
  | Country + Year,
  data    = pdata
)

#### Squared Disaggregated Energy Poisson Regression model PJ fixed year effects / High Income ####

model_quad_pj_high <- fepois(
  CO2_MT ~ COAL_PJ + COAL_PJ2 +
    OIL_PJ  + OIL_PJ2  +
    NG_PJ   + NG_PJ2   +
    NUC_PJ  + NUC_PJ2  +
    HDR_PJ  + HDR_PJ2  +
    REN_PJ  + REN_PJ2  +
    BW_PJ   + BW_PJ2
  | Country + Year,
  data = pdata.frame(pdata_split_binary[["High"]])
)

#### Energy Sources Turning Points / High Income ####

coefs_quad_pj_high <- coef(model_quad_pj_high)

turning_points_quad_pj_high <- data.frame(
  Energy_Source = c("Coal", "Oil", "Natural Gas", "Nuclear", "Hydro", "Renewables", "Biofuels"),
  Turning_Point = c(
    - coefs_quad_pj_high["COAL_PJ"] / (2 * coefs_quad_pj_high["COAL_PJ2"]),
    - coefs_quad_pj_high["OIL_PJ"]  / (2 * coefs_quad_pj_high["OIL_PJ2"]),
    - coefs_quad_pj_high["NG_PJ"]   / (2 * coefs_quad_pj_high["NG_PJ2"]),
    - coefs_quad_pj_high["NUC_PJ"]  / (2 * coefs_quad_pj_high["NUC_PJ2"]),
    - coefs_quad_pj_high["HDR_PJ"]  / (2 * coefs_quad_pj_high["HDR_PJ2"]),
    - coefs_quad_pj_high["REN_PJ"]  / (2 * coefs_quad_pj_high["REN_PJ2"]),
    - coefs_quad_pj_high["BW_PJ"]   / (2 * coefs_quad_pj_high["BW_PJ2"])
  ))

print(turning_points_quad_pj_high)


#### Squared Disaggregated Energy Poisson Regression model PJ fixed year effects / Developing ####

model_quad_pj_dev <- fepois(
  CO2_MT ~ COAL_PJ + COAL_PJ2 +
    OIL_PJ  + OIL_PJ2  +
    NG_PJ   + NG_PJ2   +
    NUC_PJ  + NUC_PJ2  +
    HDR_PJ  + HDR_PJ2  +
    REN_PJ  + REN_PJ2  +
    BW_PJ   + BW_PJ2
  | Country + Year,
  data = pdata.frame(pdata_split_binary[["NonHigh"]])
)

#### Energy Sources Turning Points / Developing Countries ####

coefs_quad_pj_dev <- coef(model_quad_pj_dev)

turning_points_quad_pj_dev <- data.frame(
  Energy_Source = c("Coal", "Oil", "Natural Gas", "Nuclear", "Hydro", "Renewables", "Biofuels"),
  Turning_Point = c(
    - coefs_quad_pj_dev["COAL_PJ"] / (2 * coefs_quad_pj_dev["COAL_PJ2"]),
    - coefs_quad_pj_dev["OIL_PJ"]  / (2 * coefs_quad_pj_dev["OIL_PJ2"]),
    - coefs_quad_pj_dev["NG_PJ"]   / (2 * coefs_quad_pj_dev["NG_PJ2"]),
    - coefs_quad_pj_dev["NUC_PJ"]  / (2 * coefs_quad_pj_dev["NUC_PJ2"]),
    - coefs_quad_pj_dev["HDR_PJ"]  / (2 * coefs_quad_pj_dev["HDR_PJ2"]),
    - coefs_quad_pj_dev["REN_PJ"]  / (2 * coefs_quad_pj_dev["REN_PJ2"]),
    - coefs_quad_pj_dev["BW_PJ"]   / (2 * coefs_quad_pj_dev["BW_PJ2"])
  ))

print(turning_points_quad_pj_dev)



# BW dropped fixed year effects 

model_share_energy_noBW <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_HDR + share_REN + factor(Year),
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)


#### BW dropped linear time ####

model_share_energy_noBWlt <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_HDR + share_REN + Time,
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

# Cluster Standard errors by Country

cov_fe_clusteredBW <- vcovHC(model_share_energy_noBWlt, cluster = "group", type = "HC1")
coeftest(model_share_energy_noBWlt, cov_fe_clusteredBW)



#### BW dropped linear time High ####

model_share_energy_noBWlth <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_HDR + share_REN + Time,
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredBWh <- vcovHC(model_share_energy_noBWlth, cluster = "group", type = "HC1")
coeftest(model_share_energy_noBWlth, cov_fe_clusteredBWh)


#### BW dropped linear time NonHigh ####

model_share_energy_noBWltd <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_HDR + share_REN + Time,
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredBWltd <- vcovHC(model_share_energy_noBWltd, cluster = "group", type = "HC1")
coeftest(model_share_energy_noBWltd, cov_fe_clusteredBWltd)



# OIL dropped Fixed Year effects - Panel Data

model_share_energy_noOIL <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_NG +
    share_NUC + share_HDR + share_REN + share_BW + factor(Year),
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)


# OIL dropped Linear time - Panel Data

model_share_energy_noOILlt <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_NG +
    share_NUC + share_HDR + share_REN + share_BW + Time ,
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

#### OIL dropped linear time High ####

model_share_energy_noOILlth <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_NG +
    share_NUC + share_HDR + share_REN + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredOILlth <- vcovHC(model_share_energy_noOILlth, cluster = "group", type = "HC1")
coeftest(model_share_energy_noOILlth, cov_fe_clusteredOILlth)

#### OIL dropped linear time Non-High ####

model_share_energy_noOILltd <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_NG +
    share_NUC + share_HDR + share_REN + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredOILltd <- vcovHC(model_share_energy_noOILltd, cluster = "group", type = "HC1")
coeftest(model_share_energy_noOILltd, cov_fe_clusteredOILltd)

# NG dropped fixed year effects 

model_share_energy_noNG <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL +
    share_NUC + share_HDR + share_REN + share_BW + factor(Year),
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)


# NG dropped linear time 

model_share_energy_noNGlt <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL +
    share_NUC + share_HDR + share_REN + share_BW + Time ,
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)


#### NG dropped linear time High ####

model_share_energy_noNGlth <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL +
    share_NUC + share_HDR + share_REN + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredNGlth <- vcovHC(model_share_energy_noNGlth, cluster = "group", type = "HC1")
coeftest(model_share_energy_noNGlth, cov_fe_clusteredNGlth)


#### NG dropped linear time Non-high ####

model_share_energy_noNGltd <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL +
    share_NUC + share_HDR + share_REN + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredNGltd <- vcovHC(model_share_energy_noNGltd, cluster = "group", type = "HC1")
coeftest(model_share_energy_noNGltd, cov_fe_clusteredNGltd)

# REN dropped fixed year effects 

model_share_energy_noREN <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_HDR + share_BW + factor(Year),
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

# REN dropped Linear time 

model_share_energy_noRENlt <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_HDR + share_BW + Time ,
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

#### REN dropped linear time High ####

model_share_energy_noRENlth <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_HDR + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredRENlth <- vcovHC(model_share_energy_noRENlth, cluster = "group", type = "HC1")
coeftest(model_share_energy_noRENlth, cov_fe_clusteredRENlth)

#### REN dropped linear time Non-High ####

model_share_energy_noRENltd <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_HDR + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredRENltd <- vcovHC(model_share_energy_noRENltd, cluster = "group", type = "HC1")
coeftest(model_share_energy_noRENltd, cov_fe_clusteredRENltd)

# COAL dropped fixed year effects 

model_share_energy_noCOAL <- plm(
  ln_Co2 ~ ln_TES + share_OIL + share_NG +
    share_NUC + share_REN + share_HDR + share_BW + factor(Year),
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

# Coal dropped linear time 

model_share_energy_noCOALlt <- plm(
  ln_Co2 ~ ln_TES + share_OIL + share_NG +
    share_NUC + share_REN + share_HDR + share_BW + Time ,
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

#### COAL dropped linear time - High ####

model_share_energy_noCOALlth <- plm(
  ln_Co2 ~ ln_TES + share_OIL + share_NG +
    share_NUC + share_REN + share_HDR + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredCOALlth <- vcovHC(model_share_energy_noCOALlth, cluster = "group", type = "HC1")
coeftest(model_share_energy_noCOALlth, cov_fe_clusteredCOALlth)

#### COAL dropped linear time - NonHigh ####

model_share_energy_noCOALltd <- plm(
  ln_Co2 ~ ln_TES + share_OIL + share_NG +
    share_NUC + share_REN + share_HDR + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredCOALltd <- vcovHC(model_share_energy_noCOALltd, cluster = "group", type = "HC1")
coeftest(model_share_energy_noCOALltd, cov_fe_clusteredCOALltd)



# HDR dropped fixed year effects 

model_share_energy_noHDR <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_REN + share_BW + factor(Year),
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

# HDR dropped linear time 

model_share_energy_noHDRlt <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_REN + share_BW + Time ,
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

#### HDR dropped linear time - High ####

model_share_energy_noHDRlth <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_REN + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredHDRlth <- vcovHC(model_share_energy_noHDRlth, cluster = "group", type = "HC1")
coeftest(model_share_energy_noHDRlth, cov_fe_clusteredHDRlth)

#### HDR dropped linear time - Non-High ####

model_share_energy_noHDRltd <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_NUC + share_REN + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredHDRltd <- vcovHC(model_share_energy_noHDRltd, cluster = "group", type = "HC1")
coeftest(model_share_energy_noHDRltd, cov_fe_clusteredHDRltd)

# NUC dropped linear time

model_share_energy_noNUClt <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
  share_REN + share_HDR + share_BW + Time ,
  data = pdata,
  index = c("Country", "Year"),
  model = "within"
)

#### NUC dropped linear time - High ####

model_share_energy_noNUClth <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
  share_REN + share_HDR + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["High"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredNUClth <- vcovHC(model_share_energy_noNUClth, cluster = "group", type = "HC1")
coeftest(model_share_energy_noNUClth, cov_fe_clusteredNUClth)

#### NUC dropped linear time - Non-High ####

model_share_energy_noNUCltd <- plm(
  ln_Co2 ~ ln_TES + share_COAL + share_OIL + share_NG +
    share_REN + share_HDR + share_BW + Time,
  data = pdata.frame(pdata_split_binary[["NonHigh"]], index = c("Country", "Year")),
  model = "within"
)

cov_fe_clusteredNUCltd <- vcovHC(model_share_energy_noNUCltd, cluster = "group", type = "HC1")
coeftest(model_share_energy_noNUCltd, cov_fe_clusteredNUCltd)



