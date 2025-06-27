## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

library(psych)
library(stats)
library(knitr)
library(VIM)
library(dplyr)
library(mice)
library(miceadds)
library(finalfit)
library(naniar)
library(broom)
library(purrr)
library(dplyr)
library(ggplot2)
library(forcats)
library(tibble)
library(stringr)


## ----Load data----------------------------------------------------------------

data <- read.csv("../Data/cleanedData_all.csv")


## ----Subset dataset-----------------------------------------------------------

sub_data <- data[, c("cidB3980", "r9020", "kz021","c804", "a_happ_5g", "b_bore_5g", "c_reli_5g", "d_hope_5g","e_exci_5g", "f_fail_5g", "g_lone_5g", "h_sad_5g", "i_luck_5g", "j_rela_5g", "c645a", "psychosis1_14", "psychosis1_16", "psychosis2_14", "psychosis2_16", "psychosis_12", "psychosis_14", "psychosis_16")]


# rename
sub_data <- rename(sub_data, ID = cidB3980)
sub_data <- rename(sub_data, WeeklyIncome = r9020)
sub_data <- rename(sub_data, Sex = kz021)
sub_data <- rename(sub_data, Ethnicity = c804)


# Code as NA
sub_data <- sub_data %>% 
  mutate(
         a_happ_5g = na_if(a_happ_5g, "Consent withdrawn by YP"),
         b_bore_5g = na_if(b_bore_5g, "Consent withdrawn by YP"),
         c_reli_5g = na_if(c_reli_5g, "Consent withdrawn by YP"),
         d_hope_5g = na_if(d_hope_5g, "Consent withdrawn by YP"),
         e_exci_5g = na_if(e_exci_5g, "Consent withdrawn by YP"),
         f_fail_5g = na_if(f_fail_5g, "Consent withdrawn by YP"),
         g_lone_5g = na_if(g_lone_5g, "Consent withdrawn by YP"),
         h_sad_5g = na_if(h_sad_5g, "Consent withdrawn by YP"),
         i_luck_5g = na_if(i_luck_5g, "Consent withdrawn by YP"),
         j_rela_5g = na_if(j_rela_5g, "Consent withdrawn by YP")
         )

# Turn weekly income into factor
sub_data$WeeklyIncome <- factor(
  sub_data$WeeklyIncome,
  levels = c(
    "< £120",
    "£120 - £189",
    "£190 - £239",
    "£240 - £289",
    "£290 - £359",
    "£360 - £429",
    "£430 - £479",
    "£480 - £559",
    "£560 - £799",
    "£800 or more", 
    "Don't know"
  ),
  ordered = TRUE
)


# Remove participants with missing AMS values
AMS_vars <- c("a_happ_5g", "b_bore_5g", "c_reli_5g", "d_hope_5g",
                  "e_exci_5g", "f_fail_5g", "g_lone_5g", "h_sad_5g",
                  "i_luck_5g", "j_rela_5g")

# Filter out rows with missing values in any of the AMS_vars
sub_data <- sub_data[complete.cases(sub_data[, AMS_vars]), ]


## ----Sample characteristics---------------------------------------------------
factor_vars <- c("Sex", "Ethnicity", "c645a")
sub_data <- sub_data %>%
  mutate(across(all_of(factor_vars), as.factor))

sex_summary <- sub_data %>%
  count(Sex) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2))
sex_summary

ethnicity_summary <- sub_data %>%
  count(Ethnicity) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2))
ethnicity_summary

c645a_summary <- sub_data %>%
  count(c645a) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2))
c645a_summary

income_summary <- sub_data %>%
  count(WeeklyIncome) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2))
income_summary

sub_data <- sub_data %>% mutate(MaternalHighEd = ifelse(c645a == "A level" | c645a == "Degree", 1, 0))





## ----AMS descriptives---------------------------------------------------------


data1 <- sub_data %>%
  mutate(PosAMS = rowSums(select(.,
    c(
      "a_happ_5g",
      "c_reli_5g",
      "e_exci_5g",
      "i_luck_5g",
      "j_rela_5g"
    )
  ) == "4. specific", na.rm = FALSE)) %>%
  mutate(NegAMS = rowSums(select(.,
    c("b_bore_5g", "d_hope_5g", "f_fail_5g", "g_lone_5g", "h_sad_5g")
  ) == "4. specific", na.rm = FALSE)) %>%
  mutate(AMStotal = rowSums(select(.,
    c(
      "a_happ_5g",
      "b_bore_5g",
      "c_reli_5g",
      "d_hope_5g",
      "e_exci_5g",
      "f_fail_5g",
      "g_lone_5g",
      "h_sad_5g",
      "i_luck_5g",
      "j_rela_5g"
    )
  ) == "4. specific", na.rm = FALSE))

AMS <- data1[, c("AMStotal", "NegAMS", "PosAMS")]
describe(AMS)


## ----Psychosis descriptives---------------------------------------------------

psych <- data1[, c("ID","psychosis1_14", "psychosis1_16", "psychosis2_14", "psychosis2_16", "psychosis_12", "psychosis_14", "psychosis_16")]
psych_data <- psych %>% summary()
psych_data



## ----Subset-------------------------------------------------------------------

full_data <- data1 %>%
  select(-a_happ_5g, -b_bore_5g, -c_reli_5g, -d_hope_5g, -e_exci_5g, -f_fail_5g, -g_lone_5g, -h_sad_5g, -i_luck_5g, -j_rela_5g, -c645a)


## ----Model diagnostics function-----------------------------------------------


logit_diagnostics <- function(model, data, outcome_var) {
  library(car)
  library(ResourceSelection)
  library(pROC)
  library(dplyr)
  library(ggplot2)

  cat("---- Variance Inflation Factor (VIF) ----\n")
  print(vif(model))

  cat("\n---- Hosmer-Lemeshow Goodness-of-Fit ----\n")
  # Only use rows used in the model
  model_data <- model.frame(model)
  hl_test <- hoslem.test(model_data[[outcome_var]], fitted(model), g = 10)
  print(hl_test)

  cat("\n---- Influential Observations (Cook's Distance) ----\n")
  cooks_d <- cooks.distance(model)
  influential <- which(cooks_d > (4 / nrow(model_data)))
  cat("Number of influential points (Cook's D > 4/n):", length(influential), "\n")
  if (length(influential) > 0) {
    cat("Influential observation indices:", influential, "\n")
  }

  cat("\n---- ROC Curve and AUC ----\n")
  # Ensure we have complete cases for outcome and predictions
  pred_probs <- fitted(model)
  complete_cases <- complete.cases(model_data[[outcome_var]], pred_probs)
  roc_obj <- roc(model_data[[outcome_var]][complete_cases], pred_probs[complete_cases])
  print(roc_obj)
  plot(roc_obj, col = "blue", main = paste("ROC Curve:", outcome_var))
  cat("AUC:", auc(roc_obj), "\n")
}

## ----Logistic Regression - Psychosis 14 ~ AMS total---------------------------

model1 <- glm(psychosis_14 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model1) 

logistic_coefficients_model1 <- 
  model1$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model1 %>% 
  kable(digits = 3)

exp(coef(model1))
exp(cbind(Odds_Ratio = coef(model1), confint(model1)))

model1_diagnostics <- logit_diagnostics(model1, full_data, "psychosis_14")


## ----Logistic Regression - Psychosis 16 ~ AMS total---------------------------

model2 <- glm(psychosis_16 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model2) 

logistic_coefficients_model2 <- 
  model2$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model2 %>% 
  kable(digits = 3)

exp(coef(model2))
exp(cbind(Odds_Ratio = coef(model2), confint(model2)))

model2_diagnostics <- logit_diagnostics(model2, full_data, "psychosis_16")



## ----Logistic Regression - Hallucinations 14 ~ AMS total *--------------------

model3 <- glm(psychosis1_14 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model3) 

logistic_coefficients_model3 <- 
  model3$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model3 %>% 
  kable(digits = 3)

exp(coef(model3))
exp(cbind(Odds_Ratio = coef(model3), confint(model3)))

model3_diagnostics <- logit_diagnostics(model3, full_data, "psychosis1_14")


## ----Logistic Regression - Hallucinations 16 ~ AMS total----------------------

model4 <- glm(psychosis1_16 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model4) 

logistic_coefficients_model4 <- 
  model4$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model4 %>% 
  kable(digits = 3)

exp(coef(model4))
exp(cbind(Odds_Ratio = coef(model4), confint(model4)))

model4_diagnostics <- logit_diagnostics(model4, full_data, "psychosis1_16")


## ----Logistic Regression - Delusions 14 ~ AMS total---------------------------

model5 <- glm(psychosis2_14 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model5) 

logistic_coefficients_model5 <- 
  model5$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model5 %>% 
  kable(digits = 3)

exp(coef(model5))
exp(cbind(Odds_Ratio = coef(model5), confint(model5)))

model5_diagnostics <- logit_diagnostics(model5, full_data, "psychosis2_14")



## ----Logistic Regression - Delusions 16 ~ AMS total---------------------------

model6 <- glm(psychosis2_16 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model6) 

logistic_coefficients_model6 <- 
  model6$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model6 %>% 
  kable(digits = 3)

exp(coef(model6))
exp(cbind(Odds_Ratio = coef(model6), confint(model6)))

model6_diagnostics <- logit_diagnostics(model6, full_data, "psychosis2_16")

 

## ----Logistic Regression - Psychosis 14 ~ Pos AMS-----------------------------

model7 <- glm(psychosis_14 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model7) 

logistic_coefficients_model7 <- 
  model7$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model7 %>% 
  kable(digits = 3)

exp(coef(model7))
exp(cbind(Odds_Ratio = coef(model7), confint(model7)))

model7_diagnostics <- logit_diagnostics(model7, full_data, "psychosis_14")


## ----Logistic Regression - Psychosis 14 ~ Neg AMS-----------------------------

model8 <- glm(psychosis_14 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model8) 

logistic_coefficients_model8 <- 
  model8$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model8 %>% 
  kable(digits = 3)

exp(coef(model8))
exp(cbind(Odds_Ratio = coef(model8), confint(model8)))

model8_diagnostics <- logit_diagnostics(model8, full_data, "psychosis_14")


## ----Logistic Regression - Psychosis 16 ~ Pos AMS-----------------------------

model9 <- glm(psychosis_16 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model9) 

logistic_coefficients_model9 <- 
  model9$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model9 %>% 
  kable(digits = 3)

exp(coef(model9))
exp(cbind(Odds_Ratio = coef(model9), confint(model9)))

model9_diagnostics <- logit_diagnostics(model9, full_data, "psychosis_16")


## ----Logistic Regression - Psychosis 16 ~ Neg AMS-----------------------------

model10 <- glm(psychosis_16 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model10) 

logistic_coefficients_model10 <- 
  model10$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model10 %>% 
  kable(digits = 3)

exp(coef(model10))
exp(cbind(Odds_Ratio = coef(model10), confint(model10)))

model10_diagnostics <- logit_diagnostics(model10, full_data, "psychosis_16")


## ----Logistic Regression - Hallucinations 14 ~ Pos AMS------------------------

model11 <- glm(psychosis1_14 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model11) 

logistic_coefficients_model11 <- 
  model11$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model11 %>% 
  kable(digits = 3)

exp(coef(model11))
exp(cbind(Odds_Ratio = coef(model11), confint(model11)))

model11_diagnostics <- logit_diagnostics(model11, full_data, "psychosis1_14")


## ----Logistic Regression - Hallucinations 14 ~ Neg AMS *----------------------

model12 <- glm(psychosis1_14 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model12) 

logistic_coefficients_model12 <- 
  model12$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model12 %>% 
  kable(digits = 3)

exp(coef(model12))
exp(cbind(Odds_Ratio = coef(model12), confint(model12)))

model12_diagnostics <- logit_diagnostics(model12, full_data, "psychosis1_14")


## ----Logistic Regression - Hallucinations 16 ~ Pos AMS------------------------

model13 <- glm(psychosis1_16 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model13) 

logistic_coefficients_model13 <- 
  model13$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model13 %>% 
  kable(digits = 3)

exp(coef(model13))
exp(cbind(Odds_Ratio = coef(model13), confint(model13)))

model13_diagnostics <- logit_diagnostics(model13, full_data, "psychosis1_16")


## ----Logistic Regression - Hallucinations 16 ~ Neg AMS------------------------

model14 <- glm(psychosis1_16 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model14) 

logistic_coefficients_model14 <- 
  model14$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model14 %>% 
  kable(digits = 3)

exp(coef(model14))
exp(cbind(Odds_Ratio = coef(model14), confint(model14)))

model14_diagnostics <- logit_diagnostics(model14, full_data, "psychosis1_16")


## ----Logistic Regression - Delusions 14 ~ Pos AMS-----------------------------

model15 <- glm(psychosis2_14 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model15) 

logistic_coefficients_model15 <- 
  model15$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model15 %>% 
  kable(digits = 3)

exp(coef(model15))
exp(cbind(Odds_Ratio = coef(model15), confint(model15)))

model15_diagnostics <- logit_diagnostics(model15, full_data, "psychosis2_14")


## ----Logistic Regression - Delusions 14 ~ Neg AMS-----------------------------

model16 <- glm(psychosis2_14 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model16) 

logistic_coefficients_model16 <- 
  model16$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model16 %>% 
  kable(digits = 3)

exp(coef(model16))
exp(cbind(Odds_Ratio = coef(model16), confint(model16)))

model16_diagnostics <- logit_diagnostics(model16, full_data, "psychosis2_14")
  

## ----Logistic Regression - Delusions 16 ~ Pos AMS-----------------------------

model17 <- glm(psychosis2_16 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model17) 

logistic_coefficients_model17 <- 
  model17$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model17 %>% 
  kable(digits = 3)

exp(coef(model17))
exp(cbind(Odds_Ratio = coef(model17), confint(model17)))

model17_diagnostics <- logit_diagnostics(model17, full_data, "psychosis2_16")


## ----Logistic Regression - Delusions 16 ~ Neg AMS-----------------------------

model18 <- glm(psychosis2_16 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
summary(model18) 

logistic_coefficients_model18 <- 
  model18$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model18 %>% 
  kable(digits = 3)

exp(coef(model18))
exp(cbind(Odds_Ratio = coef(model18), confint(model18)))

model18_diagnostics <- logit_diagnostics(model18, full_data, "psychosis2_16")


## ----Plots - psychosis 14-----------------------------------------------------

models <- list(
  "AMStotal" = glm(psychosis_14 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial),
  "PosAMS"   = glm(psychosis_14 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial),
  "NegAMS"   = glm(psychosis_14 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
)

# Extract ORs, CIs, and p-values 
or_table <- map_dfr(models, function(model) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    slice(2)  
}, .id = "IV")

or_summary <- or_table %>%
  mutate(Significant = p.value < 0.05) %>%
  select(IV, OR = estimate, CI_lower = conf.low, CI_upper = conf.high, p.value, Significant)

# Plot
ggplot(or_summary, aes(x = IV, y = OR, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "gray50")) +
  ylim(0.5, 1.5) +  
  ylab("Odds Ratio") +
  xlab("Autobiographical Memory Specificity (AMS)") +
  ggtitle("Presence of Psychotic Episode at age 14") +
  theme_minimal()

## ----Plots - psychosis 16-----------------------------------------------------

models <- list(
  "AMStotal" = glm(psychosis_16 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial),
  "PosAMS"   = glm(psychosis_16 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial),
  "NegAMS"   = glm(psychosis_16 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, data = full_data, family = binomial)
)

# Extract ORs, CIs, and p-values 
or_table <- map_dfr(models, function(model) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    slice(2)  
}, .id = "IV")

or_summary <- or_table %>%
  mutate(Significant = p.value < 0.05) %>%
  select(IV, OR = estimate, CI_lower = conf.low, CI_upper = conf.high, p.value, Significant)

# Plot
ggplot(or_summary, aes(x = IV, y = OR, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "gray50")) +
  ylim(0.5, 1.5) +  
  ylab("Odds Ratio") +
  xlab("Autobiographical Memory Specificity (AMS)") +
  ggtitle("Presence of Psychotic Episode at age 16") +
  theme_minimal()

## ----Check whether data is MCAR/MNAR------------------------------------------

aggr_plot <-
  aggr(
    full_data,
    col = c('skyblue', 'lightpink'),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(full_data),
    cex.axis = .7,
    gap = 3,
    ylab = c("Histogram of missing data", "Pattern")
  )

naniar::prop_miss(full_data)
#md.pattern(full_data) # N=2157 samples complete

# 41% missing psychosis_16
# 34% missing psychosis_14

# MAR?
explanatory = c("Sex", "Ethnicity", "MaternalHighEd", "WeeklyIncome", "AMStotal", "psychosis_12")
dependent = "psychosis_14"
full_data %>% 
  missing_pairs(dependent, explanatory)

# Recode variables 
data_MAR <- full_data %>% 
  mutate(naPsych = if_else(is.na(psychosis_14), 0, 1))


amsMAR <- glm(naPsych ~ AMStotal, data = data_MAR, family = binomial)
summary_model <- summary(amsMAR)
amsMAR_pvals <- summary_model$coefficients["AMStotal", "Pr(>|z|)"]
amsMAR_pvals

exp(coef(amsMAR))
exp(cbind(Odds_Ratio = coef(amsMAR), confint(amsMAR)))

Anova(amsMAR, type="II", test="Wald")




## ----Multiple imputation------------------------------------------------------
imp_data <- full_data %>%
  select(-ID)

# Reduce multicollinearity in dataset before imputing
# Create predictor matrix
pred_matrix <- make.predictorMatrix(imp_data)

# Prevent psychosis subscales from predicting each other 
pred_matrix["psychosis1_14", "psychosis2_14"] <- 0
pred_matrix["psychosis2_14", "psychosis1_14"] <- 0

pred_matrix["psychosis1_16", "psychosis2_16"] <- 0
pred_matrix["psychosis2_16", "psychosis1_16"] <- 0

# Prevent subscales from predicting overall psychosis score
pred_matrix["psychosis_14", "psychosis1_14"] <- 0
pred_matrix["psychosis_14", "psychosis2_14"] <- 0

pred_matrix["psychosis_16", "psychosis1_16"] <- 0
pred_matrix["psychosis_16", "psychosis2_16"] <- 0

# Prevent AMStotal and its components from predicting each other
pred_matrix["AMStotal", "PosAMS"] <- 0
pred_matrix["AMStotal", "NegAMS"] <- 0

pred_matrix["PosAMS", "AMStotal"] <- 0
pred_matrix["PosAMS", "NegAMS"] <- 0

pred_matrix["NegAMS", "AMStotal"] <- 0
pred_matrix["NegAMS", "PosAMS"] <- 0


imp_method = c(
  "polyreg", #WeeklyIncome
  "logreg", #Sex
  "polyreg", #Ethnicity
  "logreg", #psychosis1_14
  "logreg", #psychosis1_16
  "logreg", #psychosis2_14
  "logreg", #psychosis2_16
  "logreg", #psychosis_12
  "logreg", #psychosis_14
  "logreg", #psychosis_16
  "logreg", #MaternalHighEd
  "pmm", #PosAMS
  "pmm", #NegAMS
  "pmm" #AMStotal
)

imp_data1 <- mice(imp_data,
              predictorMatrix = pred_matrix,
              m = 20,
              method = imp_method,
              seed = 5000)
write.mice.imputation(imp_data1, name = "data_imp", mids2spss = F)


## ----Check imputed dataset----------------------------------------------------

bwplot(imp_data1)
densityplot(imp_data1)
xyplot(imp_data1, psychosis_14 ~ AMStotal | as.factor(.imp))


## ----Psychosis 14 ~ AMStotal - MI---------------------------------------------

modelMI1 <- with(imp_data1, glm(psychosis_14 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))


# Pool results
pooled_model <- pool(modelMI1)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Psychosis 16 ~ AMStotal - MI---------------------------------------------

modelMI2 <- with(imp_data1, glm(psychosis_16 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI2)) 

# Pool results
pooled_model <- pool(modelMI2)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 16",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Hallucinations 14 ~ AMStotal - MI *--------------------------------------

modelMI3 <- with(imp_data1, glm(psychosis1_14 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI3)) 

# Pool results
pooled_model <- pool(modelMI3)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)



## ----Hallucinations 16 ~ AMStotal - MI----------------------------------------

modelMI4 <- with(imp_data1, glm(psychosis1_16 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI4)) 

# Pool results
pooled_model <- pool(modelMI4)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Delusions 14 ~ AMStotal - MI---------------------------------------------

modelMI5 <- with(imp_data1, glm(psychosis2_14 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI5)) 

# Pool results
pooled_model <- pool(modelMI5)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Delusions 16 ~ AMStotal - MI---------------------------------------------

modelMI6 <- with(imp_data1, glm(psychosis2_16 ~ AMStotal + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI6)) 

# Pool results
pooled_model <- pool(modelMI6)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 16",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Psychosis 14 ~ PosAMS - MI-----------------------------------------------

modelMI7 <- with(imp_data1, glm(psychosis_14 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI7)) 

# Pool results
pooled_model <- pool(modelMI7)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Psychosis 16 ~ PosAMS - MI-----------------------------------------------

modelMI8 <- with(imp_data1, glm(psychosis_16 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI8)) 

# Pool results
pooled_model <- pool(modelMI8)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Hallucinations 14 ~ PosAMS - MI------------------------------------------

modelMI9 <- with(imp_data1, glm(psychosis1_14 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI9)) 

# Pool results
pooled_model <- pool(modelMI9)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Hallucinations 16 ~ PosAMS - MI------------------------------------------

modelMI10 <- with(imp_data1, glm(psychosis1_16 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI10)) 

# Pool results
pooled_model <- pool(modelMI10)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 16",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)

## ----Delusions 14 ~ PosAMS - MI-----------------------------------------------

modelMI11 <- with(imp_data1, glm(psychosis2_14 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI11)) 

# Pool results
pooled_model <- pool(modelMI11)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)

## ----Delusions 16 ~ PosAMS - MI-----------------------------------------------

modelMI12 <- with(imp_data1, glm(psychosis2_16 ~ PosAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI12)) 

# Pool results
pooled_model <- pool(modelMI12)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 16",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)
      

## ----Psychosis 14 ~ NegAMS - MI *---------------------------------------------

modelMI13 <- with(imp_data1, glm(psychosis_14 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI13)) 

# Pool results
pooled_model <- pool(modelMI13)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Psychosis 16 ~ NegAMS - MI-----------------------------------------------

modelMI14 <- with(imp_data1, glm(psychosis_16 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI14)) 

# Pool results
pooled_model <- pool(modelMI14)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Hallucinations 14 ~ NegAMS - MI *----------------------------------------

modelMI15 <- with(imp_data1, glm(psychosis1_14 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI15)) 

# Pool results
pooled_model <- pool(modelMI15)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Hallucinations 16 ~ NegAMS - MI------------------------------------------

modelMI16 <- with(imp_data1, glm(psychosis1_16 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI16)) 

# Pool results
pooled_model <- pool(modelMI16)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Delusions 14 ~ NegAMS - MI-----------------------------------------------

modelMI17 <- with(imp_data1, glm(psychosis2_14 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI17)) 

# Pool results
pooled_model <- pool(modelMI17)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 14",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Delusions 16 ~ NegAMS - MI-----------------------------------------------

modelMI18 <- with(imp_data1, glm(psychosis2_16 ~ NegAMS + psychosis_12 + Ethnicity + MaternalHighEd + Sex + WeeklyIncome, family = binomial))
summary(pool(modelMI18)) 

# Pool results
pooled_model <- pool(modelMI18)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Psychosis 16",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----Plot---------------------------------------------------------------------

# Results
results <- tribble(
  ~Outcome,             ~Predictor,     ~OR,   ~CI_lower, ~CI_upper, ~p.value,
  "Psychosis 14",       "Total AMS",    1.04,	1.00,	1.09,	0.08,
  "Psychosis 14",       "Positive AMS", 1.05,	0.96,	1.14,	0.29,
  "Psychosis 14",       "Negative AMS", 1.09,	1.01,	1.18,	0.03,
  "Hallucinations 14",  "Total AMS",   1.07,	1.01,	1.13,	0.02,
  "Hallucinations 14",  "Positive AMS",1.08,	0.97,	1.20,	0.15,
  "Hallucinations 14",  "Negative AMS",1.14,	1.04,	1.26,	0.007,
  "Delusions 14",       "Total AMS",   1.00,	0.94,	1.06,	0.94,
  "Delusions 14",       "Positive AMS",1.00,	0.90,	1.13,	0.94,
  "Delusions 14",       "Negative AMS",0.99,	0.89,	1.10,	0.80,
)

results <- results %>%
  mutate(
    Significant = p.value < 0.05,
    Outcome = factor(Outcome, levels = c("Psychosis 14", "Hallucinations 14", "Delusions 14")),
    Label = Predictor,
    Outcome = str_replace(Outcome, " 14", " at age 14"),  
    Outcome = factor(Outcome, levels = c("Psychosis at age 14", "Hallucinations at age 14", "Delusions at age 14")) 
  )


# Forest plot by symptoms
plot14 <- ggplot(results, aes(x = OR, y = Label, color = Significant)) +
            geom_point(size = 3) +
            geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
            geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
            scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "gray50")) +
            xlab("Odds Ratio (95% CI)") +
            ylab("") +
            facet_wrap(~ Outcome, scales = "free_y") +
            theme_minimal(base_size = 12) +
            theme(
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black"),
              legend.position = "none",
              strip.text = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 10)
            )




