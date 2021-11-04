# Loading packages -------------------------------------------------------------
library(psych)
library(lme4)
library(nlme)
library(sjPlot)
library(effects)
library(magrittr) # part of the tidyverse but must be read in on its own
library(parameters)
library(dplyr)
library(tidyr)
library(rio)
library(ggplot2)

# Functions to clean document, get data from wide to long format----------------
source("functions/Cleaning.R")

# Importing data----------------------------------------------------------------
wide_raw <- import("data/analog2_wide_pt to remove.csv") 

# Cleaning data using functions-------------------------------------------------
wide_data_clean <- wide_factor_clean(wide_raw)

long_data <- wide_to_long(wide_data_clean)

model_data <- long_data %>% 
  select(sub_id, bfi_number, cand_pref, ingroup_ident, threat, 
         self, target_number_collapsed, target_bfi_value, 
         condition_order, covid1, covid2, covid3, target_group,
         ingroup_ident_c, self_c, threat_c, gender, ethnicity) %>% 
  drop_na()

# Model-------------------------------------------------------------------------
model_base <- lmer(target_bfi_value ~ self_c*target_number_collapsed + (self_c|sub_id), data = model_data)

# Level 1 Residuals-------------------------------------------------------------

# nlme code did not work, so extracted residuals from lmer and visually inspected

# lm.form <- formula(target_bfi_value ~ 1 + self_c + target_number_collapsed + self_c:target_number_collapsed)
# 
# control1 <- lmeControl(maxIter = 10000, msMaxIter = 10000, niterEM = 10000,
#                        msMaxEval = 10000, opt = c("nlminb"), optimMethod = "BFGS", returnObject=TRUE)
# 
# model_base_nlme1 <- lme(lm.form,
#                         random = ~1 + self_c | sub_id,
#                         data = model_data,
#                         na.action = na.exclude,
#                         varIdent(form = ~1 | sub_id),
#                         control = lmeControl(opt = "optim", tolerance = 1e-3))
# 
# # lmeControl(opt = "optim") # alternative code for control above; doesn't have convergence issue but has allocation issue
# model_base_nlme2 <- lme(lm.form,
#                         random = ~1 + self_c | sub_id,
#                         data = model_data,
#                         na.action = na.exclude)

model_data$predict <- as.numeric(predict(model_base))

model_data$resid <- as.numeric(model_data$target_bfi_value - model_data$predict)

model_data$zresid <- as.numeric(model_data$resid - mean(model_data$resid))/sd(model_data$resid)

## Normality
hist(model_data$zresid)

## Heteroscedasticity: Responses for Self
resid_mod1 <- lm(model_data$zresid ~ model_data$self_c)

par(mfrow=c(2,2))
plot(resid_mod1)
par(mfrow=c(1,1))
