# Packages ---------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(rio)
library(lubridate)

# NEED TO CLEAN PERSPECTIVE NARRATIVES FIRST TO MAKE SURE THEY RESPOND

# Import data & set-up ---------------------------------------------------------
wide_data <- import("data/analog2_wide.csv") 

# Add subject_id column
wide_data %<>% 
  mutate(subject_id = 1:n()) %>% 
  select(subject_id, everything())

# Variable cleaning 1 ----------------------------------------------------------
wide_data_clean <- wide_data %>% 
  mutate(recorded_date = mdy_hm(recorded_date),
         age = as.numeric(age),
         gender = as.factor(gender),
         ethnicity = as.factor(ethnicity),
         threat = as.numeric(threat),
         cand_pref = ifelse(cand_pref == 1, "-0.5",
                            ifelse(cand_pref == 3, "0.5", NA)),
         ingroup_ident = as.numeric(ingroup_ident)) %>% 
  select(-finished, qualtrics_response_id)

# Remove repeat Sona IDs -------------------------------------------------------
# First check whether there are repeat Sona IDs
id_df <- wide_data_clean %>% 
  select(subject_id, sona_id)

id_df$sona_id[duplicated(id_df$sona_id)]

# Kept the first response of the repeat Sona IDs
wide_clean_norepeats <- wide_data_clean %>%
  group_by(sona_id) %>%
  arrange(recorded_date) %>%
  slice(1L)
