### DEMOGRAPHICS & CREATION OF ANONIMISED PHASE 1 DATA ###

library(magrittr)

#read in data
phase1_data <- readr::read_csv(here::here("data", "phase1_edu_processed_data.csv"))

# select demographic variables
phase1_demo <- dplyr::select(phase1_data, math_edu_level:spld_other)

# remove non-numeric characters from numeric variables
numextract <- function(string){ 
  stringr::str_extract(string, "\\-*\\d+\\.*\\d*")
}

numeric_vars <- c("math_ug", "math_pg", "stats_ug", "stats_pg", "age")
phase1_demo[, numeric_vars] <- lapply(phase1_demo[, numeric_vars], numextract)
phase1_demo[, numeric_vars] <- lapply(phase1_demo[, numeric_vars], as.numeric)

# inspect numeric variables
phase1_demo$age
phase1_demo$math_ug
phase1_demo$math_pg
phase1_demo$stats_ug
phase1_demo$stats_pg

# replace nonsense responses with NA
phase1_demo$age[phase1_demo$age == 1999] <- NA
phase1_demo$math_ug[phase1_demo$math_ug == 1] <- NA
phase1_demo$stats_ug[phase1_demo$stats_ug == 1] <- NA
phase1_demo$stats_ug[phase1_demo$stats_ug == 5] <- NA

# get descriptives for relevant numeric variables
p1_num_sum <- phase1_demo %>%
  dplyr::select(age, math_ug, math_pg, stats_ug, stats_pg) %>%
  dplyr::summarise(dplyr::across(
    .cols = where(is.numeric), 
    .fns = list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE, 
    .names = "{col}_{fn}"))

# get frequencies & percentages for categorical variables

# gender identity
phase1_demo %<>% 
  dplyr::mutate(gender_id = 
                  ifelse(gender_id == "1", "female", 
                         ifelse(gender_id == "2", "male", 
                                ifelse(gender_id == "3", "non_binary", 
                                       ifelse(gender_id == "4", "other", 
                                              ifelse(gender_id == "5", "withheld", NA))))))
p1_gender_freq <- phase1_demo %>% 
  dplyr::group_by(gender_id) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(percent = (n/sum(n))*100)  

#ethnicity

#convert to text (next time, download choice text!)

phase1_demo <- phase1_demo |> 
  dplyr::mutate(ethnicity_white = dplyr::case_when(ethnicity_white == 1 ~ "White"), 
                ethnicity_black_carribean = dplyr::case_when(ethnicity_black_carribean == 1 ~ "Black Carribean"), 
                ethnicity_black_african = dplyr::case_when(ethnicity_black_african == 1 ~ "Black African"),  
                ethnicity_black_other = dplyr::case_when(ethnicity_black_other == 1 ~ "Black Other"),
                ethnicity_indian = dplyr::case_when(ethnicity_indian == 1 ~ "Indian"),
                ethnicity_pakistani = dplyr::case_when(ethnicity_pakistani == 1 ~ "Pakistani"), 
                ethnicity_bangladeshi = dplyr::case_when(ethnicity_bangladeshi == 1 ~ "Bangladeshi"),  
                ethnicity_chinese = dplyr::case_when(ethnicity_chinese == 1 ~ "Chinese"),
                ethnicity_asian_other = dplyr::case_when(ethnicity_asian_other == 1 ~ "Asian Other"),
                ethnicity_arab = dplyr::case_when(ethnicity_arab == 1 ~ "Arab"),
                ethnicity_other = dplyr::case_when(ethnicity_other_processed == 1 ~ "Other"), 
                ethnicity_pnts = dplyr::case_when(ethnicity_pnts == 1 ~ "Prefer not to say"))
                #note that eth_other_specify_processed is already text
                #also other is equal to eth_other_specify_processed so won't be united with the other variables
                
#merge into one variable             
phase1_demo %<>%
  tidyr::unite(ethnicity, c(ethnicity_white:ethnicity_arab, ethnicity_pnts, eth_other_specify_processed), sep = ", ", remove = TRUE, na.rm = TRUE)

# summarise
p1_ethnicity_freq <- phase1_demo %>% 
  dplyr::group_by(ethnicity) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(percent = (n/sum(n))*100) %>% 
  dplyr::arrange(desc(percent))

# course year
phase1_demo %<>% 
  dplyr::mutate(course_year = 
                  ifelse(course_year == "1", "year_one", 
                         ifelse(course_year == "2", "year_two", 
                                ifelse(course_year == "3", "year_three", 
                                       ifelse(course_year == "4", "year_four", 
                                              ifelse(course_year == "6", "withheld", NA))))))
p1_course_year_freq <- phase1_demo %>% 
  dplyr::group_by(course_year) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(percent = (n/sum(n))*100)

# splds
phase1_demo %<>% 
  dplyr::mutate(spld = 
                  ifelse(spld == "1", "dyslexia", 
                         ifelse(spld == "2", "dyspraxia", 
                                ifelse(spld == "3", "adhd", 
                                       ifelse(spld == "4", "dyscalculia", 
                                              ifelse(spld == "5", "dysgraphia", 
                                                     ifelse(spld == "6", "other",
                                                            ifelse(spld == "7", "no_spld",
                                                                   ifelse(spld == "8", "withheld", NA)))))))))
p1_spld_freq <- phase1_demo %>% 
  dplyr::group_by(spld) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(percent = (n/sum(n))*100)

# stats education level
phase1_demo %<>% 
  dplyr::mutate(stats_edu_level = 
                  ifelse(stats_edu_level == "1", "below_gcse", 
                         ifelse(stats_edu_level == "2", "gcse", 
                                ifelse(stats_edu_level == "3", "a_level", 
                                       ifelse(stats_edu_level == "4", "ug", 
                                              ifelse(stats_edu_level == "5", "pg", 
                                                     ifelse(stats_edu_level == "6", "withheld", NA)))))))

p1_stats_edu_level_freq <- phase1_demo %>% 
  dplyr::group_by(stats_edu_level) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(percent = (n/sum(n))*100)   

# maths education level
phase1_demo %<>% 
  dplyr::mutate(math_edu_level = 
                  ifelse(math_edu_level == "1", "below_gcse", 
                         ifelse(math_edu_level == "2", "gcse", 
                                ifelse(math_edu_level == "3", "a_level", 
                                       ifelse(math_edu_level == "4", "ug", 
                                              ifelse(math_edu_level == "5", "pg", 
                                                     ifelse(math_edu_level == "6", "withheld", NA)))))))

p1_math_edu_level_freq <- phase1_demo %>% 
  dplyr::group_by(math_edu_level) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(percent = (n/sum(n))*100)   

# remove demographic data for anonymity and other unnecessary columns
phase1_data %<>% dplyr::select(-math_edu_level:-spld_other)

# create anonymised data file
#readr::write_csv(phase1_data, here::here("data", "phase1_anon_data.csv"), col_names = T)
