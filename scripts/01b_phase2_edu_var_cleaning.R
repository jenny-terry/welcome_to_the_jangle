### CHECKING & PROCESSING EDU & ETHNICITY VARS ###

library(magrittr)

# read in data
phase2_data <- readr::read_csv(here::here("data", "phase2_data.csv"))

#rename vars
phase2_data %<>%
  dplyr::rename(
    math_edu_level = q10.1, 
    math_edu_level_txt1 = q10.1_1_text, # all NA
    math_edu_level_txt2 = q10.1_2_text, #needs processing
    math_edu_level_txt3 = q10.1_3_text, #needs processing
    math_gcse = q10.2, #needs processing
    math_alevel = q10.3, #needs processing
    math_ug = q10.4, 
    math_pg = q10.5, 
    
    stats_edu_level = q10.6, 
    stats_edu_level_txt1 = q10.6_1_text, #needs processing
    stats_edu_level_txt2 = q10.6_2_text, #needs processing
    stats_edu_level_txt3 = q10.6_3_text, #needs processing
    stats_gcse = q10.7, #needs processing
    stats_alevel = q10.8, #needs processing
    stats_ug = q10.9, 
    stats_pg = q10.10, 
    
    course_year = q11.1, 
    age = q11.2, 
    gender_id = q11.3, 
    gender_id_other = q11.3_4_text,
    ethnicity_white = q11.4_1, 
    ethnicity_black_carribean = q11.4_2, 
    ethnicity_black_african = q11.4_3, 
    ethnicity_black_other = q11.4_4, 
    ethnicity_indian = q11.4_5, 
    ethnicity_pakistani = q11.4_6, 
    ethnicity_bangladeshi = q11.4_7,
    ethnicity_chinese = q11.4_8, 
    ethnicity_asian_other = q11.4_9, 
    ethnicity_arab = q11.4_10, 
    ethnicity_other = q11.4_11, 
    ethnicity_pnts = q11.4_12, 
    eth_other_specify = q11.4_11_text, # needs processing
    spld = q11.5, 
    spld_other = q11.5_6_text
  )

#create data file and manually process free-text responses
#note that edits done in columns with `_processed` suffix
#readr::write_csv(phase2_data, here::here("data", "phase2_edu_processed_data.csv"), col_names = T)
