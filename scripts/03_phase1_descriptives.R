### VARIABLE DESCRIPTIVES FOR PHASE 1 DATA ###

library(magrittr)

#read in data
phase1_data <- readr::read_csv(here::here("data", "phase1_anon_data.csv"))

# create composites

# stars mean
phase1_data %<>% dplyr::mutate(stars = rowMeans(dplyr::select(., dplyr::starts_with("q4.1")), na.rm = TRUE))

# stars - interpretation
phase1_data %<>% dplyr::mutate(stars_int = rowMeans(dplyr::select(., q4.1_2, q4.1_5, q4.1_6, q4.1_7, q4.1_9, q4.1_11, q4.1_12, q4.1_14, q4.1_17, q4.1_18, q4.1_20), na.rm = TRUE))

# stars - test
phase1_data %<>% dplyr::mutate(stars_test = rowMeans(dplyr::select(., q4.1_1, q4.1_4, q4.1_8, q4.1_10, q4.1_13, q4.1_15, q4.1_21, q4.1_22), na.rm = TRUE))

# stars - help
phase1_data %<>% dplyr::mutate(stars_help = rowMeans(dplyr::select(., q4.1_3, q4.1_16, q4.1_19, q4.1_23), na.rm = TRUE))

# stars_m mean
phase1_data %<>% dplyr::mutate(stars_m = rowMeans(dplyr::select(., dplyr::starts_with("q4.2")), na.rm = TRUE))

# stars_m - interpretation
phase1_data %<>% dplyr::mutate(stars_m_int = rowMeans(dplyr::select(., q4.2_2, q4.2_5, q4.2_6, q4.2_7, q4.2_10, q4.2_12, q4.2_15, q4.2_17, q4.1_9, q4.1_12, q4.1_17), na.rm = TRUE))

# stars_m - test
phase1_data %<>% dplyr::mutate(stars_m_test = rowMeans(dplyr::select(., q4.2_1, q4.2_4, q4.2_8, q4.2_9, q4.2_11, q4.2_13, q4.2_18, q4.2_19), na.rm = TRUE))

# stars_m - help
phase1_data %<>% dplyr::mutate(stars_m_help = rowMeans(dplyr::select(., q4.2_3, q4.2_14, q4.2_16, q4.2_20), na.rm = TRUE))

# rmars mean
phase1_data %<>% dplyr::mutate(rmars = rowMeans(dplyr::select(., dplyr::starts_with("q4.3")), na.rm = TRUE))

# rmars - test
phase1_data %<>% dplyr::mutate(rmars_test = rowMeans(dplyr::select(., q4.3_1:q4.3_10), na.rm = TRUE))

# rmars - numerical
phase1_data %<>% dplyr::mutate(rmars_num = rowMeans(dplyr::select(., q4.3_11:q4.3_15), na.rm = TRUE))

# rmars - course
phase1_data %<>% dplyr::mutate(rmars_course = rowMeans(dplyr::select(., q4.3_16:q4.3_20), na.rm = TRUE))

# rmars_s mean
phase1_data %<>% dplyr::mutate(rmars_s = rowMeans(dplyr::select(., dplyr::starts_with("q4.4")), na.rm = TRUE))

# rmars_s - test
phase1_data %<>% dplyr::mutate(rmars_s_test = rowMeans(dplyr::select(., q4.4_1:q4.4_10), na.rm = TRUE))

# rmars_s - numerical
phase1_data %<>% dplyr::mutate(rmars_s_num = rowMeans(dplyr::select(., q4.3_11, q4.4_11:q4.4_14), na.rm = TRUE))

# rmars_s - course
phase1_data %<>% dplyr::mutate(rmars_s_course = rowMeans(dplyr::select(., q4.4_15:q4.4_19), na.rm = TRUE))

# sticsa trait mean
phase1_data %<>% dplyr::mutate(trait = rowMeans(dplyr::select(., dplyr::starts_with("q5.1")), na.rm = TRUE))

# sticsa pre-state mean
phase1_data %<>% dplyr::mutate(pre_state = rowMeans(dplyr::select(., dplyr::starts_with("q5.2")), na.rm = TRUE))

# sticsa post-state mean
phase1_data %<>% dplyr::mutate(post_state = rowMeans(dplyr::select(., dplyr::starts_with("q8.1")), na.rm = TRUE))

# sticsa trait cognitive mean
phase1_data %<>% dplyr::mutate(trait_cog = rowMeans(dplyr::select(., q5.1_3, q5.1_4, q5.1_5, q5.1_9, q5.1_10, q5.1_11, q5.1_13, q5.1_16, q5.1_17, q5.1_19), na.rm = TRUE))

# sticsa trait somatic mean
phase1_data %<>% dplyr::mutate(trait_som = rowMeans(dplyr::select(., q5.1_1, q5.1_2, q5.1_6, q5.1_7, q5.1_8, q5.1_12, q5.1_14, q5.1_15, q5.1_18, q5.1_20, q5.1_21), na.rm = TRUE))

# sticsa pre-state cognitive mean
phase1_data %<>% dplyr::mutate(pre_state_cog = rowMeans(dplyr::select(., q5.2_3, q5.2_4, q5.2_5, q5.2_9, q5.2_10, q5.2_11, q5.2_13, q5.2_16, q5.2_17, q5.2_19), na.rm = TRUE))

# sticsa pre-state somatic mean
phase1_data %<>% dplyr::mutate(pre_state_som = rowMeans(dplyr::select(., q5.2_1, q5.2_2, q5.2_6, q5.2_7, q5.2_8, q5.2_12, q5.2_14, q5.2_15, q5.2_18, q5.2_20, q5.2_21), na.rm = TRUE))

# sticsa post-state cognitive mean
phase1_data %<>% dplyr::mutate(post_state_cog = rowMeans(dplyr::select(., q8.1_3, q8.1_4, q8.1_5, q8.1_9, q8.1_10, q8.1_11, q8.1_13, q8.1_16, q8.1_17, q8.1_19), na.rm = TRUE))

# sticsa post-state somatic mean
phase1_data %<>% dplyr::mutate(post_state_som = rowMeans(dplyr::select(., q8.1_1, q8.1_2, q8.1_6, q8.1_7, q8.1_8, q8.1_12, q8.1_14, q8.1_15, q8.1_18, q8.1_20, q8.1_21), na.rm = TRUE))

p1_composite_vars <- phase1_data |>
  dplyr::select(stars, stars_m, rmars, rmars_s, trait, pre_state, post_state, mcq) |>
  dplyr::rename(STARS = stars,
                "R-MARS" = rmars,
                "STARS-M" = stars_m,
                "R-MARS-S" = rmars_s,
                Trait = trait,
                "State (Pre)"= pre_state,
                "State (Post)" = post_state,
                MCQ = mcq)

#check missing
p1_composite_vars %>% finalfit::ff_glimpse()

# get descriptives for composites 
fun_list <- list(
  Mean = ~mean(.x, na.rm = T),
  SD = ~sd(.x, na.rm = T),
  Min = ~min(.x, na.rm = T),
  Max = ~max(.x, na.rm = T),
  Skew = ~moments::skewness(.x, na.rm = T),        
  Kurtosis = ~moments::kurtosis(.x, na.rm = T),
  `CI Lower` = ~ggplot2::mean_cl_normal(.x)$ymin,
  `CI Upper` = ~ggplot2::mean_cl_normal(.x)$ymax
)

p1_var_summary <- p1_composite_vars %>%
    dplyr::summarise(
    dplyr::across(c("STARS":"State (Post)"), fun_list)
)

p1_desc_summary <- p1_var_summary |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = c("Variable", ".value"),
                      names_sep = "_(?=[^_]+$)") 


p1_desc_corr_p <- p1_composite_vars |> 
  dplyr::select(-MCQ) |>
  lavaan::lavCor(object = _,
                 ordered = names(p1_composite_vars),
                 missing = "pairwise",
                 output = "cor")


p1_subscale_vars <- phase1_data |>
  dplyr::select(stars, stars_int, stars_test, stars_help, stars_m, stars_m_int, stars_m_test, stars_m_help, 
                rmars, rmars_test, rmars_course, rmars_num, rmars_s, rmars_s_test, rmars_s_course, rmars_s_num) |>
  dplyr::rename(STARS = stars,
                "STARS - int"  = stars_int,
                "STARS - test" = stars_test,
                "STARS - help" = stars_help,
                "STARS-M" = stars_m,
                "STARS-M - int"  = stars_m_int,
                "STARS-M - test" = stars_m_test,
                "STARS-M - help" = stars_m_help,
                "R-MARS" = rmars,
                "R-MARS - test" = rmars_test,
                "R-MARS - course" = rmars_course,
                "R-MARS - num" = rmars_num,
                "R-MARS-S" = rmars_s,
                "R-MARS-S - test" = rmars_s_test,
                "R-MARS-S - course" = rmars_s_course,
                "R-MARS-S - num" = rmars_s_num)

# get descriptives for subscales 
p1_var_ss_summary <- p1_subscale_vars %>%
  dplyr::summarise(
    dplyr::across(c("STARS - int":"R-MARS-S - num"), fun_list)
  )

p1_desc_ss_summary <- p1_var_ss_summary |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = c("Variable", ".value"),
                      names_sep = "_(?=[^_]+$)") 

p1_desc_ss_corr_p <- p1_subscale_vars |> 
  lavaan::lavCor(object = _,
                 ordered = names(p1_subscale_vars),
                 missing = "pairwise",
                 output = "cor")

# check variable scores in each mcq condition
p1_mcq_summary <- p1_composite_vars %>%
  dplyr::group_by(MCQ) %>%
  dplyr::summarise(
    dplyr::across(c("STARS":"State (Post)"), fun_list)
  )