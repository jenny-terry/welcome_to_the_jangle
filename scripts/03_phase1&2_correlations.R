### COMBINING POLYCHORIC CORR MATRIX FOR PHASE 1 & 2 DATA ###

library(magrittr)

#read in data
phase1_data <- readr::read_csv(here::here("data", "phase1_anon_data.csv"))
phase2_data <- readr::read_csv(here::here("data", "phase2_anon_data.csv"))

# create phase 1 composites

# stars mean
phase1_data %<>% dplyr::mutate(stars = rowMeans(dplyr::select(., dplyr::starts_with("q4.1")), na.rm = TRUE))

# stars_m mean
phase1_data %<>% dplyr::mutate(stars_m = rowMeans(dplyr::select(., dplyr::starts_with("q4.2")), na.rm = TRUE))

# rmars mean
phase1_data %<>% dplyr::mutate(rmars = rowMeans(dplyr::select(., dplyr::starts_with("q4.3")), na.rm = TRUE))

# rmars_s mean
phase1_data %<>% dplyr::mutate(rmars_s = rowMeans(dplyr::select(., dplyr::starts_with("q4.4")), na.rm = TRUE))

# sticsa trait mean
phase1_data %<>% dplyr::mutate(trait = rowMeans(dplyr::select(., dplyr::starts_with("q5.1")), na.rm = TRUE))

# sticsa pre-state mean
phase1_data %<>% dplyr::mutate(pre_state = rowMeans(dplyr::select(., dplyr::starts_with("q5.2")), na.rm = TRUE))

# sticsa post-state mean
phase1_data %<>% dplyr::mutate(post_state = rowMeans(dplyr::select(., dplyr::starts_with("q8.1")), na.rm = TRUE))

p1_composite_vars <- phase1_data |>
  dplyr::select(stars, stars_m, rmars, rmars_s, trait, pre_state, post_state) |>
  dplyr::rename(STARS = stars,
                "R-MARS" = rmars,
                "STARS-M" = stars_m,
                "R-MARS-S" = rmars_s,
                Trait = trait,
                "State (Pre)"= pre_state,
                "State (Post)" = post_state)

# create phase 2 composites

# stars mean
phase2_data %<>% dplyr::mutate(stars = rowMeans(dplyr::select(., dplyr::starts_with("q4.1")), na.rm = TRUE))

# stars_m mean
phase2_data %<>% dplyr::mutate(stars_m = rowMeans(dplyr::select(., dplyr::starts_with("q4.2")), na.rm = TRUE))

# rmars mean
phase2_data %<>% dplyr::mutate(rmars = rowMeans(dplyr::select(., dplyr::starts_with("q4.3")), na.rm = TRUE))

# rmars_s mean
phase2_data %<>% dplyr::mutate(rmars_s = rowMeans(dplyr::select(., dplyr::starts_with("q4.4")), na.rm = TRUE))

# sticsa trait mean
phase2_data %<>% dplyr::mutate(trait = rowMeans(dplyr::select(., dplyr::starts_with("q5.1")), na.rm = TRUE))

# sticsa pre-state mean
phase2_data %<>% dplyr::mutate(pre_state = rowMeans(dplyr::select(., dplyr::starts_with("q5.2")), na.rm = TRUE))

# sticsa post-state mean
phase2_data %<>% dplyr::mutate(post_state = rowMeans(dplyr::select(., dplyr::starts_with("q8.1")), na.rm = TRUE))

p2_composite_vars <- phase2_data |>
  dplyr::select(stars, stars_m, rmars, rmars_s, trait, pre_state, post_state) |>
  dplyr::rename(STARS = stars,
                "R-MARS" = rmars,
                "STARS-M" = stars_m,
                "R-MARS-S" = rmars_s,
                Trait = trait,
                "State (Pre)"= pre_state,
                "State (Post)" = post_state)

# get corrs

cor1 <- cor(p1_composite_vars, use = "pairwise.complete.obs")  
cor2 <- cor(p2_composite_vars, use = "pairwise.complete.obs") 

# combine triangles

cor_combined <- cor1
cor_combined[upper.tri(cor_combined)] <- cor2[upper.tri(cor2)]
diag(cor_combined) <- NA

cor_combined
