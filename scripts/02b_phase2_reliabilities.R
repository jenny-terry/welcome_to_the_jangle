# read in anonymised data file
phase2_data <- readr::read_csv(here::here("data", "phase2_data.csv"))

# create dataframes containing raw scores for each original factor
p2_stars <- dplyr::select(phase2_data, c("q4.1_1":"q4.1_23"))

p2_stars_m <- dplyr::select(phase2_data, c("q4.2_1":"q4.2_20", "q4.1_9", "q4.1_12", "q4.1_17"))

p2_rmars <- dplyr::select(phase2_data, c("q4.3_1":"q4.3_20"))

p2_rmars_s <- dplyr::select(phase2_data, c("q4.4_1":"q4.4_19", "q4.3_11"))

p2_trait <- dplyr::select(phase2_data, c("q5.1_1":"q5.1_21"))

p2_pre_state <- dplyr::select(phase2_data, c("q5.2_1":"q5.2_21"))

p2_post_state <- dplyr::select(phase2_data, c("q8.1_1":"q8.1_21"))

# calculate the point estimate and confidence interval for omega reliability coefficient

p2_stars_omega <- MBESS::ci.reliability(p2_stars, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p2_stars_m_omega <- MBESS::ci.reliability(p2_stars_m, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p2_rmars_omega <- MBESS::ci.reliability(p2_rmars, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p2_rmars_s_omega <- MBESS::ci.reliability(p2_rmars_s, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p2_trait_omega <- MBESS::ci.reliability(p2_trait, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p2_pre_state_omega <- MBESS::ci.reliability(p2_pre_state, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p2_post_state_omega <- MBESS::ci.reliability(p2_post_state, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

# get alphas for comparison

psych::alpha(p2_stars)
psych::alpha(p2_stars_m)
psych::alpha(p2_rmars)
psych::alpha(p2_rmars_s)
psych::alpha(p2_trait)
psych::alpha(p2_pre_state)
psych::alpha(p2_post_state)

# the above code was used to create the file that I will read in and use in the manuscript

p2_reliabilities_data <- readr::read_csv(here::here("data", "phase2_reliabilities.csv"))