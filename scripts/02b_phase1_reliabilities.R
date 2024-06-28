# read in anonymised data file
phase1_data <- readr::read_csv(here::here("data", "phase1_data.csv"))

# create dataframes containing raw scores for each original factor
p1_stars <- dplyr::select(phase1_data, c("q4.1_1":"q4.1_23"))

p1_stars_m <- dplyr::select(phase1_data, c("q4.2_1":"q4.2_20", "q4.1_9", "q4.1_12", "q4.1_17"))

p1_rmars <- dplyr::select(phase1_data, c("q4.3_1":"q4.3_20"))

p1_rmars_s <- dplyr::select(phase1_data, c("q4.4_1":"q4.4_19", "q4.3_11"))

p1_trait <- dplyr::select(phase1_data, c("q5.1_1":"q5.1_21"))

p1_pre_state <- dplyr::select(phase1_data, c("q5.2_1":"q5.2_21"))

p1_post_state <- dplyr::select(phase1_data, c("q8.1_1":"q8.1_21"))

# calculate the point estimate and confidence interval for omega reliability coefficient

p1_stars_omega <- MBESS::ci.reliability(p1_stars, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p1_stars_m_omega <- MBESS::ci.reliability(p1_stars_m, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p1_rmars_omega <- MBESS::ci.reliability(p1_rmars, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p1_rmars_s_omega <- MBESS::ci.reliability(p1_rmars_s, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p1_trait_omega <- MBESS::ci.reliability(p1_trait, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p1_pre_state_omega <- MBESS::ci.reliability(p1_pre_state, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

p1_post_state_omega <- MBESS::ci.reliability(p1_post_state, type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)

# get alphas for sense-check comparison

psych::alpha(p1_stars)
psych::alpha(p1_stars_m)
psych::alpha(p1_rmars)
psych::alpha(p1_rmars_s)
psych::alpha(p1_trait)
psych::alpha(p1_pre_state)
psych::alpha(p1_post_state)

# results from the above were used to manually create the file that I will read in and use in the manuscript

p1_reliabilities_data <- readr::read_csv(here::here("data", "phase1_reliabilities.csv"))