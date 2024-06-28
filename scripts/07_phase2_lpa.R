library(magrittr)
library(ggplot2)

# read in anonymised data file
phase2_data <- readr::read_csv(here::here("data", "phase2_anon_data.csv"))

### create composite scores

# stars mean
phase2_data %<>% dplyr::mutate(stars = rowMeans(dplyr::select(., dplyr::starts_with("q4.1")), na.rm = TRUE))

# stars_m mean
phase2_data %<>% dplyr::mutate(stars_m = rowMeans(dplyr::select(., dplyr::starts_with("q4.2")), na.rm = TRUE))

# rmars mean
phase2_data %<>% dplyr::mutate(rmars = rowMeans(dplyr::select(., dplyr::starts_with("q4.3")), na.rm = TRUE))

# rmars_s mean
phase2_data %<>% dplyr::mutate(rmars_s = rowMeans(dplyr::select(., dplyr::starts_with("q4.4")), na.rm = TRUE))

### estimate profiles

# fit models
p2b_lpa_mod <- phase2_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>% 
  scale() %>% # centre variables
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(1:6) %>%
  tidyLPA::get_fit()

# compare different variance/covariance models
# NOTE: models 4 & 5 are only available in MPlus.
p2b_compare_cov <- phase2_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>% 
  scale() %>%
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(6, models = c(1,2,3,6)) %>%
  tidyLPA::get_fit()

# re-run model with retained no. of profiles
# varying variances and varying covariances (model 6)
p2b_mod6 <- phase2_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>%
  scale() %>%
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(6, variances = "varying", covariances = "varying")

### extract profiles

# create object containing profile no. for each case
library(tidyLPA) # for some reason using the namespace before the get_data function doesn't work - package needs to be loaded in advance
p2b_profiles <- get_data(p2b_mod6)

# add class variable to main dataset
phase2_data$profile <- p2b_profiles$Class

# get class frequencies
dplyr::count(phase2_data, vars = profile)

p2b_profile_freq <- phase2_data %>% 
  dplyr::group_by(profile) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(percent = (n/sum(n))*100) 

# plot profiles

# create better plot (credit to my UG diss student, Skylar Taylor, for the code!)
p2b_estimates_table <- tidyLPA::get_estimates(p2b_mod6) %>%
  dplyr::filter(Category == "Means") %>%
  dplyr::select("Parameter", "Estimate", "Class")

p2b_estimates_desc_table <- p2b_estimates_table |> 
  tidyr::pivot_wider(names_from = Parameter, 
                     values_from = Estimate)

p2b_lpa_plot <- p2b_estimates_table %>%
  ggplot(aes(x = Parameter, y = Estimate, group = Class, colour = Class)) +  
  geom_point() + 
  geom_line() +
  scale_x_discrete(name = "Variable", 
                   labels = c("STARS", "STARS-M", "R-MARS", "R-MARS-S")) +
  scale_y_continuous(name = "Mean Value (Centred)", 
                     limits = c(-2.5, 2.5), 
                     n.breaks = 10) +
  labs(colour = "Profile") +
  viridis::scale_color_viridis() +
  theme_classic()
p2b_lpa_plot

p2b_estimates_table$Class <- as.factor(p2b_estimates_table$Class)

p2b_lpa_plot_bw <- p2b_estimates_table %>%
  ggplot(aes(x = Parameter, y = Estimate, group = Class)) +  
  geom_point() + 
  geom_line(aes(linetype = Class)) +
  scale_x_discrete(name = "Variable", 
                   labels = c("STARS", "STARS-M", "R-MARS", "R-MARS-S")) +
  scale_y_continuous(name = "Mean Value (Centred)", 
                     limits = c(-2.5, 2.5), 
                     n.breaks = 10) +
  labs(linetype = "Profile") +
  jtools::theme_apa() +
  theme(plot.title = element_blank(),
        
        axis.title.x = element_text(family = "serif", 
                                    size = 14),
        axis.text.x = element_text(family = "serif"),
        
        axis.title.y = element_text(family = "serif", 
                                    size = 14),
        axis.text.y = element_text(family = "serif"),
        
        legend.text = element_text(family = "serif", size = 14),
        
        strip.text.x = element_text(family = "serif", size = 14),
        
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        
        axis.line = element_line(color = "black", size = .5))
p2b_lpa_plot_bw


# check 7 profile solution
p2b_lpa_mod7 <- phase2_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>% 
  scale() %>% # centre variables
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(1:7) %>%
  tidyLPA::get_fit()

# compare different variance/covariance models
# NOTE: models 4 & 5 are only available in MPlus.
p2b_compare_cov7 <- phase2_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>% 
  scale() %>%
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(7, models = c(1,2,3,6)) %>%
  tidyLPA::get_fit()

p2b_mod7 <- phase2_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>%
  scale() %>%
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(7, variances = "varying", covariances = "zero")

p2b_estimates_table_mod7 <- tidyLPA::get_estimates(p2b_mod7) %>%
  dplyr::filter(Category == "Means") %>%
  dplyr::select("Parameter", "Estimate", "Class")

p2b_estimates_desc_table_mod7 <- p2b_estimates_table_mod7 |> 
  tidyr::pivot_wider(names_from = Parameter, 
                     values_from = Estimate)

p2b_lpa_plot_mod7 <- p2b_estimates_table_mod7 %>%
  ggplot(aes(x = Parameter, y = Estimate, group = Class, colour = Class)) +  
  geom_point() + 
  geom_line() +
  scale_x_discrete(name = "Variable", 
                   labels = c("STARS", "STARS-M", "R-MARS", "R-MARS-S")) +
  scale_y_continuous(name = "Mean Value (Centred)", 
                     limits = c(-2.5, 2.5), 
                     n.breaks = 10) +
  labs(colour = "Profile") +
  viridis::scale_color_viridis() +
  theme_classic()
p2b_lpa_plot_mod7

p2b_estimates_table_mod7$Class <- as.factor(p2b_estimates_table_mod7$Class)

p2b_lpa_plot_bw_mod7 <- p2b_estimates_table_mod7 %>%
  ggplot(aes(x = Parameter, y = Estimate, group = Class)) +  
  geom_point() + 
  geom_line(aes(linetype = Class)) +
  scale_x_discrete(name = "Variable", 
                   labels = c("STARS", "STARS-M", "R-MARS", "R-MARS-S")) +
  scale_y_continuous(name = "Mean Value (Centred)", 
                     limits = c(-2.5, 2.5), 
                     n.breaks = 10) +
  labs(linetype = "Profile") +
  jtools::theme_apa() +
  theme(plot.title = element_blank(),
        
        axis.title.x = element_text(family = "serif", 
                                    size = 14),
        axis.text.x = element_text(family = "serif"),
        
        axis.title.y = element_text(family = "serif", 
                                    size = 14),
        axis.text.y = element_text(family = "serif"),
        
        legend.text = element_text(family = "serif", size = 14),
        
        strip.text.x = element_text(family = "serif", size = 14),
        
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        
        axis.line = element_line(color = "black", size = .5))
p2b_lpa_plot_bw_mod7