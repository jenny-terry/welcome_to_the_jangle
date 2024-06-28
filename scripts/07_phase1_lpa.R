library(magrittr)
library(ggplot2)

# read in anonymised data file
phase1_data <- readr::read_csv(here::here("data", "phase1_anon_data.csv"))

### estimate profiles

# fit models
p1b_lpa_mod <- phase1_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>% 
  scale() %>% # centre variables
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(1:6) %>%
  tidyLPA::get_fit()

# compare different variance/covariance models
# NOTE: models 4 & 5 are only available in MPlus.
p1b_compare_cov <- phase1_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>% 
  scale() %>%
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(6, models = c(1,2,3,6)) %>%
  tidyLPA::get_fit()

# re-run retained model
# varying variances and covariances fixed to zero (model 2)
p1b_mod2 <- phase1_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>%
  scale() %>%
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(6, variances = "varying", covariances = "zero")

### extract profiles

# create object containing profile no. for each case
library(tidyLPA) # for some reason using the namespace before the get_data function doesn't work - package needs to be loaded in advance
p1b_profiles <- get_data(p1b_mod2)

# correlations within profiles
p1b_profile_corrs <- p1b_profiles %>%
  dplyr::group_by(classes_number) %>% 
  dplyr::summarise(r = cor(stars, rmars))
p1b_profile_corrs

p1b_profile_corrs <- p1b_profiles %>%
  dplyr::group_by(classes_number) %>% 
  dplyr::summarise(r = cor(stars_m, rmars))
p1b_profile_corrs

# add class variable to main dataset
phase1_data$profile <- p1b_profiles$Class

# get class frequencies
dplyr::count(phase1_data, vars = profile)

p1b_profile_freq <- phase1_data %>% 
  dplyr::group_by(profile) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::mutate(percent = (n/sum(n))*100) 

# plot profiles

# create better plot (credit to my UG diss student, Skylar Taylor, for the code!)
p1b_estimates_table <- get_estimates(p1b_mod2) %>%
  dplyr::filter(Category == "Means") %>%
  dplyr::select("Parameter", "Estimate", "Class")

p1b_estimates_desc_table <- p1b_estimates_table |> 
  tidyr::pivot_wider(names_from = Parameter,
                     values_from = Estimate)

p1b_estimates_table$Class <- as.factor(p1b_estimates_table$Class)

p1b_lpa_plot <- p1b_estimates_table %>%
  ggplot(aes(x = Parameter, y = Estimate, group = Class, colour = Class)) +  
  geom_point() + 
  geom_line(aes(linetype = Class, colour = Class)) +
  scale_x_discrete(name = "Variable", 
                   labels = c("STARS", "STARS-M", "R-MARS", "R-MARS-S")) +
  scale_y_continuous(name = "Mean Value (Centred)", 
                     limits = c(-2.5, 2.5), 
                     n.breaks = 10) +
  labs(linetype = "Profile", colour = "Profile") +
  theme_classic()
p1b_lpa_plot

p1b_lpa_plot_bw <- p1b_estimates_table %>%
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
p1b_lpa_plot_bw

# check 7 profile solution
p1b_lpa_mod7 <- phase1_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>% 
  scale() %>% # centre variables
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(1:7) %>%
  tidyLPA::get_fit()

p1b_compare_cov7 <- phase1_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>% 
  scale() %>%
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(7, models = c(1,2,3,6)) %>%
  tidyLPA::get_fit()

p1b_mod7 <- phase1_data %>%
  dplyr::select(stars, stars_m, rmars, rmars_s) %>%
  scale() %>%
  tidyLPA::single_imputation(method = "missForest") %>%
  tidyLPA::estimate_profiles(7, variances = "varying", covariances = "zero")

p1b_estimates_table_mod7 <- get_estimates(p1b_mod7) %>%
  dplyr::filter(Category == "Means") %>%
  dplyr::select("Parameter", "Estimate", "Class")

p1b_estimates_desc_table_mod7 <- p1b_estimates_table_mod7 |> 
  tidyr::pivot_wider(names_from = Parameter,
                     values_from = Estimate)

p1b_estimates_table_mod7$Class <- as.factor(p1b_estimates_table_mod7$Class)

p1b_lpa_plot_mod7 <- p1b_estimates_table_mod7 %>%
  ggplot(aes(x = Parameter, y = Estimate, group = Class, colour = Class)) +  
  geom_point() + 
  geom_line(aes(linetype = Class, colour = Class)) +
  scale_x_discrete(name = "Variable", 
                   labels = c("STARS", "STARS-M", "R-MARS", "R-MARS-S")) +
  scale_y_continuous(name = "Mean Value (Centred)", 
                     limits = c(-2.5, 2.5), 
                     n.breaks = 10) +
  labs(linetype = "Profile", colour = "Profile") +
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
p1b_lpa_plot_mod7

p1b_lpa_plot_bw_mod7 <- p1b_estimates_table_mod7 %>%
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
p1b_lpa_plot_bw_mod7