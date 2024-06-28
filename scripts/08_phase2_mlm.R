library(magrittr)
library(ggplot2)

# read in anonymised data file
phase2_data <- readr::read_csv(here::here("data", "phase2_anon_data.csv"))

# check missing data at item level
p2_mlm_item_missing <- phase2_data %>% 
  dplyr::select(c(q5.1_1:q5.2_21, q8.1_1:q8.1_21)) %>%
  finalfit::ff_glimpse()

p2_mlm_item_missing_max <- max(p2_mlm_item_missing$Continuous$missing_percent)

# check % item missing per scale
p2_mlm_stars_item_missing <- phase2_data %>%
  dplyr::select(c(q4.1_1:q4.1_23)) %>%
  naniar::pct_miss()

p2_mlm_rmars_item_missing <- phase2_data %>%
  dplyr::select(c(q4.3_1:q4.3_20)) %>%
  naniar::pct_miss()

p2_mlm_trait_item_missing <- phase2_data %>%
  dplyr::select(c(q5.1_1:q5.1_21)) %>%
  naniar::pct_miss()

p2_mlm_pre_state_item_missing <- phase2_data %>%
  dplyr::select(c(q5.2_1:q5.2_21)) %>%
  naniar::pct_miss()

p2_mlm_post_state_item_missing <- phase2_data %>%
  dplyr::select(c(q8.1_1:q8.1_21)) %>%
  naniar::pct_miss()

# check pattern of missing
p2_mlm_pattern_missing <- phase2_data %>% 
  dplyr::select(c(q4.1_1:q5.2_21, q8.1_1:q8.1_21)) 

p2_mlm_pattern_missing %>%
  mice::md.pattern()

# check mcar
naniar::mcar_test(p2_mlm_pattern_missing)

# check dependancy on outcome (just a sample)
explanatory = c("q5.2_1", "q5.2_2", "q5.2_3", "q5.2_4")
dependent = c("q8.1_1")

p2_mlm_pattern_missing %>% 
  finalfit::missing_pairs(dependent, explanatory)

psych::ICC(p2_mlm_pattern_missing, missing = T)

# create composite scores

# stars mean
phase2_data <- phase2_data %>% dplyr::mutate(stars = rowMeans(dplyr::select(., dplyr::starts_with("q4.1")), na.rm = TRUE))

# rmars mean
phase2_data <- phase2_data %>% dplyr::mutate(rmars = rowMeans(dplyr::select(., dplyr::starts_with("q4.3")), na.rm = TRUE))

# sticsa trait mean
phase2_data <- phase2_data %>% dplyr::mutate(trait = rowMeans(dplyr::select(., dplyr::starts_with("q5.1")), na.rm = TRUE))

# sticsa pre-state mean
phase2_data <- phase2_data %>% dplyr::mutate(pre_state = rowMeans(dplyr::select(., dplyr::starts_with("q5.2")), na.rm = TRUE))

# sticsa post-state mean
phase2_data <- phase2_data %>% dplyr::mutate(post_state = rowMeans(dplyr::select(., dplyr::starts_with("q8.1")), na.rm = TRUE))

# select variables that we'll be using in this analysis
p2_mlm_data <- dplyr::select(phase2_data, id, stars, rmars, trait, pre_state, post_state, mcq)

# check missing data per variable
p2_mlm_data %>% 
  dplyr::summarise_each(list(~sum(is.na(.)))) %>%
  tidyr::gather()

p2_mlm_missing_check <- p2_mlm_data |> 
  dplyr::filter(is.na(trait) | is.na(pre_state) | is.na(post_state) | is.na(mcq))

p2_mlm_missing_n <- p1_mlm_data |>
  dplyr::filter(is.na(trait)|is.na(pre_state) | is.na(post_state) | is.na(mcq)) |>
  nrow()

p2_mlm_data %<>% dplyr::filter(!is.na(trait) & !is.na(pre_state) & !is.na(post_state) & !is.na(mcq))

p2_mlm_n <- nrow(p2_mlm_data)

# descriptives

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

p2_mlm_summary <- p2_mlm_data |>
  dplyr::select(stars, rmars, trait, pre_state, post_state, mcq) |>
  dplyr::rename(STARS = stars,
                "R-MARS" = rmars,
                Trait = trait,
                "State (Pre)"= pre_state,
                "State (Post)" = post_state,
                MCQ = mcq) |>
  dplyr::group_by(MCQ) |>
  dplyr::summarise(
    dplyr::across(c("STARS":"State (Post)"), fun_list)
  ) |>
  tidyr::pivot_longer(!MCQ,
                      names_to = c("Variable", ".value"),
                      names_sep = "_") |>
  dplyr::arrange(Variable)


# standardise maths anxiety and stats anxiety (mean = 0, sd = 1)
p2_mlm_data$stars <- scale(p2_mlm_data$stars)
p2_mlm_data$rmars <- scale(p2_mlm_data$rmars)

# convert wide to long
p2_long_data <- p2_mlm_data %>% tidyr::pivot_longer(cols = c("pre_state", "post_state"), names_to = "time", values_to = "state_anx")

# convert character variables to factors
p2_long_data$mcq <- factor(p2_long_data$mcq)
p2_long_data$id <- factor(p2_long_data$id)

# create numerical time variable
p2_long_data %<>% dplyr::mutate(
  time_num = ifelse(time == "pre_state", 0, 1)
)

# group mean centre level 1 continuous predictors
p2_long_data <- p2_long_data %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(gmc_trait = mean(trait, na.rm = TRUE))

# create ICC function (credit to Rod Bond)
ICC <- function(out) {
  var_est <- as.data.frame(VarCorr(out))[4] 
  return(paste("ICC = ", var_est[1, 1]/sum(var_est)))
}

# unconditional means model
p2_mod_a <- lmerTest::lmer(state_anx ~ 1 + (1|id), p2_long_data, REML = FALSE)
p2_mod_a_summary <- summary(p2_mod_a)
p2_mod_a_sum_ci <- confint(p2_mod_a)

icc_null <- ICC(p2_mod_a)

# unconditional_growth
p2_mod_b <- lmerTest::lmer(state_anx ~ time_num + (1|id), p2_long_data, REML = FALSE)
p2_mod_b_summary <- summary(p2_mod_b)
p2_mod_b_sum_ci <- tibble::as_tibble(confint(p2_mod_b)) |> 
  dplyr::rename("lower" = "2.5 %",
                "upper" = "97.5 %")

# interaction model equation 1
p2_mod_c <- lmerTest::lmer(state_anx ~ gmc_trait + time_num*mcq*stars + (1|id) + (0 + time_num|id), p2_long_data, REML = FALSE)
p2_mod_c_summary <- summary(p2_mod_c)
p2_mod_c_sum_ci <- tibble::as_tibble(confint(p2_mod_c)) |> 
  dplyr::rename("lower" = "2.5 %",
                "upper" = "97.5 %")

# interaction model equation 2
p2_mod_d <- lmerTest::lmer(state_anx ~ gmc_trait + time_num*mcq*rmars + (1|id) + (0 + time_num|id), p2_long_data, REML = FALSE)
p2_mod_d_summary <- summary(p2_mod_d)
p2_mod_d_sum_ci <- tibble::as_tibble(confint(p2_mod_d)) |> 
  dplyr::rename("lower" = "2.5 %",
                "upper" = "97.5 %")

p2_mod_d_homo <- plot(p2_mod_d) # mixed advice on whether this is an assumption
p2_mod_d_qqnorm <- qqnorm(resid(p2_mod_d))

# rename vars for plots ('cos this is much easier than trying to change facet labels when using sjPlot!)
p2_long_data %<>% dplyr::rename(`Statistics Anxiety` = stars,
                                `Maths Anxiety` = rmars)

#re-run p2_models
p2_mod_c_plot <- lmerTest::lmer(state_anx ~ gmc_trait + time_num*mcq*`Statistics Anxiety` + (1|id) + (0 + time_num|id), p2_long_data, REML = FALSE)

p2_mod_d_plot <- lmerTest::lmer(state_anx ~ gmc_trait + time_num*mcq*`Maths Anxiety` + (1|id) + (0 + time_num|id), p2_long_data, REML = FALSE)

# plots (for Scooby poster)

#p2_mlm_plot_eq1_poster <- sjPlot::plot_model(p2_mod_c_plot, type = "pred", terms = c("time_num", "mcq", "Statistics Anxiety")) +
#coord_cartesian(ylim = c(1, 4)) +
#scale_y_continuous(breaks = seq(1, 4, 0.5)) +
#scale_x_continuous(breaks = c(0, 1), labels = c("Pre", "Post")) +
#scale_colour_manual(values = c("#ffa500", "#2462E5"), name = "MCQ condition") +
#scale_fill_manual(values = c("#ffa500", "#2462E5"), name = "MCQ condition") +
#labs(x = "Time", y = "State anxiety (mean, range 1:4)", colour = "MCQ condition", title = "Predicted mean values of state anxiety change at different levels of \nstatistics anxiety for each MCQ condition") +
#theme(strip.background = element_rect(fill = "black"), strip.text.x = element_text(size = 10, color = "white"))
#p2_mlm_plot_eq1_poster 

#p2_mlm_plot_eq2_poster <-sjPlot::plot_model(p2_mod_d_plot, type = "pred", terms = c("time_num", "mcq", "Mathematics Anxiety")) +
#coord_cartesian(ylim = c(1, 4)) +
#scale_y_continuous(breaks = seq(1, 4, 0.5)) +
#scale_x_continuous(breaks = c(0, 1), labels = c("Pre", "Post")) +
#scale_colour_manual(values = c("#ffa500", "#2462E5"), name = "MCQ condition") +
#scale_fill_manual(values = c("#ffa500", "#2462E5"), name = "MCQ condition") +
#labs(x = "Time", y = "State anxiety (mean, range 1:4)", colour = "MCQ condition", title = "Predicted mean values of state anxiety change at different levels of \nMATHS ANXIETY for each MCQ condition") +
#theme(strip.background = element_rect(fill = "black"), strip.text.x = element_text(size = 10, color = "white"))
#p2_mlm_plot_eq2_poster

# plots (for manuscript)

p2_mlm_plot_eq1 <- sjPlot::plot_model(p2_mod_c_plot, type = "pred", 
                                      terms = c("time_num", "mcq", "Statistics Anxiety")) +
  scale_y_continuous(breaks = seq(1, 4, 0.5), 
                     limits = c(1, 4)) +
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Pre", "Post")) +
  scale_colour_discrete(labels = c("Maths MCQ", "Statistics MCQ"), 
                        type = c("black", "darkgrey")) +
  scale_fill_manual(values = c("black", "darkgrey")) +
  labs(x = "Time (Pre- or Post- MCQ)", 
       y = "State Anxiety (Range 1:4)", 
       colour = "MCQ Condition") +
  jtools::theme_apa() +
  theme(plot.title = element_blank(),
        
        axis.title.x = element_text(family = "serif", 
                                    size = 14,),
        axis.text.x = element_text(family = "serif"),
        
        axis.title.y = element_text(family = "serif", 
                                    size = 14,),
        axis.text.y = element_text(family = "serif"),
        
        legend.text = element_text(family = "serif", size = 14),
        
        strip.text.x = element_text(family = "serif", size = 14),
        
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        
        axis.line = element_line(color = "black", size = .5))
p2_mlm_plot_eq1

p2_mlm_plot_eq2 <-sjPlot::plot_model(p2_mod_d_plot, type = "pred", terms = c("time_num", "mcq", "Maths Anxiety")) +
  scale_y_continuous(breaks = seq(1, 4, 0.5), 
                     limits = c(1, 4)) +
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Pre", "Post")) +
  scale_colour_discrete(labels = c("Maths MCQ", "Statistics MCQ"), 
                        type = c("black", "darkgrey")) +
  scale_fill_manual(values = c("black", "darkgrey")) +
  labs(x = "Time (Pre- or Post- MCQ)", 
       y = "State Anxiety (Range 1:4)", 
       colour = "MCQ Condition") +
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
p2_mlm_plot_eq2