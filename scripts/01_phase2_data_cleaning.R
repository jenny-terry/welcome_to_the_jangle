### DATA PREPARATION FOR PHASE 2 DATA ###

library(magrittr)
library(ggplot2)

# read in data file
phase2_data <- readxl::read_excel(here::here("data", "setc_data.xlsx"))

# remove unnecessary subheadings
phase2_data <- dplyr::slice(phase2_data, -1, -2)

# store initial N
p2_initial_n <- nrow(phase2_data)

# remove unnecessary columns
phase2_data %<>% 
  dplyr::select(-EndDate, -Finished, -Status, -RecordedDate, -ResponseId, -DistributionChannel, -UserLanguage, -`Create New Field or Choose From Dropdown...`, -id)

# convert variable names to lower case
names(phase2_data) <- tolower(names(phase2_data))

# remove spaces in variable names
names(phase2_data) <- stringr::str_replace_all(names(phase2_data), " ", "_")

# remove cases where consent was not given
p2_no_consent <- phase2_data %>% dplyr::filter(q1.2 == "2")
phase2_data %<>% dplyr::filter(q1.2 == "1")

# convert progress variable to numeric format
phase2_data$progress <- as.numeric(phase2_data$progress)

# retain only cases where more than 10% progress (i.e. completed more than just the ID question) was recorded
p2_no_responses <- phase2_data %>% dplyr::filter(progress <= 10)
phase2_data %<>% dplyr::filter(progress > 10)

# identify duplicate cases by self-generated id code
dups <- phase2_data[duplicated(phase2_data$q2.1)|duplicated(phase2_data$q2.1, fromLast = TRUE),]

# sort by id code
dups %>% dplyr::arrange(q2.1)

# remove duplicates
p2_duplicates <- phase2_data %>% dplyr::filter(startdate == "44140.145324074074" | startdate == "44282.673946759256")
phase2_data %<>% dplyr::filter(startdate != "44140.145324074074", startdate != "44282.673946759256")

# identify & remove speeders

# rename `duration_(in_seconds)` variable
phase2_data %<>%
  dplyr::rename(duration = `duration_(in_seconds)`)

# convert required variables to numeric
phase2_data$duration <- as.numeric(phase2_data$duration)
phase2_data$q6.2_page_submit <- as.numeric(phase2_data$q6.2_page_submit)
phase2_data$q7.2_page_submit <- as.numeric(phase2_data$q7.2_page_submit)

# check the distribution of the time taken to complete the survey to identify the cut-off time for speedy responders
speed_plot <- ggplot(phase2_data, aes(duration)) +
  geom_histogram(binwidth = 30) +
  xlim(0, 800)
speed_plot

# there appears to be a break at around 480s but we will use the more conservative estimate of 520s as determined by and used in the phase 1 data because there is a larger sample size which is presumably more accurate

# establish the % of the survey completed by each participant
# ncol-3 is used to ignore the Qualtrics metadata still in the dataset
phase2_data %<>%
  dplyr::mutate(percent_complete = 100/(ncol(.)-3)*apply(., MARGIN = 1, function(x) sum(!is.na(x))))

# calculate a new variable (a) which contains the no. of seconds taken to complete the survey (duration) minus the no. of seconds taken to complete the MCQs
phase2_data %<>% 
  dplyr::mutate(a = ifelse(is.na(q6.2_page_submit), 
                           duration - q7.2_page_submit, 
                           duration - q6.2_page_submit))   

# calculate the rate at which participants completed 1% of the survey and store that in a new variable (b)
phase2_data %<>% dplyr::mutate(b = a/percent_complete)


# calculate the difference between the expected plausible rate (5.20) and the actual rate and store that in a new variable (c)
phase2_data %<>% dplyr::mutate(c = b - 5.20)

# if the difference (c) is greater than 0, the participant will have completed the survey faster than has been deemed plausible and will be removed

# first, we want to store the suspected speeders in a new object so that we can run further checks on them

# store cases where c is below zero 
p2_speeders <- phase2_data %>% dplyr::filter(c < 0)

# check that the speeders are not being removed simply because there is a large % missing (which might happen if it takes significantly less time to skip questions than to respond to them)

apply(p2_speeders, MARGIN = 1, function(x) sum(is.na(x)))
apply(phase2_data, MARGIN = 1, function(x) sum(is.na(x)))

# the no. missing for speeders seems consistent with the no. missing in the retained data so there is no concern that we are screening participants out unnecessarily

# retain cases where c is greater or equal to zero 
phase2_data %<>% dplyr::filter(c >= 0 | is.na(c))

# Qualtrics did not create a variable that indicates which of the conditions (mcq test: stats, maths) participants were allocated to so we need to create such a variable

# participants that took the maths mcq will have NA values for the variable which records the time they submitted the answers to the stats mcq questions ('Page Submit') and vice-versa
# the 'Page Submit' variables will, therefore, be used to identify which mcq test participants took and stored as a new categorical variable
# participants that did not progress through to the mcq stage of the survey will have NAs for both the maths and stats versions of the 'Page Submit' variable and these will be stored as NA in the new mcq variable
phase2_data %<>% 
  dplyr::mutate(mcq = dplyr::case_when(
    is.na(q6.2_page_submit) & is.na(q7.2_page_submit) ~ "NA",
    !is.na(q6.2_page_submit) ~ "stats", 
    !is.na(q7.2_page_submit) ~ "maths")
  )

# change the id codes to enhance anonymity and ensure consistency of id data.
# stupidly, I forgot to set the seed
phase2_data %<>% dplyr::rename(id = q2.1)
phase2_data$id <- stringi::stri_rand_strings(n = 245, length = 5)

#remove the variables only needed in the data preparation stage
phase2_data %<>% dplyr::select(-startdate, -progress, -duration, -q1.2, -q6.2_first_click:-q6.2_click_count, -q7.2_first_click:-q7.2_click_count, -a, -b, -c, -percent_complete, -sc0)

# create cleaned data file (uncomment if need to recreate, but note the id vars will be different)
# readr::write_csv(phase2_data, here::here("data", "phase2_data.csv"), col_names = T)