### DATA PREPARATION FOR PHASE 1 DATA ###

library(magrittr)
library(ggplot2)

# read in data files (one from SONA, one from QQM)
seta_data <- readxl::read_excel(here::here("data", "seta_data.xlsx"))
setb_data <- readxl::read_excel(here::here("data", "setb_data.xlsx"))

# remove unnecessary subheadings
seta_data <- dplyr::slice(seta_data, -1)
setb_data <- dplyr::slice(setb_data, -1)

# remove Q9 in dataset a (used for QQM pedagogical exercise).
seta_data %<>% dplyr::select(-Q9.1_1:-Q9.1_28)

# join the two datasets
phase1_data <- dplyr::bind_rows(seta_data, setb_data)

# store initial N
p1_initial_n <- nrow(phase1_data)

# remove unnecessary columns
phase1_data %<>% 
  dplyr::select(-EndDate, -Finished, -Status, -RecordedDate, -ResponseId, -DistributionChannel, -UserLanguage, -`Create New Field or Choose From Dropdown...`, -id)

# convert variable names to lower case
names(phase1_data) <- tolower(names(phase1_data))

# remove spaces in variable names
names(phase1_data) <- stringr::str_replace_all(names(phase1_data), " ", "_")

# remove cases where consent was not given
p1_no_consent <- phase1_data %>% dplyr::filter(q1.2 == "2")
phase1_data %<>% dplyr::filter(q1.2 == "1")

# convert progress variable to numeric format
phase1_data$progress <- as.numeric(phase1_data$progress)

# retain only cases where more than 10% progress (i.e. completed more than just the ID question) was recorded
p1_no_responses <- phase1_data %>% dplyr::filter(progress <= 10)
phase1_data %<>% dplyr::filter(progress > 10)

# identify duplicate cases by self-generated id code
dups <- phase1_data[duplicated(phase1_data$q2.1)|duplicated(phase1_data$q2.1, fromLast = TRUE),]

# sort by id code
dups %>% dplyr::arrange(q2.1)

# remove duplicates
p1_duplicates <- phase1_data %>% dplyr::filter(startdate == "43899.332395833335" | startdate == "43908.593530092592" | startdate == "43903.2502662037" | startdate == "43906.275590277779")

phase1_data %<>% dplyr::filter(startdate != "43899.332395833335", startdate != "43908.593530092592", startdate != "43903.2502662037", startdate != "43906.275590277779")

# identify & remove speeders

# rename `duration_(in_seconds)` variable
phase1_data %<>%
  dplyr::rename(duration = `duration_(in_seconds)`)

# convert required variables to numeric
phase1_data$duration <- as.numeric(phase1_data$duration)
phase1_data$q6.2_page_submit <- as.numeric(phase1_data$q6.2_page_submit)
phase1_data$q7.2_page_submit <- as.numeric(phase1_data$q7.2_page_submit)

# check the distribution of the time taken to complete the survey to identify the cut-off time for speedy responders
speed_plot <- ggplot(phase1_data, aes(duration)) +
  geom_histogram(binwidth = 30) +
  xlim(0, 800)
speed_plot

# there appears to be breaks at around 380s and 520s but, because 380s is considerably under my trial times (M = 485s), we will use the more conservative estimate of 520s

# establish the % of the survey completed by each participant
# ncol-3 is used to ignore the Qualtrics metadata still in the dataset
phase1_data %<>%
  dplyr::mutate(percent_complete = 100/(ncol(.)-3)*apply(., MARGIN = 1, function(x) sum(!is.na(x))))

# calculate a new variable (a) which contains the no. of seconds taken to complete the survey (duration) minus the no. of seconds taken to complete the MCQs
phase1_data %<>% 
  dplyr::mutate(a = ifelse(is.na(q6.2_page_submit), 
                           duration - q7.2_page_submit, 
                           duration - q6.2_page_submit))   

# calculate the rate at which participants completed 1% of the survey and store that in a new variable (b)
phase1_data %<>% dplyr::mutate(b = a/percent_complete)


# calculate the difference between the expected plausible rate (5.20) and the actual rate and store that in a new variable (c)
phase1_data %<>% dplyr::mutate(c = b - 5.20)

# if the difference (c) is greater than 0, the participant will have completed the survey faster than has been deemed plausible and will be removed

# first, we want to store the suspected speeders in a new object so that we can run further checks on them

# store cases where c is below zero 
p1_speeders <- phase1_data %>% dplyr::filter(c < 0)

# check that the speeders are not being removed simply because there is a large % missing (which might happen if it takes significantly less time to skip questions than to respond to them)

apply(p1_speeders, MARGIN = 1, function(x) sum(is.na(x)))
apply(phase1_data, MARGIN = 1, function(x) sum(is.na(x)))

# the no. missing for speeders seems consistent with the no. missing in the retained data so there is no concern that we are screening participants out unnecessarily

# retain cases where c is greater or equal to zero 
phase1_data %<>% dplyr::filter(c >= 0 | is.na(c))

# Qualtrics did not create a variable that indicates which of the conditions (mcq test: stats, maths) participants were allocated to so we need to create such a variable

# participants that took the maths mcq will have NA values for the variable which records the time they submitted the answers to the stats mcq questions ('Page Submit') and vice-versa
# the 'Page Submit' variables will, therefore, be used to identify which mcq test participants took and stored as a new categorical variable
# participants that did not progress through to the mcq stage of the survey will have NAs for both the maths and stats versions of the 'Page Submit' variable and these will be stored as NA in the new mcq variable
phase1_data %<>% 
  dplyr::mutate(mcq = dplyr::case_when(
    is.na(q6.2_page_submit) & is.na(q7.2_page_submit) ~ "NA",
    !is.na(q6.2_page_submit) ~ "stats", 
    !is.na(q7.2_page_submit) ~ "maths")
  )

# change the id codes to enhance anonymity and ensure consistency of id data.
set.seed(140424)
phase1_data %<>% dplyr::rename(id = q2.1)
phase1_data$id <- stringi::stri_rand_strings(n = 489, length = 5)

#remove the variables only needed in the data preparation stage
phase1_data %<>% dplyr::select(-startdate, -progress, -duration, -q1.2, -q6.2_first_click:-q6.2_click_count, -q7.2_first_click:-q7.2_click_count, -a, -b, -c, -percent_complete)

# create cleaned data file
#readr::write_csv(phase1_data, here::here("data", "phase1_data_new.csv"), col_names = T)