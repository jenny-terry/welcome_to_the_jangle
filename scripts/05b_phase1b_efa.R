library(magrittr)

# read in anonymised data file
phase1_data <- readr::read_csv(here::here("data", "phase1_anon_data.csv"))

# select only variables required for this analysis
p1b_efa_data <- dplyr::select(phase1_data, q4.1_1:q4.4_19)

#rename variables (yes, there probably is a better way to do this but it is more complex than it looks!)
p1b_efa_data %<>%
  dplyr::rename(
    "STARS - Test: Studying for an examination in a statistics course" = q4.1_1, 
    "STARS - Int: Interpreting the meaning of a table in a journal article" = q4.1_2, 
    "STARS - Help: Going to ask my statistics teacher for individual help with material I am having difficulty understanding" = q4.1_3, 
    "STARS - Test: Doing the coursework for a statistics course" = q4.1_4, 
    "STARS - Int: Making an objective decision based on empirical data" = q4.1_5, 
    "STARS - Int: Reading a journal article that includes some statistical analyses" = q4.1_6, 
    "STARS - Int: Trying to decide which analysis is appropriate for my research project" = q4.1_7, 
    "STARS - Test: Doing an examination in a statistics course" = q4.1_8, 
    "STARS - Int: Reading an advertisement for a car which includes figures on miles per gallon, depreciation, etc."
    = q4.1_9, 
    "STARS - Test: Walking into the room to take a statistics test"
    = q4.1_10, 
    "STARS - Int: Interpreting the meaning of a probability value once I have found it" = q4.1_11, 
    "STARS - Int: Arranging to have a body of data put into the computer" = q4.1_12, 
    "STARS - Test: Finding that another student in class got a different answer than I did to a statistical problem" = q4.1_13, 
    "STARS - Int: Determining whether to reject or retain the null hypothesis" = q4.1_14, 
    "STARS - Test: Waking up in the morning on the day of a statistics test" = q4.1_15, 
    "STARS - Help: Asking one of my teachers for help in understanding statistical output" = q4.1_16,
    "STARS - Int: Trying to understand the odds in a lottery" = q4.1_17, 
    "STARS - Int: Watching a student search through a load of computer output from his/her research" = q4.1_18,
    "STARS - Help: Asking someone in the computer lab for help in understanding statistical output" = q4.1_19,
    "STARS - Int: Trying to understand the statistical analyses described in the abstract of a journal article" = q4.1_20, 
    "STARS - Test: Enrolling in a statistics course" = q4.1_21, 
    "STARS - Test: Going over a final examination in statistics after it has been marked" = q4.1_22,
    "STARS - Help: Asking a fellow student for help in understanding output" = q4.1_23, 
    "STARS-M - Test: Studying for an examination in a maths course" = q4.2_1, 
    "STARS-M - Int: Interpreting numbers in a table in a journal article" = q4.2_2, 
    "STARS-M - Help: Going to ask my maths teacher for individual help with material I am having difficulty understanding" = q4.2_3, 
    "STARS-M - Test: Doing the coursework for a maths course" = q4.2_4, 
    "STARS-M - Int: Making an objective decision based on numerical information" = q4.2_5, 
    "STARS-M - Int: Reading a journal article that includes some mathematical analyses" = q4.2_6, 
    "STARS-M - Int: Trying to decide how to approach a mathematical problem to solve it" = q4.2_7, 
    "STARS-M - Test: Doing an examination in a maths course" = q4.2_8, 
    "STARS-M - Test: Walking into the room to take a maths test" = q4.2_9, 
    "STARS-M - Int: Interpreting the probability of it raining on a weather app" = q4.2_10, 
    "STARS-M - Test: Finding that another student in class got a different answer than I did to a mathematical problem" = q4.2_11, 
    "STARS-M - Int: Determining whether a mathematical statement is true or false" = q4.2_12, 
    "STARS-M - Test: Waking up in the morning on the day of a maths test" = q4.2_13, 
    "STARS-M - Help: Asking one of my teachers for help in understanding a mathematical solution" = q4.2_14, 
    "STARS-M - Int: Watching a student search through a load of computer output from his/her maths project" = q4.2_15, 
    "STARS-M - Help: Asking someone in the computer lab for help in understanding a mathematical solution" = q4.2_16,
    "STARS-M - Int: Trying to understand numerical information described in an article" = q4.2_17, 
    "STARS-M - Test: Enrolling in a maths course" = q4.2_18,
    "STARS-M - Test: Going over a final examination in maths after it has been marked" = q4.2_19,
    "STARS-M - Help: Asking a fellow student for help in understanding a mathematical solution" = q4.2_20, 
    "R-MARS - Test: Studying for a maths test" = q4.3_1, 
    "R-MARS - Test: Taking the maths section of a university entrance exam" = q4.3_2, 
    "R-MARS - Test: Taking an exam (quiz) in a maths course" = q4.3_3, 
    "R-MARS - Test: Taking an exam (final) in a maths course" = q4.3_4, 
    "R-MARS - Test: Thinking about an upcoming maths test 1 week before" = q4.3_5, 
    "R-MARS - Test: Thinking about an upcoming maths test 1 day before" = q4.3_6, 
    "R-MARS - Test: Thinking about an upcoming maths test 1 hour before" = q4.3_7, 
    "R-MARS - Test: Realizing you have to take a certain number of maths classes to fulfill requirements for your degree" = q4.3_8, 
    "R-MARS - Test: Receiving your final maths grade" = q4.3_9, 
    "R-MARS - Test: Being given a suprise test in a maths class" = q4.3_10, 
    "R-MARS - Num: Reading a cash register receipt after your purchase" = q4.3_11, 
    "R-MARS - Num: Being given a set of numerical problems involving addition to solve on paper" = q4.3_12, 
    "R-MARS - Num: Being given a set of subtraction problems to solve" = q4.3_13, 
    "R-MARS - Num: Being given a set of multiplication problems to solve" = q4.3_14, 
    "R-MARS - Num: Being given a set of division problems to solve" = q4.3_15, 
    "R-MARS - Course: Buying a maths textbook" = q4.3_16, 
    "R-MARS - Course: Watching a teacher work on an algebraic equation on the board" = q4.3_17, 
    "R-MARS - Course: Signing up for a maths course" = q4.3_18, 
    "R-MARS - Course: Listening to another student explain a maths formula" = q4.3_19,
    "R-MARS - Course: Walking into a maths class" = q4.3_20, 
    "R-MARS-S - Test: Studying for a statistics test" = q4.4_1,
    "R-MARS-S - Test: Taking the statistics section of a university entrance exam" = q4.4_2, 
    "R-MARS-S - Test: Taking an exam (quiz) in a statistics course" = q4.4_3, 
    "R-MARS-S - Test: Taking an exam (final) in a statistics course" = q4.4_4,
    "R-MARS-S - Test: Thinking about an upcoming statistics test 1 week before" = q4.4_5, 
    "R-MARS-S - Test: Thinking about an upcoming statistics test 1 day before" = q4.4_6, 
    "R-MARS-S - Test: Thinking about an upcoming statistics test 1 hour before" = q4.4_7, 
    "R-MARS-S - Test: Realizing you have to take a certain number of statistics classes to fulfill requirements for your degree" = q4.4_8, 
    "R-MARS-S - Test: Receiving your final statistics grade" = q4.4_9, 
    "R-MARS-S - Test: Being give a surprise test in a statistics class" = q4.4_10, 
    "R-MARS-S - Num: Calculating the deviances of a set of scores on paper, with each deviance being the difference between the mean of the scores and each individual score in the set" = q4.4_11, 
    "R-MARS-S - Num: Calculating the squared deviances by multiplying each deviance by itself" = q4.4_12, 
    "R-MARS-S - Num: Calculating the sum of squared deviances by adding the squared deviances together" = q4.4_13, 
    "R-MARS-S - Num: Calculating the variance of scores by dividing the sum of squared deviances by the number of scores" = q4.4_14, 
    "R-MARS-S - Course: Buying a statistics textbook" = q4.4_15, 
    "R-MARS-S - Course: Watching a teacher work on a statistical equation on the board" = q4.4_16, 
    "R-MARS-S - Course: Signing up for a statistics course" = q4.4_17, 
    "R-MARS-S - Course: Listening to another student explain a statistics formula" = q4.4_18, 
    "R-MARS-S - Course: Walking into a statistics class" = q4.4_19)

#check missing
p1b_efa_data %>% finalfit::ff_glimpse()

# descriptive statistics (this section hates piping, so meh)
p1b_efa_descriptives <- psych::describe(p1b_efa_data) 
p1b_efa_descriptives$item <- row.names(p1b_efa_descriptives)  
p1b_efa_descriptives <- tibble::as_tibble(p1b_efa_descriptives) 
p1b_efa_descriptives <- p1b_efa_descriptives |>
  dplyr::select(item, mean, sd, min, max, skew, kurtosis, se)

# covariance matrix using fiml
p1b_efa_cov <- psych::corFiml(p1b_efa_data)

# descriptive correlations 
p1b_efa_corr <- cov2cor(p1b_efa_cov)

# high & low corrs
p1b_efa_corr %<>% tibble::as_tibble() 
p1b_high_corrs <- sum(p1b_efa_corr >= 0.9)  
p1b_high_corrs70 <- sum(p1b_efa_corr >= 0.7)  
p1b_low_corrs <- sum(p1b_efa_corr < 0.2)
p1b_no_corrs <- nrow(p1b_efa_corr)*ncol(p1b_efa_corr)
p1b_high_corrs_percent <- p1b_high_corrs/p1b_no_corrs*100
p1b_high_corrs70_percent <- p1b_high_corrs70/p1b_no_corrs*100
p1b_low_corrs_percent <- p1b_low_corrs/p1b_no_corrs*100

# KMO test
p1b_efa_kmo <- psych::KMO(p1b_efa_data)

# determinant
p1b_efa_det <- det(as.matrix(p1b_efa_corr))

# test for the number of factors 
p1b_efa_out1 <- psych::fa.parallel(p1b_efa_cov, n.obs = 489, fa = "fa", fm = "pa", SMC = T, n.iter = 100)
p1b_efa_out2 <- psych::vss(p1b_efa_cov, n.obs = 489, fm = "pa")

# fancy scree plot
# https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/#psych

# create data frame 'obs' from observed eigenvalue data
obs <- data.frame(p1b_efa_out1$fa.values)
obs$type <- c('Observed Data')
obs$num <- c(row.names(obs))
obs$num <- as.numeric(obs$num)
colnames(obs) <- c('eigenvalue', 'type', 'num')

# calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
percentile <- apply(p1b_efa_out1$values, 2, function(x) quantile(x,.95))
min <- as.numeric(nrow(obs))
min <- (4*min) - (min-1)
max <- as.numeric(nrow(obs))
max <- 4*max
percentile1 <- percentile[min:max]

# create data frame called 'sim' with simulated eigenvalue data
sim <- data.frame(percentile1)
sim$type <- c('Simulated Data (95th %ile)')
sim$num <- c(row.names(obs))
sim$num <- as.numeric(sim$num)
colnames(sim) <- c('eigenvalue', 'type', 'num')

# merge the two data frames (obs and sim) together into data frame called eigendat
eigendat <- rbind(obs, sim)

# use data from eigendat, map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
library(ggplot2)
p1b_scree_plot <- ggplot(eigendat, aes(x = num, y = eigenvalue, shape = type)) +
  geom_line() +
  geom_point(size = 4) +
  scale_y_continuous(name = 'Eigenvalue') +
  scale_x_continuous(name = 'Factor Number', breaks = min(eigendat$num):max(eigendat$num)) +
  scale_shape_manual(values = c(16, 1)) +
  geom_vline(xintercept = p1b_efa_out1$nfact, linetype = 'dashed') +
  jtools::theme_apa()
p1b_scree_plot

# create indicator table
Indicator <- c("Parallel Analysis", "Scree Plot", "VSS Complexity 1", "VSS Complexity 2", "Velicer MAP", "BIC", "SABIC", "RMSEA", "SRMR")
Factors <- c(p1b_efa_out1$nfact, 9, 1, 2, 8, 7, 8, 8, 8)

p1b_efa_nfact_table <- tibble::tibble(Indicator, Factors) |> 
  dplyr::arrange(desc(Factors))

# rotate solution
p1b_efa_mod8 <- psych::fa(p1b_efa_data, nfactors = 8, rotate = "oblimin", fm = "ml", cor = "poly", SMC = T, p = .0)

# factor loading table (code is from this Stack Exchange post: https://stackoverflow.com/questions/17371266/extracting-output-from-principal-function-in-psych-package-as-a-data-frame)
# there is probably a much simpler way, but I am scared to mess with anything that is working!
getS3method("print","loadings")
printLoadings <- function (x, digits = 3, cutoff = 0, sort = FALSE, ...) 
{
  Lambda <- unclass(x)
  p <- nrow(Lambda)
  factors <- ncol(Lambda)
  if (sort) {
    mx <- max.col(abs(Lambda))
    ind <- cbind(1L:p, mx)
    mx[abs(Lambda[ind]) < 0.5] <- factors + 1
    Lambda <- Lambda[order(mx, 1L:p), ]
  }
  cat("\nLoadings:\n")
  fx <- format(round(Lambda, digits))
  names(fx) <- NULL
  nc <- nchar(fx[1L], type = "c")
  fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
  newx <- print(fx, quote = FALSE, ...)
  vx <- colSums(x^2)
  varex <- rbind(`SS loadings` = vx)
  if (is.null(attr(x, "covariance"))) {
    varex <- rbind(varex, `Proportion Var` = vx/p)
    if (factors > 1) 
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx/p))
  }
  cat("\n")
  print(round(varex, digits))
  invisible(newx)
}

p1b_efa_loadings8 <- printLoadings(p1b_efa_mod8$loadings, digits = 2, cutoff = 0, sort = T)
p1b_efa_loadings8 <- tibble::rownames_to_column(as.data.frame(p1b_efa_loadings8), "items")