library(magrittr)
options(scipen = 999)

# read in anonymised data file
phase1_data <- readr::read_csv(here::here("data", "phase1_anon_data.csv"))

# select vars of interest
stars_data <- phase1_data %>% dplyr::select(q4.1_1:q4.1_23)

# rename items
stars_data %<>% 
  dplyr::rename(
    stars_test1 = q4.1_1, 
    stars_int1 = q4.1_2, 
    stars_help1 = q4.1_3, 
    stars_test2 = q4.1_4, 
    stars_int2 = q4.1_5, 
    stars_int3 = q4.1_6, 
    stars_int4 = q4.1_7, 
    stars_test3 = q4.1_8, 
    stars_int5 = q4.1_9, 
    stars_test4 = q4.1_10, 
    stars_int6 = q4.1_11, 
    stars_int7 = q4.1_12, 
    stars_test5 = q4.1_13, 
    stars_int8 = q4.1_14, 
    stars_test6 = q4.1_15, 
    stars_help2 = q4.1_16,
    stars_int9 = q4.1_17, 
    stars_int10 = q4.1_18,
    stars_help3 = q4.1_19,
    stars_int11 = q4.1_20, 
    stars_test7 = q4.1_21, 
    stars_test8 = q4.1_22,
    stars_help4 = q4.1_23
  )

# specify measurement model
stars_mod <- '
test =~ stars_test1 + stars_test2 + stars_test3 + stars_test4 + stars_test5 + stars_test6 + stars_test7 + stars_test8

help =~ stars_help1 + stars_help2 + stars_help3 + stars_help4

interpret =~ stars_int1 + stars_int2 + stars_int3 + stars_int4 + stars_int5 + stars_int6 + stars_int7 + stars_int8 + stars_int9 + stars_int10 + stars_int11
'

# fit the model 
p1_stars_cfa_out <- lavaan::cfa(stars_mod, data = stars_data, estimator = "MLR", std.lv = TRUE)

# display model output
p1_stars_cfa_summary <- lavaan::summary(p1_stars_cfa_out, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)

# print table of fit measures
p1_stars_cfa_fit <- lavaan::fitMeasures(p1_stars_cfa_out, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.upper", "rmsea.ci.lower", "srmr"))
p1_stars_cfa_fit_inline <- tidyr::as_tibble(p1_stars_cfa_fit)

Value <- p1_stars_cfa_fit_inline
`Fit Measure` <- tidyr::as_tibble(c("chisq","df","pvalue", "cfi", "tli", "rmsea", "rmsea.ci.upper", "rmsea.ci.lower", "srmr"))

p1_stars_cfa_fit_table <- tibble::tibble(`Fit Measure`, Value, .name_repair = "unique") |>
  dplyr::rename("Fit Measure" = `value...1`,
                "Value" = `value...2`)

#null RMSEA
p1_stars_nullRMSEA <- semTools::nullRMSEA(p1_stars_cfa_out)

# print standardised residuals
p1_stars_cfa_sr <- lavaan::lavResiduals(p1_stars_cfa_out, zstat = TRUE)

# print modification indices above 4, sorted by size
p1_stars_cfa_mi <- lavaan::modindices(p1_stars_cfa_out, sort. = TRUE)

# print table of parameter estimates (for ease of reference)
p1_stars_cfa_estimates <- lavaan::parameterEstimates(p1_stars_cfa_out, standardized = TRUE)

# plot CFA model 
p1_stars_plot <- semPlot::semPaths(p1_stars_cfa_out, whatLabels = "std")

