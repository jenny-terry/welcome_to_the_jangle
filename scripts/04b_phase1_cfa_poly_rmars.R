library(magrittr)
options(scipen = 999)

# read in anonymised data file
phase1_data <- readr::read_csv(here::here("data", "phase1_anon_data.csv"))

# prepare dataset for factor analysis
rmars_poly_data <- phase1_data %>% dplyr::select(., q4.3_1:q4.3_20)

# rename items
rmars_poly_data %<>% 
  dplyr::rename(
    rmars_test1 = q4.3_1, 
    rmars_test2 = q4.3_2, 
    rmars_test3 = q4.3_3, 
    rmars_test4 = q4.3_4, 
    rmars_test5 = q4.3_5, 
    rmars_test6 = q4.3_6, 
    rmars_test7 = q4.3_7, 
    rmars_test8 = q4.3_8, 
    rmars_test9 = q4.3_9, 
    rmars_test10 = q4.3_10, 
    rmars_num1 = q4.3_11, 
    rmars_num2 = q4.3_12, 
    rmars_num3 = q4.3_13, 
    rmars_num4 = q4.3_14, 
    rmars_num5 = q4.3_15, 
    rmars_course1 = q4.3_16, 
    rmars_course2 = q4.3_17, 
    rmars_course3 = q4.3_18, 
    rmars_course4 = q4.3_19,
    rmars_course5 = q4.3_20
  )

# specify measurement model
rmars_mod <- 'test =~ rmars_test1 + rmars_test2 + rmars_test3 + rmars_test4 + rmars_test5 + rmars_test6 +                         rmars_test7 + rmars_test8 + rmars_test9 + rmars_test10
              num =~ rmars_num1 + rmars_num2 + rmars_num3 + rmars_num4 + rmars_num5
              course =~ rmars_course1 + rmars_course2 + rmars_course3 + rmars_course4 + rmars_course5'

# fit the model
p1_rmars_poly_cfa_out <- lavaan::cfa(rmars_mod, data = rmars_poly_data, estimator = "WLSMV", ordered = names(rmars_poly_data), std.lv = TRUE)

# display model output
p1_rmars_poly_cfa_summary <- lavaan::summary(p1_rmars_poly_cfa_out, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)

# print table of fit measures
p1_rmars_poly_cfa_fit <- lavaan::fitMeasures(p1_rmars_poly_cfa_out, c("chisq.scaled","df.scaled","pvalue.scaled", "cfi.scaled", "cfi.robust", "tli.robust", "tli.scaled", "rmsea.robust", "rmsea.ci.upper.robust", "rmsea.ci.lower.robust", "rmsea.scaled", "rmsea.ci.upper.scaled", "rmsea.ci.lower.scaled", "srmr"))
p1_rmars_poly_cfa_fit_inline <- tidyr::as_tibble(p1_rmars_poly_cfa_fit)

Value <- p1_rmars_poly_cfa_fit_inline
`Fit Measure` <- tidyr::as_tibble(c("chisq.scaled","df.scaled","pvalue.scaled", "cfi.scaled", "cfi.robust", "tli.robust", "tli.scaled", "rmsea.robust", "rmsea.ci.upper.robust", "rmsea.ci.lower.robust", "rmsea.scaled", "rmsea.ci.upper.scaled", "rmsea.ci.lower.scaled", "srmr"))
p1_rmars_poly_cfa_fit_table <- tibble::tibble(`Fit Measure`, Value, .name_repair = "unique") |>
  dplyr::rename("Fit Measure" = `value...1`,
                "Value" = `value...2`)

#null RMSEA
p1_rmars_poly_nullRMSEA <- semTools::nullRMSEA(p1_rmars_poly_cfa_out)

# print standardised residuals
p1_rmars_poly_cfa_sr <- lavaan::lavResiduals(p1_rmars_poly_cfa_out, zstat = TRUE)

# print modification indices above 4, sorted by size
p1_rmars_poly_cfa_mi <- lavaan::modindices(p1_rmars_poly_cfa_out, sort. = TRUE)

# print table of parameter estimates (for ease of reference)
p1_rmars_poly_cfa_estimates <- lavaan::parameterEstimates(p1_rmars_poly_cfa_out, standardized = TRUE)

# plot CFA model 
p1_rmars_poly_plot <- semPlot::semPaths(p1_rmars_poly_cfa_out, whatLabels = "std")

