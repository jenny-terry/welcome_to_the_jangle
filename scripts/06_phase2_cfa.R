library(magrittr)

# read in anonymised data file
phase2_data <- readr::read_csv(here::here("data", "phase2_anon_data.csv"))

# prepare dataset for factor analysis
p2_cfa_data <- phase2_data %>% dplyr::select(., q4.1_1:q4.4_19)

p2_cfa_data %<>%
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
    stars_help4 = q4.1_23, 
    stars_m_test1 = q4.2_1, 
    stars_m_int1 = q4.2_2, 
    stars_m_help1 = q4.2_3, 
    stars_m_test2 = q4.2_4, 
    stars_m_int2 = q4.2_5, 
    stars_m_int3 = q4.2_6, 
    stars_m_int4 = q4.2_7, 
    stars_m_test3 = q4.2_8, 
    stars_m_test4 = q4.2_9, 
    stars_m_int6 = q4.2_10, 
    stars_m_test5 = q4.2_11, 
    stars_m_int8 = q4.2_12, 
    stars_m_test6 = q4.2_13, 
    stars_m_help2 = q4.2_14, 
    stars_m_int10 = q4.2_15, 
    stars_m_help3 = q4.2_16,
    stars_m_int11 = q4.2_17, 
    stars_m_test7 = q4.2_18,
    stars_m_test8 = q4.2_19,
    stars_m_help4 = q4.2_20, 
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
    rmars_course5 = q4.3_20, 
    rmars_s_test1 = q4.4_1,
    rmars_s_test2 = q4.4_2, 
    rmars_s_test3 = q4.4_3, 
    rmars_s_test4 = q4.4_4,
    rmars_s_test5 = q4.4_5, 
    rmars_s_test6 = q4.4_6, 
    rmars_s_test7 = q4.4_7, 
    rmars_s_test8 = q4.4_8, 
    rmars_s_test9 = q4.4_9, 
    rmars_s_test10 = q4.4_10, 
    rmars_s_num2 = q4.4_11, 
    rmars_s_num3 = q4.4_12, 
    rmars_s_num4 = q4.4_13, 
    rmars_s_num5 = q4.4_14, 
    rmars_s_course1 = q4.4_15, 
    rmars_s_course2 = q4.4_16, 
    rmars_s_course3 = q4.4_17, 
    rmars_s_course4 = q4.4_18, 
    rmars_s_course5 = q4.4_19)

# specify measurement model:
p2_cfa_mod <- '
interpret =~ stars_m_int1 + stars_m_int11 + stars_m_int3 + stars_int1 + stars_int11 + stars_int3 + stars_m_int2 + stars_m_int8 + stars_int6 + stars_int2 + stars_m_int6 + stars_int4 + stars_int10 + stars_m_int10 + stars_int9 + stars_int8 + stars_m_int4

help =~ stars_help1 + stars_help2 + stars_m_help1 + stars_m_help2 + stars_help3 + stars_m_help3 + stars_help4 + stars_m_help4

test_taking =~ rmars_s_test9 + rmars_s_test4 + rmars_test9 + rmars_s_test10 + stars_test3 + stars_test4 + rmars_test4 + rmars_s_test2 + rmars_s_test3  + + stars_m_test3 + stars_test8 + rmars_test10 + stars_test5 + rmars_test2 + stars_test6 

class =~ stars_m_test7 + rmars_s_course3 + stars_test7 + rmars_course3 + rmars_test8 + rmars_s_test8

test_antic =~ rmars_test5 + rmars_test6 + rmars_s_test5 + rmars_s_test6 + rmars_s_test7 + rmars_test7

stats_calc =~ rmars_s_num4 + rmars_s_num5 + rmars_s_num3 + rmars_s_num2

num_calc =~ rmars_num2 + rmars_num3 + rmars_num4 + rmars_num5 + rmars_num1
'

# fit the model 
p2_cfa_out <- lavaan::cfa(p2_cfa_mod, data = p2_cfa_data, missing = "ML", std.lv = TRUE)

# display model output
p2_cfa_summary <- lavaan::summary(p2_cfa_out, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)

#null RMSEA
p2_cfa_nullRMSEA <- semTools::nullRMSEA(p2_cfa_out)

# print table of fit measures
p2_cfa_fit <- lavaan::fitMeasures(p2_cfa_out, c("chisq","df","pvalue", "cfi", "tli", "rmsea", "rmsea.ci.upper", "rmsea.ci.lower", "srmr"))
p2_cfa_fit_inline <- tidyr::as_tibble(p2_cfa_fit)

Value <- p2_cfa_fit_inline
`Fit Measure` <- tidyr::as_tibble(c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.upper", "rmsea.ci.lower", "srmr"))

p2_cfa_fit_table <- tibble::tibble(`Fit Measure`, Value, .name_repair = "unique") |>
  dplyr::rename("Fit Measure" = `value...1`,
                "Value" = `value...2`)

# print standardised residuals
p2_cfa_residuals <- lavaan::residuals(p2_cfa_out, type = "standardized")

# print modification indices above 4, sorted by size
p2_cfa_mod_indices <- lavaan::modindices(p2_cfa_out, sort. = TRUE, minimum.value = 3.86)

# print table of parameter estimates (for ease of reference)
p2_cfa_estimates <- lavaan::parameterEstimates(p2_cfa_out, standardized = TRUE)

# plot CFA model 
p2_cfa_semplot <- semPlot::semPaths(p2_cfa_out, whatLabels = "std")

p2_cfa_loadings_table <- lavaan::parameterEstimates(p2_cfa_out, standardized = TRUE) |>
  dplyr::select(lhs, op, rhs, est, se, pvalue, ci.upper, ci.lower) |>
  dplyr::filter(op == "=~")