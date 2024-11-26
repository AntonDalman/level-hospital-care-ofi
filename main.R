## Welcome!

## This is your project's main script file and together with
## manuscript.Rmd it provides and entry point for you and other people
## coming to the project. The code in this file should give an outline
## of the different steps conducted in your study, from importing data
## to producing results.

## This file should be relatively short, and most of the heavy
## lifting should be done by specialised functions. These functions
## live in the folder functions/ and you create a new function using
## create_function().

## Feel free to remove this introductory text as you get started.

## Source all functions (if you tick the box "Source on save" in
## RStudio functions will be automatically sourced when you save
## them). They all need to be sourced however when you compile your
## manuscript file or run this file as a job, as that happens in a
## clean R session.
#noacsr::source_all_functions()

# Load packages
library(rofi)
library(dplyr)
library(gtsummary)
library(labelled)
library(knitr)
library(nnet)
library(gt)

# Import data
data <- import_data()

# Merge data
merged.data <- merge_data(data)

# Add the OFI outcome
merged.data$ofi <- create_ofi(merged.data)

# create OFI variable
merged.data <- add_ofi_categories(merged.data)

# Select variables, this is just an example. The function select comes from the
# dplyr package
study.data <- merged.data |>
  select(
    pt_age_yrs,
    pt_Gender,
    inj_mechanism,
    pt_asa_preinjury,
    ed_gcs_sum,
    ed_sbp_value,
    ed_rr_value,
    ISS,
    host_care_level,
    res_survival,
    ofi,
    ofi.categories.broad,
    ofi.categories.detailed
  )


# Exclude patients who were not reviewed for the presence of OFI
study.sample <- study.data |>
  filter(!is.na(ofi))

# Label levels
study.sample$pt_Gender <- factor(study.sample$pt_Gender, levels = c(1, 2), labels = c("Male", "Female"))

# Label variables
var_label(study.sample$pt_age_yrs) <- "Age in years"
var_label(study.sample$ofi) <- "Opportunities for improvement"
var_label(study.sample$inj_mechanism) <- "Injury mechanism"
var_label(study.sample$pt_Gender) <- "Gender"
var_label(study.sample$pt_asa_preinjury) <- "ASA-score"
var_label(study.sample$ed_gcs_sum) <- "GCS at arrival"
var_label(study.sample$ed_sbp_value) <- "Blood pressure at arrival"
var_label(study.sample$ed_rr_value) <- "Respiratory rate at arrival"
<<<<<<< HEAD
=======
# var_label(study.sample$ed_be_art) <- "Base excess at arrival"
>>>>>>> 9d0e3317bdd2402da6eedd0b60961c8ebc09059b
var_label(study.sample$ISS) <- "ISS"
var_label(study.sample$host_care_level) <- "Highest level of hospital care"
var_label(study.sample$res_survival) <- "30 day survival rate"

# Create a table of sample characteristics
sample.characteristics.table <- tbl_summary(study.sample,
  by = ofi
) |> add_overall()

print(sample.characteristics.table)



# Descriptive data. These objects are used in the manuscript.Rmd file.
ofi <- paste0(sum(study.sample$ofi == "Yes"), " (", round(sum(study.sample$ofi == "Yes") / nrow(study.sample) * 100, 2), "%)")
age <- inline_text(sample.characteristics.table, "pt_age_yrs", column = stat_0)
men <- inline_text(sample.characteristics.table, "pt_Gender", column = stat_0, level = "Male")


# make ofi into numeric from character, presence of OFI=1
study.sample <- study.sample %>%
  mutate(ofi_numeric = ifelse(ofi == "Yes", 1, 0))

# make gender into numeric. Male =1, Female= 2
study.sample <- study.sample %>%
  mutate(pt_Gender_numeric = ifelse(pt_Gender == "Male", 1, 2))


# making the OFI.broad into numeric
word_to_numeric1 <- c(
  "Clinical judgement error" = 1, "Inadequate resources" = 2,
  "Inadequate protocols" = 3, "Other errors" = 4, "Missed diagnosis" = 5
)
study.sample$ofi.broad.numeric <- word_to_numeric1[study.sample$ofi.categories.broad]


#create a table with how many OFI per level of care 
carelevel_ofi <- study.sample[which(study.sample$ofi_numeric == 1), ]
carelevel_ofi_table <- table(carelevel_ofi$host_care_level)
carelevel_ofi_df <- as.data.frame(carelevel_ofi_table)

carelevel_ofi_df %>%
  gt() %>%
  tab_header(
    title = "How many OFI at each level of care"
  ) %>%
  cols_label(
    Var1 = "Highest level of care",  
    Freq = "Number of OFI"           
  )


# multivariable logistic regression using type of OFI(broad), host_care_level
# outcome is 30 day survival.

# 30 day survival. Changed 1(death)=0, 2(Alive)=1, 999(unknown)=NA, and excluded all NA

study.sample <- study.sample %>%
  mutate(res_survivalBinary = case_when(
    res_survival == 1 ~ 0,
    res_survival == 2 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(res_survivalBinary))

# Convert from numeric into a factor, remove NA and 999
study.sample$res_survivalBinary <- factor(study.sample$res_survivalBinary)
study.sample$ofi.broad.numeric <- factor(study.sample$ofi.broad.numeric)
study.sample$host_care_level <- factor(study.sample$host_care_level)
study.sample$inj_mechanism <- factor(study.sample$inj_mechanism)
study.sample$pt_Gender_numeric <- factor(study.sample$pt_Gender_numeric)

study.sample$inj_mechanism <- factor(study.sample$inj_mechanism,
  levels = setdiff(levels(study.sample$inj_mechanism), c("999", NA))
)


study.sample$pt_asa_preinjury <- factor(study.sample$pt_asa_preinjury)

study.sample$pt_asa_preinjury <- factor(study.sample$pt_asa_preinjury,
  levels = setdiff(levels(study.sample$pt_asa_preinjury), c("999", NA))
)
# Filtered out 99 and 999 from ed_gcs, but kept it as a numeric variable.
study.sample <- study.sample %>%
  filter(!ed_gcs_sum %in% c(99, 999))

study.sample$res_survival <- factor(study.sample$res_survival)

study.sample$ofi_numeric <- factor(study.sample$ofi_numeric)


# Unadjusted bivariable logistic regression using OFI, Host_care_level, OFI as outcome
UnadjustedBivariableLR <- glm(ofi_numeric ~ host_care_level, data = study.sample, family = binomial)

UnadjustedBivariableLRPrint <- tbl_regression(UnadjustedBivariableLR,
                                              exponentiate = TRUE) %>%
  as_gt() %>%
  # Add a header to the table
  tab_header(
    title = "Unadjusted Bivariable Logistic Regression, with OFI as outcome and Highest level of care as predictor"
  ) %>%
  set_variable_labels(
    host_care_level = "Highest level of hospital care"
  ) %>% 
  # Modify the row names using modify_table_body
  modify_table_body(
    rows = 1, 
    columns = 1, 
    value = "Emergency department"
  ) %>%
  modify_table_body(
    rows = 2, 
    columns = 1, 
    value = "General ward"
  ) %>%
  modify_table_body(
    rows = 3, 
    columns = 1, 
    value = "Operative theatre"
  ) %>%
  modify_table_body(
    rows = 4, 
    columns = 1, 
    value = "High dependency unit"
  ) %>%
  modify_table_body(
    rows = 5, 
    columns = 1, 
    value = "Intensive care unit"
  )
  

print(UnadjustedBivariableLRPrint)



# Adjusted logistic regression, outcome OFI
AdjustedBivariableLR <- glm(
  ofi_numeric ~ host_care_level + pt_age_yrs + pt_Gender_numeric +
    pt_asa_preinjury + ed_gcs_sum + ed_sbp_value + ed_rr_value
    + inj_mechanism + ed_gcs_sum + ISS + res_survival,
  data = study.sample, family = binomial
)


AdjustedBivariableLRprint <- tbl_regression(AdjustedBivariableLR,
                                            exponentiate = TRUE)
print(AdjustedBivariableLRprint)



# categorizing OFI detailed and change into factor

word_to_numeric2 <- c(
  "Patient management/logistics" = 1, "Resources" = 2,
  "Patient management" = 3, "Logistics/technical" = 4,
  "Triage in the ED" = 5, "Communication" = 6,
  "Level of care" = 7, "Trauma criteria/guidelines" = 8,
  "Missed injury" = 10, "Inadequate routine" = 11
)
study.sample$ofi.detailed.numeric <- word_to_numeric2[study.sample$ofi.categories.detailed]
study.sample$ofi.detailed.numeric <- factor(study.sample$ofi.detailed.numeric)

<<<<<<< HEAD
#creating a multinominal regression broad
multinom_ofi_broad_unadjusted <- multinom(ofi.broad.numeric ~ host_care_level, data = study.sample)

#creating a multinominal regression detailed
multinom_ofi_detailed_unadjusted <- multinom(ofi.detailed.numeric ~ host_care_level, data = study.sample)



#creating a nice looking table similiar to gtsummary
multinom_ofi_detailed_unadjustedy <- summary(multinom_ofi_detailed_unadjusted)






=======
# creating a multinominal regression
multinom_ofi_broad_unadjusted <- multinom(ofi.detailed.numeric ~ host_care_level, data = study.sample)


# creating a nice looking table similiar to gtsummary
multinom_ofi_broad_unadjusted_summary <- summary(multinom_ofi_broad_unadjusted)

# Extract coefficients, standard errors, and p-values
multi_ofi_broad_coef <- multinom_ofi_broad_unadjusted_summary$coefficients
multi_ofi_se <- multinom_ofi_broad_unadjusted_summary$standard.errors
z_values <- multi_ofi_broad_coef[, 1] / multi_ofi_se[, 1]
p_values <- 2 * pnorm(-abs(z_values))

# Convert to a data frame
OFI_broad_Unadjusted_gtsummary <- data.frame(
  term = rownames(multi_ofi_broad_coef),
  coefficient = multi_ofi_broad_coef[, 1],
  std_error = multi_ofi_se[, 1],
  p_value = 2 * (1 - pnorm(abs(multi_ofi_broad_coef[, 1] / multi_ofi_se[, 1]))), # Two-tailed p-value
  stringsAsFactors = FALSE
)

# Summary table using gtsummary
OFI_broad_Unadjusted_gtsummary %>%
  tbl_summary(
    by = "term", # Group by term (factor levels)
    statistic = list(all_continuous() ~ "{mean} ({sd})"), # Format for continuous values
    label = list(coefficient ~ "Coefficient", std_error ~ "Standard Error", p_value ~ "p-value")
  ) %>%
  modify_caption("**Unadjusted Multinomial Logistic Regression Results**")

print(OFI_broad_Unadjusted_gtsummary)



# useful code for copy paste, will not be included in final product like this
unique_values1 <- unique(study.sample$ofi.categories.broad)
num_unique_values1 <- length(unique_values1)
print(num_unique_values1)
>>>>>>> 9d0e3317bdd2402da6eedd0b60961c8ebc09059b
