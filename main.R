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
#relevel "general ward" as baseline
study.sample$host_care_level <- relevel(as.factor(study.sample$host_care_level), ref = "General Ward")
UnadjustedBivariableLR <- glm(ofi_numeric ~ host_care_level, data = study.sample, family = binomial)

UnadjustedBivariableLRPrint <- tbl_regression(UnadjustedBivariableLR,
                                              exponentiate = TRUE)

  

print(UnadjustedBivariableLRPrint)



# Adjusted logistic regression, outcome OFI
study.sample$host_care_level <- relevel(as.factor(study.sample$host_care_level), ref = "General Ward")
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


#Multinominal code below

study.sample[is.na(study.sample$ofi.categories.broad),"ofi.categories.broad"] <- "no ofi"

study.sample$ofi.categories.broad <- as.factor(study.sample$ofi.categories.broad)
#model.data$cohort <- as.factor(model.data$cohort)
study.sample$host_care_level <- factor(
  study.sample$host_care_level,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Emergency Care", "General Ward", "Operation Theater", 
             "Intermediate Care", "ICU")
)

#Ensure both variables are factors and set reference
#model.data$cohort <- relevel(as.factor(model.data$cohort), ref = "other cohort")
study.sample$ofi.categories.broad <- relevel(as.factor(study.sample$ofi.categories.broad), ref = "no ofi")
study.sample$host_care_level <- relevel(as.factor(study.sample$host_care_level), ref = "General Ward")


#Fit the multinomial logistic
multinom_model1 <- multinom(ofi.categories.broad ~ host_care_level, data = study.sample)

model_summary1 <- tbl_regression(multinom_model1, exponentiate = TRUE, include = everything()) %>%
  modify_header(label = "Variable") %>%
  modify_caption("Multinomial Logistic Regression Results - Model 1") %>%
  bold_labels() %>%
  bold_levels() %>%
  bold_p() %>%
  modify_header(label ~ "Opportunity for improvement") %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>% 
      dplyr::mutate(
        stat_nevent_rate = 
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ), 
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_column_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "Event Rate") %>%
  modify_table_body(
    ~ .x |> 
      dplyr::filter(!(variable %in% "host_care_level" & row_type %in% "label"))
  ) 


print(model_summary1)


#TEstcode 2 adjusted multinominal

#Fit the multinomial logistic
multinom_model2 <- multinom(ofi.categories.broad ~ host_care_level + pt_age_yrs + pt_Gender_numeric +
                              pt_asa_preinjury + ed_gcs_sum + ed_sbp_value + ed_rr_value
                            + inj_mechanism + ed_gcs_sum + ISS + res_survival, data = study.sample)

model_summary2 <- tbl_regression(multinom_model2, exponentiate = TRUE, include = everything()) %>%
  modify_header(label = "Variable") %>%
  modify_caption("Multinomial Logistic Regression Results - Model 2") %>%
  bold_labels() %>%
  bold_levels() %>%
  bold_p() %>%
  modify_header(label ~ "Opportunity for improvement") %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>% 
      dplyr::mutate(
        stat_nevent_rate = 
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ), 
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_column_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "Event Rate") %>%
  modify_table_body(
    ~ .x |> 
      dplyr::filter(!(variable %in% "host_care_level" & row_type %in% "label"))
  ) 


print(model_summary2)






#Testcode multinominal unadjusted OFI detailed

study.sample$ofi.categories.detailed <- as.factor(study.sample$ofi.categories.detailed)

#Ensure both variables are factors and set reference - no reference yet!
#study.sample$ofi.categories.detailed <- relevel(as.factor(study.sample$ofi.categories.detailed), ref = "no ofi")
study.sample$host_care_level <- relevel(as.factor(study.sample$host_care_level), ref = "General Ward")


#Fit the multinomial logistic
multinom_model3 <- multinom(ofi.categories.detailed ~ host_care_level, data = study.sample)

model_summary3 <- tbl_regression(multinom_model3, exponentiate = TRUE, include = everything()) %>%
  modify_header(label = "Variable") %>%
  modify_caption("Multinomial Logistic Regression Results - Model 3") %>%
  bold_labels() %>%
  bold_levels() %>%
  bold_p() %>%
  modify_header(label ~ "Opportunity for improvement") %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>% 
      dplyr::mutate(
        stat_nevent_rate = 
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ), 
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_column_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "Event Rate") %>%
  modify_table_body(
    ~ .x |> 
      dplyr::filter(!(variable %in% "host_care_level" & row_type %in% "label"))
  ) 


print(model_summary3)




#Testcode 4 multinominal adjusted OFI detailed

#Ensure both variables are factors and set reference - no reference yet!
#study.sample$ofi.categories.detailed <- relevel(as.factor(study.sample$ofi.categories.detailed), ref = "no ofi")
study.sample$host_care_level <- relevel(as.factor(study.sample$host_care_level), ref = "General Ward")

#Fit the multinomial logistic
multinom_model4 <- multinom(ofi.categories.detailed ~ host_care_level + pt_age_yrs + pt_Gender_numeric +
                              pt_asa_preinjury + ed_gcs_sum + ed_sbp_value + ed_rr_value
                            + inj_mechanism + ed_gcs_sum + ISS + res_survival, data = study.sample)

model_summary4 <- tbl_regression(multinom_model4, exponentiate = TRUE, include = everything()) %>%
  modify_header(label = "Variable") %>%
  modify_caption("Multinomial Logistic Regression Results - Model 4") %>%
  bold_labels() %>%
  bold_levels() %>%
  bold_p() %>%
  modify_header(label ~ "Opportunity for improvement") %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>% 
      dplyr::mutate(
        stat_nevent_rate = 
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ), 
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_column_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "Event Rate") %>%
  modify_table_body(
    ~ .x |> 
      dplyr::filter(!(variable %in% "host_care_level" & row_type %in% "label"))
  ) 


print(model_summary4)





