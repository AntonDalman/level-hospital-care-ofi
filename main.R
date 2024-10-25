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
noacsr::source_all_functions()

# Load packages
library(rofi)
library(dplyr)
library(gtsummary)
library(labelled)
library(knitr)


# Import data
data <- import_data(test = TRUE)

# Merge data
merged.data <- merge_data(data, test = TRUE)

# Add the OFI outcome
merged.data$ofi <- create_ofi(merged.data)

#create OFI variable
merged.data <- add_ofi_categories(merged.data) 

# Select variables, this is just an example. The function select comes from the 
# dplyr package
study.data <- merged.data |> 
  select(pt_age_yrs, 
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
         ofi.categories.detailed)


# Exclude patients who were not reviewed for the presence of OFI
study.sample <- study.data |>
  filter(!is.na(ofi))

# Label variables
var_label(study.sample$pt_age_yrs) <- "Age in years"
var_label(study.sample$ofi) <- "Opportunities for improvement"
var_label(study.sample$inj_mechanism) <- "Injury mechanism"
var_label(study.sample$pt_Gender) <- "Gender"
var_label(study.sample$pt_asa_preinjury) <- "ASA-score"
var_label(study.sample$ed_gcs_sum) <- "GCS at arrival"
var_label(study.sample$ed_sbp_value) <- "Blood pressure at arrival"
var_label(study.sample$ed_rr_value) <-  "Respiratory rate at arrival"
var_label(study.sample$ed_be_art) <- "Base excess at arrival"
var_label(study.sample$ISS) <- "ISS"
var_label(study.sample$host_care_level) <- "Highest level of hospital care"
var_label(study.sample$res_survival) <- "30 day survival rate"

          
        
            






# Create a table of sample characteristics
sample.characteristics.table <- tbl_summary(study.sample,
                                            by = ofi)



viewtable <- kable(sample.characteristics.table, caption = "Study sample by OFI")

print(viewtable)


#make ofi into numeric from character
study.sample <- study.sample %>%
  mutate(ofi_numeric = ifelse(ofi == "Yes", 1, 0))

#see if there is any correlation between OFI and 30 day survival
correlation1 <- cor(study.sample$ofi_numeric, study.sample$res_survival, use = "complete.obs")


# Scatter plot test
plot(study.sample$ed_sbp_value, study.sample$ISS, main = "Scatter Plot test", 
          xlab = "var1", 
          ylab = "var2", 
          pch = 1,
          col = "blue",
     abline(lm(ISS ~ ed_sbp_value, data = study.sample), col = "red"))

#Bivariable logistic regression using OFI, Host_care_level, OFI as outcome
BivariableLR <- glm(ofi_numeric ~ host_care_level, data = study.sample, family = binomial)

#making the OFI.broad into numeric
word_to_numeric1 <- c("Clinical judgement error" = 1, "Inadequate resources" = 2,
                      "Inadequate protocols" = 3, "Other errors" = 4 
                      ) 
study.sample$ofi.broad.numeric <- word_to_numeric1[study.sample$ofi.categories.broad]


#multivariable logistic regression using type of OFI(broad), host_care_level
# outcome is 30 day survival. Next on the list to do. 


#useful code for copy paste, will not be included
unique_values1 <- unique(study.sample$ofi.categories.broad)
num_unique_values1 <- length(unique_values1)
print(num_unique_values1)
