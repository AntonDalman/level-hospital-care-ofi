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

# Import data
data <- import_data(test = TRUE)

# Merge data
merged.data <- merge_data(data, test = TRUE)

# Add the OFI outcome
merged.data$ofi <- create_ofi(merged.data)

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
         ed_be_art,
         ISS,
         host_care_level,
         res_survival,
         ofi)

# Exclude patients who were not reviewed for the presence of OFI
study.sample <- study.data |>
  filter(!is.na(ofi))

# Label variables
var_label(study.sample$pt_age_yrs) <- "Age in years"
var_label(study.sample$ofi) <- "Opportunities for improvement"

# Create a table of sample characteristics
sample.characteristics.table <- tbl_summary(study.sample,
                                            by = ofi)
  
# These are my dataframes, vectors and so on

# First I need to include the vectors I need to use in my dataframe. 
# I will add more vectors when I have which ones I should use. 
# the vectors added are: host_care_level - I have interpreted this to mean
# hospital care level
# I have also added personnummer. Unclear if this is the best way of following 
# a patient in different vectors (fmp_scramblec, atgarder_scrambel, and so on)

# make numeric from charcter
newNiss <- as.numeric(NISS)
newISS <- as.numeric(ISS)
newhost_care_level <- as.numeric(host_care_level)


#Exclude NA 
newNiss2 <- na.exclude(newNiss)
newISS2 <- na.exclude(newISS)
newhost_care_level2 <- na.exclude(newhost_care_level)



# test the possibility of my dataframe with only 3 vectors.
# here I could add OFI and hosp care level.
DataFrame1 <- data.frame(newhost_care_level2, newNiss2, newISS2)



#Try to find some correlation
correlations <- sapply(DataFrame1, function(x) cor(DataFrame1$host_care_level2, x))
correlations
# I failed with this code, NA creates problems for me. I will continue working
# and try to fix it



