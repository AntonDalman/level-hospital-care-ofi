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


