# Households with children under 5 in poverty by race
# among all eligible LA County households with children under 5, 2023
# Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2019-2023.pdf

library(tidyverse)
library(data.table)
library(readxl)
library(tidycensus)
library(srvyr)
library(stringr)

#SOURCE from the script that has: styling, packages, dbconnection, colors
source("W:\\RDA Team\\R\\credentials_source.R")


#### Step 1 load the data ####

# PUMS Data
root <- "W:/Data/Demographics/PUMS/"

# Load the people PUMS data
people <- fread(paste0(root, "CA_2019_2023/psam_p06.csv"), header = TRUE, data.table = FALSE,
                colClasses = list(character = c("PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", "RACNH")))


# Load the housing PUMS data
housing <- fread(paste0(root, "CA_2019_2023/psam_h06.csv"), header = TRUE, data.table = FALSE,
                 colClasses = list(character = c("PUMA")))


#### Step 2 filter for LA & Recode racial-ethnic groups ####

people <- people %>% filter(grepl('037', PUMA)) 

#SOURCE recode function
source("W:\\Project\\RDA Team\\First5LA\\Ad-hoc Research\\Laura ask poverty update\\ask_functions.R")
people_recoded <- race_recode(people)


####  Step 3: filter LA County households eligible for poverty calculation  #### 

## Select LA County households with income-to-poverty ratios (universe) by

eligible_hhs <- people_recoded %>%
  
  #filtering for poverty eligible and LA county  
  filter(!is.na(POVPIP) & (grepl('037', PUMA))) %>%
  
  #join households         
  left_join(housing, by = c("SERIALNO", "PUMA")) %>%
  
  #remove records with no weights
  filter(!is.na(WGTP)) %>%
  
  #filter for age 0-5 and select distinct households
  filter(AGEP < 5) %>% distinct(SERIALNO, .keep_all = TRUE)


#### Step 4: Set up surveys and calculate percentages by race/ethnicity

# survey design code

# Define weight variable and population base which will be used in the survey design set up
## You must use WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for Connected under5)
weight <- 'WGTP' # using WGTP b/c calculating percentage of rent-burdened households

repwlist = rep(paste0("WGTP", 1:80))

# prep data and identify/calculate indicator
hh <- eligible_hhs
hh$geoid <- "037"

hh<-hh%>%
  mutate(indicator=(ifelse(hh$POVPIP <= 100, "at or below poverty", "above poverty")))

# create survey design

hh_county <- hh %>%               
  as_survey_rep(
    variables = c(geoid, indicator, race, latino, aian, nhpi, swana),   # dplyr::select grouping variables
    weights = weight,                       #  weight
    repweights = repwlist,                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )


###### Latino ######
lat <- hh_county  %>%
  group_by(geoid,latino,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_county %>%                                        # left join in the denominators
              group_by(geoid,latino) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count


###### NHPI ######
nhpi <- hh_county  %>%
  group_by(geoid,nhpi,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_county %>%                                        # left join in the denominators
              group_by(geoid,nhpi) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count


###### AIAN ######
aian <- hh_county  %>%
  group_by(geoid,aian,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_county %>%                                        # left join in the denominators
              group_by(geoid,aian) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count


###### SWANA ######
swana <- hh_county  %>%
  group_by(geoid,swana,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_county %>%                                        # left join in the denominators
              group_by(geoid,swana) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count


###### RACE ######
race <- hh_county  %>%
  group_by(geoid,race,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_county %>%                                        # left join in the denominators
              group_by(geoid,race) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count


###### TOTAL ######
total <- hh_county  %>%
  group_by(geoid,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_county %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count


####  Step 5: format  ####

# rename race name columns as subgroup
total$subgroup = "Total"
total <- total %>% select(geoid, subgroup, everything())

aian <- aian %>% rename(subgroup = aian)
lat <- lat %>% rename(subgroup = latino)
nhpi <- nhpi %>% rename(subgroup = nhpi)
race <- race %>% rename(subgroup = race)
swana <- swana %>% rename(subgroup = swana)

# merge tables except for bipoc - need total
d_long <- rbind(total, aian, lat, race, nhpi, swana) %>%
  filter(indicator == "at or below poverty" & 
           subgroup != "Not AIAN" &
           subgroup != "Not Latinx" &
           subgroup != "Not NHPI" &
           subgroup != "Not SWANA" &
           subgroup != "AIAN placeholder" &
           subgroup != "NHPI placeholder" &
           subgroup != "Latinx placeholder")

d_long <- as.data.frame(d_long)
d_long$geoid <- "06037"
d_long <- d_long %>% select(geoid, everything())

# #write to one bigger csv with all data years
write.csv(d_long, file = "W:/Project/RDA Team/First5LA/Ad-hoc Research/Laura ask poverty update/CSVs/poverty_race_2019_23.csv")



