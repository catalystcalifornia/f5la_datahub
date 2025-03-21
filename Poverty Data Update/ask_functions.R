# Functions for F5LA Laura ask
# 1) race recode - recode racial-ethnic groups including SWANA
# 2) survey return - percent + moe, cv, etc. calculations for total populations
# 3) survey return race - percent + moe, cv, etc. calculations for recoded racial-ethnic groups


#### Function to recode racial-ethnic groups ####

library(tidyverse)
library(data.table)
library(readxl)

race_recode <- function(people) {

people_recoded<- people %>% 
  mutate(latino=(ifelse(HISP %in% "01", "Not Latinx", "Latinx")))

people_recoded<-people_recoded %>%
  mutate(aian=(ifelse(RACAIAN %in% "0", "Not AIAN", "AIAN Alone or in Combination")))

people_recoded<-people_recoded%>%
  mutate(nhpi=(ifelse(RACPI %in% "1", "NHPI Alone or in Combination",
                      ifelse(RACNH %in% '1', 'NHPI Alone or in Combination', "Not NHPI"))))

# Set up codes for SWANA
# First use ancestry codes to help identify estimated swana pop
## Create list of swana codes for PUMS
pums_swana_list<-list("Algerian","Arab","Assyrian","Bahraini","Berber","Chaldean","Egyptian","Emirati","Iranian","Iraqi","Israeli","Jordanian","Kurdish","Kuwaiti","Lebanese","Libyan","Middle Eastern","Moroccan","North African","Omani","Palestinian","Qatari","Saudi","Syriac","Syrian","Tunisian","Yazidi","Yemeni","Mideast","Saudi Arabian","Arabic","Other Arab","Libyan (2017 or later)","Kuwaiti (2017 or later)","Turkish","Sudanese","Afghan") # 2017 or later needed based on reviewing data dictionary and saw fields for Arabic and Other Arab

## import PUMS codes
pums_codes <- read_excel("W:/Data/Demographics/PUMS/CA_2018_2022/PUMS_Data_Dictionary_2018-2022_ANC1P.xlsx")%>%
  mutate_all(as.character) # create this excel document separate by opening PUMS Data Dictionary in excel and deleting everything but ancestry fields -- since ANC1P and ANC2P have same data values no need to do both
pums_codes <- pums_codes %>% dplyr::rename("ANC_Code" = "Code_1")

## filter PUMS codes for swana descriptions based on our swana_list
swana_codes<-pums_codes%>%filter(Description %in% pums_swana_list)

##swana - alone or in combination with another race or latino
people_recoded$swana <- "Not SWANA"
people_recoded$swana[people_recoded$ANC1P%in% swana_codes$ANC_Code| people_recoded$ANC2P%in% swana_codes$ANC_Code] <- "SWANA"
people_recoded$swana <- as.factor(people_recoded$swana)

# code other race groups
people_recoded$race = as.factor(ifelse(people_recoded$RAC1P == "1" & people_recoded$latino =="Not Latinx", "White NL",
                                       ifelse(people_recoded$RAC1P == "1" & people_recoded$latino =="Latinx", "Latinx placeholder",
                                              ifelse(people_recoded$RAC1P == "2" & people_recoded$latino =="Not Latinx", "Black NL",
                                                     ifelse(people_recoded$RAC1P== "3" | people_recoded$RAC1P== "4"|people_recoded$RAC1P== "5", "AIAN placeholder",
                                                            ifelse(people_recoded$RAC1P == "6" & people_recoded$latino =="Not Latinx", "Asian NL",
                                                                   ifelse(people_recoded$RAC1P == "7", "NHPI placeholder",
                                                                          ifelse(people_recoded$RAC1P == "8" & people_recoded$latino =="Not Latinx", "Other NL", 
                                                                                 ifelse(people_recoded$RAC1P== "9" 
                                                                                        & people_recoded$latino =="Not Latinx", "Two or More NL", "Latinx placeholder"))))))))) 

return(people_recoded)
}




#### Function to set up and run survey ####

survey_return <- function(hh) {

  # survey design code
  
  # Define weight variable and population base which will be used in the survey design set up
  ## You must use WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for Connected under5)
  weight <- 'WGTP' # using WGTP b/c calculating percentage of rent-burdened households
  
  repwlist = rep(paste0("WGTP", 1:80))
  
  # create survey design
  
  hh_geo <- hh %>%               
    as_survey_rep(
      variables = c(geoid, indicator),   # dplyr::select grouping variables
      weights = weight,                       #  weight
      repweights = repwlist,                  # list of replicate weights
      combined_weights = TRUE,                # tells the function that replicate weights are included in the data
      mse = TRUE,                             # tells the function to calc mse
      type="other",                           # statistical method
      scale=4/80,                             # scaling set by ACS
      rscale=rep(1,80)                        # setting specific to ACS-scaling
    )
  
  ###### TOTAL ######
  total <- hh_geo  %>%
    group_by(geoid,indicator) %>%   # group by race cat
    summarise(
      num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(hh_geo %>%                                        # left join in the denominators
                group_by(geoid) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = num_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count
  
  return(total)
}




#### Function to set up and run survey for racial-ethnic groups  ####

survey_return_race <- function(hh) {
  
# survey design code

# Define weight variable and population base which will be used in the survey design set up
## You must use WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for Connected under5)
weight <- 'WGTP' # using PWGTP b/c calculating percentage of under 5 in rent-burdened households (from psam_p06.csv)

repwlist = rep(paste0("WGTP", 1:80))

# create survey design

hh_geo <- hh %>%               
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
lat <- hh_geo  %>%
  group_by(geoid,latino,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_geo %>%                                        # left join in the denominators
              group_by(geoid,latino) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count


###### NHPI ######
nhpi <- hh_geo  %>%
  group_by(geoid,nhpi,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_geo %>%                                        # left join in the denominators
              group_by(geoid,nhpi) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### AIAN ######
aian <- hh_geo  %>%
  group_by(geoid,aian,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_geo %>%                                        # left join in the denominators
              group_by(geoid,aian) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### SWANA ######
swana <- hh_geo  %>%
  group_by(geoid,swana,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_geo %>%                                        # left join in the denominators
              group_by(geoid,swana) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### RACE ######
race <- hh_geo  %>%
  group_by(geoid,race,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_geo %>%                                        # left join in the denominators
              group_by(geoid,race) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### TOTAL ######
total <- hh_geo  %>%
  group_by(geoid,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_geo %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

####  Format  ####

####  Step 6: format  ####

# rename race name columns as subgroup
total$subgroup = "Total"
total <- total %>% select(geoid, subgroup, everything())

aian <- aian %>% rename(subgroup = aian)
lat <- lat %>% rename(subgroup = latino)
nhpi <- nhpi %>% rename(subgroup = nhpi)
race <- race %>% rename(subgroup = race)
swana <- swana %>% rename(subgroup = swana)

# merge tables, need total
d_long <- rbind(total, aian, lat, race, nhpi, swana) %>%
  filter(  subgroup != "Not AIAN" &
           subgroup != "Not Latinx" &
           subgroup != "Not NHPI" &
           subgroup != "Not SWANA" &
           subgroup != "AIAN placeholder" &
           subgroup != "NHPI placeholder" &
           subgroup != "Latinx placeholder")

d_long <- as.data.frame(d_long)

return(d_long)
}
