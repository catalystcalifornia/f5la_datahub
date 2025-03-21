# Households with children under 5 in poverty
# as percentage of all eligible LA County households with children under 5, 2023

# Data Dictionary: www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2023.pdf

library(tidyverse)
library(data.table)
library(readxl)
library(tidycensus)
library(srvyr)
library(stringr)

#SOURCE from the script that has: styling, packages, dbconnection, colors
source("W:\\RDA Team\\R\\credentials_source.R")


#### Step 1: load the data ####

# PUMS Data
root <- "W:/Data/Demographics/PUMS/"

# Load the people PUMS data
people <- fread(paste0(root, "CA_2023/psam_p06.csv"), header = TRUE, data.table = FALSE,
                colClasses = list(character = c("PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", "RACNH")))


# Load the housing PUMS data
housing <- fread(paste0(root, "CA_2023/psam_h06.csv"), header = TRUE, data.table = FALSE,
                 colClasses = list(character = c("PUMA")))


####  Step 2: filter households eligible for calculation  #### 

## Select LA County people
eligible_hhs <- people %>% 
  
  #filtering for universe and LA county
  filter(!is.na(POVPIP) & grepl('037', PUMA))   %>% 
  
  # join their housing info  
  left_join(housing %>% filter(grepl('037', PUMA)), 
            by = c("SERIALNO", "PUMA")) %>%
  
  #remove records with no weights
  filter(!is.na(WGTP)) %>%
  
  #filter for age 0-5 and select distinct households
  filter(AGEP < 5) %>% distinct(SERIALNO, .keep_all = TRUE)


####  Step 3: set up and run survey and format  #### 

# add geoid and indicator
eligible_hhs$geoid <- "037"
eligible_hhs$indicator=(ifelse(eligible_hhs$POVPIP <= 100, "at or below poverty", "above poverty"))

#SOURCE survey return function
source("W:\\Project\\RDA Team\\First5LA\\Ad-hoc Research\\Laura ask poverty update\\ask_functions.R")
total <- survey_return(eligible_hhs)


# select burdened and not NA
d_long <- total %>% filter(indicator == "at or below poverty" & !is.na(geoid))

# make data frame
d_long <- as.data.frame(d_long)


#### Step 4: Repeat steps 2 and 3 above without the 0-5 filter ####

## Select LA County people
eligible_hhs2 <- people %>% 
  
  #filtering for universe and LA county
  filter(!is.na(POVPIP) & grepl('037', PUMA))   %>% 
  
  # join their housing info  
  left_join(housing %>% filter(grepl('037', PUMA)), 
            by = c("SERIALNO", "PUMA")) %>%
  
  #remove records with no weights
  filter(!is.na(WGTP)) %>%
  
  #filter for age 0-5 and select distinct households
  distinct(SERIALNO, .keep_all = TRUE)

# add geoid and indicator
eligible_hhs2$geoid <- "037"
eligible_hhs2$indicator=(ifelse(eligible_hhs2$POVPIP <= 100, "at or below poverty", "above poverty"))


#run survey return function
source("W:\\Project\\RDA Team\\First5LA\\Ad-hoc Research\\Laura ask poverty update\\ask_functions.R")
total <- survey_return(eligible_hhs2)

# select burdened and not NA
d_long2 <- total %>% filter(indicator == "at or below poverty" & !is.na(geoid))

# make data frame
d_long2 <- as.data.frame(d_long2)

# bind both data frames in final
d_final <- rbind(d_long, d_long2)

# #write to one bigger csv with all data years
write.csv(d_final, file = "W:/Project/RDA Team/First5LA/Ad-hoc Research/Laura ask poverty update/CSVs/poverty_county_2023.csv")