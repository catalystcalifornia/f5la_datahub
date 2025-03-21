# Households with children under 5 earning incomes below 138% of the federal poverty level by SPA
# as percentage of all eligible  LA County households under 5, 2023
 
# Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2019-2023.pdf

library(tidyverse)
library(data.table)
library(readxl)
library(tidycensus)
library(srvyr)
library(stringr)
library(sf)


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


#### Step 3: get PUMA SPA xwalk and join ####

#get puma-spa xwalk
conn <- connect_to_db("f5la_v2")
puma_spa_xwalk <- st_read(conn, query = "SELECT * FROM puma_spa_xwalk_2022")
dbDisconnect(conn)

# join
hh <- eligible_hhs %>% left_join(puma_spa_xwalk, by = "PUMA", relationship = "many-to-many")


#### Step 4 run survey function and format

# prep data and define indicator
hh$geoid <- hh$SPA
hh<-hh%>%
  mutate(indicator=(ifelse(hh$POVPIP <= 138, "at or below 138% of the federal poverty level", "above 138% of the federal poverty level")))

source("W:\\Project\\RDA Team\\First5LA\\Ad-hoc Research\\Laura ask poverty update\\ask_functions.R")
total <- survey_return(hh)

# select burdened and not NA
d_long <- total %>% filter(indicator == "at or below 138% of the federal poverty level" & !is.na(geoid))

# make data frame
d_long <- as.data.frame(d_long)

# #write to one bigger csv with all data years
write.csv(d_long, file = "W:/Project/RDA Team/First5LA/Ad-hoc Research/Laura ask poverty update/CSVs/poverty_spa_2023_138pct.csv")




