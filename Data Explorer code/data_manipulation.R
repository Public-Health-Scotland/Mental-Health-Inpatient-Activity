#Name: Data explorer
#Author: Nikos Alexandrou
#Modified: 12/11/2020
#Type: Data visualisation
#Written on: RStudio
#Written for: R version 3.6.1 
#Output: Shiny application
#Approximate run time: < 1 minute
#Description: This syntax creates a Shiny application that allows the... 
#user to visualise mental health data in a variety of ways.


### SECTION 1: DATA MANIPULATION ----


##* Load the necessary libraries ----
library("shiny")#interactivity
library("dplyr")#easier data manipulation
library("plotly")#data viz 
library("DT")#for the tables under the graphs
library("readr")#read .csv files into R
library("forcats")#for working with factors more efficiently
library("RColorBrewer")#pre-made colour palettes
library("googleVis")#cross-boundary flow chart (Sankey diagram)
library("shinyWidgets")#filters
library("leaflet")#beautiful maps
library("magrittr")#pipe operators
library("stringr")#for string operations
library("rgdal")#essential for reading polygon shapefiles into R
library("rmapshaper")#reduce shapefile size

##* Define the working directory and the filepath ----
setwd("mywd")
filepath <- paste0("myfilepath")

##* Import the data and make the necessary transformations ----

#Import the file that will be used to create the trends in diagnoses tab.
diagnoses <- read_csv("MentalHealthInpatientActivity_DiagnosisTrends_20201124.csv", 
                      col_types = "ccccccn")

#Round the rates to two decimal points only. This fixes the problem in the...
#"Table" tab, where the numeric filters have too many decimal points.
#Change the text "crude rate" to just "rate".
#Then, use the select() command to change the order of columns in the dataset.
#Also, sort the tibble in a way that makes it a bit more readable when...
#viewed in the "Table" tab (optional).
diagnoses <- diagnoses %>% mutate(value = round(value, 2)) %>%
  mutate(measure = recode(measure, 
                          "Crude rate of patients (per 100,000 population)" = 
                            "Rate of patients (per 100,000 population)", 
                          "Crude rate of discharges (per 100,000 population)" = 
                            "Rate of discharges (per 100,000 population)", 
                          "Crude rate of hospital residents (per 100,000 population)" = 
                            "Rate of hospital residents (per 100,000 population)")) %>% 
  select(dataset, year, geography1, geography2, diagnosis_groupings, measure, 
         value) %>%
  arrange(dataset, year, desc(geography1), geography2, diagnosis_groupings, 
          desc(measure), value)

#Save certain variables as objects, to be used as selections in the...
#filters. 
diag_dataset <- diagnoses %>% distinct(dataset) %>% pull(dataset)
diag_location_types <- diagnoses %>% distinct(geography1) %>% pull(geography1)
diag_diagnosis_groupings <- diagnoses %>% distinct(diagnosis_groupings) %>% 
  pull(diagnosis_groupings)
diag_measures <- diagnoses %>% distinct(measure) %>% pull(measure)

#Finally, transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#These filters work best when the column type is factor.
diagnoses <- diagnoses %>% mutate_if(is.character, as.factor)

#Import the two files that will be used to create the geography tab.
#'geography' contains the rates of patients/discharges that will be...
#displayed in the tooltip of our interactive map.
#'CA' (see below) is the shapefile containing the council area polygons, ...
#which will be projected onto a leaflet map.
geography <- read_csv("MentalHealthInpatientActivity_GeographyData_20201124.csv", col_types = "ccccn")

#Round the rates to two decimal points only. This fixes the problem in the...
#"Table" tab, where the numeric filters have too many decimal points.
#Change the text "crude rate" to just "rate".
#Then, use the select() command to change the order of columns in the dataset.
#Also, sort the tibble in a way that makes it a bit more readable when...
#viewed in the "Table" tab (optional).
geography <- geography %>% mutate(Value = round(Value, 2)) %>% 
  mutate(measure = recode(measure, 
                          "Crude rate of patients (per 100,000 population)" = 
                            "Rate of patients (per 100,000 population)", 
                          "Crude rate of discharges (per 100,000 population)" = 
                            "Rate of discharges (per 100,000 population)")) %>% 
  select(dataset, fyear, ca, measure, Value) %>%
  arrange(dataset, fyear, ca, desc(measure), Value) 

#Save certain variables as objects, to be used as selections in the...
#filters. 
geography_dataset <- geography %>% distinct(dataset) %>% pull(dataset) 
geography_fin_years <- geography %>% arrange(desc(fyear)) %>% distinct(fyear) %>% 
  pull(fyear)
geography_measures <- geography %>% distinct(measure) %>% pull(measure)

#Finally, transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#These filters work best when the column type is factor.
geography <- geography %>% mutate_if(is.character, as.factor)

#Before moving on to age/sex, read in the shapefile which contains the...
#CA polygons.
#PROCESS:
#Download the shapefile from the following URL. Download the one called "Scotland...
#(all zones as a single file for Scotland)". You only need its .shp version:
#https://saspac.org/data/2011-census/scotland-2011/
#Unzip the downloaded folder and copy-paste all the files beginning with...
#"CA_2011_EoR_Scotland" into a new folder in your working directory.
#Name this folder "Map".
#There should be seven files beginning with "CA_2011_EoR_Scotland".
#We will use ms_simplify() to decrease the size of the shapefile -this will help R to...
#render the map much faster.
CA <- readOGR(dsn = paste0(filepath, "Map"),
              layer = "CA_2011_EoR_Scotland") %>% 
  rmapshaper::ms_simplify(keep = 0.0025)

#Also, change to the correct projection.
CA <- spTransform(CA, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Save the simplified shapefile.
writeOGR(obj = CA, 
         dsn = paste0(filepath, "Map"),
         layer = "CA_simpl", 
         driver = "ESRI Shapefile", 
         overwrite_layer = TRUE)

#Save as .rds, as it is much faster to read.
ca_bound <- readOGR(dsn = paste0(filepath, "Map"), 
                    layer = "CA_simpl")
saveRDS(ca_bound, paste0(filepath, "Map/CA_boundary.rds"))

#Read the .rds file back into R.
CA_smaller <- readRDS("Map/CA_boundary.rds")

#Import the file that will be used to create the age/sex tab.
age_sex <- read_csv("MentalHealthInpatientActivity_AgeSexData_20201124.csv", col_types = "cccccccn")

#Change the text "crude rate" to just "rate".
age_sex <- age_sex %>% 
  mutate(measure = recode(measure, 
                          "Crude rate of patients (per 100,000 population)" = 
                            "Rate of patients (per 100,000 population)", 
                          "Crude rate of discharges (per 100,000 population)" = 
                            "Rate of discharges (per 100,000 population)"))  

#Save certain variables as objects, to be used as selections in the...
#filters.  
as_dataset <- age_sex %>% arrange(dataset) %>% distinct(dataset) %>% 
  pull(dataset)
as_location_types <- age_sex %>% arrange(desc(geography1)) %>% 
  distinct(geography1) %>% pull(geography1)
as_financial_years <- age_sex %>% arrange(desc(year)) %>% distinct(year) %>% 
  pull(year)
as_measures <- age_sex %>% arrange(desc(measure)) %>% distinct(measure) %>% 
  pull(measure)

#Transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#These filters work best when the column type is factor.
#Then, round the rates to two decimal points only. This fixes the problem in...
#the "Table" tab, where the numeric filters have too many decimal points.
#Next step: reverse the order of levels in the factor "sex_char".
#We want males to be first and females second, so that the legend in the age/...
#sex pyramid is consistent with what the pyramid shows (males are visualised...
#first, and females second).
#Finally, use the select() command to change the order of columns in the...
#dataset, and then sort the tibble in a way that makes it a bit more...
#readable when viewed in the "Table" tab (optional).
age_sex <- age_sex %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(value = round(value, 2)) %>%
  mutate(sex_char = fct_rev(sex_char)) %>% 
  select(dataset, year, geography1, geography2, ageband, sex_char, measure, 
         value) %>% 
  arrange(dataset, year, desc(geography1), geography2, ageband, desc(measure), 
          sex_char, value)

#Import the files that will be used to create the deprivation tab.
#The deprivation tab will contain two graphs.
#The first graph will show activity broken down by deprivation quintile,...
#whereas the second graph will display the Relative Index of Inequality (RII)...
#as a trend over time.
#We start with the file containing the quintiles, called 'deprivation'.
deprivation <- read_csv("MentalHealthInpatientActivity_SIMDData_20201124.csv", col_types = "ccccccn")

#Change the text "crude rate" to just "rate".
#Transform the simd variable too:
#Recode value "1" to "1 - Most deprived" and "5" to "5 - Least deprived".
#This will make the first graph as well as the table under it...
#easier to understand.
#Then, round the rates to two decimal points only. This fixes the problem in...
#the "Table" tab, where the numeric filters have too many decimal points.
#Finally, use the select() command to change the order of columns in the...
#dataset, and then sort the tibble in a way that makes it a bit more...
#readable when viewed in the "Table" tab (optional).
deprivation <- deprivation %>%
  mutate(measure = recode(measure, 
                          "Crude rate of patients (per 100,000 population)" = 
                            "Rate of patients (per 100,000 population)", 
                          "Crude rate of discharges (per 100,000 population)" = 
                            "Rate of discharges (per 100,000 population)", 
                          "Crude rate of hospital residents (per 100,000 population)" = 
                            "Rate of hospital residents (per 100,000 population)")) %>%
  mutate(simd = recode(simd, "1" = "1 - Most deprived", 
                       "5" = "5 - Least deprived")) %>% 
  mutate(value = round(value, 2)) %>% 
  select(dataset, year, geography1, geography2, simd, measure, value) %>%
  arrange(dataset, year, desc(geography1), simd, geography2, desc(measure), 
          value)

#Save certain variables as objects, to be used as selections in the...
#filters over the first graph. 
depr_dataset <- deprivation %>% distinct(dataset) %>% pull(dataset)
depr_location_types <- deprivation %>% distinct(geography1) %>% pull(geography1)
depr_financial_years <- deprivation %>% arrange(desc(year)) %>%
  distinct(year) %>% pull(year)
depr_measures <- deprivation %>% distinct(measure) %>% pull(measure)

#Last step: transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#These filters work best when the column type is factor.
deprivation <- deprivation %>% mutate_if(is.character, as.factor)

#Moving on to the second file utilised in the Deprivation tab, called 'RII'.
RII <- read_csv("MentalHealthInpatientActivity_RIIData_20201124.csv", col_types = "ccccn")

#Transform the 'measure' variable: change the value 'Residents' to 'Hospital...
#residents' in order to make its meaning clearer.
#Also, round the rates to two decimal points only. This fixes the problem in...
#the "Table" tab, where the numeric filters have too many decimal points.
#Finally, use the select() command to change the order of columns in the...
#dataset, and then sort the tibble in a way that makes it a bit more...
#readable when viewed in the "Table" tab (optional).
RII <- RII %>%
  mutate(measure = recode(measure, "Residents" = "Hospital residents")) %>% 
  mutate(value = round(value, 2)) %>% 
  select(dataset, year, geography1, measure, value) %>%
  arrange(dataset, year, geography1, desc(measure), value)

#Save certain variables as objects, to be used as selections in the...
#filters over the second graph. 
RII_dataset <- RII %>% distinct(dataset) %>% pull(dataset)
RII_measures <- RII %>% distinct(measure) %>% pull(measure)

#Last step: transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#These filters work best when the column type is factor.
RII <- RII %>% mutate_if(is.character, as.factor)

#Import the file that will be used to create the cross-boundary flow tab.
flow <- read_csv("MentalHealthInpatientActivity_CrossBoundaryFlow_20201124.csv", col_types = "cccccnn")

#Save certain variables as objects, to be used as selections in the...
#filters. 
fl_dataset <- flow %>% arrange(dataset) %>% distinct(dataset) %>% 
  pull(dataset)
fl_boards_of_residence <- flow %>% distinct(`health board of residence`) %>% 
  pull(`health board of residence`)
fl_financial_years <- flow %>% arrange(desc(year)) %>% distinct(year) %>% 
  pull(year)

#Use the select() command to change the order of columns in the dataset, and...
#then sort the tibble in a way that makes it a bit more readable when...
#viewed in the "Table" tab (optional).
#Also, transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#These filters work best when the column type is factor.
flow <- flow %>% 
  select(dataset, year, `health board of residence`, 
         `health board of treatment`, flow, measure, value) %>%
  arrange(dataset, year, `health board of residence`, 
          `health board of treatment`, flow, desc(measure), value) %>%
  mutate_if(is.character, as.factor) 

#Import the file that will be used to create the readmissions tab.
readmissions <- read_csv("MentalHealthInpatientActivity_PercentageReadmissionData_20201124.csv", col_types = "ccccnc")

#Round the rates to two decimal points only. This fixes the problem in...
#the "Table" tab, where the numeric filters have too many decimal points.
#Then, use the select() command to change the order of columns in the dataset.
#Finally, sort the tibble in a way that makes it a bit more readable...
#when viewed in the "Table" tab (optional).
readmissions <- readmissions %>%
  mutate(value = round(value, 2)) %>%
  select(dataset, year, geography1, geography2, measure, value) %>%
  arrange(dataset, year, desc(geography1), geography2, desc(measure), value)

#Save certain variables as objects, to be used as selections in the...
#filters. 
readm_dataset <- readmissions %>% distinct(dataset) %>% pull(dataset) 
readm_financial_years <- readmissions %>% arrange(desc(year)) %>% 
  distinct(year) %>% pull(year)
readm_measures <- readmissions %>% distinct(measure) %>% pull(measure)
readm_locations <- readmissions %>% distinct(geography2) %>% pull(geography2)

#Last step: transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#These filters work best when the column type is factor.
readmissions <- readmissions %>% mutate_if(is.character, as.factor) 

#In the 'Table' tab, in addition to the data explorer files, we will be...
#visualising the two files from the 'Trend data' page.

#Import these two files: 1) activity by hospital, and 2) length of stay.
#Once they have been imported, we a) do some basic recoding, b) transform all...
#character variables into factors, and c) filter out the columns we don't need.
#We can also reorder the columns (with select()) and sort the datasets...
#(with arrange()) more optimally. 
#For the activity by hospital file, we also filter out Golden Jubilee and the...
#State Hospitals Board as these are alternative names for the National Waiting...
#Times Centre and the State Hospital respectively. No need to have two names, ...
#and thus duplicate entries, for each of these hospitals.
activity_by_hospital <- read_csv("MentalHealthInpatientActivity_TrendData_20201124.csv", 
                                 col_types = "cccccccn")

activity_by_hospital <- activity_by_hospital %>%
  mutate(TrendType = recode(TrendType, 
                            "HospitalResidents" = "Number of hospital residents", 
                            "Admissions" = "Number of admissions", 
                            "Discharges" = "Number of discharges", 
                            "Stays" = "Number of stays", 
                            "Patients" = "Number of patients")) %>%
  mutate(Specialty = recode(Specialty, "NonPsychiatric" = "Non-psychiatric")) %>% 
  filter(hospital_name != "Golden Jubilee National Hospital") %>% 
  filter(hospital_name != "The State Hospitals Board for Scotland") %>%
  mutate_if(is.character, as.factor) %>%
  select(-c(year_ending, hospital_code)) %>%
  select(Specialty, year, hbtreat_name, hospital_name, TrendType, Value) %>%
  arrange(Specialty, year, hbtreat_name, hospital_name, TrendType)

length_of_stay <- read_csv("MentalHealthInpatientActivity_LengthOfStay_20201124.csv", 
                           col_types = "ccccccn")

length_of_stay <- length_of_stay %>%
  mutate(LengthOfStay = recode(LengthOfStay, 
                               "< 1 day" = "Less than a day", 
                               "> 6 months" = "Over 6 months", 
                               "1 - 7 days" = "1 to 7 days", 
                               "29 days - 6 months" = "29 days to 6 months", 
                               "8 - 28 days" = "8 to 28 days")) %>% 
  mutate_if(is.character, as.factor) %>%
  select(-c(year_ending, geography1)) %>%
  select(Specialty, fyear, geography2, LengthOfStay, NumberOfStays) %>%
  arrange(Specialty, fyear, geography2)

#We can now start building the Shiny app.
