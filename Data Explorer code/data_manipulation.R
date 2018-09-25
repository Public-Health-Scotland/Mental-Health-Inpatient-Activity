#Name: Psychiatric inpatient activity data explorer
#Author: Nikos Alexandrou
#Modified: 17/09/2018
#Type: Data visualisation
#Written on: RStudio
#Written for: R version 3.2.3 
#Output: Shiny application
#Approximate run time: < 1 minute
#Description: This syntax creates a Shiny application that allows the... 
#user to visualise psychiatric inpatient data in a variety of ways.

#Load the necessary libraries.

library("shiny")#interactivity
library("dplyr")#easier variable manipulation
library("plotly")#create graphs 
library("DT")#insert tables under the graphs
library("readr")#read CSV files into R
library("forcats")#change the order of factor levels more easily
library("RColorBrewer")#pre-made colour palettes
library("googleVis")#cross-boundary flow chart
library("shinyWidgets")#reactive filters

#Import the data that will be visualised in the explorer.

#Start by defining the relevant filepath.

filepath <- paste0("//PHI_conf/MentalHealth1/Inpatient care/Publications/",
                   "Hospital-Inpatient-Care-of-People-with-Mental-Health-",
                   "Problems-in-Scotland/20180925/Output/")

#Import the file that will be used to create the time trend tab.

time_trend <- read_csv(paste0(filepath, 
                              "20180925_MH_data_by_diagnosis.csv"), 
                       col_types = "cccccn")

#Round the rates to two decimal points only. This fixes the problem in the...
#"Table" tab, where the numeric filters have too many decimal points.
#Also, sort the tibble in a way that makes it a bit more understandable when...
#viewed in the "Table" tab (optional).

time_trend <- time_trend %>% mutate(value = round(value, 2)) %>%
  arrange(year, desc(geography1), geography2, diagnosis_groupings, 
          desc(measure), value)

#Save certain variables as objects, to be used as selections in the...
#reactive filters. 

tt_location_types <- time_trend %>% distinct(geography1) %>% pull(geography1)
tt_diagnoses <- time_trend %>% 
  distinct(diagnosis_groupings) %>% pull(diagnosis_groupings)
tt_measures <- time_trend %>% distinct(measure) %>% pull(measure)

#Finally, transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#Transforming these variables into factors is the only way for these... 
#filters to work.

time_trend <- time_trend %>% mutate_if(is.character, as.factor)

#Import the file that will be used to create the age/sex tab.

age_sex <- read_csv(paste0(filepath, "20180925_MH_age_sex.csv"),
                    col_types = "ccccccn")

#Save certain variables as objects, to be used as selections in the...
#reactive filters.  

as_location_types <- age_sex %>% arrange(desc(geography1)) %>% 
  distinct(geography1) %>% pull(geography1)
as_financial_years <- age_sex %>% arrange(desc(year)) %>%
  distinct(year) %>% pull(year)
as_measures <- age_sex %>% arrange(desc(measure)) %>% 
  distinct(measure) %>% pull(measure)

#Transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#Transforming these variables into factors is the only way for these... 
#filters to work.
#Then, round the rates to two decimal points only. This fixes the problem in...
#the "Table" tab, where the numeric filters have too many decimal points.
#Next step: reverse the order of levels in the factor "sex_char".
#We want males to be first and females second, so that the legend in the age/...
#sex pyramid is consistent with what the pyramid shows (males are visualised...
#first, and females second).
#Finally, sort the tibble in a way that makes it a bit more understandable...
#when viewed in the "Table" tab (optional).

age_sex <- age_sex %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(value = round(value, 2)) %>%
  mutate(sex_char = fct_rev(sex_char)) %>% 
  arrange(year, desc(geography1), geography2, ageband, desc(measure), 
          sex_char, value)

#Import the file that will be used to create the deprivation tab.

deprivation <- read_csv(paste0(filepath, "20180925_MH_deprivation.csv"), 
                        col_types = "cccccn")

#Transform the simd variable:
#Rename value "1" as "1 - Most deprived" and "5" as "5 - Least deprived".
#This will make the deprivation bar chart as well as the table under it...
#easier to understand.
#Then, round the rates to two decimal points only. This fixes the problem in...
#the "Table" tab, where the numeric filters have too many decimal points.
#Finally, sort the tibble in a way that makes it a bit more understandable...
#when viewed in the "Table" tab (optional).

deprivation <- deprivation %>%
  mutate(simd = recode(simd, "1" = "1 - Most deprived", 
                       "5" = "5 - Least deprived")) %>% 
  mutate(value = round(value, 2)) %>% 
  arrange(year, desc(geography1), simd, geography2, desc(measure), value)

#Save certain variables as objects, to be used as selections in the...
#reactive filters. 

depr_location_types <- deprivation %>% distinct(geography1) %>% pull(geography1)
depr_financial_years <- deprivation %>% arrange(desc(year)) %>%
  distinct(year) %>% pull(year)
depr_measures <- deprivation %>% distinct(measure) %>% pull(measure)

#Last step: transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#Transforming these variables into factors is the only way for these... 
#filters to work.

deprivation <- deprivation %>% mutate_if(is.character, as.factor) 

#Import the file that will be used to create the cross-boundary flow tab.

flow <- read_csv(paste0(filepath, "20180925_MH_cross_boundary_flow.csv"), 
                 col_types = "ccccnn")

#Save certain variables as objects, to be used as selections in the...
#reactive filters. 

fl_boards_of_residence <- flow %>% distinct(`health board of residence`) %>% 
  pull(`health board of residence`)
fl_financial_years <- flow %>% arrange(desc(year)) %>% 
  distinct(year) %>% pull(year)

#Finally, transform all the character variables into factors.
#The table in the "Table" tab is supposed to contain filters below the...
#column names, to allow the users to create their own table.
#Transforming these variables into factors is the only way for these... 
#filters to work.

flow <- flow %>% mutate_if(is.character, as.factor)

#In the 'Table' tab, in addition to the data explorer files, we will be...
#visualising the files from the 'Data Trends' page.

#Import these two files: 1) length of stay, and 2) activity by hospital.
#Once they have been imported, we transform all character variables into...
#factors and filter out the columns we don't need.

activity_by_hospital <- read_csv(paste0(filepath, "20180925_MH_trend_data.csv"), 
                                 col_types = "cccccnnnnn")

activity_by_hospital <- activity_by_hospital %>% 
  mutate_if(is.character, as.factor) %>%
  select(year, hbtreat_name, hospital_name, admissions, discharges, 
         patients, stays, hospital_residents)

length_of_stay <- read_csv(paste0(filepath, "20180925_MH_length_of_stay.csv"), 
                           col_types = "ccccnnnnn")

length_of_stay <- length_of_stay %>% mutate_if(is.character, as.factor) %>%
  select(fyear, geography1, geography2, `Less than a day`, `1 to 7 days`, 
         `8 to 28 days`, `29 days to 6 months`, `Over 6 months`) %>%
  
  #Finally, we sort the length of stay tibble in a way that makes it a bit...
  #more understandable when viewed in the "Table" tab (optional).
  
  arrange(fyear, desc(geography1), geography2)

#We can now start building the shiny app.
