# Name: Data explorer
# Update Author: Alex Bruce
# Modified: 26/09/2023
# Type: Data visualisation
# 
# Original Author: Nikos Alexandrou
# Written on: RStudio
# Written for: R version 3.6.1
# Output: Shiny application
# Approximate run time: < 1 minute
# Description: This syntax creates a Shiny application that allows the...
# user to visualise mental health data in a variety of ways.


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
library("magrittr")#pipe operators
library("stringr")#for string operations
# library("shinymanager")#apply password protection to the app

# Specify publication date which is used in data file names
pub_date <- "20231212"


### SECTION 1: DATA MANIPULATION ----

### Diagnosis Import ----

## Import the file that will be used to create the trends in diagnoses tab.
diagnoses <- arrow::read_parquet(
  paste0("data/MHIA_DiagnosisTrends_DataExplorer_",pub_date,".parquet"))

# diagnoses <- read_csv("SMR01_SMR04_data_by_diagnosis.csv", 
#                       col_types = "ccccccn")

# Round the rates to two decimal points only. This fixes the problem in the...
# "Table" tab, where the numeric filters have too many decimal points.
# Change the text "crude rate" to just "rate".
# Then, use the select() command to change the order of columns in the dataset.
# Also, sort the tibble in a way that makes it a bit more readable when...
# viewed in the "Table" tab (optional).
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

# Save certain variables as objects, to be used as selections in the...
# filters. 
diag_dataset <- diagnoses %>% distinct(dataset) %>% pull(dataset)
diag_location_types <- diagnoses %>% distinct(geography1) %>% pull(geography1)
diag_diagnosis_groupings <- diagnoses %>% distinct(diagnosis_groupings) %>% 
  pull(diagnosis_groupings)
diag_measures <- diagnoses %>% distinct(measure) %>% pull(measure)

# Finally, transform all the character variables into factors.
# The table in the "Table" tab is supposed to contain filters below the...
# column names, to allow the users to create their own table.
# These filters work best when the column type is factor.
diagnoses <- diagnoses %>% mutate_if(is.character, as.factor)


### Geography Import ----

## Import the two files that will be used to create the geography tab.
# 'geography' contains the rates of patients/discharges that will be...
# displayed in the tooltip of our interactive map.
# 'CA' (see below) is the shapefile containing the council area polygons, ...
# which will be projected onto a leaflet map.
geography <- arrow::read_parquet(
  paste0("data/MHIA_GeographyRates_DataExplorer_",pub_date,".parquet"))

# geography <- read_csv("SMR01SMR04_geographyrates.csv", col_types = "ccccn")

# Round the rates to two decimal points only. This fixes the problem in the...
# "Table" tab, where the numeric filters have too many decimal points.
# Change the text "crude rate" to just "rate".
# Then, use the select() command to change the order of columns in the dataset.
# Also, sort the tibble in a way that makes it a bit more readable when...
# viewed in the "Table" tab (optional).
geography <- geography %>% mutate(Value = round(Value, 2)) %>% 
  mutate(measure = recode(measure, 
                          "Crude rate of patients (per 100,000 population)" = 
                            "Rate of patients (per 100,000 population)", 
                          "Crude rate of discharges (per 100,000 population)" = 
                            "Rate of discharges (per 100,000 population)")) %>% 
  select(dataset, fyear, ca, measure, Value) %>%
  arrange(dataset, fyear, ca, desc(measure), Value) 

# Save certain variables as objects, to be used as selections in the...
# filters. 
geography_dataset <- geography %>% distinct(dataset) %>% pull(dataset) 
geography_measures <- geography %>% distinct(measure) %>% pull(measure)
#geography_locations <- geography %>% distinct(ca) %>% pull(ca)
## Potentially remove if not used
geography_fin_years <- geography %>% arrange(desc(fyear)) %>% distinct(fyear) %>% 
  pull(fyear)

# Finally, transform all the character variables into factors.
# The table in the "Table" tab is supposed to contain filters below the...
# column names, to allow the users to create their own table.
# These filters work best when the column type is factor.
geography <- geography %>% mutate_if(is.character, as.factor)


### Age-Sex Import ----

## Import the file that will be used to create the age/sex tab.
age_sex<- arrow::read_parquet(
  paste0("data/MHIA_AgeSex_DataExplorer_",pub_date,".parquet"))

# age_sex <- read_csv("SMR01SMR04_age_sex.csv", col_types = "cccccccn")

# Change the text "crude rate" to just "rate".
age_sex <- age_sex %>% 
  mutate(measure = recode(measure, 
                          "Crude rate of patients (per 100,000 population)" = 
                            "Rate of patients (per 100,000 population)", 
                          "Crude rate of discharges (per 100,000 population)" = 
                            "Rate of discharges (per 100,000 population)"))  

# Save certain variables as objects, to be used as selections in the...
# filters.  
as_dataset <- age_sex %>% arrange(dataset) %>% distinct(dataset) %>% 
  pull(dataset)
as_location_types <- age_sex %>% arrange(desc(geography1)) %>% 
  distinct(geography1) %>% pull(geography1)
as_financial_years <- age_sex %>% arrange(desc(year)) %>% distinct(year) %>% 
  pull(year)
as_measures <- age_sex %>% arrange(desc(measure)) %>% distinct(measure) %>% 
  pull(measure)

# Transform all the character variables into factors.
# The table in the "Table" tab is supposed to contain filters below the...
# column names, to allow the users to create their own table.
# These filters work best when the column type is factor.
# Then, round the rates to two decimal points only. This fixes the problem in...
# the "Table" tab, where the numeric filters have too many decimal points.
# Next step: reverse the order of levels in the factor "sex_char".
# We want males to be first and females second, so that the legend in the age/...
# sex pyramid is consistent with what the pyramid shows (males are visualised...
# first, and females second).
# Finally, use the select() command to change the order of columns in the...
# dataset, and then sort the tibble in a way that makes it a bit more...
# readable when viewed in the "Table" tab (optional).
age_sex <- age_sex %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(value = round(value, 2)) %>%
  mutate(sex_char = fct_rev(sex_char)) %>% 
  select(dataset, year, geography1, geography2, ageband, sex_char, measure, 
         value) %>% 
  arrange(dataset, year, desc(geography1), geography2, ageband, desc(measure), 
          sex_char, value)


### Deprivation Import ----

## Import the files that will be used to create the deprivation tab.
# The deprivation tab will contain two graphs.
# The first graph will show activity broken down by deprivation quintile,...
# whereas the second graph will display the Relative Index of Inequality (RII)...
# as a trend over time.
# We start with the file containing the quintiles, called 'deprivation'.
deprivation<- arrow::read_parquet(
  paste0("data/MHIA_Deprivation_DataExplorer_",pub_date,".parquet"))

# deprivation <- read_csv("SMR01SMR04_deprivation.csv", col_types = "ccccccn")

# Change the text "crude rate" to just "rate".
# Transform the simd variable too:
# Recode value "1" to "1 - Most deprived" and "5" to "5 - Least deprived".
# This will make the first graph as well as the table under it...
# easier to understand.
# Then, round the rates to two decimal points only. This fixes the problem in...
# the "Table" tab, where the numeric filters have too many decimal points.
# Finally, use the select() command to change the order of columns in the...
# dataset, and then sort the tibble in a way that makes it a bit more...
# readable when viewed in the "Table" tab (optional).
deprivation <- deprivation %>%
  mutate(measure = recode(measure, 
                          "Crude rate of patients (per 100,000 population)" = 
                            "Rate of patients (per 100,000 population)", 
                          "Crude rate of discharges (per 100,000 population)" = 
                            "Rate of discharges (per 100,000 population)", 
                          "Crude rate of hospital residents (per 100,000 population)" = 
                            "Rate of hospital residents (per 100,000 population)")) %>%
  mutate(simd = recode(simd, "1" = "1 - Most deprived",
                       "2" = "2",
                       "3" = "3",
                       "4" = "4",
                       "5" = "5 - Least deprived")) %>% 
  mutate(value = round(value, 2)) %>% 
  select(dataset, year, geography1, geography2, simd, measure, value) %>%
  arrange(dataset, year, desc(geography1), simd, geography2, desc(measure), 
          value)

# Save certain variables as objects, to be used as selections in the...
# filters over the first graph. 
depr_dataset <- deprivation %>% distinct(dataset) %>% pull(dataset)
depr_location_types <- deprivation %>% distinct(geography1) %>% pull(geography1)
depr_financial_years <- deprivation %>% arrange(desc(year)) %>%
  distinct(year) %>% pull(year)
depr_measures <- deprivation %>% distinct(measure) %>% pull(measure)

# Last step: transform all the character variables into factors.
# The table in the "Table" tab is supposed to contain filters below the...
# column names, to allow the users to create their own table.
# These filters work best when the column type is factor.
deprivation <- deprivation %>% mutate_if(is.character, as.factor)


### Deprivation RII Import ----

## Moving on to the second file utilised in the Deprivation tab, called 'RII'.
RII<- arrow::read_parquet(
  paste0("data/MHIA_RII_DataExplorer_",pub_date,".parquet"))

# RII <- read_csv("RII_data.csv", col_types = "ccccn")

# Transform the 'measure' variable: change the value 'Residents' to 'Hospital...
# residents' in order to make its meaning clearer.
# Also, round the rates to two decimal points only. This fixes the problem in...
# the "Table" tab, where the numeric filters have too many decimal points.
# Finally, use the select() command to change the order of columns in the...
# dataset, and then sort the tibble in a way that makes it a bit more...
# readable when viewed in the "Table" tab (optional).
RII <- RII %>%
  mutate(measure = recode(measure, "Residents" = "Hospital residents")) %>% 
  mutate(value = round(value, 2)) %>% 
  select(dataset, year, geography1, measure, value) %>%
  arrange(dataset, year, geography1, desc(measure), value)

# Save certain variables as objects, to be used as selections in the...
# filters over the second graph. 
RII_dataset <- RII %>% distinct(dataset) %>% pull(dataset)
RII_measures <- RII %>% distinct(measure) %>% pull(measure)

# Last step: transform all the character variables into factors.
# The table in the "Table" tab is supposed to contain filters below the...
# column names, to allow the users to create their own table.
# These filters work best when the column type is factor.
RII <- RII %>% mutate_if(is.character, as.factor)


### Crossboundary Flow Import ----

## Import the file that will be used to create the cross-boundary flow tab.
flow<- arrow::read_parquet(
  paste0("data/MHIA_CrossBoundaryFlow_DataExplorer_",pub_date,".parquet"))

# flow <- read_csv("SMR01SMR04_cross_boundary_flow.csv", col_types = "cccccnn")

# Save certain variables as objects, to be used as selections in the...
# filters. 
fl_dataset <- flow %>% arrange(dataset) %>% distinct(dataset) %>% 
  pull(dataset)
fl_boards_of_residence <- flow %>% distinct(`health board of residence`) %>% 
  pull(`health board of residence`)
fl_financial_years <- flow %>% arrange(desc(year)) %>% distinct(year) %>% 
  pull(year)

# Use the select() command to change the order of columns in the dataset, and...
# then sort the tibble in a way that makes it a bit more readable when...
# viewed in the "Table" tab (optional).
# Also, transform all the character variables into factors.
# The table in the "Table" tab is supposed to contain filters below the...
# column names, to allow the users to create their own table.
# These filters work best when the column type is factor.
flow <- flow %>% 
  select(dataset, year, `health board of residence`, 
         `health board of treatment`, flow, measure, value) %>%
  arrange(dataset, year, `health board of residence`, 
          `health board of treatment`, flow, desc(measure), value) %>%
  mutate_if(is.character, as.factor) 


### Readmissions Import ----

## Import the file that will be used to create the readmissions tab.
readmissions<- arrow::read_parquet(
  paste0("data/MHIA_Readmissions_DataExplorer_",pub_date,".parquet"))

# readmissions <- read_csv("SMR04_Readmissions.csv", col_types = "ccccnc")

# Round the rates to two decimal points only. This fixes the problem in...
# the "Table" tab, where the numeric filters have too many decimal points.
# Then, use the select() command to change the order of columns in the dataset.
# Finally, sort the tibble in a way that makes it a bit more readable...
# when viewed in the "Table" tab (optional).
readmissions <- readmissions %>%
  mutate(value = round(value, 2)) %>%
  select(dataset, year, geography1, geography2, measure, value) %>%
  arrange(dataset, year, desc(geography1), geography2, desc(measure), value)

# Save certain variables as objects, to be used as selections in the...
# filters. 
readm_dataset <- readmissions %>% distinct(dataset) %>% pull(dataset) 
readm_financial_years <- readmissions %>% arrange(desc(year)) %>% 
  distinct(year) %>% pull(year)
readm_measures <- readmissions %>% distinct(measure) %>% pull(measure)
readm_locations <- readmissions %>% distinct(geography2) %>% pull(geography2)

# Last step: transform all the character variables into factors.
# The table in the "Table" tab is supposed to contain filters below the...
# column names, to allow the users to create their own table.
# These filters work best when the column type is factor.
readmissions <- readmissions %>% mutate_if(is.character, as.factor) 


### Trends Data Import ----

## In the 'Table' tab, in addition to the data explorer files, we will be...
# visualising the two files from the 'Trend data' page.

# Import these two files: 1) activity by hospital, and 2) length of stay.
# Once they have been imported, we a) do some basic recoding, b) transform all...
# character variables into factors, and c) filter out the columns we don't need.
# We can also reorder the columns (with select()) and sort the datasets...
# (with arrange()) more optimally.
# For the activity by hospital file, we also filter out Golden Jubilee and the...
# State Hospitals Board as these are alternative names for the National Waiting...
# Times Centre and the State Hospital respectively. No need to have two names, ...
# and thus duplicate entries, for each of these hospitals.


activity_by_hospital <- arrow::read_parquet(
  paste0("data/MHIA_DataTrends_DataExplorer_",pub_date,".parquet"))

# activity_by_hospital <- read_csv("SMR01SMR04_MH_trend_data_DataExplorer.csv", 
#                                  col_types = "cccccccn")

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
  select(Specialty, year, hbtreat_name, hospital_name, TrendType, Value) %>%
  arrange(Specialty, year, hbtreat_name, hospital_name, TrendType)


# Length of Stay Import ----
length_of_stay <- arrow::read_parquet(
  paste0("data/MHIA_LengthOfStay_DataExplorer_",pub_date,".parquet"))

# length_of_stay <- read_csv("SMR01_SMR04_length_of_stay_DataExplorer.csv", 
#                            col_types = "ccccccn")

length_of_stay <- length_of_stay %>%
  mutate(LengthOfStay = recode(LengthOfStay, 
                               "< 1 day" = "Less than a day", 
                               "> 6 months" = "Over 6 months", 
                               "1 - 7 days" = "1 to 7 days", 
                               "29 days - 6 months" = "29 days to 6 months", 
                               "8 - 28 days" = "8 to 28 days")) %>% 
  mutate_if(is.character, as.factor) %>%
  select(Specialty, fyear, geography2, LengthOfStay, NumberOfStays) %>%
  arrange(Specialty, fyear, geography2)

# #* Read in credentials for password-protecting the app ----
# credentials <- readRDS("admin/credentials.rds")

# We can now start building the Shiny app.


### SECTION 2: USER INTERFACE ----

# Source the ui file

ui <- source('ui.R', local = TRUE)$value
# Needs $value after source expression for ui to prevent 'TRUE' being printed 
# at the bottom of the app following R Shiny evaluation sequence


### SECTION 3: SERVER ----


# A Shiny app needs two things: the Server and the User Interface.
# We can begin with the Server.
server <- function(input, output, session) {
  
  # # * Shinymanager authorisation ----
  # # Uncomment this section to password protect the app.
  # # Re-comment out to remove password protection on launch day.
  #  res_auth <- secure_server(
  #  check_credentials = check_credentials(credentials)
  #  )
  # 
  #  output$auth_output <- renderPrint({
  #  reactiveValuesToList(res_auth)
  #  })
  
  ##* ObserveEvent() commands ----
  
  # These observeEvent() commands will be combined with action buttons in...
  # the User Interface to allow the user to navigate to each tab in the Explorer...
  # by clicking on links in the Introduction page (in addition to the classic...
  # way of navigating, which is by clicking on the tab headers).
  observeEvent(input$link_to_trends_in_diagnoses_tab, {
    updateTabsetPanel(session, "Panels", selected = "Trends in diagnoses")
  })
  observeEvent(input$link_to_geography_tab, {
    updateTabsetPanel(session, "Panels", selected = "Geography")
  })
  observeEvent(input$link_to_age_sex_tab, {
    updateTabsetPanel(session, "Panels", selected = "Age/sex")
  })
  observeEvent(input$link_to_deprivation_tab, {
    updateTabsetPanel(session, "Panels", selected = "Deprivation")
  })
  observeEvent(input$link_to_cross_boundary_flow_tab, {
    updateTabsetPanel(session, "Panels", selected = "Cross-boundary flow")
  })
  observeEvent(input$link_to_readmissions_tab, {
    updateTabsetPanel(session, "Panels", selected = "Readmissions")
  })
  observeEvent(input$link_to_table_tab, {
    updateTabsetPanel(session, "Panels", selected = "Table")
  })
  
  
  # Source Server files ----
  ###* Trends in diagnoses server ----
  source('modules/server_01_diagnosis_trends.R', local = TRUE)
  
  ###* Geography server ----
  source('modules/server_02_geography.R', local = TRUE)
  
  ##* Age/sex server ----
  source('modules/server_03_agesex.R', local = TRUE)
  
  ##* Deprivation server ----
  source('modules/server_04_deprivation.R', local = TRUE)
  
  ##* Cross-boundary flow server ----
  source('modules/server_05_crossboundary.R', local = TRUE)

  ##* Readmissions server ----
  source('modules/server_06_readmissions.R', local = TRUE)
  
  ##* Table server ----
  source('modules/server_07_table.R', local = TRUE)  
 
  
  ##* Glossary ----    
  
  # We have prepared a glossary to help the user understand the graphs and...
  # tables more easily.
  
  # Create a download button for the glossary.
  # The glossary is created in MS Word, and then converted into a PDF.
  # The PDF was then placed inside the folder called "www".
  glossary_code_shortcut <- downloadHandler(
    filename = 'Mental Health Inpatient Activity glossary.pdf',
    content = function(file) {
      file.copy("www/MHIA Glossary.pdf", file)
    }
  )
  
  # There must be one download button in each tab (incl. the Introduction tab).
  # Therefore, we need to execute the command eight times.
  output$download_glossary_one <- glossary_code_shortcut
  output$download_glossary_two <- glossary_code_shortcut
  output$download_glossary_three <- glossary_code_shortcut  
  output$download_glossary_four <- glossary_code_shortcut 
  output$download_glossary_five <- glossary_code_shortcut
  output$download_glossary_six <- glossary_code_shortcut
  output$download_glossary_seven <- glossary_code_shortcut
  output$download_glossary_eight <- glossary_code_shortcut
  
}

# We are now finished with the Server syntax.





### Launch the application ----
shinyApp(ui = ui, server = server)

# End of script.