# Name: Data explorer
# Update Author: Alex Bruce
# Modified: 26/09/2023
# Type: Data visualisation
#
# Major rework: May 2024
# Author: Alex Bruce
# Rework features: 
# - R 4.1.2
# - no longer one file, uses source throughout to split app into more 
# manageable sections
# - accessibility improvements
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

# Read in data prep file ----
source('data_preparation.R', local = TRUE)

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