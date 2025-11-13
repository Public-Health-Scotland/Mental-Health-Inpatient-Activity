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
library('purrr') # map function
library("plotly")#data viz 
library("DT")#for the tables under the graphs
library("readr")#read .csv and .rds files into R
library("forcats")#for working with factors more efficiently
library("RColorBrewer")#pre-made colour palettes
library("googleVis")#cross-boundary flow chart (Sankey diagram)
library("shinyWidgets")#filters
library("magrittr")#pipe operators
library("stringr")#for string operations
library('shinycssloaders')
library("shinymanager")#apply password protection to the app

#* Specify publication date which is used in data file names
pub_date <- "2025_December"

#* Read in data prep file ----
source('data_preparation.R', local = TRUE)

#* sourcing functions created for app (see functions folder) -------------------------------
list.files("functions") %>%
  map(~ source(paste0("functions/", .)))

# #* Read in credentials for password-protecting the app ----
# credentials <- readRDS("admin/credentials.rds")


### SECTION 2: USER INTERFACE ----

# Create the fluidPage that will house the data explorer. 

ui <- 
  # secure_app( # If password protection is needed.
  fluidPage(
  
  ## load the CSS stylesheet that defines how things look 
  tags$head(includeCSS("www/stylesheet.css")),
  style = "width: 100%; height: 100%; max-width: 1200px;", 

  # We are going to divide our UI into discrete sections, called tab panels.
  # To do this, we need the layout "tabsetPanel()".
  tabsetPanel(
    id = "Panels", 
    
    ##* Source the ui files for each tab of the dashboard ----
    # Needs $value after source expression for ui to prevent 'TRUE' being printed 
    # at the bottom of the app following R Shiny evaluation sequence
    
    source("modules/ui_00_introduction.R", local = TRUE)$value,

    source("modules/ui_01_diagnosis_trends.R", local = TRUE)$value,
    
    source("modules/ui_02_geography.R", local = TRUE)$value,
    
    source("modules/ui_03_agesex.R", local = TRUE)$value,
    
    source("modules/ui_04_deprivation.R", local = TRUE)$value,
    
    source("modules/ui_05_crossboundary.R", local = TRUE)$value,
    
    source("modules/ui_06_readmissions.R", local = TRUE)$value,
    
    source("modules/ui_07_learning_disabilities.R", local = TRUE)$value,
    
    source("modules/ui_08_table.R", local = TRUE)$value

  ) # End of tab set. 

) # End of ui fluid page.

# ) # End of password-protection.


### SECTION 3: SERVER ----

server <- function(input, output, session) {
  
  # # * Shinymanager authorisation ----
  # # Uncomment this section to password protect the app.
  # # Re-comment out to remove password protection on launch day.
  # res_auth <- secure_server(
  # check_credentials = check_credentials(credentials)
  # )
  # 
  # output$auth_output <- renderPrint({
  # reactiveValuesToList(res_auth)
  # })
  
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
  observeEvent(input$link_to_learning_disabilities_tab, {
    updateTabsetPanel(session, "Panels", selected = "Learning Disabilities")
  })
  observeEvent(input$link_to_table_tab, {
    updateTabsetPanel(session, "Panels", selected = "Table")
  })
  
  
  ##* Source the Server files for each tab ----
  source('modules/server_01_diagnosis_trends.R', local = TRUE)
  
  source('modules/server_02_geography.R', local = TRUE)
  
  source('modules/server_03_agesex.R', local = TRUE)
  
  source('modules/server_04_deprivation.R', local = TRUE)
  
  source('modules/server_05_crossboundary.R', local = TRUE)

  source('modules/server_06_readmissions.R', local = TRUE)
  
  source("modules/server_07_learning_disabilities.R", local = TRUE)
  
  source('modules/server_08_table.R', local = TRUE)  
 
  
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
  output$download_glossary_00 <- glossary_code_shortcut
  output$download_glossary_01 <- glossary_code_shortcut
  output$download_glossary_02 <- glossary_code_shortcut  
  output$download_glossary_03 <- glossary_code_shortcut 
  output$download_glossary_04 <- glossary_code_shortcut
  output$download_glossary_05 <- glossary_code_shortcut
  output$download_glossary_06 <- glossary_code_shortcut
  output$download_glossary_07 <- glossary_code_shortcut
  output$download_glossary_08 <- glossary_code_shortcut
  
  ##* Keep dashboard active indefinitely to meet accessibility requirements
  # (Keep at the end of server)
  auto_invalidate <- reactiveTimer(10000)
  observe({
    auto_invalidate()
    cat(".")
  })
}

# Sets language right at the top of source (required this way for screen readers)
attr(ui, "lang") = "en"

# We are now finished with the Server syntax.


### Launch the application ----
shinyApp(ui = ui, server = server)

# End of script.