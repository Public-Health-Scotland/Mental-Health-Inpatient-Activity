##* Table user interface ----    

# Create the table tab.

# Create the final tabPanel, stylise it, and give a title to the tab. 
tabPanel(
  "Table", 
  icon = icon("table"), 
  style = "float: top; height: 95%; width: 95%; background-color: #FFFFFF; 
      border: 0px solid #FFFFFF;", 
  h1("Table", id = 'table_top'),
  
  # Provide a description for the tab.
  p("This section allows you to view the data in table format. Use the  
        'Select data file' filter to visualise the file you are interested in 
        (the list includes data files used in the 'Trend data' page). You can then
        click on the filters below the column names of the table and use the 
        dropdowns to modify the table. To download your data selection as a .csv 
        file, use the 'Download as .csv' button."),
  
  # Download button for the glossary.
  p("You can also download our glossary of commonly used terms in
        mental health care, which has been created to help you
        understand the information visualised in the explorer:"),
  downloadButton(outputId = "download_glossary_eight", 
                 label = "Download glossary", 
                 class = "glossaryeight"),
  tags$head(
    tags$style(".glossaryeight { background-color: #0072B2; } 
                   .glossaryeight { color: #FFFFFF; }")
  ),
  
  # Statistical disclosure control note.
  p(br(),
    tags$b(
      "Note: Statistical disclosure control has been applied to protect 
          patient confidentiality. As a result, the figures presented here may 
          not be additive and may differ from previous sources of information. 
          Please see the respective tab for specific disclosure information on
          each topic.
          "
    )),
  p(""),
  wellPanel(tags$style(".well { background-color: #FFFFFF; 
                           border: 0px solid #336699; }"),
            
            # We are only using one filter here, which contains the...
            # names of the files.
            # We also insert the 'Download as .csv' button.
            column(6,
                   shinyWidgets::pickerInput(
                     inputId = "table_filenames", 
                     label = "Select data file",  
                     choices = c("Trends in diagnoses (Data explorer)",
                                 "Geography (Data explorer)",
                                 "Age/sex (Data explorer)", 
                                 "Deprivation - SIMD quintiles (Data explorer)",
                                 "Deprivation - Relative Index of Inequality (Data explorer)",
                                 "Cross-boundary flow (Data explorer)",
                                 "Readmissions (Data explorer)",
                                 "Activity by hospital (Trend data)",
                                 "Length of stay (Trend data)"), 
                     width = "95%"
                   )
            ), 
            
            column(4,
                   downloadButton(outputId = 'download_table', 
                                  label = 'Download as .csv', 
                                  class = "mytablebutton", 
                                  style = "margin: 25px 10px 25px 10px")
            )
  ),
  
  tags$head(
    tags$style(".mytablebutton { background-color: #0072B2; } 
                   .mytablebutton { color: #FFFFFF; }")
  ),
  
  # Finally, insert the actual table.
  mainPanel(width = 12, 
            dataTableOutput("table_tab"),
            br(),
            # Add a button that allows the user to go back to the top of the...
            # page.
            tags$a(href = '#table_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            br(),
            br()
  ) 
  
) # End of tab panel.