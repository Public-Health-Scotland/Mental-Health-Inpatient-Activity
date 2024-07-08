##* Geography user interface ----   

# Create the geography tab.

# Create a new tabPanel, stylise it, and give a title to the tab.
tabPanel(
  "Geography",
  icon = icon("globe"),
  style = "height: 95%; width: 95%; background-color: #FFFFFF;
      border: 0px solid #FFFFFF;",
  h1("Geography", id = 'geography_top'),
  
  # Provide a description for the tab.
  p(HTML("This section allows you to explore the rate of discharges and 
             patients over time, broken down by council area of residence. 
             Use the filters to visualise the data you are interested in. 
             To view the data in a table,
             use the <a href = '#geographies_link'> 'Show/hide table' </a> button
             at the bottom of the page. To download your data selection as a .csv
             file, use the 'Download as .csv' button next to the filters.")),
  
  # Download button for the glossary.
  p("You can also download our glossary of commonly used terms in
        mental health care, which has been created to help you
        understand the information visualised in the explorer:"),
  downloadButton(outputId = "download_glossary_three",
                 label = "Download glossary",
                 class = "glossarythree"),
  tags$head(
    tags$style(".glossarythree { background-color: #0072B2; }
                   .glossarythree { color: #FFFFFF; }")
  ),
  
  # Repeat the standard note regarding disclosure control.
  p(br(),
    tags$b(
      "Note: Statistical disclosure control has been applied to protect 
          patient confidentiality by rounding values to the nearest 5 following 
          their calculation. Rates were calculated using the rounded values and are displayed
          to 2 decimal places. As a result, the figures presented here may not 
          be additive and may differ from previous sources of information."
    )),
  p(""),
  wellPanel(tags$style(".well { background-color: #FFFFFF;
                           border: 0px solid #336699; }"),
            
            # Insert the filters.
            # We have three filters in total.
            # We arrange our filters in columns.
            # The third column contains the 'Download as .csv' button.
            column(3,
                   shinyWidgets::radioGroupButtons(
                     inputId = "geography_dataset_input",
                     label = "Select treatment specialty",
                     choices = geography_dataset,
                     selected = "Psychiatric",
                     justified = TRUE,
                     checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                     direction = "vertical"
                   )
            ),
            
            column(5, 
                   uiOutput("geography_location_input")
            ),
            
            column(5,
                   shinyWidgets::pickerInput(
                     inputId = "geography_measure_input",
                     label = "Select measure",
                     choices = geography_measures,
                     selected =
                       "Rate of patients (per 100,000 population)"
                   )
            ),
            
            column(3,
                   downloadButton(outputId = "download_geography",
                                  label = "Download as .csv",
                                  class = "mygeographybutton",
                                  style = "margin: 25px 10px 25px 10px"),
                   tags$head(
                     tags$style(".mygeographybutton { background-color:
                                    #0072B2; }
                                    .mygeographybutton { color: #FFFFFF; }")
                   )
            )
  ),
  
  
  # In the main panel of the geography tab, insert the geography line chart
  # the 'Show/hide table' button, and the geography table. 
  mainPanel(width = 12,
            plotlyOutput("geography_plot", 
                         width = "1090px",
                         height = "600px"),
            br(),
            br(),
            HTML("<button data-toggle = 'collapse' href = '#geography'
                     class = 'btn btn-primary' id = 'geography_link'> 
                     <strong> Show/hide table </strong></button>"),
            HTML("<div id = 'geography' class = 'collapse'>"),
            br(),
            dataTableOutput("geography_table"),
            HTML("</div>"),
            br(),
            br(),
            # Finally, add a button that allows the user to go back to the...
            # top of the page.
            tags$a(href = '#geography_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            br(),
            br())
)