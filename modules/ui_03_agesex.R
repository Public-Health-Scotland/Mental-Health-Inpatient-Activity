##* Age/sex user interface ----   

# Create the age/sex tab.

# Create a new tabPanel, stylise it, and give a title to the tab. 
tabPanel(
  "Age/sex", 
  icon = icon("child"), 
  style = "height: 95%; width: 95%; background-color: #FFFFFF; 
      border: 0px solid #FFFFFF;",
  h1("Age/sex", id = 'age_sex_top'),
  
  # Provide a description for the tab.
  p(HTML("This section allows you to explore the age and sex distribution 
             of the data. Use the filters to visualise the data you are 
             interested in. To view your data selection in a table, use the 
             <a href = '#age_and_sex_link'> 'Show/hide table' </a> button at 
             the bottom of the page. To download your data selection as a .csv 
             file, use the 'Download as .csv' button under the filters. At the 
             top-right corner of the graph, you will see a toolbar with four 
             buttons:")),
  
  # Insert instructions on how to use the plotly toolbar.
  tags$ul(
    tags$li(tags$b("Download plot as a png"), 
            icon("camera"),
            " - click this button to save the graph as an image 
                (please note that Internet Explorer does not support 
                this function)."),
    tags$li(tags$b("Zoom"), 
            icon("search"),
            " - zoom into the graph by clicking this button and then 
                clicking and dragging your mouse over the area of the 
                graph you are interested in."),  
    tags$li(tags$b("Pan"), 
            icon("move", lib = "glyphicon"),
            " - adjust the axes of the graph by clicking this button
                and then clicking and moving your mouse in any 
                direction you want."),
    tags$li(tags$b("Reset axes"), 
            icon("home"),
            " - click this button to return the axes to their 
                default range.")
  ),
  
  # Download button for the glossary.
  p("You can also download our glossary of commonly used terms in
        mental health care, which has been created to help you
        understand the information visualised in the explorer:"),
  downloadButton(outputId = "download_glossary_four", 
                 label = "Download glossary", 
                 class = "glossaryfour"),
  tags$head(
    tags$style(".glossaryfour { background-color: #0072B2; } 
                   .glossaryfour { color: #FFFFFF; }")
  ),
  
  # Repeat the point regarding disclosure control.
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
            # We have five filters in total.
            # Two of our filters have already been created in...
            # the Server syntax, using renderUI().
            # We just need to make them appear using the uiOutput() command.
            # We arrange our filters in columns.
            # The last column contains the 'Download as .csv' button.
            column(3,
                   shinyWidgets::radioGroupButtons(
                     inputId = "age_sex_dataset",
                     label = "Select treatment specialty", 
                     choices = as_dataset, 
                     selected = "Psychiatric",
                     justified = TRUE,
                     checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                     direction = "vertical"
                   )
            ),
            
            column(4,
                   uiOutput("age_sex_location_types")
            ),
            
            column(4,
                   uiOutput("age_sex_locations")
            ),
            
            column(4,
                   shinyWidgets::pickerInput(
                     inputId = "age_sex_financial_year",
                     label = "Select financial year", 
                     choices = as_financial_years,
                     selected = "2022/2023"
                   )
            ),
            
            column(4,
                   shinyWidgets::pickerInput(
                     inputId = "age_sex_measure_type",
                     label = "Select measure",
                     choices = as_measures, 
                     selected = "Number of patients"
                   )
            ),
            
            column(4, 
                   downloadButton(outputId = "download_age_sex", 
                                  label = "Download as .csv", 
                                  class = "myagesexbutton"),
                   tags$head(
                     tags$style(".myagesexbutton { background-color: 
                                    #0072B2; } 
                                    .myagesexbutton { color: #FFFFFF; }")
                   )
            )
  ),
  
  # In the main panel, we insert the age/sex pyramid, the 'Show/hide... 
  # table' button, and the age/sex table.
  mainPanel(width = 12, 
            plotlyOutput("age_sex_pyramid", 
                         width = "1090px",
                         height = "400px"),
            br(),
            br(),
            HTML("<button data-toggle = 'collapse' href = '#ageandsex'
                     class = 'btn btn-primary' id = 'age_and_sex_link'> 
                     <strong>Show/hide table</strong></button>"),
            HTML("<div id = 'ageandsex' class = 'collapse'>"),
            br(),
            dataTableOutput("age_sex_table"),
            HTML("</div>"),
            br(),
            br(),
            # Finally, add a button that allows the user to go back to...
            # the top of the page.
            tags$a(href = '#age_sex_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            
            br(),
            br())
)