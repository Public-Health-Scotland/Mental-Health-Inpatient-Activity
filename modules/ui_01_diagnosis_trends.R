##* Trends in diagnoses user interface ----   

# Create the diagnoses tab.

# Create a new tabPanel, stylise it, and give a title to the tab.
tabPanel(
  "Trends in diagnoses", 
  icon = icon("line-chart"), 
  style = "height: 95%; width: 95%; background-color: #FFFFFF; 
      border: 0px solid #FFFFFF;",
  h1("Trends in diagnoses", id = 'diagnoses_top'),
  
  # Provide a description for the tab.
  p(HTML("This section allows you to see changes in mental health 
             conditions since 1997/1998. Use the filters to visualise the data you 
             are interested in. You can visualise multiple health boards of 
             treatment at the same time. To view your data selection in a table,
             use the <a href = '#diagnoses_link'> 'Show/hide table' </a> 
             button at the bottom of the page. To download your data selection 
             as a .csv file, use the 'Download as .csv' button under the filters. 
             At the top-right corner of the graph, you will see a toolbar with 
             four buttons:")),
  
  # Insert instructions on how to use the plotly toolbar.
  tags$ul(
    tags$li(tags$b("Download plot as a png"), 
            icon("camera"), 
            " - click this button to save the graph as an image 
                (please note that Internet Explorer does not support this 
                function)."),
    tags$li(tags$b("Zoom"), 
            icon("search"), 
            " - zoom into the graph by clicking this button and then 
                clicking and dragging your mouse over the area of the 
                graph you are interested in."),  
    tags$li(tags$b("Pan"), 
            icon("move", lib = "glyphicon"), 
            " - adjust the axes of the graph by clicking this button 
                and then clicking and moving your mouse in any direction
                you want."),  
    tags$li(tags$b("Reset axes"), 
            icon("home"), 
            " - click this button to return the axes to their 
                default range.")
  ),
  
  # Download button for the glossary.
  p("You can also download our glossary of commonly used terms in
        mental health care, which has been created to help you 
        understand the information visualised in the explorer:"),
  downloadButton(outputId = "download_glossary_two", 
                 label = "Download glossary", 
                 class = "glossarytwo"),
  tags$head(
    tags$style(".glossarytwo { background-color: #0072B2; } 
                   .glossarytwo { color: #FFFFFF; }")
  ),
  
  # Repeat the standard note regarding disclosure control from the...
  # Introduction tab. Add another note that explains the difference between...
  # the psychiatric data presented in this tab v. the psychiatric data presented...
  # elsewhere in the publication.
  p(br(),
    tags$b("Notes:"),
    br(),
    tags$b("1. Statistical disclosure control has been applied to protect 
               patient confidentiality by rounding values to the nearest 5. 
               Rates were calculated using the rounded values and are displayed
               to 2 decimal places. As a result, the figures presented here 
               may not be additive and may differ from previous 
               sources of information."),
    br(),
    tags$b("2. When viewing data for psychiatric specialties in the 'Trends in 
               diagnoses' page, please keep in mind that only people with a primary diagnosis 
               of F00-F99 (International Classification of Diseases, Tenth Revision) 
               have been included. By contrast, all other psychiatric specialty 
               data in this publication includes both F and non-F codes. As a result, 
               the psychiatric specialty figures presented in the 'Trends in diagnoses' 
               page are not comparable with the psychiatric specialty figures elsewhere 
               in this statistical release.")),
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
                     inputId = "diagnoses_dataset",
                     label = "Select treatment specialty", 
                     choices = diag_dataset, 
                     selected = "Psychiatric",
                     justified = TRUE,
                     checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                     direction = "vertical"
                   )
            ),
            
            column(4,
                   uiOutput("diagnoses_location_types")
            ),
            
            column(5, 
                   uiOutput("diagnoses_locations")
            ),
            
            column(4, 
                   shinyWidgets::pickerInput(
                     inputId = "diagnoses_diagnosis_groupings",
                     label = "Select diagnosis grouping",
                     choices = diag_diagnosis_groupings,
                     selected = 
                       "Disorders of adult behaviour and personality"
                   )
            ),
            
            column(5, 
                   shinyWidgets::pickerInput(
                     inputId = "diagnoses_measure_type",
                     label = "Select measure",
                     choices = diag_measures, 
                     selected = "Number of patients"
                   )
            ),
            
            column(4,
                   downloadButton(outputId = "download_diagnoses", 
                                  label = "Download as .csv", 
                                  class = "mytimetrendbutton"),
                   tags$head(
                     tags$style(".mytimetrendbutton { background-color: 
                                    #0072B2; } 
                                    .mytimetrendbutton { color: #FFFFFF; }")
                   )
            )
  ),
  
  # In the main panel of the diagnoses tab, insert the diagnoses line...
  # chart, the 'Show/hide table' button, and the diagnoses table. 
  mainPanel(width = 12,
            plotlyOutput("diagnoses_plot", 
                         width = "1090px",
                         height = "600px"),
            br(),
            br(),
            HTML("<button data-toggle = 'collapse' href = '#diagnoses'
                     class = 'btn btn-primary' id = 'diagnoses_link'> 
                     <strong> Show/hide table </strong></button>"),
            HTML("<div id = 'diagnoses' class = 'collapse'>"),
            br(),
            dataTableOutput("diagnoses_table"),
            HTML("</div>"),
            br(),
            br(),
            # Finally, add a button that allows the user to go back to the...
            # top of the page.
            tags$a(href = '#diagnoses_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            br(),
            br())
)