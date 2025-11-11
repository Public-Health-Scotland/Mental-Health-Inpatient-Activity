##* Readmissions user interface ----   

# Create the readmissions tab.

# Create a new tabPanel, stylise it, and give a title to the tab. 
tabPanel(
  "Readmissions", 
  icon = icon("bed"), 
  style = "height: 95%; width: 95%; background-color: #FFFFFF; 
      border: 0px solid #FFFFFF;",
  h1("Readmissions", id = 'readm_top'),
  
  # Provide a description for the tab.
  p(HTML("This section presents percentage readmissions within 28 and 133 
             days after discharge. There are two graphs in this page: the 
             <a href = '#readm_HB_comparison_link'> first one </a> allows you to 
             compare multiple health boards of treatment in a 
             single year, whereas the <a href = '#readm_trend_link'> second one </a> 
             is a time trend for each board. Use the filters to visualise the data 
             you are interested in. It is possible to select multiple health boards 
             in the second graph. To view your data selection in a table, use the 
             <a href = '#read_link_two'> 'Show/hide table' </a> button under each 
             graph. To download your data selection as a .csv file, use the 
             'Download as .csv' button next to each set of filters. At the top-right 
             corner of each graph, you will see a toolbar with four buttons:")),
  
  # Instructions on how to use the plotly toolbar.
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
  downloadButton(outputId = "download_glossary_seven", 
                 label = "Download glossary", 
                 class = "glossaryseven"),
  tags$head(
    tags$style(".glossaryseven { background-color: #0072B2; } 
                   .glossaryseven { color: #FFFFFF; }")
  ),
  
  # Repeat the disclosure control note and add another note to clarify...
  # which psychiatric specialties were included in the readmission analysis.
  p(br(),
    tags$b("Notes:"),
    br(),
    tags$b(
      "1. Statistical disclosure control has been applied to protect 
          patient confidentiality by rounding percentage values to 1 decimal 
          place following their calculation. As a result, the figures 
          presented here may not be additive and may differ from previous 
          sources of information."
    ),
    br(), 
    tags$b(
      "2. Only readmissions in the following psychiatric specialties are 
          included in this page: G1 - General Psychiatry and G4 – Psychiatry of 
          Old Age."
    )),
  p(""),
  wellPanel(tags$style(".well { background-color: #FFFFFF; 
                           border: 0px solid #336699; }"),
            
            # Since this page contains two graphs and, therefore, two...
            # distinct sections, we can insert a title for each section to...
            # clarify what each of them visualises.
            # This is the title of the first section.
            h2("Health board comparison", 
               id = 'readm_HB_comparison_link'),
            br(),
            
            # Insert the filters for the first graph, i.e., the...
            # bar chart.
            # We are using three filters, arranged in three columns.
            # A fourth column contains the 'Download as .csv' button.
            column(3,
                   shinyWidgets::radioGroupButtons(
                     inputId = "readmissions_dataset",
                     label = "Select treatment specialty", 
                     choices = readm_dataset, 
                     selected = "Psychiatric",
                     justified = TRUE,
                     checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                     direction = "vertical"
                   )
            ),
            
            column(2,
                   shinyWidgets::pickerInput(
                     inputId = "readmissions_financial_year",
                     label = "Select financial year", 
                     choices = readm_financial_years, 
                     selected = "2022/2023"
                   )
            ),
            
            column(4,
                   shinyWidgets::pickerInput(
                     inputId = "readmissions_measure_type",
                     label = "Select measure",
                     choices = readm_measures, 
                     selected = "Percentage readmissions within 28 days"
                   )
            ),
            
            column(3, 
                   downloadButton(outputId = "first_download_readmissions", 
                                  label = "Download as .csv", 
                                  class = "myfirstreadmbutton", 
                                  style = "margin: 25px 10px 25px 10px"),
                   tags$head(
                     tags$style(".myfirstreadmbutton { background-color: 
                                    #0072B2; } 
                                    .myfirstreadmbutton { color: #FFFFFF; }")
                   )
            )),
  
  # Visualise the bar chart, the 'Show/hide table' button, and the table...
  # associated with the bar chart.
  mainPanel(width = 12, 
            plotlyOutput("readm_bar_chart", 
                         width = "1090px",
                         height = "600px"),
            HTML("<button data-toggle = 'collapse' href = '#read'
                     class = 'btn btn-primary' id = 'read_link'> 
                     <strong>Show/hide table</strong></button>"),
            HTML("<div id = 'read' class = 'collapse'>"),
            br(),
            dataTableOutput("first_readm_table"),
            HTML("</div>"),
            br(),
            br(),
            # Add a button that allows the user to go back to the top of the...
            # page.
            tags$a(href = '#readm_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            # As mentioned above, this tab contains two charts.
            # As such, we need to divide our page into two sections.
            # To do this, we use a horizontal line as a page break.
            hr(),
            # Header or title for the second section.
            h2("Trend over time", 
               id = 'readm_trend_link'),
            br(),
            
            # Insert the filters for the second graph, i.e., the...
            # line chart.
            # We have three filters, arranged in columns.
            # Add the 'Download as .csv' button too.
            column(3,
                   shinyWidgets::radioGroupButtons(
                     inputId = "readmissions_dataset_two",
                     label = "Select treatment specialty", 
                     choices = readm_dataset, 
                     selected = "Psychiatric",
                     justified = TRUE,
                     checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                     direction = "vertical"
                   )
            ),
            
            column(5, 
                   shinyWidgets::pickerInput(
                     "readmissions_location", 
                     label = "Select treatment location (up to four selections allowed)", 
                     choices = readm_locations, 
                     multiple = TRUE,
                     options = list(
                       "max-options" = 4,
                       `selected-text-format` = "count > 1"
                     ),
                     selected = "Scotland",
                     width = '100%'
                   ) 
            ),
            
            column(4,
                   shinyWidgets::pickerInput(
                     inputId = "readmissions_measure_type_two",
                     label = "Select measure",
                     choices = readm_measures, 
                     selected = "Percentage readmissions within 28 days"
                   )
            ),
            
            column(12, 
                   downloadButton(outputId = "second_download_readmissions", 
                                  label = "Download as .csv", 
                                  class = "mysecondreadmbutton", 
                                  style = "margin: 10px 0px 0px 0px"),
                   tags$head(
                     tags$style(".mysecondreadmbutton { background-color: 
                                    #0072B2; } 
                                    .mysecondreadmbutton { color: #FFFFFF; }")
                   )
            ),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            
            # Finally, visualise the line chart, the 'Show/hide table'... 
            # button, and the table associated with the line chart.
            plotlyOutput("readm_line_chart", 
                         width = "1090px",
                         height = "600px"),
            br(),
            br(),
            br(),
            HTML("<button data-toggle = 'collapse' href = '#read_two'
                     class = 'btn btn-primary' id = 'read_link_two'> 
                     <strong>Show/hide table</strong></button>"),
            HTML("<div id = 'read_two' class = 'collapse'>"),
            br(),
            dataTableOutput("second_readm_table"),
            HTML("</div>"),
            br(),
            br(),
            # You can also add a button that allows the user to go back...
            # to the top of the page.
            tags$a(href = '#readm_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            br(),
            br()
            
  ))