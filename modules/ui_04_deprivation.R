##* Deprivation user interface ----   

# Create the deprivation tab.

# Create a new tabPanel, stylise it, and give a title to the tab. 
tabPanel(
  "Deprivation", 
  icon = icon("bar-chart"), 
  style = "height: 95%; width: 95%; background-color: #FFFFFF; 
      border: 0px solid #FFFFFF;",
  h1("Deprivation", id = 'depr_top'),
  
  # Provide a description for the tab.
  p(HTML("This section contains two graphs, both revolving around deprivation. 
         The <a href = '#quintile_graph_link'> first graph </a> 
         shows inpatient activity broken down by deprivation quintile, whereas the 
         <a href = '#RII_trend_link'> second graph </a> displays the Relative 
         Index of Inequality (RII) as a trend over time.")),
  
  p(HTML("Use the filters to visualise the data you are interested in. It is 
         possible to select multiple health boards of residence in the first graph. 
         To view your data selection in a table, use the 
         <a href = '#RII_link'> 'Show/hide table' </a> 
         button under each graph. To download your data selection as a .csv file, 
         use the 'Download as .csv' button created for each graph. At the top-right 
         corner of each graph, you will see a toolbar with four buttons:")),
         
  
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
  downloadButton(outputId = "download_glossary_04", 
                 label = "Download glossary", 
                 class = "glossaryButton"),
  
  # Repeat the disclosure control note.
  p(br(),
    tags$b(
      "Note: Statistical disclosure control has been applied to protect 
          patient confidentiality by rounding values to the nearest 5 following 
          their calculation. Rates and RII figures were calculated using the rounded values and are displayed
          to 2 decimal places. As a result, the figures presented here may not 
          be additive and may differ from previous sources of information."
    )),
  p(""),
  wellPanel(tags$style(".well { background-color: #FFFFFF; 
                           border: 0px solid #336699; }"),
            
            # Since this page contains two graphs and, therefore, two...
            # distinct sections, we can insert a title for each section to...
            # clarify what each of them visualises.
            # This is the title of the first section.
            h2("Activity by deprivation quintile", 
               id = 'quintile_graph_link'),
            br(),
            
            # Insert the filters for the first chart, i.e., the...
            # bar chart.
            # We have five filters.
            # Our first two filters have already been created in...
            # the Server, using renderUI().
            # We just need to make them appear using the uiOutput() command.
            # We arrange our filters in columns.
            # The last column contains the 'Download as .csv' button.
            column(3,
                   shinyWidgets::radioGroupButtons(
                     inputId = "deprivation_dataset",
                     label = "Select treatment specialty", 
                     choices = depr_dataset, 
                     selected = "Psychiatric",
                     justified = TRUE,
                     checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                     direction = "vertical"
                   )
                   
            ),
            
            column(4,
                   uiOutput("deprivation_location_types")
            ),
            
            column(5,
                   uiOutput("deprivation_locations")
            ),
            
            column(4,
                   shinyWidgets::pickerInput(
                     inputId = "deprivation_financial_year",
                     label = "Select financial year", 
                     choices = depr_financial_years,
                     # Choose default option
                     selected = "2024/2025"
                   )
            ),
            
            column(5,
                   shinyWidgets::pickerInput(
                     inputId = "deprivation_measure_type",
                     label = "Select measure",
                     choices = depr_measures, 
                     selected = "Number of patients"
                   )
            ),
            
            column(4, 
                   downloadButton(outputId = "download_deprivation", 
                                  label = "Download as .csv", 
                                  class = "mydeprivationbutton"),
                   tags$head(
                     tags$style(".mydeprivationbutton { background-color: 
                                    #0072B2; } 
                                    .mydeprivationbutton { color: #FFFFFF; }")
                   )
            )
  ),
  
  # Visualise the deprivation bar chart, the 'Show/hide table' button, and...
  # the table associated with the bar chart.
  mainPanel(width = 12,
            # Add chart using custom function that includes spinner aesthetics
            phs_spinner("deprivation_bar_chart", "1090px", "450px"),
            br(),
            br(),
            HTML("<button data-toggle = 'collapse' href = '#depr'
                     class = 'btn btn-primary' id = 'depr_link'> 
                     <strong>Show/hide table</strong></button>"),
            HTML("<div id = 'depr' class = 'collapse'>"),
            br(),
            dataTableOutput("deprivation_table"),
            HTML("</div>"),
            br(),
            br(),
            # Add a button that allows the user to go back to the top of the...
            # page.
            tags$a(href = '#depr_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            
            # As mentioned above, this tab contains two charts.
            # As such, we need to divide our page into two sections.
            # To do this, we use a horizontal line as a page break.
            hr(),
            
            # Header or title for the second section.
            h2("Relative Index of Inequality (RII) time trend", 
               id = 'RII_trend_link'),
            br(),
            
            # Insert the filters for the second graph, i.e., the...
            # line chart.
            # We have two filters, which will be arranged in two separate...
            # columns.
            # There is another column which contains the 'Download as .csv'...
            # button.
            column(3,
                   shinyWidgets::radioGroupButtons(
                     inputId = "RII_datasets",
                     label = "Select treatment specialty", 
                     choices = RII_dataset, 
                     selected = "Psychiatric",
                     justified = TRUE,
                     checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                     direction = "vertical"
                   )
            ),
            
            column(3,
                   shinyWidgets::pickerInput(
                     inputId = "RII_measure_type",
                     label = "Select measure",
                     choices = RII_measures, 
                     selected = "Patients"
                   )
            ),
            
            column(3,
                   downloadButton(outputId = "download_RII", 
                                  label = "Download as .csv", 
                                  class = "myRIIbutton", 
                                  style = "margin: 25px 10px 25px 10px"),
                   tags$head(
                     tags$style(".myRIIbutton { background-color: #0072B2; } 
                                    .myRIIbutton { color: #FFFFFF; }")
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
            plotlyOutput("RII_line_chart", 
                         width = "1090px",
                         height = "600px"),
            
            # RII explanation text
            p("The Relative Index of Inequality (RII) describes the gradient of 
              health observed across the deprivation scale, relative to the mean 
              health of the whole population. The RII is the Slope Index of 
              Inequality (SII) divided by the population mean rate. It is similar 
              to the range, but takes into consideration the values for all of 
              the deprivation quintiles and the population size of each quintile."),
            # RII links
            p("A more detailed explanation can be found on the ",
              tags$a("Scottish Public Health Observatory Website",
                     href = 'https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/#siirii',
                     target = "_blank"),
              " and the ",
              tags$a("Scottish Government Website.",
                     href = 'https://www.gov.scot/publications/long-term-monitoring-health-inequalities-march-2022-report/pages/19/',
                     target = "_blank")
              ),
            br(),
            # Data underneath graph toggle button
            HTML("<button data-toggle = 'collapse' href = '#RII'
                     class = 'btn btn-primary' id = 'RII_link'> 
                     <strong>Show/hide table</strong></button>"),
            HTML("<div id = 'RII' class = 'collapse'>"),
            br(),
            dataTableOutput("RII_table"),
            HTML("</div>"),
            br(),
            br(),
            # You can also add a button that allows the user to go back to...
            # the top of the page.
            tags$a(href = '#depr_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            
            br(),
            br()
            
  ))