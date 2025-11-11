##* Trends in diagnoses user interface ----   

# Create the diagnoses tab.

# Create a new tabPanel, stylise it, and give a title to the tab.
tabPanel(
  "Learning Disabilities", 
  icon = icon("line-chart"), 
  style = "height: 95%; width: 95%; background-color: #FFFFFF; 
      border: 0px solid #FFFFFF;",
  h1("Learning Disabilities", id = 'LD_top'),
  
  # Provide a description for the tab.
  p("This section allows you to see an overview of changes in the number of 
    Discharges, Patients and Stays in the Learning Disability specialty since 1997/1998.
    This analysis covers patients being treated in the Learning Disability 
    specialty of Pscyhiatric Inpatient facilities only. The data for this 
    analysis are extracted from Scottish Morbidity Record 04 (SMR04) and 
    filtered to the Learning Disability Specialty - G5."),
  
  # Learning Disabilities Excel workbook download
  p(tags$ul(
    tags$li("Further breakdowns of data for the Learning Disability specialty are
           available for download here:",
           br(),
           icon("database"),
           downloadLink(outputId = "LD_supplement",
                        label = "Download additional Learning Disability data breakdowns.")
          ))),
  
  p(HTML("It should be noted that an increasing amount of care for Learning 
         Disabilities takes place in the community, for example through 
         specialist community teams and general practice.")),
  
  p(HTML("Use the filters to visualise the data you are interested in. 
         To view your data selection in a table, use the 
         <a href = '#LD_link'> 'Show/hide table' </a> 
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
  downloadButton(outputId = "download_glossary_07", 
                 label = "Download glossary", 
                 class = "glossaryButton"),
  
  # Repeat the standard note regarding disclosure control from the...
  # Introduction tab. Add another note that explains the difference between...
  # the psychiatric data presented in this tab v. the psychiatric data presented...
  # elsewhere in the publication.
  p(br(),
    tags$b("Notes:"),
    br(),
    tags$b(
      tags$li("1. Unlike other analyses in this release, statistical disclosure control 
           has not been applied to this analysis as resultant data is non disclosive.")
      # ,
      # tags$li("2. Further breakdowns of data for the Learning Disability specialty are
      #      available for download here:",
      #      downloadLink(outputId = "LD_supplement",
      #                   label = "Download additional Learning Disability data breakdowns."), 
      #      icon("database")),
    )),
            
  p(""),
  wellPanel(tags$style(".well { background-color: #FFFFFF; 
                           border: 0px solid #336699; }"),
            
            # Insert the filters.
            column(4,
                   uiOutput("LD_measure_output")
            ),
            
            column(4,
                   downloadButton(outputId = "download_LD", 
                                  label = "Download as .csv", 
                                  class = "mytimetrendbutton"),
                   tags$head(
                     tags$style(".mytimetrendbutton { background-color: 
                                    #0072B2; } 
                                    .mytimetrendbutton { color: #FFFFFF; }")
                   )
            )
  ),
  
  # In the main panel of the LD tab, insert the LD graph.
  #  the 'Show/hide table' button, and the LD table. 
  mainPanel(width = 12,
            plotlyOutput("LD_plot", 
                         width = "1090px",
                         height = "600px"),
            br(),
            br(),
            HTML("<button data-toggle = 'collapse' href = '#LD'
                     class = 'btn btn-primary' id = 'LD_link'> 
                     <strong> Show/hide table </strong></button>"),
            HTML("<div id = 'LD' class = 'collapse'>"),
            br(),
            dataTableOutput("LD_table"),
            HTML("</div>"),
            br(),
            br(),
            # Finally, add a button that allows the user to go back to the...
            # top of the page.
            tags$a(href = '#LD_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            br(),
            br())
)