##* Cross-boundary flow user interface ----   

# Create the cross-boundary flow tab.

# Create a new tabPanel, stylise it, and give a title to the tab. 
tabPanel(
  "Cross-boundary flow", 
  icon = icon("exchange"),  
  style = "height: 95%; width: 95%; background-color: #FFFFFF; 
      border: 0px solid #FFFFFF;",
  h1("Cross-boundary flow", id = 'cbf_top'),
  
  # Provide a description for the tab.
  p(HTML("The following diagram shows you how many patients living in the 
             selected NHS board of residence were treated outside their board. 
             Use the filters to visualise the data you are interested in. To 
             view your data selection in a table, use the <a href = '#flow_link'> 
             'Show/hide table' </a> button at the bottom of the page. To 
             download your data selection as a .csv file, use the 'Download as .csv'
             button next to the filters.")),
  
  # Download button for the glossary.
  p("You can also download our glossary of commonly used terms in
        mental health care, which has been created to help you
        understand the information visualised in the explorer:"),
  downloadButton(outputId = "download_glossary_six", 
                 label = "Download glossary", 
                 class = "glossarysix"),
  tags$head(
    tags$style(".glossarysix { background-color: #0072B2; } 
                   .glossarysix { color: #FFFFFF; }")
  ),
  
  # Repeat the disclosure control note.
  p(br(),
    tags$b(
      "Note: Statistical disclosure control has been applied to protect 
          patient confidentiality by rounding all values to the nearest 5
          following their calculation. As a result, the figures 
          presented here may not be additive and may differ from previous 
          sources of information."
    )),
  p(""),
  wellPanel(tags$style(".well { background-color: #FFFFFF; 
                           border: 0px solid #336699; }"),
            
            # Insert the filters.
            # We are using three filters, arranged in two columns.
            # A third column contains the 'Download as .csv' button.
            column(3,
                   shinyWidgets::radioGroupButtons(
                     inputId = "flow_dataset",
                     label = "Select treatment specialty", 
                     choices = fl_dataset, 
                     selected = "Psychiatric",
                     justified = TRUE,
                     checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                     direction = "vertical"
                   )
            ),
            
            column(4, 
                   shinyWidgets::pickerInput(
                     inputId = "flow_board_of_residence",
                     label = "Select health board of residence",
                     choices = fl_boards_of_residence, 
                     selected = "NHS Ayrshire & Arran"
                   ),
                   
                   shinyWidgets::pickerInput(
                     inputId = "flow_financial_year",
                     label = "Select financial year", 
                     choices = fl_financial_years, 
                     selected = "2022/2023"
                   )
            ),
            
            column(3,
                   downloadButton(outputId = "download_flow", 
                                  label = "Download as .csv", 
                                  class = "myflowbutton",
                                  style = "margin: 25px 10px 25px 10px"),
                   tags$head(
                     tags$style(".myflowbutton { background-color: 
                                    #0072B2; } 
                                    .myflowbutton { color: #FFFFFF; }")
                   )
            )
  ),
  
  # In the main panel, we insert the cross-boundary flow sentence, the...
  # diagram, the 'Show/hide table' button, and the cross-boundary flow table.
  mainPanel(width = 12,
            br(),
            column(12, 
                   htmlOutput("flow_text") 
            ),
            br(),
            br(),
            br(),
            htmlOutput("flow_graph"),
            br(),
            br(),
            HTML("<button data-toggle = 'collapse' href = '#flow'
                     class = 'btn btn-primary' id = 'flow_link'> 
                     <strong>Show/hide table</strong></button>"),
            HTML("<div id = 'flow' class = 'collapse'>"),
            br(),
            dataTableOutput("flow_table"),
            HTML("</div>"),
            br(),
            br(),
            # Finally, add a button that allows the user to go back to the...
            # top of the page.
            tags$a(href = '#cbf_top', 
                   icon("circle-arrow-up", 
                        lib = "glyphicon"), 
                   "Back to top"),
            
            br(),
            br())
)