#Name: Psychiatric inpatient activity data explorer
#Author: Nikos Alexandrou
#Modified: 17/09/2018
#Type: Data visualisation
#Written on: RStudio
#Written for: R version 3.2.3 
#Output: Shiny application
#Approximate run time: < 1 minute
#Description: This syntax creates a Shiny application that allows the... 
#user to visualise psychiatric inpatient data in a variety of ways.

#Moving on to the User Interface (UI) side of things.
#Create the fluidPage that will house the data explorer.

fluidPage(
  style = "width: 100%; height: 100%; max-width: 1200px;", 
  tags$head( 
    tags$style(
      type = "text/css", 
      ".shiny-output-error { visibility: hidden; }", 
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    #The following chunk of code does three things:
    # 1. Paints the ribbon that contains the tab headers white.
    # 2. Highlights the header of the active tab in blue.
    # 3. Sets the font size for the sentence that appears above the...
    # cross-boundary flow diagram.
    
    tags$style(HTML(".tabbable > .nav > li > a { 
                    color: #000000; 
                    }
                    
                    .tabbable > .nav > li[class = active] > a {
                    background-color: #0072B2;
                    color: #FFFFFF;
                    }
                    
                    #flow_text {
                    font-size: 15px;
                    }"))
  ),
  
  #We are going to divide our UI into discrete sections, called tab panels.
  #To do this, we need the layout "tabsetPanel()".
  
  tabsetPanel(
    id = "Panels", 
    
    ##############################################.             
    ############## Introduction tab ----   
    ##############################################. 
    
    #We begin with an introduction tab, where we introduce the explorer and... 
    #its purpose.
    #We also insert the download button for the glossary.
    
    tabPanel(
      "Introduction", 
      icon = icon("info-circle"), 
      style = "float: top; height: 95%; width: 95%;
      background-color: #FFFFFF; border: 0px solid #FFFFFF;",
      column(2,
             h3("Data explorer")
      ),
      column(8, 
             p(
               br(), 
               "The explorer allows you to visualise psychiatric inpatient data
               in a variety of ways. Within each of the following five 
               sections, there are filters that let you select the data you 
               are interested in:"
             ),
             tags$ul( 
               tags$li(
                 tags$b(
                   actionLink("link_to_tabpanel_time_trend", "Time trend")
                 ), 
                 icon("line-chart"), 
                 " - shows data on specific mental health conditions over 
                 time."
               ),
               tags$li(
                 tags$b(
                   actionLink("link_to_tabpanel_age_sex", "Age/sex")
                 ), 
                 icon("child"), 
                 " - shows the age and sex distribution of the data."
               ),
               tags$li(
                 tags$b(
                   actionLink("link_to_tabpanel_deprivation", "Deprivation")
                 ), 
                 icon("bar-chart"),
                 " - shows activity across different levels of deprivation."
               ),
               tags$li(
                 tags$b(
                   actionLink("link_to_tabpanel_flow", "Cross-boundary flow")
                 ),
                 icon("exchange"),
                 " - shows the relationship between where patients live and 
                 where they are treated."
               ),
               tags$li(
                 tags$b(
                   actionLink("link_to_tabpanel_table", "Table")
                 ),
                 icon("table"),
                 " - allows you to view the data in a table."
               )
               ),
             
             p(
               "When using the data explorer, please take the following 
               factors into consideration:"
             ),
             tags$ul( 
               tags$li(
                 "The explorer visualises information recorded in the Scottish 
                 Morbidity Record 04 (SMR04) dataset, which deals exclusively 
                 with psychiatric inpatient activity. As such, information on 
                 non-psychiatric specialties is absent from the explorer."
               ),
               tags$li(
                 "SMR04 data completeness varies from year to year. As a result,
                 data is provisional and subject to change. For more 
                 information, visit the ", 
                 tags$a(
                   href = "http://www.isdscotland.org/products-and-Services/Data-Support-and-Monitoring/SMR-Completeness/", 
                   "SMR Completeness"
                 ),
                 " webpage, which contains an Excel file with completeness
                 estimates for all SMR datasets."
               ), 
               tags$li(
                 "Statistical disclosure control has been applied to protect 
                 patient confidentiality. Therefore, the figures presented in 
                 this explorer may not be additive and may differ to previous 
                 sources of information. For more information, please refer to 
                 the ",
                 tags$a(
                   href = "http://www.isdscotland.org/About-ISD/Confidentiality/disclosure_protocol_v3.pdf", 
                   "NSS Statistical Disclosure Control Protocol."
                 ), 
                 ""
               )
             ),
             
             p(
               "To help you understand the information visualised in the 
               explorer, we have created a glossary of commonly used terms in
               psychiatric inpatient care. 
               Click the button below to download the glossary:"
             ),
             
             downloadButton(outputId = "download_glossary_one", 
                            label = "Download glossary", 
                            class = "glossaryone"),
             tags$head(
               tags$style(".glossaryone { background-color: #0072B2; } 
                          .glossaryone { color: #FFFFFF; }")
               ),
             
             p(
               br(),
               "If you have any trouble using the explorer or have further 
               questions relating to the data, please contact us at:",
               tags$b(
                 tags$a(href = "mailto:nss.isdtransformingpublishing@nhs.net", 
                        "nss.isdtransformingpublishing@nhs.net.")
               ),
               br(),
               br(),
               br(),
               br()
             )
    )
  ), 
  
  
  ##############################################.             
  ############## Time trend tab ----   
  ##############################################.  
  
  #Create a separate tab for the time trend data.
  #Insert the description as well as the download button for the glossary.
  
  tabPanel(
    "Time trend", 
    icon = icon("line-chart"), 
    style = "height: 95%; width: 95%; background-color: #FFFFFF; 
    border: 0px solid #FFFFFF;",
    h3("Time trend"),
    p(HTML("This section allows you to see changes in mental health 
           conditions over time. The period covered is financial years 
           1997/1998 - 2017/2018. Use the filters to visualise the data you 
           are interested in. You can visualise multiple health boards of 
           treatment at the same time. To view your data selection in a table,
           use the <a href = '#timetrends_link'> 'Show/hide table' </a> 
           button at the bottom of the page. To download your data selection 
           as a CSV file, use the 'Download data' button under the filters. 
           At the top-right corner of the graph, you will see a toolbar with 
           four buttons:")),
    
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
    
    p(
      "You can also download our glossary of commonly used terms in
      psychiatric inpatient care, which has been created to help you 
      understand the information visualised in the explorer:"
    ),
    
    downloadButton(outputId = "download_glossary_two", 
                   label = "Download glossary", 
                   class = "glossarytwo"),
    tags$head(
      tags$style(".glossarytwo { background-color: #0072B2; } 
                 .glossarytwo { color: #FFFFFF; }")
      ),
    
    p(
      br(),
      tags$b(
        "Note: Statistical disclosure control has been applied to protect 
        patient confidentiality. Therefore, the figures presented here 
        may not be additive and may differ to previous 
        sources of information."
      )
      ),
    
    p(""),
    
    wellPanel(tags$style(".well { background-color: #FFFFFF; 
                         border: 0px solid #336699; }"),
              
              #Insert the reactive filters.
              #We have four filters in total.
              #Two of our filters have already been created in... 
              #the Server syntax, using renderUI().
              #We just need to make them appear using the uiOutput() command.
              #We arrange our filters in two columns. 
              #The first column contains the 'Download data' button as well.
              
              column(6,
                     
                     uiOutput("time_trend_location_types"),
                     
                     shinyWidgets::pickerInput(
                       inputId = "time_trend_diagnoses",
                       label = "Select diagnosis grouping",
                       choices = tt_diagnoses,
                       selected = 
                         "Disorders of adult behaviour and personality"
                     ),
                     
                     downloadButton(outputId = "download_time_trend", 
                                    label = "Download data", 
                                    class = "mytimetrendbutton"),
                     
                     tags$head(
                       tags$style(".mytimetrendbutton { background-color: 
                                  #0072B2; } 
                                  .mytimetrendbutton { color: #FFFFFF; }")
                       )
                     
                     ),
              
              column(6, 
                     
                     uiOutput("time_trend_locations"),
                     
                     shinyWidgets::pickerInput(
                       inputId = "time_trend_measure_type",
                       label = "Select measure",
                       choices = tt_measures, 
                       selected = "Number of patients"
                     )
                     
              )
    ),
    
    #In the main panel of the time trend tab, insert the time trend line...
    #chart, the 'Show/hide table' button, and the time trend table. 
    
    mainPanel(width = 12,
              plotlyOutput("time_trend_plot", 
                           width = "1090px",
                           height = "600px"),
              br(),
              br(),
              HTML("<button data-toggle = 'collapse' href = '#timetrends'
                   class = 'btn btn-primary' id = 'timetrends_link'> 
                   <strong> Show/hide table </strong></button>"),
              HTML("<div id = 'timetrends' class = 'collapse'>"),
              br(),
              dataTableOutput("time_trend_table"),
              HTML("</div>"),
              br(),
              br(),
              br(),
              br())
    ),
  
  
  ##############################################.             
  ############## Age/sex tab ----   
  ##############################################.  
  
  #Create a tab for the age/sex data.
  #Provide a description and insert the download button for the glossary. 
  
  tabPanel(
    "Age/sex", 
    icon = icon("child"), 
    style = "height: 95%; width: 95%; background-color: #FFFFFF; 
    border: 0px solid #FFFFFF;",
    h3("Age/sex"),
    p(HTML("This section allows you to explore the age and sex distribution 
           of the data. Use the filters to visualise the data you are 
           interested in. To view your data selection in a table, use the 
           <a href = '#age_and_sex_link'> 'Show/hide table' </a> button at 
           the bottom of the page. To download your data selection as a CSV 
           file, use the 'Download data' button under the filters. At the 
           top-right corner of the graph, you will see a toolbar with four 
           buttons:")),
    
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
    
    p(
      "You can also download our glossary of commonly used terms in
      psychiatric inpatient care, which has been created to help you
      understand the information visualised in the explorer:"
    ),
    
    downloadButton(outputId = "download_glossary_three", 
                   label = "Download glossary", 
                   class = "glossarythree"),
    tags$head(
      tags$style(".glossarythree { background-color: #0072B2; } 
                 .glossarythree { color: #FFFFFF; }")
      ),
    
    p(
      br(),
      tags$b(
        "Note: Statistical disclosure control has been applied to protect 
        patient confidentiality. Therefore, the figures presented here 
        may not be additive and may differ to previous 
        sources of information."
      )
      ),
    
    p(""),
    
    wellPanel(tags$style(".well { background-color: #FFFFFF; 
                         border: 0px solid #336699; }"),
              
              #Insert the reactive filters.                                                
              #We have four filters in total.
              #Two of our filters have already been created in...
              #the Server syntax, using renderUI().
              #We just need to make them appear using the uiOutput() command.
              #We arrange our filters in two columns.
              #The first column contains the 'Download data' button as well.
              
              column(6,
                     
                     uiOutput("age_sex_location_types"),
                     
                     shinyWidgets::pickerInput(
                       inputId = "age_sex_financial_year",
                       label = "Select financial year", 
                       choices = as_financial_years,
                       selected = "2017/2018"
                     ),
                     
                     downloadButton(outputId = "download_age_sex", 
                                    label = "Download data", 
                                    class = "myagesexbutton"),
                     tags$head(
                       tags$style(".myagesexbutton { background-color: 
                                  #0072B2; } 
                                  .myagesexbutton { color: #FFFFFF; }")
                       )
                     
                     ),
              
              column(6,
                     
                     uiOutput("age_sex_locations"),
                     
                     shinyWidgets::pickerInput(
                       inputId = "age_sex_measure_type",
                       label = "Select measure",
                       choices = as_measures, 
                       selected = "Number of patients"
                     )
                     
              )
    ),
    
    #In the main panel, we insert the age/sex pyramid, the 'Show/hide... 
    #table' button, and the age/sex table.
    
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
              br(),
              br())
    ),
  
  
  ##############################################.             
  ############## Deprivation tab ----   
  ##############################################.  
  
  #Create a tab for the deprivation data.
  #Provide a description and insert the download button for the glossary. 
  
  tabPanel(
    "Deprivation", 
    icon = icon("bar-chart"), 
    style = "height: 95%; width: 95%; background-color: #FFFFFF; 
    border: 0px solid #FFFFFF;",
    h3("Deprivation"),
    p(HTML("This section breaks the data down by different levels of 
           deprivation. Use the filters to visualise the data you are 
           interested in. You can visualise multiple health boards of 
           residence at the same time. To view your data selection in a 
           table, use the <a href = '#depr_link'> 'Show/hide table' </a> 
           button at the bottom of the page. To download your data selection 
           as a CSV file, use the 'Download data' button under the filters. 
           At the top-right corner of the graph, you will see a toolbar with 
           four buttons:")),
    
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
    
    p(
      "You can also download our glossary of commonly used terms in
      psychiatric inpatient care, which has been created to help you
      understand the information visualised in the explorer:"
    ),
    
    downloadButton(outputId = "download_glossary_four", 
                   label = "Download glossary", 
                   class = "glossaryfour"),
    tags$head(
      tags$style(".glossaryfour { background-color: #0072B2; } 
                 .glossaryfour { color: #FFFFFF; }")
      ),
    
    p(
      br(),
      tags$b(
        "Note: Statistical disclosure control has been applied to protect 
        patient confidentiality. Therefore, the figures presented here 
        may not be additive and may differ to previous 
        sources of information."
      )
      ),
    
    p(""),
    
    wellPanel(tags$style(".well { background-color: #FFFFFF; 
                         border: 0px solid #336699; }"),
              
              #Insert the reactive filters.                                             
              #We have four filters in total.
              #Our first two filters have already been created in...
              #the Server, using renderUI().
              #We just need to make them appear using the uiOutput() command.
              #We arrange our filters in two columns.
              #The first column contains the 'Download data' button as well.
              
              column(6,
                     
                     uiOutput("deprivation_location_types"),
                     
                     shinyWidgets::pickerInput(
                       inputId = "deprivation_financial_year",
                       label = "Select financial year", 
                       choices = depr_financial_years, 
                       selected = "2017/2018"
                     ),
                     
                     downloadButton(outputId = "download_deprivation", 
                                    label = "Download data", 
                                    class = "mydeprivationbutton"),
                     tags$head(
                       tags$style(".mydeprivationbutton { background-color: 
                                  #0072B2; } 
                                  .mydeprivationbutton { color: #FFFFFF; }")
                       )
                     
                     ),
              
              column(6,
                     
                     uiOutput("deprivation_locations"),
                     
                     shinyWidgets::pickerInput(
                       inputId = "deprivation_measure_type",
                       label = "Select measure",
                       choices = depr_measures, 
                       selected = "Number of patients"
                     )
                     
              )
    ),
    
    #In the main panel, we insert the deprivation bar chart, the 'Show/...
    #hide table' button, and the deprivation table.
    
    mainPanel(width = 12,
              plotlyOutput("deprivation_bar_chart", 
                           width = "1090px",
                           height = "450px"),
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
              br(),
              br())
    ),
  
  
  ##############################################.             
  ############## Cross-boundary flow tab ----   
  ##############################################.  
  
  #Create a tab for the cross-boundary flow data.
  #Provide a description and insert the download button for the glossary. 
  
  tabPanel(
    "Cross-boundary flow", 
    icon = icon("exchange"),  
    style = "height: 95%; width: 95%; background-color: #FFFFFF; 
    border: 0px solid #FFFFFF;",
    h3("Cross-boundary flow"),
    p(HTML("The following diagram shows you how many patients living in the 
           selected NHS board of residence were treated outside their board. 
           Use the filters to visualise the data you are interested in. To 
           view your data selection in a table, use the <a href = '#flow_link'> 
           'Show/hide table' </a> button at the bottom of the page. To 
           download your data selection as a CSV file, use the 'Download data'
           button under the filters.")),
    
    p(
      "You can also download our glossary of commonly used terms in
      psychiatric inpatient care, which has been created to help you
      understand the information visualised in the explorer:"
    ),
    
    downloadButton(outputId = "download_glossary_five", 
                   label = "Download glossary", 
                   class = "glossaryfive"),
    tags$head(
      tags$style(".glossaryfive { background-color: #0072B2; } 
                 .glossaryfive { color: #FFFFFF; }")
      ),
    
    p(
      br(),
      tags$b(
        "Note: Statistical disclosure control has been applied to protect 
        patient confidentiality. Therefore, the figures presented here 
        may not be additive and may differ to previous 
        sources of information."
      )
      ),
    
    p(""),
    
    wellPanel(tags$style(".well { background-color: #FFFFFF; 
                         border: 0px solid #336699; }"),
              
              #Insert the reactive filters.                                             
              #We are using two filters, arranged in two columns.
              #The first column contains the 'Download data' button as well.
              
              column(4, 
                     
                     shinyWidgets::pickerInput(
                       inputId = "flow_board_of_residence",
                       label = "Select health board of residence",
                       choices = fl_boards_of_residence, 
                       selected = "NHS Ayrshire & Arran"
                     ),
                     
                     downloadButton(outputId = "download_flow", 
                                    label = "Download data", 
                                    class = "myflowbutton"),
                     tags$head(
                       tags$style(".myflowbutton { background-color: 
                                  #0072B2; } 
                                  .myflowbutton { color: #FFFFFF; }")
                       )
                     
                     ),
              
              column(2,
                     
                     shinyWidgets::pickerInput(
                       inputId = "flow_financial_year",
                       label = "Select financial year", 
                       choices = fl_financial_years, 
                       selected = "2017/2018"
                     )
                     
              )
    ),
    
    #In the main panel, we insert the cross-boundary flow chart, the 'Show/...
    #hide table' button, and the cross-boundary flow table.
    
    mainPanel(width = 12,
              br(),
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
              br(),
              br())
    ),
  
  ##############################################.              
  ############## Table tab ----    
  ##############################################. 
  
  #Create a tab for the table data.
  #Insert the description for this tab and a download button for the glossary.                             
  
  tabPanel(
    "Table", 
    icon = icon("table"), 
    style = "float: top; height: 95%; width: 95%; background-color: #FFFFFF; 
    border: 0px solid #FFFFFF;", 
    h3("Table"), 
    p(
      "This section allows you to view the data in table format. Use the  
      filter below to visualise the dataset you are interested in (the list 
      includes datasets used in the 'Data trends' page). You can then use the 
      filters situated below the column names of the table to modify the table
      as you please. To download your data selection as a CSV file, use the 
      'Download data' button."
    ),
    
    p(
      "You can also download our glossary of commonly used terms in
      psychiatric inpatient care, which has been created to help you
      understand the information visualised in the explorer:"
    ),
    
    downloadButton(outputId = "download_glossary_six", 
                   label = "Download glossary", 
                   class = "glossarysix"),
    tags$head(
      tags$style(".glossarysix { background-color: #0072B2; } 
                 .glossarysix { color: #FFFFFF; }")
      ),
    
    p(
      br(),
      tags$b(
        "Note: Statistical disclosure control has been applied to protect 
        patient confidentiality. Therefore, the figures presented here 
        may not be additive and may differ to previous 
        sources of information."
      )
      ),
    
    p(""),
    
    wellPanel(tags$style(".well { background-color: #FFFFFF; 
                         border: 0px solid #336699; }"),
              
              #We are only using one filter here, which contains the...
              #names of the files.
              
              column(6,
                     
                     shinyWidgets::pickerInput(
                       inputId = "table_filenames", 
                       label = "Select data file",  
                       choices = c("Time trend (Data explorer)", 
                                   "Age/sex (Data explorer)", 
                                   "Deprivation (Data explorer)", 
                                   "Cross-boundary flow (Data explorer)",
                                   "Activity by hospital (Data trends)",
                                   "Length of stay (Data trends)"), 
                       width = "95%"
                     )
                     
              ), 
              
              #We also insert the 'Download data' button.
              
              column(4,
                     
                     downloadButton(outputId = 'download_table', 
                                    label = 'Download data', 
                                    class = "mytablebutton", 
                                    style = "margin: 25px 10px 25px 10px")
                     
              )
    ),
    
    tags$head(
      tags$style(".mytablebutton { background-color: #0072B2; } 
                    .mytablebutton { color: #FFFFFF; }")
    ),
    
    
    #Finally, insert the actual table.
    
    mainPanel(width = 12, 
              dataTableOutput("table_tab")) 
    
      )
    )
  )

#We are finished with the UI syntax.
