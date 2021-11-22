#Name: Data explorer
#Author: Nikos Alexandrou
#Modified: 10/11/2021
#Type: Data visualisation
#Written on: RStudio
#Written for: R version 3.6.1 
#Output: Shiny application
#Approximate run time: < 1 minute
#Description: This syntax creates a Shiny application that allows the... 
#user to visualise mental health data in a variety of ways.

### SECTION 3: USER INTERFACE ----


#Moving on to the User Interface (UI) side of things.

#Create the fluidPage that will house the data explorer.
fluidPage(
  style = "width: 100%; height: 100%; max-width: 1200px;", 
  tags$head( 
    tags$style(
      type = "text/css",
      
      #Prevent error messages from popping up on the interface.
      ".shiny-output-error { visibility: hidden; }", 
      ".shiny-output-error:before { visibility: hidden; }"
      
    ),
    
    #The following chunk of code does three things:
    # 1. Paints the ribbon that contains the tab headers white.
    # 2. Highlights the header of the active tab in blue.
    # 3. Sets the font size for the sentence that appears above the...
    #cross-boundary flow diagram.
    tags$style(HTML(".tabbable > .nav > li > a { 
                      color: #000000; 
                      }
                      .tabbable > .nav > li[class = active] > a {
                      color: #FFFFFF;
                      background-color: #0072B2;
                      }
                      #flow_text {
                      font-size: 15px;
                      }"))
  ),
  
  #The following line of code sets the properties of the horizontal lines...
  #we will be inserting below as page breaks.
  tags$head(tags$style(HTML("hr { border: 1px solid #000000; }"))),
  
  #We are going to divide our UI into discrete sections, called tab panels.
  #To do this, we need the layout "tabsetPanel()".
  tabsetPanel(
    id = "Panels", 
    
    ##* Introduction user interface ----   
    
    #We begin with an introduction tab, where we introduce the explorer and... 
    #its purpose.
    
    #Create a new tabPanel, stylise it, and give it a title. 
    tabPanel(
      "Introduction", 
      icon = icon("info-circle"), 
      style = "float: top; height: 95%; width: 95%;
        background-color: #FFFFFF; border: 0px solid #FFFFFF;",
      h1("Introduction"),
      
      #Explain to the user what each tab visualises.
      #Each tab title is a hyperlink, linking to its respective tab.
      p("The explorer allows you to visualise mental health inpatient 
          data in a variety of ways. Within each of the following seven 
          sections, there are filters that let you select the data you are 
          interested in:"),
      tags$ul( 
        tags$li(
          tags$b(
            actionLink("link_to_trends_in_diagnoses_tab", "Trends in diagnoses")
          ), 
          icon("line-chart"), 
          " - shows changes in mental health conditions over 
            time."
        ),
        tags$li(
          tags$b(
            actionLink("link_to_geography_tab", "Geography")
          ), 
          icon("globe"), 
          " - shows activity broken down by council area of residence."
        ),
        tags$li(
          tags$b(
            actionLink("link_to_age_sex_tab", "Age/sex")
          ), 
          icon("child"), 
          " - shows the age and sex distribution of the data."
        ),
        tags$li(
          tags$b(
            actionLink("link_to_deprivation_tab", "Deprivation")
          ), 
          icon("bar-chart"),
          " - shows activity across different levels of deprivation as 
            well as the Relative Index of Inequality as a time trend."
        ),
        tags$li(
          tags$b(
            actionLink("link_to_cross_boundary_flow_tab", "Cross-boundary flow")
          ),
          icon("exchange"),
          " - shows the relationship between where patients live and 
            where they are treated."
        ),
        tags$li(
          tags$b(
            actionLink("link_to_readmissions_tab", "Readmissions")
          ),
          icon("bed"),
          " - shows information on readmissions within 28 and 133 days."
        ),
        tags$li(
          tags$b(
            actionLink("link_to_table_tab", "Table")
          ),
          icon("table"),
          " - allows you to view the data in a table."
        )
      ),
      
      #Insert the standard notes.
      p("When using the data explorer, please take the following 
          factors into consideration:"),
      tags$ul( 
        tags$li(
          "The explorer visualises information recorded in the Scottish 
            Morbidity Record 01 (SMR01) and Scottish Morbidity Record 04 
            (SMR04) datasets. SMR04 includes all inpatients and day cases 
            admitted to and discharged from psychiatric specialties. SMR01 
            includes all inpatients and day cases discharged from non-obstetric 
            and non-psychiatric specialties. For a complete list of 
            specialties, open the ",
          tags$a(
            href = "https://www.ndc.scot.nhs.uk/docs/2020-04-23%20Specialty-Codes-and-Values.xlsx", 
            "Specialty Codes and Descriptions document"
          ), 
          " and consult the columns SMR01 and SMR04. Please note that
            this data release excludes activity in the Learning Disability 
            specialty. Additionally, SMR01 has been restricted to diagnoses 
            of 'Mental and behavioural disorders' only, which were identified 
            using codes F00-F99 from the International Classification of Diseases, 
            Tenth Revision."
        ),
        tags$li(
          "SMR01 and SMR04 data completeness varies from year to year. 
            As a result, data is provisional and subject to change. For 
            more information, visit the ", 
          tags$a(
            href = "https://beta.isdscotland.org/products-and-services/data-management-hospital-activity/smr-completeness/", 
            "SMR Completeness"
          ),
          " webpage, which contains an Excel file with completeness
            estimates for all SMR datasets. For this release, data from the State 
            Hospital (SMR04) was only available up until 31 December 2020 at the
            time of data extraction. This was due to systems issues at the State
            Hospital. The average number of State Hospital discharges since 
            1997/1998 is 140 per year and as such the missing fourth quarter for 
            2020/2021 should not affect the data significantly."
        ), 
        tags$li(
          "Statistical disclosure control has been applied to protect 
            patient confidentiality. As a result, the figures presented in 
            this explorer may not be additive and may differ from previous 
            sources of information. For more information, please refer to 
            the ",
          tags$a(
            href = "https://publichealthscotland.scot/publications/statistical-disclosure-protocol/statistical-disclosure-protocol/", 
            "PHS Statistical Disclosure Control Protocol."
          ), 
          ""
        )
      ),
      
      #Provide a download button for the glossary.
      p("To help you understand the information visualised in the 
          explorer, we have created a glossary of commonly used terms in
          mental health care. Click the button below to download the glossary:"),
      downloadButton(outputId = "download_glossary_one", 
                     label = "Download glossary", 
                     class = "glossaryone"),
      tags$head(
        tags$style(".glossaryone { background-color: #0072B2; } 
                     .glossaryone { color: #FFFFFF; }")
      ),
      
      #Provide contact details for the team.
      p(br(),
        "If you have any trouble using the explorer or have further 
          questions about the data, please contact us at:",
        tags$b(
          tags$a(href = "mailto:phs.mentalhealth@phs.scot", 
                 "phs.mentalhealth@phs.scot.")),
        br(),
        br(),
        br(),
        br()
      )
    ), 
    
    ##* Trends in diagnoses user interface ----   
    
    #Create the diagnoses tab.
    
    #Create a new tabPanel, stylise it, and give a title to the tab.
    tabPanel(
      "Trends in diagnoses", 
      icon = icon("line-chart"), 
      style = "height: 95%; width: 95%; background-color: #FFFFFF; 
        border: 0px solid #FFFFFF;",
      h1("Trends in diagnoses", id = 'diagnoses_top'),
      
      #Provide a description for the tab.
      p(HTML("This section allows you to see changes in mental health 
               conditions since 1997/1998. Use the filters to visualise the data you 
               are interested in. You can visualise multiple health boards of 
               treatment at the same time. To view your data selection in a table,
               use the <a href = '#diagnoses_link'> 'Show/hide table' </a> 
               button at the bottom of the page. To download your data selection 
               as a .csv file, use the 'Download as .csv' button under the filters. 
               At the top-right corner of the graph, you will see a toolbar with 
               four buttons:")),
      
      #Insert instructions on how to use the plotly toolbar.
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
      
      #Download button for the glossary.
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
      
      #Repeat the standard note regarding disclosure control from the...
      #Introduction tab. Add another note that explains the difference between...
      #the psychiatric data presented in this tab v. the psychiatric data presented...
      #elsewhere in the publication.
      p(br(),
        tags$b("Notes:"),
        br(),
        tags$b("1. Statistical disclosure control has been applied to protect 
                 patient confidentiality. As a result, the figures presented here 
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
                
                #Insert the filters.
                #We have five filters in total.
                #Two of our filters have already been created in... 
                #the Server syntax, using renderUI().
                #We just need to make them appear using the uiOutput() command.
                #We arrange our filters in columns. 
                #The last column contains the 'Download as .csv' button.
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
      
      #In the main panel of the diagnoses tab, insert the diagnoses line...
      #chart, the 'Show/hide table' button, and the diagnoses table. 
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
                #Finally, add a button that allows the user to go back to the...
                #top of the page.
                tags$a(href = '#diagnoses_top', 
                       icon("circle-arrow-up", 
                            lib = "glyphicon"), 
                       "Back to top"),
                br(),
                br())
    ),
    
    ##* Geography user interface ----   
    
    #Create the geography tab.
    
    #Create a new tabPanel, stylise it, and give a title to the tab.
    tabPanel(
      "Geography", 
      icon = icon("globe"), 
      style = "height: 95%; width: 95%; background-color: #FFFFFF; 
        border: 0px solid #FFFFFF;",
      h1("Geography", id = 'geography_top'),
      
      #Provide a description for the tab.
      p(HTML("This section contains an interactive map that presents data broken 
               down by council area of residence. Use the filters to visualise the 
               data you are interested in. Click on a council area on the map to 
               reveal the rate of patients/discharges. To view the data in a table, 
               use the <a href = '#geographies_link'> 'Show/hide table' </a> button 
               at the bottom of the page. To download your data selection as a .csv 
               file, use the 'Download as .csv' button next to the filters.")),
      
      #Download button for the glossary.
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
      
      #Repeat the standard note regarding disclosure control.
      p(br(),
        tags$b(
          "Note: Statistical disclosure control has been applied to protect 
            patient confidentiality. As a result, the figures presented here 
            may not be additive and may differ from previous 
            sources of information."
        )),
      p(""),
      wellPanel(tags$style(".well { background-color: #FFFFFF; 
                             border: 0px solid #336699; }"),
                
                #Insert the filters.
                #We have three filters in total.
                #We arrange our filters in columns. 
                #The third column contains the 'Download as .csv' button.
                column(3,
                       shinyWidgets::radioGroupButtons(
                         inputId = "geography_datasets",
                         label = "Select treatment specialty", 
                         choices = geography_dataset, 
                         selected = "Psychiatric",
                         justified = TRUE,
                         checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                         direction = "vertical"
                       )
                ),
                
                column(5, 
                       shinyWidgets::pickerInput(
                         inputId = "geography_measure_type",
                         label = "Select measure",
                         choices = geography_measures, 
                         selected = 
                           "Rate of patients (per 100,000 population)"
                       ), 
                       shinyWidgets::pickerInput(
                         inputId = "geography_financial_years",
                         label = "Select financial year",
                         choices = geography_fin_years,
                         selected = "2020/2021"
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
      
      #In the main panel of the geography tab, insert the council area map, ...
      #the 'Show/hide table' button, and the geography table. 
      mainPanel(width = 12,
                br(),
                leafletOutput("mymap", height = 600),
                br(),
                br(),
                HTML("<button data-toggle = 'collapse' href = '#geographies'
                       class = 'btn btn-primary' id = 'geographies_link'> 
                       <strong> Show/hide table </strong></button>"),
                HTML("<div id = 'geographies' class = 'collapse'>"),
                br(),
                dataTableOutput("geography_table"),
                HTML("</div>"),
                br(),
                br(),                
                #Finally, add a button that allows the user to go back to the...
                #top of the page.
                tags$a(href = '#geography_top', 
                       icon("circle-arrow-up", 
                            lib = "glyphicon"), 
                       "Back to top"),
                
                br(),
                br())
    ),
    
    ##* Age/sex user interface ----   
    
    #Create the age/sex tab.
    
    #Create a new tabPanel, stylise it, and give a title to the tab. 
    tabPanel(
      "Age/sex", 
      icon = icon("child"), 
      style = "height: 95%; width: 95%; background-color: #FFFFFF; 
        border: 0px solid #FFFFFF;",
      h1("Age/sex", id = 'age_sex_top'),
      
      #Provide a description for the tab.
      p(HTML("This section allows you to explore the age and sex distribution 
               of the data. Use the filters to visualise the data you are 
               interested in. To view your data selection in a table, use the 
               <a href = '#age_and_sex_link'> 'Show/hide table' </a> button at 
               the bottom of the page. To download your data selection as a .csv 
               file, use the 'Download as .csv' button under the filters. At the 
               top-right corner of the graph, you will see a toolbar with four 
               buttons:")),
      
      #Insert instructions on how to use the plotly toolbar.
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
      
      #Download button for the glossary.
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
      
      #Repeat the point regarding disclosure control.
      p(br(),
        tags$b(
          "Note: Statistical disclosure control has been applied to protect 
            patient confidentiality. As a result, the figures presented here 
            may not be additive and may differ from previous 
            sources of information."
        )),
      p(""),
      wellPanel(tags$style(".well { background-color: #FFFFFF; 
                             border: 0px solid #336699; }"),
                
                #Insert the filters.                                                
                #We have five filters in total.
                #Two of our filters have already been created in...
                #the Server syntax, using renderUI().
                #We just need to make them appear using the uiOutput() command.
                #We arrange our filters in columns.
                #The last column contains the 'Download as .csv' button.
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
                         selected = "2020/2021"
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
                #Finally, add a button that allows the user to go back to...
                #the top of the page.
                tags$a(href = '#age_sex_top', 
                       icon("circle-arrow-up", 
                            lib = "glyphicon"), 
                       "Back to top"),
                
                br(),
                br())
    ),
    
    ##* Deprivation user interface ----   
    
    #Create the deprivation tab.
    
    #Create a new tabPanel, stylise it, and give a title to the tab. 
    tabPanel(
      "Deprivation", 
      icon = icon("bar-chart"), 
      style = "height: 95%; width: 95%; background-color: #FFFFFF; 
        border: 0px solid #FFFFFF;",
      h1("Deprivation", id = 'depr_top'),
      
      #Provide a description for the tab.
      p(HTML("This section contains two graphs, both revolving around 
               deprivation. The <a href = '#quintile_graph_link'> first graph </a> 
               shows inpatient activity broken down by 
               deprivation quintile, whereas the 
               <a href = '#RII_trend_link'> second graph </a> displays the Relative 
               Index of Inequality as a trend over time. Use the filters to visualise
               the data you are interested in. It is possible to select multiple 
               health boards of residence in the first graph. To view your data 
               selection in a table, use the 
               <a href = '#RII_link'> 'Show/hide table' </a> button under each 
               graph. To download your data selection as a .csv file, use the 
               'Download as .csv' button created for each graph. At the top-right 
               corner of each graph, you will see a toolbar with four buttons:")),
      
      #Insert instructions on how to use the plotly toolbar.
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
      
      #Download button for the glossary.
      p("You can also download our glossary of commonly used terms in
          mental health care, which has been created to help you
          understand the information visualised in the explorer:"),
      downloadButton(outputId = "download_glossary_five", 
                     label = "Download glossary", 
                     class = "glossaryfive"),
      tags$head(
        tags$style(".glossaryfive { background-color: #0072B2; } 
                     .glossaryfive { color: #FFFFFF; }")
      ),
      
      #Repeat the disclosure control note.
      p(br(),
        tags$b(
          "Note: Statistical disclosure control has been applied to protect 
            patient confidentiality. As a result, the figures presented here 
            may not be additive and may differ from previous 
            sources of information."
        )),
      p(""),
      wellPanel(tags$style(".well { background-color: #FFFFFF; 
                             border: 0px solid #336699; }"),
                
                #Since this page contains two graphs and, therefore, two...
                #distinct sections, we can insert a title for each section to...
                #clarify what each of them visualises.
                #This is the title of the first section.
                h2("Activity by deprivation quintile", 
                   id = 'quintile_graph_link'),
                br(),
                
                #Insert the filters for the first chart, i.e., the...
                #bar chart.                                             
                #We have five filters.
                #Our first two filters have already been created in...
                #the Server, using renderUI().
                #We just need to make them appear using the uiOutput() command.
                #We arrange our filters in columns.
                #The last column contains the 'Download as .csv' button.
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
                         selected = "2020/2021"
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
      
      #Visualise the deprivation bar chart, the 'Show/hide table' button, and...
      #the table associated with the bar chart.
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
                #Add a button that allows the user to go back to the top of the...
                #page.
                tags$a(href = '#depr_top', 
                       icon("circle-arrow-up", 
                            lib = "glyphicon"), 
                       "Back to top"),
                
                #As mentioned above, this tab contains two charts.
                #As such, we need to divide our page into two sections.
                #To do this, we use a horizontal line as a page break.
                hr(),
                
                #Header or title for the second section.
                h2("Relative Index of Inequality time trend", 
                   id = 'RII_trend_link'),
                br(),
                
                #Insert the filters for the second graph, i.e., the...
                #line chart.
                #We have two filters, which will be arranged in two separate...
                #columns.
                #There is another column which contains the 'Download as .csv'...
                #button.
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
                
                #Finally, visualise the line chart, the 'Show/hide table'... 
                #button, and the table associated with the line chart.
                plotlyOutput("RII_line_chart", 
                             width = "1090px",
                             height = "600px"),
                br(),
                br(),
                br(),
                HTML("<button data-toggle = 'collapse' href = '#RII'
                       class = 'btn btn-primary' id = 'RII_link'> 
                       <strong>Show/hide table</strong></button>"),
                HTML("<div id = 'RII' class = 'collapse'>"),
                br(),
                dataTableOutput("RII_table"),
                HTML("</div>"),
                br(),
                br(),
                #You can also add a button that allows the user to go back to...
                #the top of the page.
                tags$a(href = '#depr_top', 
                       icon("circle-arrow-up", 
                            lib = "glyphicon"), 
                       "Back to top"),
                
                br(),
                br()
                
      )),
    
    ##* Cross-boundary flow user interface ----   
    
    #Create the cross-boundary flow tab.
    
    #Create a new tabPanel, stylise it, and give a title to the tab. 
    tabPanel(
      "Cross-boundary flow", 
      icon = icon("exchange"),  
      style = "height: 95%; width: 95%; background-color: #FFFFFF; 
        border: 0px solid #FFFFFF;",
      h1("Cross-boundary flow", id = 'cbf_top'),
      
      #Provide a description for the tab.
      p(HTML("The following diagram shows you how many patients living in the 
               selected NHS board of residence were treated outside their board. 
               Use the filters to visualise the data you are interested in. To 
               view your data selection in a table, use the <a href = '#flow_link'> 
               'Show/hide table' </a> button at the bottom of the page. To 
               download your data selection as a .csv file, use the 'Download as .csv'
               button next to the filters.")),
      
      #Download button for the glossary.
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
      
      #Repeat the disclosure control note.
      p(br(),
        tags$b(
          "Note: Statistical disclosure control has been applied to protect 
            patient confidentiality. As a result, the figures presented here 
            may not be additive and may differ from previous 
            sources of information."
        )),
      p(""),
      wellPanel(tags$style(".well { background-color: #FFFFFF; 
                             border: 0px solid #336699; }"),
                
                #Insert the filters.                                             
                #We are using three filters, arranged in two columns.
                #A third column contains the 'Download as .csv' button.
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
                         selected = "2020/2021"
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
      
      #In the main panel, we insert the cross-boundary flow sentence, the...
      #diagram, the 'Show/hide table' button, and the cross-boundary flow table.
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
                #Finally, add a button that allows the user to go back to the...
                #top of the page.
                tags$a(href = '#cbf_top', 
                       icon("circle-arrow-up", 
                            lib = "glyphicon"), 
                       "Back to top"),
                
                br(),
                br())
    ),
    
    ##* Readmissions user interface ----   
    
    #Create the readmissions tab.
    
    #Create a new tabPanel, stylise it, and give a title to the tab. 
    tabPanel(
      "Readmissions", 
      icon = icon("bed"), 
      style = "height: 95%; width: 95%; background-color: #FFFFFF; 
        border: 0px solid #FFFFFF;",
      h1("Readmissions", id = 'readm_top'),
      
      #Provide a description for the tab.
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
      
      #Instructions on how to use the plotly toolbar.
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
      
      #Download button for the glossary.
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
      
      #Repeat the disclosure control note and add another note to clarify...
      #which psychiatric specialties were included in the readmission analysis.
      p(br(),
        tags$b("Notes:"),
        br(),
        tags$b(
          "1. Statistical disclosure control has been applied to protect 
            patient confidentiality. As a result, the figures presented here 
            may not be additive and may differ from previous 
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
                
                #Since this page contains two graphs and, therefore, two...
                #distinct sections, we can insert a title for each section to...
                #clarify what each of them visualises.
                #This is the title of the first section.
                h2("Health board comparison", 
                   id = 'readm_HB_comparison_link'),
                br(),
                
                #Insert the filters for the first graph, i.e., the...
                #bar chart.                                             
                #We are using three filters, arranged in three columns.
                #A fourth column contains the 'Download as .csv' button.
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
                         selected = "2020/2021"
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
      
      #Visualise the bar chart, the 'Show/hide table' button, and the table...
      #associated with the bar chart.
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
                #Add a button that allows the user to go back to the top of the...
                #page.
                tags$a(href = '#readm_top', 
                       icon("circle-arrow-up", 
                            lib = "glyphicon"), 
                       "Back to top"),
                #As mentioned above, this tab contains two charts.
                #As such, we need to divide our page into two sections.
                #To do this, we use a horizontal line as a page break.
                hr(),
                #Header or title for the second section.
                h2("Trend over time", 
                   id = 'readm_trend_link'),
                br(),
                
                #Insert the filters for the second graph, i.e., the...
                #line chart.
                #We have three filters, arranged in columns.
                #Add the 'Download as .csv' button too.
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
                
                #Finally, visualise the line chart, the 'Show/hide table'... 
                #button, and the table associated with the line chart.
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
                #You can also add a button that allows the user to go back...
                #to the top of the page.
                tags$a(href = '#readm_top', 
                       icon("circle-arrow-up", 
                            lib = "glyphicon"), 
                       "Back to top"),
                br(),
                br()
                
      )),
    
    ##* Table user interface ----    
    
    #Create the table tab.
    
    #Create the final tabPanel, stylise it, and give a title to the tab. 
    tabPanel(
      "Table", 
      icon = icon("table"), 
      style = "float: top; height: 95%; width: 95%; background-color: #FFFFFF; 
        border: 0px solid #FFFFFF;", 
      h1("Table", id = 'table_top'),
      
      #Provide a description for the tab.
      p("This section allows you to view the data in table format. Use the  
          'Select data file' filter to visualise the file you are interested in 
          (the list includes data files used in the 'Trend data' page). You can then
          click on the filters below the column names of the table and use the 
          dropdowns to modify the table. To download your data selection as a .csv 
          file, use the 'Download as .csv' button."),
      
      #Download button for the glossary.
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
      
      #Statistical disclosure control note.
      p(br(),
        tags$b(
          "Note: Statistical disclosure control has been applied to protect 
            patient confidentiality. As a result, the figures presented here 
            may not be additive and may differ from previous 
            sources of information."
        )),
      p(""),
      wellPanel(tags$style(".well { background-color: #FFFFFF; 
                             border: 0px solid #336699; }"),
                
                #We are only using one filter here, which contains the...
                #names of the files.
                #We also insert the 'Download as .csv' button.
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
      
      #Finally, insert the actual table.
      mainPanel(width = 12, 
                dataTableOutput("table_tab"),
                br(),
                #Add a button that allows the user to go back to the top of the...
                #page.
                tags$a(href = '#table_top', 
                       icon("circle-arrow-up", 
                            lib = "glyphicon"), 
                       "Back to top"),
                br(),
                br()
      ) 
      
    ) #End of tab panel.
    
  ) #End of tab set. 
  
) #End of fluid page.

#We are finished with the user interface syntax.
