##* Introduction user interface ----   

# We begin with an introduction tab, where we introduce the explorer and...
# its purpose.

# Create a new tabPanel, stylise it, and give it a title.
tabPanel(
  "Introduction",
  icon = icon("info-circle"),
  style = "float: top; height: 95%; width: 95%;
      background-color: #FFFFFF; border: 0px solid #FFFFFF;",
  h1("Introduction"),
  
  # Explain to the user what each tab visualises.
  # Each tab title is a hyperlink, linking to its respective tab.
  p("The explorer allows you to visualise mental health inpatient and day case 
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
          well as the Relative Index of Inequality (RII) as a time trend."
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
        actionLink("link_to_learning_disabilities_tab", "Learning Disabilities")
      ),
      icon("school"),
      " - shows activity in the Learning Disability specialty."
    ),
    
    tags$li(
      tags$b(
        actionLink("link_to_table_tab", "Table")
      ),
      icon("table"),
      " - allows you to view the data in a table."
    )
  ),
  
  # Insert the standard notes.
  p("When using the data explorer, please take the following
        factors into consideration:"),
  tags$ul(
    tags$li(
      "The explorer visualises information recorded in the Scottish
          Morbidity Record 01 (SMR01) and Scottish Morbidity Record 04
          (SMR04) datasets. SMR04 includes all inpatients and day cases
          admitted to and discharged from psychiatric specialties. SMR01
          includes all inpatients and day cases discharged from non-obstetric
          and non-psychiatric specialties. Click the following links for a 
      complete list of ",
      tags$a("SMR01 Specialty Codes",
        href = "https://publichealthscotland.scot/resources-and-tools/health-intelligence-and-data-management/national-data-catalogue/data-dictionary/search-the-data-dictionary/smr01-significant-facilities-and-codes/",
        target = "_blank"
        ),
      " and ",
      tags$a("SMR04 Specialty Codes.",
        href = "https://publichealthscotland.scot/resources-and-tools/health-intelligence-and-data-management/national-data-catalogue/data-dictionary/search-the-data-dictionary/smr04-significant-facilities-and-codes/",
        target = "_blank"
      ))),
  tags$ul(
    tags$li(
      "Please note that this data release excludes activity in the Learning Disability 
      specialty except on the Learning Disabilities tab.")),
  tags$ul(
    tags$li(
      "Data on patients being treated in Long Stay Geriatric specialties (SMR01_1E) 
      and data from maternity inpatient and day cases (SMR02) are not represented in this data release.")),
  tags$ul(
    tags$li(
      "Additionally, SMR01 has been restricted to diagnoses of 'Mental and behavioural disorders' only, 
      which were identified using codes F00-F99 from the International 
      Classification of Diseases, Tenth Revision.")),
  tags$ul(
    tags$li(
      "SMR01 and SMR04 data completeness varies from year to year.
          As a result, data is provisional and subject to change. For
          more information, visit the ",
      tags$a("SMR Completeness",
        href = "https://publichealthscotland.scot/resources-and-tools/health-intelligence-and-data-management/data-management-in-secondary-care-hospital-activity/scottish-morbidity-records-smr/completeness/",
        target = "_blank"),
      " webpage, which contains an Excel file with completeness estimates 
        for all SMR datasets. For this release, data for the State Hospital (SMR04) 
        was unavailable for the year 2023/24 due to issues with their systems. 
        A judgement has been made to 
        proceed with the publication as the lower data completeness for
        this health board has only a very minor effect on the overall data
        quality for this release and is very unlikely to affect the patterns
        of activity seen or conclusions drawn at a national level. Data 
        presented for the year 2023/24 is 98% complete overall, well above 
        the 90% PHS standard for recording of data."
    )),
  tags$ul(
    tags$li(
      "Statistical disclosure control has been applied to protect patient 
          confidentiality by rounding values to the nearest 5. As a result, the 
          figures presented in this explorer may not be additive and may differ 
          from previous sources of information. For more information, 
          please refer to the ",
      tags$a("PHS Statistical Disclosure Control Protocol.",
        href = "https://publichealthscotland.scot/publications/public-health-scotland-statistical-disclosure-protocol/",
        target = "_blank"
      )
    )),
  
  # Provide a download button for the glossary.
  p("To help you understand the information visualised in the
        explorer, we have created a glossary of commonly used terms in
        mental health care. Click the button below to download the glossary:"),
  downloadButton(outputId = "download_glossary_00",
                 label = "Download glossary",
                 class = "glossaryButton"),
  
  # Provide contact details for the team.
  p(br(),
    "If you have any trouble using the explorer or have further
        questions about the data, please contact us at:",
    tags$b(
      tags$a(href = "mailto:phs.mentalhealthanalytics@phs.scot",
             "phs.mentalhealthanalytics@phs.scot.")),
    br(),
    br(),
    br(),
    br()
  )
)