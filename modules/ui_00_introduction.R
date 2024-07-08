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
          and non-psychiatric specialties. For a complete list of
          specialties, open the ",
      tags$a(
        href = "https://www.ndc.scot.nhs.uk/docs/2020-04-23%20Specialty-Codes-and-Values.xlsx",
        "Specialty Codes and Descriptions document"
      ),
      " and consult the columns SMR01 and SMR04. Please note that
          this data release excludes activity in the Learning Disability
          specialty. Data on patients being treated in Long Stay Geriatric 
          specialties is also not represented in this data release as it is 
          recorded in a separate dataset. Additionally, SMR01 has been 
          restricted to diagnoses of 'Mental and behavioural disorders' only, 
          which were identified using codes F00-F99 from the International 
          Classification of Diseases, Tenth Revision."
    ),
    tags$li(
      "SMR01 and SMR04 data completeness varies from year to year.
          As a result, data is provisional and subject to change. For
          more information, visit the ",
      tags$a(
        href = "https://beta.isdscotland.org/products-and-services/data-management-hospital-activity/smr-completeness/",
        "SMR Completeness"
      ),
      " webpage, which contains an Excel file with completeness estimates 
          for all SMR datasets. For this release, data completeness for the State 
          Hospital (SMR04) was slightly lower than other health boards at the
          time of data extraction, at 87%. This incompleteness only affects
          Quarter 4 (38%) of the financial year. This was due to ongoing 
          staffing issues at the State Hospital. A judgement has been made to
          proceed with the publication as the lower data completeness for
          this health board has only a very minor effect on the overall data
          quality for this release and is very unlikely to affect the patterns
          of activity seen or conclusions drawn at a national level. Data 
          presented for the year 2022/23 is 99% complete overall, well above 
          the 90% PHS standard for recording of data."
    ),
    tags$li(
      "Statistical disclosure control has been applied to protect patient 
          confidentiality by rounding values to the nearest 5. As a result, the 
          figures presented in this explorer may not be additive and may differ 
          from previous sources of information. For more information, 
          please refer to the ",
      tags$a(
        href = "https://publichealthscotland.scot/publications/public-health-scotland-statistical-disclosure-protocol/",
        "PHS Statistical Disclosure Control Protocol."
      ),
      ""
    )
  ),
  
  # Provide a download button for the glossary.
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