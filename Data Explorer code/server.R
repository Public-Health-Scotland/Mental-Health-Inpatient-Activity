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

#A shiny app needs two things: the Server and the User Interface. 
#We can begin with the Server.

function(input, output, session) {
  
  #These observeEvent() commands will be combined with action buttons in...
  #the User Interface to allow the user to navigate to each tab by clicking...
  #on links in the Introduction page (in addition to the classic way of...
  #navigating, which is by clicking on the tab header itself).   
  
  observeEvent(input$link_to_tabpanel_time_trend, {
    updateTabsetPanel(session, "Panels", selected = "Time trend")
    
  })
  
  observeEvent(input$link_to_tabpanel_age_sex, {
    updateTabsetPanel(session, "Panels", selected = "Age/sex")
    
  })
  
  observeEvent(input$link_to_tabpanel_deprivation, {
    updateTabsetPanel(session, "Panels", selected = "Deprivation")
    
  })
  
  observeEvent(input$link_to_tabpanel_flow, {
    updateTabsetPanel(session, "Panels", selected = "Cross-boundary flow")
    
  })
  
  observeEvent(input$link_to_tabpanel_table, {
    updateTabsetPanel(session, "Panels", selected = "Table")
    
  })
  
  ##############################################.             
  ############## Time trend ----   
  ##############################################.
  
  #We start with the time trend data.
  #The user's selection in the filter SELECT TYPE OF LOCATION determines...
  #which choices appear in the filter SELECT LOCATION.
  #The renderUI() command makes the process of creating these two dynamic... 
  #filters very easy.
  #In the second renderUI() command, we set multiple to TRUE, as we want the...
  #user to be able to select multiple locations.
  
  output$time_trend_location_types <- renderUI({
    shinyWidgets::pickerInput("time_trend_location_type", 
                              label = "Select type of location",
                              choices = tt_location_types, 
                              selected = "Scotland")
  })
  
  output$time_trend_locations <- renderUI({
    shinyWidgets::pickerInput( 
      "time_trend_location",
      label = "Select location (multiple selections allowed)",  
      choices = sort(
        unique(
          as.character(
            time_trend$geography2
            [time_trend$geography1 %in% input$time_trend_location_type]
          )
        )
      ),
      multiple = TRUE,
      options = list( 
        `selected-text-format` = "count > 1"
      ),
      selected = if(input$time_trend_location_type == "Scotland") 
      { print("Scotland") } else { print("NHS Ayrshire & Arran") }
    )
  }) 
  
  #There are four filters in total: SELECT TYPE OF LOCATION, SELECT LOCATION,...
  #SELECT DIAGNOSIS GROUPING, and SELECT MEASURE.
  #The reactive() command below creates a subset of the time trend...
  #dataset based on the user's selections in these four filters.
  #This subset will then "feed" the time trend line chart.
  
  time_trend_new <- reactive({
    time_trend %>% 
      filter(geography1 %in% input$time_trend_location_type 
             & geography2 %in% input$time_trend_location
             & diagnosis_groupings %in% input$time_trend_diagnoses 
             & measure %in% input$time_trend_measure_type)
  })
  
  #Create the line chart for the time trend tab.
  #We create this using the plotly library.
  #Plotly graphs are a lot more interactive compared to ggplot2 graphs.
  
  output$time_trend_plot <- renderPlotly({
    
    #If the user selects State Hospital AND crude rates, there needs to be...
    #a message saying that no crude rates are available for State Hospital.
    
    if (any(input$time_trend_location == 
            "The State Hospitals Board for Scotland") &  
        (input$time_trend_measure_type == 
         "Crude rate of patients (per 100,000 population)" |
         input$time_trend_measure_type == 
         "Crude rate of discharges (per 100,000 population)" | 
         input$time_trend_measure_type == 
         "Crude rate of hospital residents (per 100,000 population)")) 
      
    { 
      
      #This is the message we are using.
      
      text_state_hosp <- list(
        x = 5, 
        y = 2,
        font = list(color = "#0072B2", size = 20),
        text = 
          "No crude rates available for location of treatment 'State Hospital'.", 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      ) 
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_state_hosp, 
               yaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE), 
               xaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE)) %>%  
        config(displayModeBar = FALSE,
               displaylogo = F, collaborate = F, editable = F) 
      
    }
    
    #Now let's create the normal graph.
    
    else {
      
      #Create the tooltip, i.e., insert the information that appears when you...
      #hover over a dot in the line graph.
      #E.g.: 
      #"Financial year: 2017/2018" 
      #"Location of treatment: Scotland"
      #"Diagnosis grouping: Disorders of adult behaviour and personality"
      #"Number of discharges: XXXX"
      
      tooltip_time_trend <- paste0("Financial year: ", 
                                   time_trend_new()$year, "<br>",
                                   "Location of treatment: ",
                                   time_trend_new()$geography2, "<br>",
                                   "Diagnosis grouping: ",
                                   time_trend_new()$diagnosis_groupings, "<br>",
                                   input$time_trend_measure_type, ": ",
                                   time_trend_new()$value)
      
      #Create the main body of the chart.
      
      plot_ly(data = time_trend_new(), 
              
              x = ~year, y = ~value, color = ~geography2,  
              
              #Specify the colours to be used:
              #Dark blue for Scotland.
              #Pre-made colour palettes for the boards (from package...
              #"RColorBrewer").
              
              colors = if(input$time_trend_location == "Scotland") 
              { print(c("#0072B2")) } 
              else 
              { print(c(
                brewer.pal(12, "Paired")[c(10, 9, 8, 7, 6, 5, 4, 3, 11, 12)],
                brewer.pal(9, "Set1")[c(6, 9)], 
                "#000000"
              )) },
              
              text = tooltip_time_trend, hoverinfo = "text", 
              
              #Select the type of chart you want, in this case a line chart,...
              #and set the mode to 'lines+markers' in order to visualise the...
              #line as well as the dots.
              
              type = 'scatter', mode = 'lines+markers', 
              marker = list(size = 8), 
              width = 1000, height = 600) %>%
        
        #Make the graph title reactive.
        
        layout(title = 
                 paste0(
                   "<b>", 
                   input$time_trend_measure_type, 
                   if (input$time_trend_diagnoses == "All")
                   { paste0(" in the period ",
                            first(as.vector(time_trend_new()$year)), 
                            " - ", 
                            last(as.vector(time_trend_new()$year)),
                            ",",
                            "<br>",
                            "by financial year and location of treatment") }
                   else 
                   { paste0(" with main diagnosis", 
                            "<br>", 
                            "'", input$time_trend_diagnoses, "',", 
                            "<br>",
                            first(as.vector(time_trend_new()$year)), 
                            " - ", 
                            last(as.vector(time_trend_new()$year)),
                            ", by financial year and location of treatment") },
                   "</b>"
                 ),
               
               separators = ".",
               
               #We need to fix the range of the y axis, as R refuses to set...
               #the lower end of this axis to zero.
               #The following "range" command fixes the lower end to...
               #zero, and calculates the upper end as the maximum...
               #number visualised in the graph + 10% of this number.
               #Also, wrap the y axis title in blank spaces so it doesn't...
               #overlap with the y axis tick labels.
               #Finally, make the y axis title reactive.
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(time_trend_new()$value, na.rm = TRUE) + 
                             (max(time_trend_new()$value, na.rm = TRUE) 
                              * 10 / 100)), 
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   if (input$time_trend_measure_type == 
                       "Crude rate of discharges (per 100,000 population)")
                   { print(c("Crude rate of discharges")) }
                   else if (input$time_trend_measure_type == 
                            "Crude rate of patients (per 100,000 population)")
                   { print(c("Crude rate of patients")) }
                   else if (input$time_trend_measure_type == 
                            "Crude rate of hospital residents (per 100,000 population)")
                   { print(c("Crude rate of hospital residents")) }
                   else { print(c(input$time_trend_measure_type)) }, 
                   rep("&nbsp;", 20),
                   rep("\n&nbsp;", 3)
                 ), 
                 collapse = ""),
                 showline = TRUE, 
                 ticks = "outside"
                 
               ),
               
               #Set the tick angle to minus 45. It's the only way for the x...
               #axis tick labels (fin. years) to display without overlapping...
               #with each other.
               #Wrap the x axis title in blank spaces so that it doesn't...
               #overlap with the x axis tick labels.
               
               xaxis = list(tickangle = -45, 
                            title = paste0(c(rep("&nbsp;", 20),
                                             "<br>", 
                                             "Financial year",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""),
                            showline = TRUE, 
                            ticks = "outside"),
               
               #Fix the margins so that the graph and axis titles have enough...
               #room to display nicely.
               #Set the font sizes.
               
               margin = list(l = 90, r = 60, b = 120, t = 90),
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #Insert a legend so that the user knows which colour...
               #corresponds to which location of treatment.
               #Make the legend background and legend border white.              
               
               showlegend = TRUE,
               legend = list(orientation = 'h',
                             x = 0, 
                             y = -0.5,
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove unnecessary buttons from the modebar.
        
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                             'zoomOut2d', 'autoScale2d', 
                                             'toggleSpikelines', 
                                             'hoverCompareCartesian', 
                                             'hoverClosestCartesian'), 
               displaylogo = F, collaborate = F, editable = F)
      
    }
    
  })                                                     
  
  #This reactive() creates the subset that will be used to create the table...
  #that will appear under the line chart.
  
  table_time_trend <- reactive({
    time_trend %>% 
      filter(geography1 %in% input$time_trend_location_type 
             & geography2 %in% input$time_trend_location
             & diagnosis_groupings %in% input$time_trend_diagnoses 
             & measure %in% input$time_trend_measure_type) %>%
      select(year, geography1, geography2, diagnosis_groupings, value) %>%
      filter(complete.cases(.))
  })
  
  #We now create the table that will go under the time trend line chart.
  #We give the columns clearer names.
  #The name of the last column, which is the measure column, changes...
  #according to the user's input in the filter SELECT MEASURE.
  
  output$time_trend_table <- renderDataTable({
    datatable(table_time_trend(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'),
              colnames = c("Financial year", "Type of location", 
                           "Location", "Diagnosis grouping", 
                           input$time_trend_measure_type))
  })
  
  #We also create a download button which the user can use to download the...
  #table in CSV format.
  
  output$download_time_trend <- downloadHandler(
    filename = 'time_trend_data.csv',
    content = function(file) {
      write.table(table_time_trend(), 
                  file,
                  
                  #Remove row numbers as the CSV file already has row numbers.
                  
                  row.names = FALSE,
                  col.names = c("Financial year", "Type of location", 
                                "Location", "Diagnosis grouping", 
                                input$time_trend_measure_type), 
                  sep = ",")
    }
  )
  
  ##############################################.             
  ############## Age/sex ----   
  ##############################################.
  
  #Moving on to the age/sex data.
  #The user's selection in the filter SELECT TYPE OF LOCATION determines...
  #which choices appear in the filter SELECT LOCATION.
  #The renderUI() command makes the process of creating these two dynamic... 
  #filters very easy. 
  
  output$age_sex_location_types <- renderUI({
    shinyWidgets::pickerInput("age_sex_location_type", 
                              label = "Select type of location", 
                              choices = as_location_types, 
                              selected = "Scotland") 
  }) 
  
  output$age_sex_locations <- renderUI({
    shinyWidgets::pickerInput("age_sex_location", 
                              label = "Select location", 
                              choices = sort(
                                unique(
                                  as.character(
                                    age_sex$geography2
                                    [age_sex$geography1 %in% 
                                      input$age_sex_location_type]
                                  )
                                )
                              ), 
                              selected = "Scotland")
  }) 
  
  #There are four filters in total: SELECT TYPE OF LOCATION, SELECT LOCATION,...
  #SELECT FINANCIAL YEAR, and SELECT MEASURE.
  #The reactive() command below creates a subset of the age/sex...
  #dataset based on the user's selections in these four filters.
  #This subset will then "feed" the age/sex pyramid.
  
  age_sex_new <- reactive({
    age_sex %>% 
      filter(geography1 %in% input$age_sex_location_type 
             & geography2 %in% input$age_sex_location 
             & year %in% input$age_sex_financial_year 
             & measure %in% input$age_sex_measure_type)
  })
  
  #Create the pyramid chart for the age/sex tab.
  #We create this using the plotly library.
  
  output$age_sex_pyramid <- renderPlotly({
    
    age_sex_no_patients_found <- age_sex %>% 
      filter(geography1 %in% input$age_sex_location_type 
             & geography2 %in% input$age_sex_location 
             & year %in% input$age_sex_financial_year 
             & measure %in% input$age_sex_measure_type) %>%
      mutate(value = abs(value))
    
    #If the user makes a combination of selections that leads to zero...
    #patients or discharges, there needs to be a message saying no patients/...
    #discharges found. Otherwise the user would see an empty graph and...
    #mistakenly think the app is not working.
    
    if (sum(age_sex_no_patients_found$value) == 0 & 
        !is.na(sum(age_sex_no_patients_found$value))) 
      
    { 
      
      #This is the message we are using.
      
      text_no_patients_found <- list(
        x = 5, 
        y = 2, 
        font = list(color = "#0072B2", size = 20),
        text = paste0("No ", 
                      if (input$age_sex_measure_type == 
                          "Number of discharges" |
                          input$age_sex_measure_type == 
                          "Crude rate of discharges (per 100,000 population)")
                      { print(c("discharges ")) }
                      else { print(c("patients ")) },
                      "found."),
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      )
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_no_patients_found, 
               yaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE), 
               xaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE)) %>%  
        config(displayModeBar = FALSE,
               displaylogo = F, collaborate = F, editable = F) 
      
    }
    
    #If the user selects the location 'Other' AND crude rates, there...
    #needs to be a message saying that no crude rates are available for...
    #'Other'.
    
    else if (input$age_sex_location == "Other" &  
             (input$age_sex_measure_type == 
              "Crude rate of patients (per 100,000 population)" |
              input$age_sex_measure_type == 
              "Crude rate of discharges (per 100,000 population)"))
      
    {
      
      #This is the message we are using.
      
      text_other <- list(
        x = 5, 
        y = 2, 
        font = list(color = "#0072B2", size = 20),
        text = "No crude rates available for location of residence 'Other'.", 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      )
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_other, 
               yaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE), 
               xaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE)) %>%  
        config(displayModeBar = FALSE,
               displaylogo = F, collaborate = F, editable = F) 
      
    }
    
    #Now let's create the normal graph.
    
    else {
      
      #Create the tooltip, i.e., insert the information that appears when...
      #you hover over a bar in the pyramid.
      #E.g.: 
      #"Sex: Female" 
      #"Age group: Ages 65+"
      #"Location of residence: Scotland"
      #"Financial year: 2017/2018"
      #"Number of discharges: XXXX"
      
      tooltip_age_sex <- paste0("Sex: ", age_sex_new()$sex_char, "<br>",
                                "Age group: ", age_sex_new()$ageband, "<br>",
                                "Location of residence: ", 
                                age_sex_new()$geography2, "<br>",
                                "Financial year: ", age_sex_new()$year, "<br>",
                                input$age_sex_measure_type, ": ", 
                                abs(age_sex_new()$value))
      
      #Create the main body of the chart.
      
      plot_ly(data = age_sex_new(), 
              
              x = ~value, y = ~ageband, color = ~sex_char, 
              
              #Colour palette:
              #Dark blue for males and light blue for females.
              
              colors = c("#0072B2", "#ADD8E6"),
              
              text = tooltip_age_sex, hoverinfo = "text", 
              
              #Select the type of chart you want, in this case a bar chart,...
              #and set the orientation to horizontal to achieve the...
              #"pyramid" look.
              
              type = 'bar', orientation = 'h',
              width = 1000, height = 400) %>%
        
        #Make the graph title reactive.
        
        layout(title = paste0("<b>", 
                              input$age_sex_measure_type, 
                              " in financial year ", 
                              input$age_sex_financial_year, 
                              ",", 
                              "<br>",
                              "by age group and sex, residents of ", 
                              input$age_sex_location,
                              "</b>"),
               
               separators = ".",
               
               #Set the gap size between bars, and make sure that the bars...
               #overlay, again to achieve that "pyramid" look.
               
               bargap = 0.2, barmode = 'overlay',
               
               #Wrap the y axis title in empty spaces so that it doesn't...
               #overlap with the y axis tick labels.
               
               yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                             "Age group",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""), 
                            showline = TRUE, ticks = "outside"),
               
               #Insert breaks and labels for the x axis, and define its range.  
               
               xaxis = list(
                 
                 exponentformat = "none",
                 separatethousands = TRUE,
                 tickmode = 'array',
                 range = c(
                   -round(max(abs(age_sex_new()$value))
                          * 110 / 100),
                   round(max(abs(age_sex_new()$value))
                         * 110 / 100)
                 ),
                 tickangle = 0,
                 tickvals = c(
                   -round(max(abs(age_sex_new()$value))),
                   -round(max(abs(age_sex_new()$value)) 
                          * 66 / 100),
                   -round(max(abs(age_sex_new()$value)) 
                          * 33 / 100), 
                   0, 
                   round(max(abs(age_sex_new()$value)) 
                         * 33 / 100), 
                   round(max(abs(age_sex_new()$value)) 
                         * 66 / 100), 
                   round(max(abs(age_sex_new()$value)))
                 ), 
                 ticktext = paste0(
                   as.character(c(
                     round(max(abs(age_sex_new()$value))),
                     round(max(abs(age_sex_new()$value)) 
                           * 66 / 100),
                     round(max(abs(age_sex_new()$value)) 
                           * 33 / 100), 
                     0, 
                     round(max(abs(age_sex_new()$value)) 
                           * 33 / 100), 
                     round(max(abs(age_sex_new()$value)) 
                           * 66 / 100),
                     round(max(abs(age_sex_new()$value)))
                   ))
                 ),
                 
                 #Make the x axis title reactive.
                 
                 title = 
                   if (input$age_sex_measure_type == 
                       "Crude rate of discharges (per 100,000 population)")
                   { print(c("Crude rate of discharges")) }
                 else if (input$age_sex_measure_type == 
                          "Crude rate of patients (per 100,000 population)")
                 { print(c("Crude rate of patients")) }
                 else { print(c(input$age_sex_measure_type)) },
                 
                 showline = TRUE, 
                 ticks = "outside"
                 
               ),
               
               #Fix the margins so that the graph and axis titles have...
               #enough room to display nicely.
               #Set the font sizes.
               
               margin = list(l = 140, r = 10, b = 70, t = 90), 
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #Insert a legend so that the user knows which colour...
               #corresponds to which sex.
               #Make the legend background and legend border white.              
               
               showlegend = TRUE, 
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove unnecessary buttons from the modebar.
        
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                             'zoomIn2d', 'zoomOut2d', 
                                             'autoScale2d', 
                                             'toggleSpikelines', 
                                             'hoverCompareCartesian', 
                                             'hoverClosestCartesian'), 
               displaylogo = F, collaborate = F, editable = F)
      
    }
    
  })
  
  #This reactive() creates the subset that will be used to create the...
  #table that will appear under the pyramid.
  
  table_age_sex <- reactive({
    age_sex %>% 
      filter(geography1 %in% input$age_sex_location_type
             & geography2 %in% input$age_sex_location 
             & year %in% input$age_sex_financial_year 
             & measure %in% input$age_sex_measure_type) %>% 
      
      #Create a new version of the numeric variable, where all...
      #the numbers become positive numbers.
      #To achieve the "pyramid" look, the original CSV file had to be...
      #created in such a way that males were given negative values and...
      #females were given positive values.
      #With abs(), we ensure that both males and females display as positive...
      #values on the table.
      
      mutate(numbers_v2 = abs(value)) %>%
      select(year, geography1, geography2, ageband, sex_char, numbers_v2) %>%
      filter(complete.cases(.)) 
  })
  
  #Create the table that will appear under the pyramid.
  #Give the columns clearer names.
  #The name of the last column depends on the user's input in the...
  #filter SELECT MEASURE.
  
  output$age_sex_table <- renderDataTable({
    datatable(table_age_sex(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'), 
              colnames = c("Financial year", "Type of location", 
                           "Location", "Age group", "Sex", 
                           input$age_sex_measure_type))
  })
  
  #Create a download button that allows the user to download the table...
  #in CSV format. 
  
  output$download_age_sex <- downloadHandler(
    filename = 'age_sex_data.csv',
    content = function(file) {
      write.table(table_age_sex(), 
                  file, 
                  row.names = FALSE, 
                  col.names = c("Financial year", "Type of location", 
                                "Location", "Age group", "Sex", 
                                input$age_sex_measure_type), 
                  sep = ",")
    }
  )
  
  ##############################################.             
  ############## Deprivation ----   
  ##############################################.
  
  #Moving on to the deprivation data.
  #The user's selection in the filter SELECT TYPE OF LOCATION determines...
  #which choices appear in the filter SELECT LOCATION.
  #The renderUI() command makes the process of creating these two dynamic... 
  #filters very easy.
  #In the second filter, we set multiple to TRUE, as we want the user to be...
  #able to compare multiple health boards at the same time.
  
  output$deprivation_location_types <- renderUI({
    shinyWidgets::pickerInput("deprivation_location_type", 
                              label = "Select type of location", 
                              choices = depr_location_types, 
                              selected = "Scotland")
  })
  
  output$deprivation_locations <- renderUI({
    shinyWidgets::pickerInput("deprivation_location", 
                              label = 
                                "Select location (multiple selections allowed)", 
                              choices = sort(
                                unique(
                                  as.character(
                                    deprivation$geography2
                                    [deprivation$geography1 %in% 
                                      input$deprivation_location_type]
                                  )
                                )
                              ), 
                              multiple = TRUE,
                              options = list( 
                                `selected-text-format` = "count > 1"
                              ),
                              selected = if(input$deprivation_location_type 
                                            == "Scotland")
                              { print("Scotland") } 
                              else 
                              { print("NHS Ayrshire & Arran") }
    )
  })
  
  #There are four filters in total: SELECT TYPE OF LOCATION, SELECT LOCATION,...
  #SELECT FINANCIAL YEAR, and SELECT MEASURE.
  #The reactive() command below creates a subset of the deprivation...
  #dataset based on the user's selections in these four filters.
  #This subset will then "feed" the deprivation bar chart.
  
  deprivation_new <- reactive({
    deprivation %>% 
      filter(geography1 %in% input$deprivation_location_type 
             & geography2 %in% input$deprivation_location 
             & year %in% input$deprivation_financial_year 
             & measure %in% input$deprivation_measure_type)
  })
  
  #Create the bar chart for the deprivation tab.
  #We create this using the plotly library.
  
  output$deprivation_bar_chart <- renderPlotly({
    
    #If the user makes a combination of selections that leads to zero...
    #patients or discharges, there needs to be a message saying no patients/...
    #discharges found. Otherwise the user would see an empty graph and...
    #mistakenly think the app is not working.
    
    if (sum(deprivation_new()$value) == 0 & 
        !is.na(sum(deprivation_new()$value)) &
        input$deprivation_location != "") 
      
    { 
      
      #This is the message we are using.
      
      text_no_patients_found <- list(
        x = 5, 
        y = 2, 
        font = list(color = "#0072B2", size = 20),
        text = paste0("No ", 
                      if (input$deprivation_measure_type == 
                          "Number of discharges" |
                          input$deprivation_measure_type == 
                          "Crude rate of discharges (per 100,000 population)")
                      { print(c("discharges ")) }
                      else if (input$deprivation_measure_type == 
                               "Number of patients" |
                               input$deprivation_measure_type == 
                               "Crude rate of patients (per 100,000 population)") 
                      { print(c("patients ")) }
                      else { print(c("hospital residents ")) }, 
                      "found."),
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      )
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_no_patients_found, 
               yaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE), 
               xaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE)) %>%  
        config(displayModeBar = FALSE,
               displaylogo = F, collaborate = F, editable = F) 
      
    }
    
    #Now let's create the normal graph.
    
    else {
      
      #Create the tooltip, i.e., insert the information that appears when the...
      #user hovers over a bar in the graph.
      #E.g.: 
      #"Location of residence: Scotland"
      #"Financial year: 2017/2018"
      #"Deprivation quintile (SIMD): 2"
      #"Number of discharges: XXXX"
      
      tooltip_deprivation <- paste0("Location of residence: ", 
                                    deprivation_new()$geography2, 
                                    "<br>",
                                    "Financial year: ",
                                    deprivation_new()$year, 
                                    "<br>",
                                    "Deprivation quintile (SIMD): ", 
                                    deprivation_new()$simd, 
                                    "<br>",
                                    input$deprivation_measure_type, ": ",
                                    deprivation_new()$value)
      
      #Create the main body of the chart.
      
      plot_ly(data = deprivation_new(), 
              
              x = ~simd, y = ~value, color = ~geography2, 
              
              #Specify the colours to be used:
              #Dark blue for Scotland.
              #Pre-made colour palettes for the boards (again from package...
              #"RColorBrewer").
              
              colors = if(input$deprivation_location == "Scotland") 
              { print(c("#0072B2")) } 
              else 
              { print(c(
                brewer.pal(12, "Paired")[c(10, 9, 8, 7, 6, 5, 4, 3, 11, 12)],
                brewer.pal(9, "Set1")[c(2, 6, 9)], 
                "#000000"
              )) },
              
              text = tooltip_deprivation, hoverinfo = "text",
              
              #Select the type of chart you want, in this case a bar chart.
              
              type = 'bar', width = 1000, height = 450) %>%
        
        #Make the graph title reactive.
        
        layout(title = paste0("<b>",
                              input$deprivation_measure_type, 
                              " in financial year ",
                              input$deprivation_financial_year,
                              ",",
                              "<br>",
                              "by deprivation quintile and ", 
                              "location of residence",
                              "</b>"),
               
               separators = ".",
               
               #We need to fix the range of the y axis, as R refuses to set...
               #the lower end of this axis to zero.
               #The following "range" command fixes the lower end to...
               #zero, and calculates the upper end as the maximum...
               #number visualised in the graph + 10% of this number.
               #Also, wrap the y axis title in blank spaces so that it...
               #doesn't overlap with the y axis tick labels.
               #Finally, make the y axis title reactive.
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(deprivation_new()$value, na.rm = TRUE) + 
                             (max(deprivation_new()$value, na.rm = TRUE) 
                              * 10 / 100)),
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   if (input$deprivation_measure_type == 
                       "Crude rate of discharges (per 100,000 population)")
                   { print(c("Crude rate of discharges")) }
                   else if (input$deprivation_measure_type == 
                            "Crude rate of patients (per 100,000 population)")
                   { print(c("Crude rate of patients")) }
                   else if (input$deprivation_measure_type == 
                            "Crude rate of hospital residents (per 100,000 population)")
                   { print(c("Crude rate of hospital residents")) }
                   else { print(c(input$deprivation_measure_type)) },
                   rep("&nbsp;", 20),
                   rep("\n&nbsp;", 3)
                 ),
                 collapse = ""),
                 
                 showline = TRUE, 
                 ticks = "outside"
                 
               ),
               
               #Label the x axis.
               
               xaxis = list(title = "Deprivation quintile (SIMD)", 
                            showline = TRUE, 
                            ticks = "outside"),        
               
               #Fix the margins so that the graph and axis titles have enough...
               #room to display nicely.
               #Set the font sizes.
               
               margin = list(l = 80, r = 10, b = 50, t = 90), 
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #Insert a legend so that the user knows which colour...
               #corresponds to which location of residence.
               #Make the legend background and legend border white.             
               
               showlegend = TRUE, 
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove unnecessary buttons from the modebar.
        
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                             'zoomOut2d', 'autoScale2d', 
                                             'toggleSpikelines', 
                                             'hoverCompareCartesian', 
                                             'hoverClosestCartesian'), 
               displaylogo = F, collaborate = F, editable = F)
      
    }
    
  })
  
  #This reactive() creates the subset that will be used to create the...
  #table that will appear under the bar chart.
  
  table_deprivation <- reactive({
    deprivation %>% 
      filter(geography1 %in% input$deprivation_location_type 
             & geography2 %in% input$deprivation_location 
             & year %in% input$deprivation_financial_year 
             & measure %in% input$deprivation_measure_type) %>% 
      select(year, geography1, geography2, simd, value)
  })
  
  #Create the table that will appear under the deprivation bar chart.
  #Give the columns clearer names.
  #The name of the final column reacts to the user's input in the filter...
  #SELECT MEASURE.
  
  output$deprivation_table <- renderDataTable({
    datatable(table_deprivation(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'), 
              colnames = c("Financial year", "Type of location", 
                           "Location", "Deprivation quintile (SIMD)", 
                           input$deprivation_measure_type))
  })
  
  #Create a download button that allows the user to download the table...
  #in CSV format.
  
  output$download_deprivation <- downloadHandler(
    filename = 'deprivation_data.csv',
    content = function(file) {
      write.table(table_deprivation(), 
                  file, 
                  row.names = FALSE, 
                  col.names = c("Financial year", "Type of location", 
                                "Location", 
                                "Deprivation quintile (SIMD)", 
                                input$deprivation_measure_type), 
                  sep = ",")
    }
  )
  
  ##############################################.             
  ############## Cross-boundary flow ----   
  ##############################################. 
  
  #There are two filters here: SELECT FINANCIAL YEAR and SELECT HEALTH BOARD...
  #OF RESIDENCE.
  #The reactive() command below creates a subset of the cross-boundary flow...
  #dataset based on the user's selections in the two aforementioned filters.
  #This subset will then "feed" the cross-boundary flow chart.
  #Additional transformations:
  #In the measure column, we filter out 'Number of discharges'. We want the...
  #cross-boundary flow diagram to show only number of patients.
  #Moreover, we are only visualising inter-board flow, so we need to filter...
  #out intra-board flow.
  
  flow_new <- reactive({
    flow %>% 
      filter(`health board of residence` %in% input$flow_board_of_residence 
             & year %in% input$flow_financial_year) %>%
      filter(measure != "Number of discharges") %>%
      filter(flow == 0) %>%
      filter(value > 0) %>%
      rename(`Number of patients` = value) %>%
      select(`health board of residence`, `health board of treatment`,
             `Number of patients`)
  })
  
  #Calculate the percentages and numbers which will be used in the...
  #reactive sentence that appears above our Sankey diagram. We need to...
  #calculate 5 different values (listed below).
  
  output$flow_text <- renderText({
    
    flow_txt <- flow %>% 
      filter(`health board of residence` %in% input$flow_board_of_residence 
             & year %in% input$flow_financial_year) %>%
      filter(measure != "Number of discharges")
    
    if (sum(flow_txt$flow) == 0) {
      
      # 1
      
      no_intraboard_flow_number <- flow_txt %>%
        summarise(sum(value[flow == 0])) %>%
        pull()
      
    }
    
    else {
      
      # 2
      
      flow_intraboard_percentage <- flow_txt %>%
        summarise(round(value[flow == 1]
                        / sum(value) * 100, 1)) %>%
        pull()
      
      # 3
      
      flow_interboard_percentage <- flow_txt %>%
        summarise(round(sum(value[flow == 0])
                        / sum(value) * 100, 1)) %>%
        pull()
      
      # 4 
      
      flow_intraboard_number <- flow_txt %>%
        summarise(value[flow == 1]) %>%
        pull()
      
      # 5
      
      flow_interboard_number <- flow_txt %>%
        summarise(sum(value[flow == 0])) %>%
        pull()
      
    }
    
    #We can now build our reactive sentence.
    #This will be done using nested "if" statements.
    
    #Statement 1: If there are no patients for a given combination of...
    #selections, the user gets the message "No patients found".
    
    if (sum(flow_txt$value) == 0) {
      
      paste0("<b>",
             "No patients found.",
             "</b>")
      
    }
    
    #Statement 2: If there are patients, but no intra-board flow, the user...
    #gets a message saying that the percentage of patients from NHS X who...
    #were treated inside their board was 0%. We also clarify that that's...
    #because this board has no psychiatric hospitals within its area (NHS...
    #Orkney and NHS Shetland).
    
    else if (sum(flow_txt$value) != 0 & 
             sum(flow_txt$flow) == 0) {
      
      paste0("<b>",
             "In ",
             input$flow_financial_year,
             ", 0% of patients from ",
             input$flow_board_of_residence,
             " were treated within their own board. This is because ",
             input$flow_board_of_residence,
             " has no psychiatric hospitals within its area. ",
             input$flow_board_of_residence,
             " patients (",
             no_intraboard_flow_number,
             " individuals) were instead treated in:",
             "</b>")
      
    }
    
    #Statement 3:
    #If there are patients, and there is intra-board flow, but the...
    #intra-board flow accounts for 100% of the activity, inform the user that...
    #the percentage of patients from NHS X who were treated inside their...
    #board was 100%.
    
    else if (sum(flow_txt$value) != 0 &
             sum(flow_txt$flow) != 0 & 
             flow_intraboard_percentage == 100) {
      
      paste0("<b>",
             "In ",
             input$flow_financial_year,
             ", 100% of patients from ",
             input$flow_board_of_residence,
             " were treated within their own board (",
             flow_intraboard_number,
             " individuals).",
             "</b>")
      
    }
    
    #Statement 4:
    #Finally, if there are patients, and there is intra-board flow, and...
    #the intra-board activity does not account for 100% of the activity,...
    #insert a sentence saying here's how many patients were treated...
    #inside their board, and here's how many were treated outside their board.
    
    else if (sum(flow_txt$value) != 0 &
             sum(flow_txt$flow) != 0 & 
             flow_intraboard_percentage != 100) {
      
      paste0("<b>",
             "In ",
             input$flow_financial_year,
             ", ",
             flow_intraboard_percentage,
             "% of patients from ",
             input$flow_board_of_residence,
             " were treated within their own board (",
             flow_intraboard_number,
             " individuals). The other ",
             flow_interboard_percentage,
             "% (",
             flow_interboard_number,
             " individuals) were treated in:",
             "</b>")
      
    }
    
    else {
      
      paste0(" ")
      
    }
    
  })
  
  #Create the Sankey diagram.
  
  output$flow_graph <- renderGvis({
    
    gvisSankey(flow_new(), from = "`health board of residence`",
               to = "`health board of treatment`",
               weight = "`Number of patients`",
               options = list(
                 width = "automatic",
                 height = "automatic",
                 sankey = "{link: {colorMode: 'gradient'},
                 node: {width: 15, 
                 label: {fontSize: 15, 
                 color: 'black',
                 bold: 'true'}}
  }" 
               )
               )
    
    })
  
  #This reactive() creates the subset that will be used to create the...
  #table that will appear under the diagram.
  
  table_flow <- reactive({
    flow %>% 
      filter(`health board of residence` %in% input$flow_board_of_residence 
             & year %in% input$flow_financial_year) %>%
      filter(measure != "Number of discharges") %>%
      select(year, `health board of residence`, `health board of treatment`,
             value)
  })
  
  #Create the table that will appear under the visual.
  #Give the columns clearer names.
  
  output$flow_table <- renderDataTable({
    datatable(table_flow(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'), 
              colnames = c("Financial year", "Health board of residence", 
                           "Health board of treatment", "Number of patients"))
  })
  
  #Create a download button that allows the user to download the table...
  #in CSV format.
  
  output$download_flow <- downloadHandler(
    filename = 'cross_boundary_flow_data.csv',
    content = function(file) {
      write.table(table_flow(), 
                  file, 
                  row.names = FALSE, 
                  col.names = c("Financial year", "Health board of residence", 
                                "Health board of treatment", 
                                "Number of patients"), 
                  sep = ",")
    }
  )
  
  ##############################################.              
  ############## Table tab ----    
  ##############################################.
  
  #On to the final tab, which is the Table tab.
  #The following piece of syntax tells R to switch between files...
  #according to the user's input in the filter SELECT DATA FILE.
  #The files below are the ones we read into R at the very beginning.
  #However, they require a few transformations before they can be displayed...
  #as a table.
  
  data_table <- reactive({
    switch(input$table_filenames,
           "Time trend (Data explorer)" = time_trend %>%
             mutate(measure = fct_rev(measure)) %>%
             mutate(geography1 = fct_rev(geography1)) %>%
             rename(`Financial year` = year, 
                    `Type of location` = geography1, 
                    `Location` = geography2, 
                    `Diagnosis grouping` = diagnosis_groupings, 
                    `Type of measure` = measure, 
                    `Number` = value) %>%
             filter(complete.cases(.)),
           "Age/sex (Data explorer)" = age_sex %>%
             mutate(measure = fct_rev(measure)) %>%
             mutate(geography1 = fct_rev(geography1)) %>%
             rename(`Financial year` = year, 
                    `Type of location` = geography1, 
                    `Location` = geography2, 
                    `Sex` = sex_char, 
                    `Age group` = ageband, 
                    `Type of measure` = measure, 
                    `Number` = value) %>%
             mutate(`Number` = abs(`Number`)) %>%
             filter(complete.cases(.)),
           "Deprivation (Data explorer)" = deprivation %>%
             mutate(measure = fct_rev(measure)) %>%
             mutate(geography1 = fct_rev(geography1)) %>%
             rename(`Financial year` = year, 
                    `Type of location` = geography1, 
                    `Location` = geography2, 
                    `Deprivation quintile (SIMD)` = simd, 
                    `Type of measure` = measure, 
                    `Number` = value),
           "Cross-boundary flow (Data explorer)" = flow %>%
             select(year, `health board of residence`, 
                    `health board of treatment`, measure, value) %>%
             mutate(measure = fct_rev(measure)) %>%
             rename(`Financial year` = year, 
                    `Health board of residence` = `health board of residence`,
                    `Health board of treatment` = `health board of treatment`,
                    `Type of measure` = measure,
                    `Number` = value),
           "Activity by hospital (Data trends)" = activity_by_hospital %>%
             rename(`Financial year` = year, 
                    `Health board of treatment (HBT)` = hbtreat_name, 
                    `Hospital (includes the total for each HBT)` = 
                      hospital_name, 
                    `Number of admissions` = admissions, 
                    `Number of discharges` = discharges, 
                    `Number of patients` = patients,
                    `Number of stays` = stays,
                    `Number of hospital residents` = hospital_residents),
           "Length of stay (Data trends)" = length_of_stay %>%
             mutate(geography1 = fct_rev(geography1)) %>%
             rename(`Financial year` = fyear, 
                    `Type of location` = geography1, 
                    `Location` = geography2, 
                    `No of stays with length less than a day` = 
                      `Less than a day`, 
                    `No of stays with length 1 to 7 days` = 
                      `1 to 7 days`, 
                    `No of stays with length 8 to 28 days` = 
                      `8 to 28 days`, 
                    `No of stays with length 29 days to 6 months` = 
                      `29 days to 6 months`, 
                    `No of stays with length over 6 months` = 
                      `Over 6 months`)
    )
  })
  
  
  #Create the actual table for the Table tab.
  
  output$table_tab <- renderDataTable({
    datatable(data_table(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed',
              rownames = FALSE, 
              options = list(
                pageLength = 20, 
                autoWidth = TRUE, 
                dom = 'tip'),
              
              #Insert filters at the top of each column.
              
              filter = list(position = 'top'))
  }) 
  
  #We also create a download button for the table tab.
  
  output$download_table <- downloadHandler(
    filename = 'table_data.csv', 
    content = function(file) { 
      write.csv(
        
        #The command "input[["table_tab_rows_all"]]" tells R to create a CSV...
        #file that takes into account the user's input in the filters below...
        #the column names.
        
        data_table()[input[["table_tab_rows_all"]], ], 
        file, 
        row.names = FALSE
      )
    } 
  )
  
  ##############################################.              
  ##############Glossary ----    
  ##############################################.
  
  #We have prepared a glossary to help the user understand the graphs...
  #and tables more easily.
  
  #Create download buttons for the glossary.
  #There must be one in each tab (including the introduction tab).
  #Therefore, we need to repeat the command six times.
  
  glossary_code_shortcut <- downloadHandler(
    filename = 'glossary.pdf',
    content = function(file) {
      file.copy(paste0(filepath, "www/glossary.pdf"), file)
    }
  )
  
  output$download_glossary_one <- glossary_code_shortcut
  
  output$download_glossary_two <- glossary_code_shortcut
  
  output$download_glossary_three <- glossary_code_shortcut  
  
  output$download_glossary_four <- glossary_code_shortcut 
  
  output$download_glossary_five <- glossary_code_shortcut
  
  output$download_glossary_six <- glossary_code_shortcut
  
  }

#We are now finished with the Server syntax.
