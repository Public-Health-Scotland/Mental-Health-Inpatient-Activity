#Name: Data explorer
#Author: Nikos Alexandrou
#Modified: 05/09/2019
#Type: Data visualisation
#Written on: RStudio
#Written for: R version 3.5.1 
#Output: Shiny application
#Approximate run time: < 1 minute
#Description: This syntax creates a Shiny application that allows the... 
#user to visualise mental health data in a variety of ways.

#A shiny app needs two things: the Server and the User Interface. 
#We can begin with the Server.

function(input, output, session) {
  
  #These observeEvent() commands will be combined with action buttons in...
  #the User Interface to allow the user to navigate to each tab by clicking...
  #on links in the Introduction page (in addition to the classic way of...
  #navigating, which is by clicking on the tab header itself).   
  
  observeEvent(input$link_to_tabpanel_time_trend, {
    updateTabsetPanel(session, "Panels", selected = "Trends in diagnoses")
    
  })
  
  observeEvent(input$link_to_tabpanel_geography, {
    updateTabsetPanel(session, "Panels", selected = "Geography")
    
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
  
  observeEvent(input$link_to_tabpanel_readmissions, {
    updateTabsetPanel(session, "Panels", selected = "Readmissions")
    
  })
  
  observeEvent(input$link_to_tabpanel_table, {
    updateTabsetPanel(session, "Panels", selected = "Table")
    
  })
  
  ##############################################.             
  ############## Time trend ----   
  ##############################################.
  
  #We start with the time trend data.
  
  ### 1 ---
  
  #The user's selection in the filter SELECT TYPE OF LOCATION determines...
  #which choices appear in the filter SELECT LOCATION.
  #The renderUI() command makes the process of creating these two dynamic... 
  #filters very easy.
  #In the second renderUI() command, we set multiple to TRUE, as we want the...
  #user to be able to select multiple locations. However, we set the limit to...
  #four selections.
  
  output$time_trend_location_types <- renderUI({
    shinyWidgets::pickerInput("time_trend_location_type", 
                              label = "Select type of location",
                              choices = tt_location_types, 
                              selected = "Scotland")
  })
  
  output$time_trend_locations <- renderUI({
    shinyWidgets::pickerInput( 
      "time_trend_location",
      label = "Select location (up to four selections allowed)",  
      choices = sort(
        unique(
          as.character(
            time_trend$geography2
            [time_trend$geography1 %in% input$time_trend_location_type]
          )
        )
      ),
      multiple = TRUE,
      width = '100%',
      options = list(
        "max-options" = 4,
        `selected-text-format` = "count > 1"
      ),
      selected = if(input$time_trend_location_type == "Scotland") 
      { print("Scotland") } else { print("NHS Ayrshire & Arran") }
    )
  })
  
  ### 2 ---
  
  #There are five filters in total: SELECT TREATMENT SPECIALTY, SELECT TYPE... 
  #OF LOCATION, SELECT LOCATION, SELECT DIAGNOSIS GROUPING, and SELECT MEASURE.
  #The reactive() command below creates a subset of the time trend dataset...
  #based on the user's selections in these five filters.
  #This subset will then "feed" the time trend line chart.
  #Don't forget to drop the unused levels in your factors. This is essential...
  #because, in the graph, we will be matching colours to factor levels.
  
  time_trend_new <- reactive({
    time_trend %>%
      filter(dataset %in% input$time_trend_dataset
             & geography1 %in% input$time_trend_location_type 
             & geography2 %in% input$time_trend_location
             & diagnosis_groupings %in% input$time_trend_diagnoses 
             & measure %in% input$time_trend_measure_type) %>%
      droplevels
  })
  
  #Create the line chart for the time trend tab.
  #We create this using the plotly library.
  #Plotly graphs are a lot more interactive compared to ggplot2 graphs.
  
  output$time_trend_plot <- renderPlotly({
    
    ### 3 ---
    
    #Insert various IF statements, in case the user makes a combination of...
    #selections that isn't valid.
    
    #If the user selects psychiatric/total specialties + State Hospital +...
    #crude rates, there needs to be a message saying that no crude rates are...
    #available for State Hospital.
    
    if ((input$time_trend_dataset == "Psychiatric" | 
         input$time_trend_dataset == "Total") & 
        any(input$time_trend_location == 
            "The State Hospitals Board for Scotland") &
        (input$time_trend_measure_type == 
         "Rate of patients (per 100,000 population)" |
         input$time_trend_measure_type == 
         "Rate of discharges (per 100,000 population)" | 
         input$time_trend_measure_type == 
         "Rate of hospital residents (per 100,000 population)")) 
      
    { 
      
      #This is the message we are using.
      
      text_state_hosp_crude_rates <- list(
        x = 5, 
        y = 2,
        font = list(color = "#0072B2", size = 20),
        text = 
          "No rates available for location of treatment 'State Hospital'.", 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      ) 
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_state_hosp_crude_rates, 
               yaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE), 
               xaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE)) %>%  
        config(displayModeBar = FALSE,
               displaylogo = F, collaborate = F, editable = F) 
      
    }
    
    #If the user selects non-psychiatric specialties + State Hospital, there...
    #needs to be a message saying that State Hospital is an invalid...
    #selection for non-psychiatric specialties.
    
    else if (input$time_trend_dataset == "Non-psychiatric" & 
             any(input$time_trend_location == 
                 "The State Hospitals Board for Scotland"))
      
    { 
      
      #This is the message we are using.
      
      text_state_hosp_non_psych_spec <- list(
        x = 5, 
        y = 2,
        font = list(color = "#0072B2", size = 20),
        text = paste0(
          "'State Hospital' is not a valid selection for",
          "<br>",
          "non-psychiatric specialties, as it is a psychiatric hospital."
        ), 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      ) 
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_state_hosp_non_psych_spec, 
               yaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE), 
               xaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE)) %>%  
        config(displayModeBar = FALSE,
               displaylogo = F, collaborate = F, editable = F) 
      
    }
    
    #If the user selects psychiatric specialties and then either Orkney or...
    #Shetland, we need to clarify that these boards have no psychiatric...
    #facilities.
    
    else if (input$time_trend_dataset == "Psychiatric" & 
             any(input$time_trend_location == "NHS Shetland" | 
                 input$time_trend_location == "NHS Orkney"))
      
    { 
      
      #This is the message we are using.
      
      text_orkn_shetl_psych_spec <- list(
        x = 5, 
        y = 2,
        font = list(color = "#0072B2", size = 20),
        text = 
          "NHS Orkney and NHS Shetland have no psychiatric hospitals within their area.", 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      ) 
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_orkn_shetl_psych_spec, 
               yaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE), 
               xaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE)) %>%  
        config(displayModeBar = FALSE,
               displaylogo = F, collaborate = F, editable = F) 
      
    }
    
    #If the user selects 'Total' specialty + any of the hospital residents...
    #measures + NHS Orkney/NHS Shetland, we display a message that says it...
    #was not possible to calculate this (a) because these boards have no...
    #psychiatric hospitals and (b) because hospital residents have not...
    #been calculated for non-psychiatric specialties.
    
    else if (input$time_trend_dataset == "Total" & 
             any(input$time_trend_location == "NHS Shetland" | 
                 input$time_trend_location == "NHS Orkney") & 
             (input$time_trend_measure_type == "Number of hospital residents" |
              input$time_trend_measure_type == 
              "Rate of hospital residents (per 100,000 population)"))
      
    { 
      
      #This is the message we are using.
      
      text_orkn_shetl_total_spec_SH <- list(
        x = 5, 
        y = 2,
        font = list(color = "#0072B2", size = 20),
        text = paste0("Because NHS Orkney and NHS Shetland have no psychiatric hospitals within their area,",
                      "<br>",
                      "and because hospital residents is not a valid measure for non-psychiatric specialties,",
                      "<br>",
                      "the total number or rate of hospital residents could not be calculated."), 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      ) 
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_orkn_shetl_total_spec_SH, 
               yaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE), 
               xaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE)) %>%  
        config(displayModeBar = FALSE,
               displaylogo = F, collaborate = F, editable = F) 
      
    }
    
    #If the user selects non-psychiatric specialties + hospital...
    #residents, we need to clarify that this type of measure is meaningless...
    #for non-psychiatric specialties.
    
    else if (input$time_trend_dataset == "Non-psychiatric" & 
             (input$time_trend_measure_type == "Number of hospital residents" |
              input$time_trend_measure_type == 
              "Rate of hospital residents (per 100,000 population)"))
      
    { 
      
      #This is the message we are using.
      
      text_hosp_res_non_psych_spec <- list(
        x = 5, 
        y = 2,
        font = list(color = "#0072B2", size = 20),
        text = 
          "'Hospital residents' is not a valid measure for non-psychiatric specialties.", 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      ) 
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_hosp_res_non_psych_spec, 
               yaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE), 
               xaxis = list(showline = FALSE, 
                            showticklabels = FALSE, 
                            showgrid = FALSE)) %>%  
        config(displayModeBar = FALSE,
               displaylogo = F, collaborate = F, editable = F) 
      
    }
    
    #If the user makes a combination of selections that leads to zero...
    #patients/discharges/residents, insert a message saying zero patients/...
    #discharges/residents found.
    #Otherwise, the user might see an empty graph and mistakenly think there...
    #is something wrong with it.
    
    else if (sum(time_trend_new()$value) == 0 & 
             !is.na(sum(time_trend_new()$value)) &
             input$time_trend_location != "") 
      
    { 
      
      #This is the message we are using.
      
      text_no_patients_found <- list(
        x = 5, 
        y = 2, 
        font = list(color = "#0072B2", size = 20),
        text = paste0("No ", 
                      if (input$time_trend_measure_type == 
                          "Number of discharges" |
                          input$time_trend_measure_type == 
                          "Rate of discharges (per 100,000 population)")
                      { print(c("discharges ")) }
                      else if (input$time_trend_measure_type == 
                               "Number of patients" |
                               input$time_trend_measure_type == 
                               "Rate of patients (per 100,000 population)") 
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
      
      ### 4 ---
      
      #Create the tooltip, i.e., insert the information that appears when you...
      #hover your mouse over a dot in the line graph.
      #E.g.:
      #"Treatment specialty: Non-psychiatric"
      #"Financial year: 2018/2019" 
      #"Location of treatment: Scotland"
      #"Diagnosis grouping: Disorders of adult behaviour and personality"
      #"Number of discharges: XXXX"
      
      tooltip_time_trend <- paste0("Treatment specialty: ",
                                   time_trend_new()$dataset, "<br>",
                                   "Financial year: ", 
                                   time_trend_new()$year, "<br>",
                                   "Location of treatment: ", 
                                   time_trend_new()$geography2, "<br>",
                                   "Diagnosis grouping: ", 
                                   time_trend_new()$diagnosis_groupings, "<br>",
                                   input$time_trend_measure_type, ": ", 
                                   time_trend_new()$value)
      
      #Create the main body of the chart.
      
      plot_ly(data = time_trend_new(), 
              
              ### 5 ---
              
              #Select your variables.
              
              x = ~year, y = ~value, color = ~geography2,
              
              ### 6 ---
              
              #Assign a colour to each location of treatment.
              #The user can select a maximum of four health boards, so we...
              #need four colours.
              #We will be using only two colours, but we can make the lines...
              #distinguishable in other ways, i.e., with symbols and different...
              #types of lines (see below).
              #Both colours here are shades of blue, for accessibility...
              #purposes. 
              
              colors = c("#4CBEED", "#004785", "#4CBEED", "#004785"),
              
              text = tooltip_time_trend, hoverinfo = "text",  
              
              ### 7 ---
              
              #Select the type of chart you want, in this case a scatter...
              #plot, but set the mode to 'lines+markers' in order to...
              #transform it into a line chart.
              
              type = 'scatter', mode = 'lines+markers',
              
              ### 8 ---
              
              #Select the width of the lines, and set the type of line for...
              #each health board.
              #Some line types will be repeated, but we will also be using...
              #symbols (see below), so in the end, it will be clear which...
              #line corresponds to which HB.
              
              line = list(width = 3),
              linetype = ~geography2,
              linetypes = c("solid", "solid", "dot", "dot"),
              
              ### 9 ---
              
              #Select symbols for the health boards, and set their size.
              
              symbol = ~geography2, 
              symbols = c("square-open", "x", "circle", "diamond"),
              marker = list(size = 12),
              
              ### 10 ---
              
              #Set the width and height of the graph.
              #The "name" attribute tells plotly which variable to base the...
              #legend labels on. We use a string wrapper, so that only 10...
              #characters are visualised per line. This is meant to deal with...
              #boards with long names, like NHS Greater Glasgow & Clyde.
              
              width = 1000, height = 600, 
              name = ~str_wrap(geography2, 10)) %>%
        
        ### 11 ---
        
        #Insert the title of the graph.
        #Make the graph title reactive.
        
        layout(title = 
                 paste0(
                   "<b>", 
                   input$time_trend_measure_type,
                   if (input$time_trend_measure_type == 
                       "Number of discharges" | 
                       input$time_trend_measure_type == 
                       "Rate of discharges (per 100,000 population)") 
                   { paste0(" from") }
                   else { paste0(" in") },
                   if(input$time_trend_dataset == "Psychiatric") 
                   {paste0(" psychiatric specialties")}
                   else if(input$time_trend_dataset == "Non-psychiatric") 
                   {paste0(" non-psychiatric specialties")}
                   else {paste0(" any treatment specialty")},
                   " with main diagnosis", 
                   "<br>", 
                   "'", input$time_trend_diagnoses, "',", 
                   "<br>",
                   first(as.vector(time_trend_new()$year)), 
                   " - ", 
                   last(as.vector(time_trend_new()$year)),
                   ", by financial year and location of treatment",
                   "<br>",
                   "</b>"
                 ),
               
               separators = ".",
               
               ### 12 ---
               
               #We need to fix the range of the y axis, as R refuses to set...
               #the lower end of this axis to zero.
               #The following "range" command fixes the lower end to...
               #zero, and calculates the upper end as the maximum...
               #number visualised in the graph + 10% of this number.
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(time_trend_new()$value, na.rm = TRUE) + 
                             (max(time_trend_new()$value, na.rm = TRUE) 
                              * 10 / 100)), 
                 
                 ### 13 ---
                 
                 #Insert the titles of the axes.
                 
                 #Wrap the y axis title in blank spaces so it doesn't...
                 #overlap with the y axis tick labels.
                 #Also, make the y axis title reactive.
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   if (input$time_trend_measure_type == 
                       "Rate of discharges (per 100,000 population)")
                   { print(c("Rate of discharges")) }
                   else if (input$time_trend_measure_type == 
                            "Rate of patients (per 100,000 population)")
                   { print(c("Rate of patients")) }
                   else if (input$time_trend_measure_type == 
                            "Rate of hospital residents (per 100,000 population)")
                   { print(c("Rate of hospital residents")) }
                   else { print(c(input$time_trend_measure_type)) }, 
                   rep("&nbsp;", 20),
                   rep("\n&nbsp;", 3)
                 ), 
                 collapse = ""),
                 showline = TRUE, 
                 ticks = "outside"
                 
               ),
               
               #Set the x axis tick angle to minus 45. It's the only way for...
               #the x axis tick labels (fin. years) to display without...
               #overlapping with each other.
               #Also, wrap the x axis title in blank spaces so it doesn't...
               #overlap with the x axis tick labels.
               
               xaxis = list(tickangle = -45, 
                            title = paste0(c(rep("&nbsp;", 20),
                                             "<br>",
                                             "<br>",
                                             "Financial year",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""),
                            showline = TRUE, 
                            ticks = "outside"),
               
               ### 14 ---
               
               #Fix the margins so that the graph and axis titles have enough...
               #room to display nicely.
               
               margin = list(l = 90, r = 60, b = 170, t = 90),
               
               ### 15 ---
               
               #Set the font sizes.
               
               font = list(size = 13),
               titlefont = list(size = 15),
               
               ### 16 ---
               
               #Insert a legend so that the user knows which colour, line type...
               #and symbol corresponds to which location of treatment.
               #Make the legend background and legend border white.              
               
               showlegend = TRUE,
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        ### 17 ---
        
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
  
  ### 18 ---
  
  #This reactive() creates the subset that will be used to populate the table...
  #appearing under the line chart.
  #Select the columns you need for the table, and filter out NAs.
  
  table_time_trend <- reactive({
    time_trend %>% 
      filter(dataset %in% input$time_trend_dataset 
             & geography1 %in% input$time_trend_location_type 
             & geography2 %in% input$time_trend_location
             & diagnosis_groupings %in% input$time_trend_diagnoses 
             & measure %in% input$time_trend_measure_type) %>%
      select(dataset, year, geography1, geography2, diagnosis_groupings, 
             value) %>%
      filter(complete.cases(.))
  })
  
  ### 19 ---
  
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
              colnames = c("Treatment specialty", "Financial year", 
                           "Type of location", 
                           "Location", "Diagnosis grouping", 
                           input$time_trend_measure_type))
  })
  
  ### 20 ---
  
  #We also create a download button which the user can use to download the...
  #table in CSV format.
  
  output$download_time_trend <- downloadHandler(
    filename = 'diagnosis_data.csv',
    content = function(file) {
      write.table(table_time_trend(), 
                  file,
                  
                  #Remove row numbers as the CSV file already has row numbers.
                  
                  row.names = FALSE,
                  col.names = c("Treatment specialty", "Financial year", 
                                "Type of location", "Location", 
                                "Diagnosis grouping", 
                                input$time_trend_measure_type), 
                  sep = ",")
    }
  )
  
  ##############################################.             
  ############## Geography ----   
  ##############################################.
  
  ### 1 ---
  
  #Merge the shapefile with the file containing the crude rates.
  #Use the column holding the council area names to do this.
  
  ca_data <- sp::merge(CA_smaller, 
                       geography, 
                       by.x = "NAME", 
                       by.y = "ca", 
                       duplicateGeoms = TRUE)
  
  ### 2 ---
  
  #Create a subset of the dataset which looks at rate of patients in...
  #psychiatric specialties in 2018/2019.
  #This subset will be visualised on the map on initiation.
  #And then, whenever the user makes a new selection, this initial map will...
  #be overwritten by new polygons, using the command leafletProxy().
  #leafletProxy() is great as the map will not need to be redrawn from...
  #scratch every time the user makes a new selection (see below).
  
  ca_data_init_selection <- subset(ca_data,
                                   dataset == "Psychiatric"  
                                   & fyear == "2018/2019"  
                                   & measure == "Rate of patients (per 100,000 population)")
  
  output$mymap <- renderLeaflet({
    
    ### 3 ---
    
    #Create the initial map.
    
    leaflet() %>% 
      
      #Choose a map provider.
      
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      
      #Set the zoom level on initiation.
      
      setView(lng = -3.9031, lat = 57.8772, zoom = 6) %>%
      
      #Use the polygons from the subsetted dataset above.
      
      addPolygons(data = ca_data_init_selection,
                  stroke = TRUE,
                  weight = 2, 
                  smoothFactor = 0.5, 
                  opacity = 1, 
                  fillOpacity = 1, 
                  highlightOptions = highlightOptions(color = "#000000", 
                                                      weight = 5),
                  color = "#000000", 
                  
                  #Colour the council areas according to the crude rate of...
                  #patients they have.
                  #Light blue for below average and dark blue for the...
                  #extreme values.
                  
                  fillColor = colorNumeric(palette = "Blues", 
                                           domain = ca_data_init_selection$Value)(ca_data_init_selection$Value),
                  
                  #Create the tooltip, i.e., the information that the users...
                  #see when they click on a council area on the map.
                  
                  popup = paste0("Council area of residence: ",
                                 ca_data_init_selection$NAME,
                                 "<br>", 
                                 "Treatment specialty: ",
                                 ca_data_init_selection$dataset,
                                 "<br>", 
                                 "Financial year: ",
                                 ca_data_init_selection$fyear,
                                 "<br>",
                                 "Rate of patients (per 100,000 population): ",
                                 ca_data_init_selection$Value)) %>%
      
      ### 4 ---
      
      #Add a dynamic legend.
      
      addLegend(position = "topright", 
                
                #The palette and the values are based on rate of patients,...
                #much like the colours of the council areas above.
                
                pal = colorNumeric("Blues", ca_data_init_selection$Value), 
                values = ca_data_init_selection$Value,
                
                #Legend title.
                
                title = paste0("Rate of patients", 
                               "<br>", 
                               "(per 100,000 population)",
                               "<br>",
                               "in psychiatric specialties", 
                               "<br>",
                               "in ",
                               first(as.vector(ca_data_init_selection$fyear)), 
                               ", by council area",
                               "<br>",
                               "of residence"
                ),
                opacity = 1)
    
  })
  
  ### 5 ---
  
  #Open an observer so that leafletProxy() can react to the user's selections.
  
  observe({
    
    ### 6 ---
    
    #Create a new subset of the dataset according to the user's selections...
    #beyond the initial map.
    
    ca_data_new <- subset(ca_data,
                          dataset == input$geography_datasets  
                          & fyear == input$geography_financial_years  
                          & measure == input$geography_measure_type)
    
    ### 7 ---
    
    #leafletProxy() will now build upon/overwrite our initial map.
    
    leafletProxy("mymap", session) %>%
      
      #Clear all previous polygons.
      
      clearShapes() %>%
      
      #Clear the previous legend.
      
      clearControls() %>%
      
      #Add the new polygons and tooltip, which will have the same format as...
      #above.
      
      addPolygons(data = ca_data_new,
                  stroke = TRUE,
                  weight = 2, 
                  smoothFactor = 0.5, 
                  opacity = 1, 
                  fillOpacity = 1, 
                  highlightOptions = highlightOptions(color = "#000000", 
                                                      weight = 5),
                  color = "#000000", 
                  fillColor = colorNumeric(palette = "Blues", 
                                           domain = ca_data_new$Value)(ca_data_new$Value),
                  popup = paste0("Council area of residence: ",
                                 ca_data_new$NAME,
                                 "<br>", 
                                 "Treatment specialty: ",
                                 ca_data_new$dataset,
                                 "<br>", 
                                 "Financial year: ",
                                 ca_data_new$fyear,
                                 "<br>",
                                 input$geography_measure_type, 
                                 ": ",
                                 ca_data_new$Value)) %>%
      
      #Add the new legend, which, again, will have the same format as the...
      #legend of the initial map.
      #We are now done with the map.
      
      addLegend(position = "topright", 
                pal = colorNumeric("Blues", ca_data_new$Value), 
                values = ca_data_new$Value,
                title = paste0(
                  if (input$geography_measure_type == 
                      "Rate of discharges (per 100,000 population)")
                  { paste0("Rate of discharges",
                           "<br>", 
                           "(per 100,000 population)",
                           "<br>",
                           "from ") }
                  else { paste0("Rate of patients",
                                "<br>", 
                                "(per 100,000 population)",
                                "<br>",
                                "in ") },
                  if (input$geography_datasets == "Psychiatric") 
                  { paste0("psychiatric specialties",
                           "<br>",
                           "in ", 
                           first(as.vector(ca_data_new$fyear)), 
                           ", by council area",
                           "<br>",
                           "of residence") }
                  else if (input$geography_datasets == "Non-psychiatric") 
                  { paste0("non-psychiatric specialties",
                           "<br>",
                           "in ",
                           first(as.vector(ca_data_new$fyear)), 
                           ", by council area",
                           "<br>",
                           "of residence") }
                  else { paste0("any treatment specialty",
                                "<br>",
                                "in ",
                                first(as.vector(ca_data_new$fyear)), 
                                ", by council area",
                                "<br>",
                                "of residence") }
                ),
                opacity = 1) 
    
  })
  
  ### 8 ---
  
  #This reactive() creates the subset that will be used to populate the table...
  #appearing under the map.
  #Select the columns you need for the table.
  
  table_geography <- reactive({
    geography %>% 
      filter(dataset %in% input$geography_datasets 
             & fyear %in% input$geography_financial_years 
             & measure %in% input$geography_measure_type) %>%
      select(dataset, fyear, ca, Value)
  })
  
  ### 9 ---
  
  #We now create the table that will go under the map.
  #We give the columns clearer names.
  #The name of the last column, which is the measure column, changes...
  #according to the user's input in the filter SELECT MEASURE.
  
  output$geography_table <- renderDataTable({
    datatable(table_geography(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'),
              colnames = c("Treatment specialty", "Financial year", 
                           "Council area of residence", 
                           input$geography_measure_type))
  })
  
  ### 10 ---
  
  #We also create a download button which the user can use to download the...
  #geography table in CSV format.
  
  output$download_geography <- downloadHandler(
    filename = 'geography_data.csv',
    content = function(file) {
      write.table(table_geography(), 
                  file,
                  
                  #Remove row numbers as the CSV file already has row numbers.
                  
                  row.names = FALSE,
                  col.names = c("Treatment specialty", "Financial year",
                                "Council area of residence", 
                                input$geography_measure_type), 
                  sep = ",")
    }
  )
  
  ##############################################.             
  ############## Age/sex ----   
  ##############################################.
  
  ### 1 ---
  
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
  
  ### 2 ---
  
  #There are five filters in total: SELECT TREATMENT SPECIALTY, SELECT TYPE...
  #OF LOCATION, SELECT LOCATION, SELECT FINANCIAL YEAR, and SELECT MEASURE.
  #The reactive() command below creates a subset of the age/sex...
  #dataset based on the user's selections in these five filters.
  #This subset will then "feed" the age/sex pyramid.
  
  age_sex_new <- reactive({
    age_sex %>% 
      filter(dataset %in% input$age_sex_dataset 
             & geography1 %in% input$age_sex_location_type 
             & geography2 %in% input$age_sex_location 
             & year %in% input$age_sex_financial_year 
             & measure %in% input$age_sex_measure_type)
  })
  
  #Create the pyramid chart for the age/sex tab.
  #We create this using the plotly library.
  
  output$age_sex_pyramid <- renderPlotly({
    
    ### 3 ---
    
    #Insert various IF statements, in case the user makes a combination of...
    #selections that isn't valid.
    
    #If the user makes a combination of selections that leads to zero...
    #patients or discharges, there needs to be a message saying no patients/...
    #discharges found. Otherwise the user will see an empty graph and...
    #mistakenly think the app is not working.
    
    if (sum(abs(age_sex_new()$value)) == 0 & 
        !is.na(sum(abs(age_sex_new()$value)))) 
      
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
                          "Rate of discharges (per 100,000 population)")
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
    #location 'Other'.
    
    else if (input$age_sex_location == "Other" &  
             (input$age_sex_measure_type == 
              "Rate of patients (per 100,000 population)" |
              input$age_sex_measure_type == 
              "Rate of discharges (per 100,000 population)"))
      
    {
      
      #This is the message we are using.
      
      text_other <- list(
        x = 5, 
        y = 2, 
        font = list(color = "#0072B2", size = 20),
        text = "No rates available for location of residence 'Other'.", 
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
      
      ### 4 ---
      
      #Create the tooltip, i.e., insert the information that appears when...
      #you hover your mouse over a bar in the pyramid.
      #E.g.:
      #"Treatment specialty: Psychiatric"
      #"Financial year: 2018/2019"
      #"Location of residence: Scotland"
      #"Age group: Ages 65+"
      #"Sex: Female" 
      #"Number of discharges: XXXX"
      
      tooltip_age_sex <- paste0("Treatment specialty: ", 
                                age_sex_new()$dataset, 
                                "<br>",
                                "Financial year: ", 
                                age_sex_new()$year,
                                "<br>",
                                "Location of residence: ", 
                                age_sex_new()$geography2,
                                "<br>",
                                "Age group: ", 
                                age_sex_new()$ageband, 
                                "<br>",
                                "Sex: ", 
                                age_sex_new()$sex_char, 
                                "<br>",
                                input$age_sex_measure_type, ": ", 
                                abs(age_sex_new()$value))
      
      #Create the main body of the chart.
      
      plot_ly(data = age_sex_new(),
              
              ### 5 ---
              
              #Select your variables.
              
              x = ~value, y = ~ageband, color = ~sex_char,
              
              ### 6 ---
              
              #Colour palette:
              #Dark blue for males and light blue for females.
              
              colors = c("#004785", "#4CBEED"),
              
              text = tooltip_age_sex, hoverinfo = "text", 
              
              ### 7 ---
              
              #Select the type of chart you want, in this case a bar chart,...
              #and set the orientation to horizontal to achieve the...
              #"pyramid" look. Set the width and height of the graph.
              
              type = 'bar', orientation = 'h',
              width = 1000, height = 400) %>%
        
        ### 8 ---
        
        #Insert the title of the graph.
        #Make the graph title reactive.
        
        layout(title = paste0("<b>", 
                              input$age_sex_measure_type,
                              if (input$age_sex_measure_type == 
                                  "Number of discharges" | 
                                  input$age_sex_measure_type == 
                                  "Rate of discharges (per 100,000 population)") 
                              { paste0(" from") }
                              else { paste0(" in") },
                              if (input$age_sex_dataset == "Psychiatric") 
                              { paste0(" psychiatric specialties") }
                              else if (input$age_sex_dataset == "Non-psychiatric") 
                              { paste0(" non-psychiatric specialties") }
                              else { paste0(" any treatment specialty") },
                              " in ", 
                              input$age_sex_financial_year, 
                              ",", 
                              "<br>",
                              "by age group and sex, ",
                              if (input$age_sex_location == "Other") 
                              { paste0("residents outwith Scotland or with no fixed abode") }
                              else { paste0("residents of ", 
                                            input$age_sex_location) },
                              "</b>"),
               
               separators = ".",
               
               ### 9 ---
               
               #Set the gap size between bars, and make sure that the bars...
               #overlay, again to achieve that "pyramid" look.
               
               bargap = 0.2, barmode = 'overlay',
               
               ### 10 ---
               
               #Insert the y axis title.
               #Wrap the y axis title in empty spaces so it doesn't...
               #overlap with the y axis tick labels.
               
               yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                             "Age group",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""), 
                            showline = TRUE, ticks = "outside"),
               
               xaxis = list(
                 
                 exponentformat = "none",
                 separatethousands = TRUE,
                 tickmode = 'array',
                 
                 ### 11 ---
                 
                 #Define the range of the x axis.
                 #This is recalculated every time the user makes a new selection.
                 
                 range = c(
                   -round(max(abs(age_sex_new()$value)) * 110 / 100),
                   round(max(abs(age_sex_new()$value)) * 110 / 100)
                 ),
                 tickangle = 0,
                 
                 ### 12 ---
                 
                 #Insert breaks and labels for the x axis.
                 #These are recalculated with every new selection.
                 
                 tickvals = c(
                   -round(max(abs(age_sex_new()$value))),
                   -round(max(abs(age_sex_new()$value)) * 66 / 100),
                   -round(max(abs(age_sex_new()$value)) * 33 / 100), 
                   0, 
                   round(max(abs(age_sex_new()$value)) * 33 / 100), 
                   round(max(abs(age_sex_new()$value)) * 66 / 100), 
                   round(max(abs(age_sex_new()$value)))
                 ), 
                 ticktext = paste0(
                   as.character(c(
                     round(max(abs(age_sex_new()$value))),
                     round(max(abs(age_sex_new()$value)) * 66 / 100),
                     round(max(abs(age_sex_new()$value)) * 33 / 100), 
                     0, 
                     round(max(abs(age_sex_new()$value)) * 33 / 100), 
                     round(max(abs(age_sex_new()$value)) * 66 / 100),
                     round(max(abs(age_sex_new()$value)))
                   ))
                 ),
                 
                 ### 13 ---
                 
                 #Write the title for the x axis.
                 #Make the x axis title reactive.
                 
                 title = 
                   if (input$age_sex_measure_type == 
                       "Rate of discharges (per 100,000 population)")
                   { print(c("Rate of discharges")) }
                 else if (input$age_sex_measure_type == 
                          "Rate of patients (per 100,000 population)")
                 { print(c("Rate of patients")) }
                 else { print(c(input$age_sex_measure_type)) },
                 
                 showline = TRUE, 
                 ticks = "outside"
                 
               ),
               
               ### 14 ---
               
               #Fix the margins so that the graph and axis titles have...
               #enough room to display nicely.
               
               margin = list(l = 140, r = 10, b = 70, t = 90),
               
               ### 15 ---
               
               #Set the font sizes.
               
               font = list(size = 13),
               titlefont = list(size = 15),
               
               ### 16 ---
               
               #Insert a legend so that the user knows which colour...
               #corresponds to which sex.
               #Make the legend background and legend border white.              
               
               showlegend = TRUE, 
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        ### 17 ---
        
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
  
  ### 18 ---
  
  #This reactive() creates the subset that will be used to populate the table...
  #appearing under the pyramid.
  
  table_age_sex <- reactive({
    age_sex %>% 
      filter(dataset %in% input$age_sex_dataset
             & geography1 %in% input$age_sex_location_type
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
      
      #Select the columns you need for the table, and filter out NAs.
      
      select(dataset, year, geography1, geography2, ageband, sex_char, 
             numbers_v2) %>%
      filter(complete.cases(.)) 
  })
  
  ### 19 ---
  
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
              colnames = c("Treatment specialty", "Financial year", 
                           "Type of location", "Location", "Age group", "Sex", 
                           input$age_sex_measure_type))
  })
  
  ### 20 ---
  
  #Create a download button that allows the user to download the table...
  #in CSV format. 
  
  output$download_age_sex <- downloadHandler(
    filename = 'age_sex_data.csv',
    content = function(file) {
      write.table(table_age_sex(), 
                  file, 
                  row.names = FALSE, 
                  col.names = c("Treatment specialty", "Financial year", 
                                "Type of location", "Location", "Age group", 
                                "Sex", input$age_sex_measure_type), 
                  sep = ",")
    }
  )
  
  ##############################################.             
  ############## Deprivation ----   
  ##############################################.
  
  #As mentioned above, the Deprivation tab will contain two graphs:
  #The first one will show activity broken down by deprivation quintile.
  #The second one will display the RII as a trend over time.
  #The first graph will be a bar chart, with each bar representing a quintile.
  #The second graph will be a line chart.
  
  #We start with the bar chart (numbers 1 to 18).
  
  ### 1 ---
  
  #The user's selection in the filter SELECT TYPE OF LOCATION determines...
  #which choices appear in the filter SELECT LOCATION.
  #The renderUI() command makes the process of creating these two dynamic... 
  #filters very easy.
  #In the second filter, we set multiple to TRUE, as we want the user to be...
  #able to compare multiple health boards at the same time. However, we set...
  #the limit to four selections.
  
  output$deprivation_location_types <- renderUI({
    shinyWidgets::pickerInput("deprivation_location_type", 
                              label = "Select type of location", 
                              choices = depr_location_types, 
                              selected = "Scotland")
  })
  
  output$deprivation_locations <- renderUI({
    shinyWidgets::pickerInput("deprivation_location", 
                              label = 
                                "Select location (up to four selections allowed)", 
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
                              width = '100%',
                              options = list(
                                "max-options" = 4,
                                `selected-text-format` = "count > 1"
                              ),
                              selected = if(input$deprivation_location_type 
                                            == "Scotland")
                              { print("Scotland") } 
                              else 
                              { print("NHS Ayrshire & Arran") }
    )
  })
  
  ### 2 ---
  
  #There are five filters in total: SELECT TREATMENT SPECIALTY, SELECT TYPE...
  #OF LOCATION, SELECT LOCATION, SELECT FINANCIAL YEAR, and SELECT MEASURE.
  #The reactive() command below creates a subset of the deprivation...
  #dataset based on the user's selections in these five filters.
  #This subset will then "feed" the deprivation bar chart.
  #Don't forget to drop the unused levels in your factors. This is essential...
  #because, in the graph, we will be matching colours to factor levels.
  
  deprivation_new <- reactive({
    deprivation %>%
      filter(dataset %in% input$deprivation_dataset 
             & geography1 %in% input$deprivation_location_type 
             & geography2 %in% input$deprivation_location 
             & year %in% input$deprivation_financial_year 
             & measure %in% input$deprivation_measure_type) %>%
      droplevels
  })
  
  #Create the bar chart for the deprivation tab.
  #We create this using the plotly library.
  
  output$deprivation_bar_chart <- renderPlotly({
    
    ### 3 ---
    
    #Insert various IF statements, in case the user makes a combination of...
    #selections that isn't valid.
    
    #If the user makes a combination of selections that leads to zero...
    #patients/discharges/hospital residents, there needs to be a message...
    #saying no patients/discharges/residents found. Otherwise the user will...
    #see an empty graph and mistakenly think the app is not working.
    
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
                          "Rate of discharges (per 100,000 population)")
                      { print(c("discharges ")) }
                      else if (input$deprivation_measure_type == 
                               "Number of patients" |
                               input$deprivation_measure_type == 
                               "Rate of patients (per 100,000 population)") 
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
    
    #If the user selects non-psychiatric specialties + any of the 'hospital...
    #residents' measures, there needs to be a message saying that this is not...
    #a valid measure for non-psychiatric specialties.
    
    else if (input$deprivation_dataset == "Non-psychiatric" & 
             (input$deprivation_measure_type == 
              "Rate of hospital residents (per 100,000 population)" |
              input$deprivation_measure_type == 
              "Number of hospital residents"))
      
    { 
      
      #This is the message we are using.
      
      text_hosp_res_non_psych_spec_bar_chart <- list(
        x = 5, 
        y = 2,
        font = list(color = "#0072B2", size = 20),
        text = 
          "'Hospital residents' is not a valid measure for non-psychiatric specialties.", 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      ) 
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_hosp_res_non_psych_spec_bar_chart, 
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
      
      ### 4 ---
      
      #Create the tooltip, i.e., insert the information that appears when the...
      #user hovers his/her mouse over a bar in the first graph.
      #E.g.: 
      #"Treatment specialty: Non-psychiatric"
      #"Financial year: 2018/2019"
      #"Location of residence: Scotland"
      #"Deprivation quintile (SIMD): 2"
      #"Number of discharges: XXXX"
      
      tooltip_deprivation <- paste0("Treatment specialty: ",
                                    deprivation_new()$dataset,
                                    "<br>",
                                    "Financial year: ",
                                    deprivation_new()$year, 
                                    "<br>",
                                    "Location of residence: ", 
                                    deprivation_new()$geography2, 
                                    "<br>",
                                    "Deprivation quintile (SIMD): ", 
                                    deprivation_new()$simd, 
                                    "<br>",
                                    input$deprivation_measure_type, ": ",
                                    deprivation_new()$value)
      
      #Create the main body of the bar chart.
      
      plot_ly(data = deprivation_new(),
              
              ### 5 ---
              
              #Select your variables.
              
              x = ~simd, y = ~value, color = ~geography2, 
              
              ### 6 ---
              
              #Assign a colour to each location of residence.
              #The user can select a maximum of four health boards, so we...
              #need four colours.
              #All the colours here are shades of blue, for accessibility...
              #purposes. 
              
              colors = c("#004785", "#4C7EA9", "#00A2E5", "#99DAF5"),
              
              ### 7 ---
              
              #Select the type of chart you want, in this case a bar chart.
              
              type = 'bar',
              
              text = tooltip_deprivation, hoverinfo = "text",
              
              ### 8 ---
              
              #Set the width and height of the graph.
              #The "name" attribute tells plotly which variable to base the...
              #legend labels on. We use a string wrapper, so that only 10...
              #characters are visualised per line. This is meant to deal with...
              #boards with long names, like NHS Greater Glasgow & Clyde.
              
              width = 1000, height = 450, 
              name = ~str_wrap(geography2, 20)) %>%
        
        ### 9 ---
        
        #Insert the graph title.
        #Make the graph title reactive.
        
        layout(title = paste0("<b>",
                              input$deprivation_measure_type,
                              if (input$deprivation_measure_type == 
                                  "Number of discharges" | 
                                  input$deprivation_measure_type == 
                                  "Rate of discharges (per 100,000 population)") 
                              { paste0(" from") }
                              else { paste0(" in") },
                              if (input$deprivation_dataset == "Psychiatric") 
                              { paste0(" psychiatric specialties") }
                              else if (input$deprivation_dataset == 
                                       "Non-psychiatric") 
                              { paste0(" non-psychiatric specialties") }
                              else { paste0(" any treatment specialty") },
                              "<br>",
                              "in ",
                              input$deprivation_financial_year,
                              ", by deprivation quintile and ", 
                              "location of residence",
                              "</b>"),
               
               separators = ".",
               
               ### 10 ---
               
               #We need to fix the range of the y axis, as R refuses to set...
               #the lower end of this axis to zero.
               #The following "range" command fixes the lower end to...
               #zero, and calculates the upper end as the maximum...
               #number visualised in the graph + 10% of this number.
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(deprivation_new()$value, na.rm = TRUE) + 
                             (max(deprivation_new()$value, na.rm = TRUE) 
                              * 10 / 100)),
                 
                 ### 11 ---
                 
                 #Write the titles for the axes.
                 
                 #Wrap the y axis title in blank spaces so it...
                 #doesn't overlap with the y axis tick labels.
                 #Also, make the y axis title reactive.
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   if (input$deprivation_measure_type == 
                       "Rate of discharges (per 100,000 population)")
                   { print(c("Rate of discharges")) }
                   else if (input$deprivation_measure_type == 
                            "Rate of patients (per 100,000 population)")
                   { print(c("Rate of patients")) }
                   else if (input$deprivation_measure_type == 
                            "Rate of hospital residents (per 100,000 population)")
                   { print(c("Rate of hospital residents")) }
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
               
               ### 12 ---
               
               #Fix the margins so that the graph and axis titles have enough...
               #room to display nicely.
               
               margin = list(l = 80, r = 10, b = 50, t = 90),
               
               ### 13 ---
               
               #Set the font sizes.
               
               font = list(size = 13),
               titlefont = list(size = 15),
               
               ### 14 ---
               
               #Insert a legend so that the user knows which colour...
               #corresponds to which location of residence.
               #Make the legend background and legend border white.             
               
               showlegend = TRUE, 
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        ### 15 ---
        
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
  
  ### 16 ---
  
  #This reactive() creates the subset that will be used to populate the table...
  #appearing under the bar chart.
  #Select the columns you need for the table and filter out NAs.
  
  table_deprivation <- reactive({
    deprivation %>% 
      filter(dataset %in% input$deprivation_dataset 
             & geography1 %in% input$deprivation_location_type 
             & geography2 %in% input$deprivation_location 
             & year %in% input$deprivation_financial_year 
             & measure %in% input$deprivation_measure_type) %>% 
      select(dataset, year, geography1, geography2, simd, value) %>%
      filter(complete.cases(.))
  })
  
  ### 17 ---
  
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
              colnames = c("Treatment specialty", "Financial year", 
                           "Type of location", "Location", 
                           "Deprivation quintile (SIMD)", 
                           input$deprivation_measure_type))
  })
  
  ### 18 ---
  
  #Create a download button that allows the user to download the table...
  #in CSV format.
  
  output$download_deprivation <- downloadHandler(
    filename = 'deprivation_data.csv',
    content = function(file) {
      write.table(table_deprivation(), 
                  file, 
                  row.names = FALSE, 
                  col.names = c("Treatment specialty", "Financial year", 
                                "Type of location", "Location", 
                                "Deprivation quintile (SIMD)", 
                                input$deprivation_measure_type), 
                  sep = ",")
    }
  )
  
  #We can now move on to the second graph, i.e., the line chart that shows...
  #changes in the RII over time (numbers 19 to 34).
  
  ### 19 ---
  
  #There will be two filters: SELECT TREATMENT SPECIALTY and SELECT MEASURE.
  #The reactive() command below creates a subset of the RII dataset based on...
  #the user's selections in the aforementioned two filters.
  #This subset will then "feed" the RII line chart.
  
  RII_new <- reactive({
    RII %>% 
      filter(dataset %in% input$RII_datasets 
             & measure %in% input$RII_measure_type)
  })
  
  #Create the line chart.
  #We do this using the plotly library.
  
  output$RII_line_chart <- renderPlotly({
    
    ### 20 ---
    
    #If the user selects non-psychiatric specialties + the measure 'Hospital...
    #residents', there needs to be a message saying that this is not...
    #a valid measure for non-psychiatric specialties.
    
    if (input$RII_datasets == "Non-psychiatric" & 
        input$RII_measure_type == "Hospital residents")
      
    { 
      
      #This is the message we are using.
      
      text_hosp_res_non_psych_spec_line_chart <- list(
        x = 5, 
        y = 2,
        font = list(color = "#0072B2", size = 20),
        text = 
          "'Hospital residents' is not a valid measure for non-psychiatric specialties.", 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      ) 
      
      #Visualise an empty graph with the above message in the middle.
      
      plot_ly() %>% 
        layout(annotations = text_hosp_res_non_psych_spec_line_chart, 
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
      
      ### 21 ---
      
      #Create the tooltip, i.e., insert the information that appears when the...
      #user hovers his/her mouse over a marker in the second graph.
      #E.g.: 
      #"Treatment specialty: Non-psychiatric"
      #"Financial year: 2018/2019"
      #"Location of residence: Scotland"
      #"Relative Index of Inequality - Patients: XX"
      
      tooltip_RII <- paste0("Treatment specialty: ",
                            RII_new()$dataset,
                            "<br>",
                            "Financial year: ",
                            RII_new()$year, 
                            "<br>",
                            "Location of residence: ", 
                            RII_new()$geography1, 
                            "<br>",
                            "Relative Index of Inequality - ", 
                            input$RII_measure_type, ": ",
                            RII_new()$value)
      
      #Create the main body of the line chart.
      
      plot_ly(data = RII_new(),
              
              ### 22 ---
              
              #Select your variables.
              
              x = ~year, y = ~value, color = ~geography1, 
              
              ### 23 ---
              
              #Specify the colour to be used.
              #We only need one colour, as we are only visualising Scotland.
              #Choose dark blue.
              
              colors = c("#004785"),
              
              text = tooltip_RII, hoverinfo = "text",
              
              ### 24 ---
              
              #Select the type of chart you want, in this case a scatter...
              #plot, but set the mode to 'lines+markers' in order to...
              #transform it into a line chart.
              #Modify the line type and width (optional).
              #Set the marker symbol and size (again optional), as well as the...
              #height and width of the graph.
              
              type = 'scatter', mode = 'lines+markers',
              line = list(dash = "dot", width = 3),
              marker = list(symbol = "circle-open", size = 12, opacity = 1),
              width = 1000, height = 600) %>%
        
        ### 25 ---
        
        #Insert the graph title.
        #Make the graph title reactive.
        
        layout(title = paste0("<b>",
                              "Relative Index of Inequality:",
                              if (input$RII_measure_type == "Patients") 
                              { paste0(" Patients in") }
                              else if (input$RII_measure_type == "Discharges") 
                              { paste0(" Discharges from") }
                              else { paste0(" Hospital residents in") },
                              if (input$RII_datasets == "Psychiatric") 
                              { paste0(" psychiatric specialties,") }
                              else if (input$RII_datasets == "Non-psychiatric") 
                              { paste0(" non-psychiatric specialties,") }
                              else { paste0(" any treatment specialty,") },
                              "<br>",
                              first(as.vector(RII_new()$year)), 
                              " - ", 
                              last(as.vector(RII_new()$year)),
                              ", by financial year, residents of Scotland", 
                              "</b>"),
               
               separators = ".",
               
               ### 26 ---
               
               #We need to fix the range of the y axis.
               #The following range() command calculates the lower limit as...
               #the minimum number visualised in the graph - 10% of this...
               #number, and defines the upper limit as the maximum number...
               #visualised in the graph + 10% of this number.
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(
                   min(RII_new()$value, na.rm = TRUE) - 
                     (min(RII_new()$value, na.rm = TRUE) * 10 / 100), 
                   max(RII_new()$value, na.rm = TRUE) + 
                     (max(RII_new()$value, na.rm = TRUE) * 10 / 100)
                 ),
                 
                 ### 27 ---
                 
                 #Write the titles for the axes.
                 
                 #Wrap the y axis title in blank spaces so it...
                 #doesn't overlap with the y axis tick labels.
                 #Also, make the y axis title reactive.
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   "Relative Index of Inequality - ",
                   input$RII_measure_type,
                   rep("&nbsp;", 20),
                   rep("\n&nbsp;", 3)
                 ),
                 collapse = ""),
                 
                 showline = TRUE, 
                 ticks = "outside"
                 
               ),
               
               #Label the x axis.
               #Also, set the x axis tick angle to minus 45. It's the only way...
               #for the x axis tick labels (fin. years) to display without...
               #overlapping with each other.
               
               xaxis = list(tickangle = -45, 
                            title = paste0(c(rep("&nbsp;", 20),
                                             "<br>", 
                                             "<br>",
                                             "Financial year",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""),
                            showline = TRUE, 
                            ticks = "outside"),       
               
               ### 28 ---
               
               #Fix the margins so that the graph and axis titles have enough...
               #room to display nicely.
               
               margin = list(l = 90, r = 60, b = 170, t = 70),
               
               ### 29 ---
               
               #Set the font sizes.
               
               font = list(size = 13),
               titlefont = list(size = 15),
               
               ### 30 ---
               
               #Insert a legend in order to make it clear to the user...
               #that the location of residence being visualised is Scotland.
               #Make the legend background and legend border white.             
               
               showlegend = TRUE, 
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        ### 31 ---
        
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
  
  ### 32 ---
  
  #This reactive() creates the subset that will be used to populate the table...
  #appearing under the line chart.
  #Select the columns you need for the table and filter out NAs.
  
  table_RII <- reactive({
    RII %>% 
      filter(dataset %in% input$RII_datasets 
             & measure %in% input$RII_measure_type) %>% 
      select(dataset, year, geography1, value) %>%
      filter(complete.cases(.))
  })
  
  ### 33 ---
  
  #Create the table that will appear under the RII line chart.
  #Give the columns clearer names.
  #The name of the final column reacts to the user's input in the filter...
  #SELECT MEASURE.
  
  output$RII_table <- renderDataTable({
    datatable(table_RII(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'), 
              colnames = c("Treatment specialty", "Financial year", 
                           "Location of residence", 
                           paste0("Relative Index of Inequality - ", 
                                  input$RII_measure_type)))
  })
  
  ### 34 ---
  
  #Create a download button that allows the user to download the RII table...
  #in CSV format.
  
  output$download_RII <- downloadHandler(
    filename = 'RII_data.csv',
    content = function(file) {
      write.table(table_RII(), 
                  file, 
                  row.names = FALSE, 
                  col.names = c("Treatment specialty", "Financial year", 
                                "Location of residence", 
                                paste0("Relative Index of Inequality - ", 
                                       input$RII_measure_type)), 
                  sep = ",")
    }
  )
  
  ##############################################.             
  ############## Cross-boundary flow ----   
  ##############################################. 
  
  ### 1 ---
  
  #There are three filters here: SELECT TREATMENT SPECIALTY, SELECT FINANCIAL...
  #YEAR and SELECT HEALTH BOARD OF RESIDENCE.
  #The reactive() command below creates a subset of the cross-boundary flow...
  #dataset based on the user's selections in the three aforementioned filters.
  #This subset will then "feed" the cross-boundary flow chart.
  #Additional transformations:
  #Filter out NAs. We have quite a few NAs, as there are certain...
  #combinations of values that don't make sense, e.g., State Hospital and...
  #non-psychiatric specialties.
  #In the measure column, we filter out 'Number of discharges', as we want the...
  #cross-boundary flow diagram to show only number of patients.
  #Moreover, we are only visualising inter-board flow, so we need to filter...
  #out intra-board flow. To do this, we execute 'filter(flow == 0)'.
  #Also, prevent R from visualising flows with zero patients.
  #Rename the 'value' variable to 'Number of patients'.
  #Finally, keep only the columns you need for the chart. This last step is...
  #very important, as the Sankey diagram only works with three...
  #variables: 1) origin, 2) destination, and 3) a numeric variable that flows...
  #from origin to destination.  
  
  flow_new <- reactive({
    flow %>% 
      filter(dataset %in% input$flow_dataset
             & `health board of residence` %in% input$flow_board_of_residence 
             & year %in% input$flow_financial_year) %>%
      filter(complete.cases(.)) %>%
      filter(measure != "Number of discharges") %>%
      filter(flow == 0) %>%
      filter(value > 0) %>%
      rename(`Number of patients` = value) %>%
      select(`health board of residence`, `health board of treatment`,
             `Number of patients`)
  })
  
  ### 2 ---
  
  #Calculate the percentages and numbers which will be used in the...
  #reactive sentence that appears above our Sankey diagram. We need to...
  #calculate 5 different values (listed below).
  
  output$flow_text <- renderText({
    
    flow_txt <- flow %>% 
      filter(dataset %in% input$flow_dataset
             & `health board of residence` %in% input$flow_board_of_residence 
             & year %in% input$flow_financial_year) %>%
      filter(complete.cases(.)) %>%
      filter(measure != "Number of discharges")
    
    if (sum(flow_txt$flow) == 0) {
      
      # a
      
      no_intraboard_flow_number <- flow_txt %>%
        summarise(sum(value)) %>%
        pull()
      
    }
    
    else {
      
      # b
      
      flow_intraboard_percentage <- flow_txt %>%
        summarise(round(value[flow == 1]
                        / sum(value) * 100, 1)) %>%
        pull()
      
      # c
      
      flow_interboard_percentage <- flow_txt %>%
        summarise(round(sum(value[flow == 0])
                        / sum(value) * 100, 1)) %>%
        pull()
      
      # d 
      
      flow_intraboard_number <- flow_txt %>%
        summarise(value[flow == 1]) %>%
        pull()
      
      # e
      
      flow_interboard_number <- flow_txt %>%
        summarise(sum(value[flow == 0])) %>%
        pull()
      
    }
    
    ### 3 ---
    
    #We can now build our reactive sentence.
    #This will be done using "if" statements.
    
    #Statement 1: If there are no patients for a given combination of...
    #selections, the user gets the message "No patients found".
    
    if (sum(flow_txt$value) == 0) {
      
      paste0("<b>",
             "No patients found.",
             "</b>")
      
    }
    
    #Statement 2: If there are patients, but no intra-board flow, the user...
    #gets a message saying that the percentage of patients from NHS X who...
    #were treated inside their board was 0%. The only boards with 0%... 
    #intra-board flow are NHS Orkney and NHS Shetland, in the psychiatric...
    #specialty.
    
    else if (sum(flow_txt$value) != 0 & 
             sum(flow_txt$flow) == 0) {
      
      paste0("<b>",
             "In ",
             input$flow_financial_year,
             ", 0% of patients in psychiatric specialties residing in ",
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
             ", 100% of patients",
             if(input$flow_dataset == "Psychiatric") 
             {paste0(" in psychiatric specialties")}
             else if(input$flow_dataset == "Non-psychiatric") 
             {paste0(" in non-psychiatric specialties")}
             else {paste0(" in any treatment specialty")},
             " residing in ",
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
             "% of patients",
             if(input$flow_dataset == "Psychiatric") 
             {paste0(" in psychiatric specialties")}
             else if(input$flow_dataset == "Non-psychiatric") 
             {paste0(" in non-psychiatric specialties")}
             else {paste0(" in any treatment specialty")},
             " residing in ",
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
    
    else { paste0(" ") }
    
  })
  
  ### 4 ---
  
  #Create the Sankey diagram.
  
  output$flow_graph <- renderGvis({
    
    gvisSankey(flow_new(), from = "`health board of residence`",
               to = "`health board of treatment`",
               weight = "`Number of patients`",
               options = list(
                 width = "automatic",
                 height = "automatic",
                 sankey = "{link: {color: {fill: '#4CBEED'}}, 
                 node: {width: 0,
                 label: {fontSize: 15, 
                 color: 'black',
                 bold: 'true'}}}" 
               ))
    
  })
  
  ### 5 ---
  
  #This reactive() creates the subset that will be used to populate the table...
  #appearing under the diagram.
  #Filter out NAs and discharges.
  #Select the columns you need for the table.
  
  table_flow <- reactive({
    flow %>% 
      filter(dataset %in% input$flow_dataset 
             & `health board of residence` %in% input$flow_board_of_residence 
             & year %in% input$flow_financial_year) %>%
      filter(complete.cases(.)) %>%
      filter(measure != "Number of discharges") %>%
      select(dataset, year, `health board of residence`, 
             `health board of treatment`, value)
  })
  
  ### 6 ---
  
  #Create the table that will appear under the visual.
  #Give the columns clearer names.
  
  output$flow_table <- renderDataTable({
    datatable(table_flow(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'), 
              colnames = c("Treatment specialty", "Financial year", 
                           "Health board of residence", 
                           "Health board of treatment", "Number of patients"))
  })
  
  ### 7 ---
  
  #Create a download button that allows the user to download the table...
  #in CSV format.
  
  output$download_flow <- downloadHandler(
    filename = 'cross_boundary_flow_data.csv',
    content = function(file) {
      write.table(table_flow(), 
                  file, 
                  row.names = FALSE, 
                  col.names = c("Treatment specialty", "Financial year", 
                                "Health board of residence", 
                                "Health board of treatment", 
                                "Number of patients"), 
                  sep = ",")
    }
  )
  
  ##############################################.              
  ############## Readmissions tab ----    
  ##############################################.
  
  #This tab will have two charts: one bar chart and one line chart.
  #They both utilise the same dataset.
  #The bar chart allows you to compare the readmission percentages of...
  #multiple HBs in a single year, whereas the line chart shows you a time...
  #trend of the readmission percentages of a single HB (or multiple, but...
  #that's up to the user).
  #Both graphs allow the user to compare the HB percentage against the...
  #Scotland percentage. 
  
  #Bar chart (numbers 1 to 15):
  
  ### 1 ---
  
  #Filter the dataset according to the user's selections in the filters...
  #SELECT FINANCIAL YEAR and SELECT MEASURE.
  
  readm_bar_chart_subset <- reactive({
    readmissions %>% 
      filter(year %in% input$readmissions_financial_year 
             & measure %in% input$readmissions_measure_type)
  })
  
  #Start creating the bar chart.
  
  output$readm_bar_chart <- renderPlotly({
    
    ### 2 ---
    
    #Create the tooltip, i.e., insert the text that the users see when...
    #they hover their mouse over a bar in the chart.
    #E.g.:
    #"Treatment specialty: Psychiatric"
    #"Financial year: 2018/2019"
    #"Location of treatment: Scotland"
    #"Percentage readmissions within 28 days: XX"
    
    tooltip_readm_bar_chart <- paste0("Treatment specialty: ",
                                      readm_bar_chart_subset()$dataset,
                                      "<br>",
                                      "Financial year: ",
                                      readm_bar_chart_subset()$year, 
                                      "<br>",
                                      "Location of treatment: ", 
                                      readm_bar_chart_subset()$geography2, 
                                      "<br>",
                                      input$readmissions_measure_type, ": ",
                                      readm_bar_chart_subset()$value)
    
    plot_ly(data = readm_bar_chart_subset(),
            
            ### 3 ---
            
            #Choose your x (geography2), y (value), and grouping (geography2)...
            #variables.
            
            x = ~geography2, y = ~value, color = ~geography2, 
            
            ### 4 ---
            
            #Choose colours for the bars.
            #We need to repeat the hexadecimal code for light blue 12 times,...
            #as there are 12 health boards here, and then we specify dark...
            #blue for Scotland. 
            
            colors = print(c("#4CBEED", "#4CBEED", "#4CBEED", "#4CBEED", 
                             "#4CBEED", "#4CBEED", "#4CBEED", "#4CBEED", 
                             "#4CBEED", "#4CBEED", "#4CBEED", "#4CBEED",  
                             "#004785")), 
            
            text = tooltip_readm_bar_chart, hoverinfo = "text",
            
            ### 5 ---
            
            #Choose your chart type, in this case a bar chart, and set the...
            #width and height of the chart.
            
            type = 'bar', width = 1000, height = 600) %>%
      
      ### 6 ---
      
      #Insert the title of the graph, which is fully interactive, i.e., it...
      #changes according to the user's selections in the filters.
      
      layout(title = paste0("<b>",
                            input$readmissions_measure_type,
                            "<br>",
                            "for psychiatric specialties in ",
                            input$readmissions_financial_year,
                            ", ",
                            "by location of treatment",
                            "</b>"),
             
             separators = ".",
             
             yaxis = list(
               
               ### 7 ---
               
               #We need to fix the range of the y axis, as R refuses to set...
               #the lower end of this axis to zero.
               #The following "range" command fixes the lower end to...
               #zero, and calculates the upper end as the maximum...
               #number visualised in the graph + 10% of this number.
               
               exponentformat = "none",
               
               separatethousands = TRUE,
               
               range = c(0, max(readm_bar_chart_subset()$value, na.rm = TRUE) + 
                           (max(readm_bar_chart_subset()$value, na.rm = TRUE) 
                            * 10 / 100)),
               
               ### 8 ---
               
               #The y axis title is fully interactive, i.e., it changes...
               #to reflect the user's selection in the SELECT MEASURE filter.
               #However, we need to shorten some of these phrases, as they...
               #are too long to display normally, i.e., they don't fit the...
               #length of the axis.
               #Also, wrap the y axis title in blank spaces so it...
               #doesn't overlap with the y axis tick labels.
               
               title = paste0(c(
                 rep("&nbsp;", 20),
                 if (input$readmissions_measure_type == 
                     "Percentage readmissions within 28 days")
                 { print(c("% readmissions within 28d")) }
                 else 
                 { print(c("% readmissions within 133d")) },
                 rep("&nbsp;", 20),
                 rep("\n&nbsp;", 3)
               ),
               collapse = ""),
               
               showline = TRUE, 
               ticks = "outside"
               
             ),
             
             ### 9 ---
             
             #Insert a title for the x axis.
             #But wrap it in empty spaces, so it doesn't overlap with the x...
             #axis tick marks and tick labels.
             #Also, set the tick angle to -25 degrees, otherwise the HB names...
             #(i.e., the tick labels) will overlap with each other.
             
             xaxis = list(tickangle = -25, 
                          title = paste0(c(rep("&nbsp;", 20),
                                           "<br>",
                                           "<br>",
                                           "Location of treatment",
                                           rep("&nbsp;", 20),
                                           rep("\n&nbsp;", 3)),
                                         collapse = ""),
                          showline = TRUE, 
                          ticks = "outside"), 
             
             ### 10 ---
             
             #Set the margins and font sizes.
             
             margin = list(l = 100, r = 10, b = 200, t = 70),
             
             font = list(size = 13),
             
             titlefont = list(size = 15),
             
             ### 11 ---
             
             #No legend is needed to understand this graph, so remove it.
             
             showlegend = FALSE) %>%
      
      ### 12 ---
      
      #Remove unnecessary buttons from the plotly toolbar.
      
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                           'zoomOut2d', 'autoScale2d', 
                                           'toggleSpikelines', 
                                           'hoverCompareCartesian', 
                                           'hoverClosestCartesian'), 
             displaylogo = F, collaborate = F, editable = F)
    
  })
  
  ### 13 ---
  
  #This subset "feeds" the table that appears under the bar chart.
  #Select the columns you need for the table and use arrange() to sort the...
  #data however you like (optional).
  
  first_table_readm <- reactive({
    readmissions %>% 
      filter(year %in% input$readmissions_financial_year 
             & measure %in% input$readmissions_measure_type) %>% 
      select(dataset, year, geography2, value) %>%
      arrange(year, geography2)
  })
  
  ### 14 ---
  
  #Create the actual table using the subset above.
  
  output$first_readm_table <- renderDataTable({
    datatable(first_table_readm(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'), 
              colnames = c("Treatment specialty", "Financial year", 
                           "Location of treatment", 
                           input$readmissions_measure_type))
  })
  
  ### 15 ---
  
  #Finally, insert a download button for downloading the table as a CSV file.
  
  output$first_download_readmissions <- downloadHandler(
    filename = 'readmission_data_chart_one.csv',
    content = function(file) {
      write.table(first_table_readm(), 
                  file, 
                  row.names = FALSE, 
                  col.names = c("Treatment specialty", "Financial year", 
                                "Location of treatment", 
                                input$readmissions_measure_type), 
                  sep = ",")
    }
  )
  
  
  
  #We continue with the second graph, i.e., the line chart (numbers 16 to 34):
  
  ### 16 ---
  
  #Filter the readmissions basefile according to the user's selections in the...
  #filters for location of treatment and measure type.
  #Don't forget to drop the unused levels in your factors. This is essential...
  #because, in the graph, we will be matching colours to factor levels.
  
  readm_line_chart_subset <- reactive({
    readmissions %>% 
      filter(geography2 %in% input$readmissions_location 
             & measure %in% input$readmissions_measure_type_two) %>%
      droplevels
  })
  
  #Create the line chart.
  
  output$readm_line_chart <- renderPlotly({
    
    ### 17 ---
    
    #Create the tooltip, i.e., insert the information that the users see when...
    #they hover their mouse over a dot in the line chart.
    #E.g.:
    #"Treatment specialty: Psychiatric"
    #"Financial year: 2018/2019"
    #"Location of treatment: Scotland"
    #"Percentage readmissions within 28 days: XX"
    
    tooltip_readm_line_chart <- paste0("Treatment specialty: ",
                                       readm_line_chart_subset()$dataset,
                                       "<br>", 
                                       "Financial year: ",
                                       readm_line_chart_subset()$year, 
                                       "<br>", 
                                       "Location of treatment: ", 
                                       readm_line_chart_subset()$geography2, 
                                       "<br>",
                                       input$readmissions_measure_type_two, ": ",
                                       readm_line_chart_subset()$value)
    
    plot_ly(data = readm_line_chart_subset(),
            
            ### 18 ---
            
            #Choose your x, y, and grouping variables.
            
            x = ~year, y = ~value, color = ~geography2, 
            
            ### 19 ---
            
            #Assign a colour to each location of treatment.
            #The user can select a maximum of four health boards, so we...
            #need four colours.
            #We will only be using two colours, but we can make the lines...
            #distinguishable in other ways, i.e., with symbols and different...
            #types of lines (see below).
            #Both colours here are shades of blue, for accessibility...
            #purposes.
            
            colors = c("#4CBEED", "#004785", "#4CBEED", "#004785"),
            
            text = tooltip_readm_line_chart, hoverinfo = "text",
            
            ### 20 ---
            
            #Specify the type of chart you want, in this case a scatter plot,...
            #and set the mode to 'lines+markers' to transform it into a line...
            #chart. 
            #Then, set the marker (i.e., dot) size.
            
            type = 'scatter', mode = 'lines+markers', marker = list(size = 12),
            
            ### 21 ---
            
            #Select the width of the lines, and set the type of line for...
            #each health board.
            #Some line types will be repeated, but we will also be using...
            #symbols (see below), so in the end, it will be clear which line...
            #corresponds to which HB.
            
            line = list(width = 3),
            linetype = ~geography2,
            linetypes = c("solid", "solid", "dot", "dot"),
            
            ### 22 ---
            
            #Set the symbols for the health boards. 
            
            symbol = ~geography2, 
            symbols = c("square-open", "x", "circle", "diamond"),
            
            ### 23 ---
            
            #Set the width and height of the graph.
            #The "name" attribute tells plotly which variable to base the...
            #legend labels on. We use a string wrapper, so that only 10...
            #characters are visualised per line. This is meant to deal with...
            #boards with long names, like NHS Greater Glasgow & Clyde.
            
            width = 1000, height = 600, 
            name = ~str_wrap(geography2, 10)) %>%
      
      ### 24 ---
      
      #Insert the title of the graph, and make it dynamic. This way, the...
      #title will change every time the user selects a different measure.
      
      layout(title = paste0("<b>", 
                            input$readmissions_measure_type_two,
                            " for psychiatric specialties",
                            "<br>",              
                            "in the period ",
                            first(as.vector(readm_line_chart_subset()$year)), 
                            " - ", 
                            last(as.vector(readm_line_chart_subset()$year)),
                            ", by financial year and location of treatment",
                            "</b>"),
             
             separators = ".",
             
             yaxis = list(
               
               exponentformat = "none",
               
               separatethousands = TRUE,
               
               ### 25 ---
               
               #Fix the range of the y axis.
               #Set the lower limit to zero and the upper limit to the...
               #largest number visualised plus 10% of this number.
               
               range = c(0, max(readm_line_chart_subset()$value, na.rm = TRUE) + 
                           (max(readm_line_chart_subset()$value, na.rm = TRUE) 
                            * 10 / 100)),
               
               ### 26 ---
               
               #Create a title for the y axis and make it dynamic. 
               #Shorten the descriptions of the measures, as they are too...
               #long to visualise correctly.
               
               title = paste0(c(
                 rep("&nbsp;", 20),
                 if (input$readmissions_measure_type_two == 
                     "Percentage readmissions within 28 days")
                 { print(c("% readmissions within 28d")) }
                 else 
                 { print(c("% readmissions within 133d")) },
                 rep("&nbsp;", 20),
                 rep("\n&nbsp;", 3)
               ),
               collapse = ""),
               
               showline = TRUE, ticks = "outside"
               
             ),
             
             ### 27 ---
             
             #Insert a title for the x axis, but wrap it in empty spaces...
             #so it doesn't cover the x axis tick labels. Also, set the angle...
             #of the tick labels to 45 degrees, so they don't overlap with...
             #each other.
             
             xaxis = list(tickangle = -45,
                          title = paste0(c(rep("&nbsp;", 20),
                                           "<br>",
                                           "<br>",
                                           "Financial year",
                                           rep("&nbsp;", 20),
                                           rep("\n&nbsp;", 3)),
                                         collapse = ""),
                          showline = TRUE, 
                          ticks = "outside"), 
             
             ### 28 ---
             
             #Set the margins of the graph.
             
             margin = list(l = 90, r = 60, b = 170, t = 70),
             
             ### 29 ---
             
             #Set font sizes for the different elements of the graph.
             
             font = list(size = 13),
             titlefont = list(size = 15),
             
             ### 30 ---
             
             #Insert a legend showing the colour, line type and symbol used...
             #for each location of treatment.
             #Set the legend border and legend background colour to white.
             
             showlegend = TRUE, 
             legend = list(x = 1, 
                           y = 1, 
                           bgcolor = 'rgba(255, 255, 255, 0)', 
                           bordercolor = 'rgba(255, 255, 255, 0)')) %>%
      
      ### 31 ---
      
      #Remove unnecessary buttons from the plotly toolbar.
      
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                           'zoomOut2d', 'autoScale2d', 
                                           'toggleSpikelines', 
                                           'hoverCompareCartesian', 
                                           'hoverClosestCartesian'), 
             displaylogo = F, collaborate = F, editable = F)
    
  })
  
  ### 32 ---
  
  #The following reactive() creates the subset that will be used to populate...
  #the table appearing under the line chart.
  #Select the columns you need for the table and then sort the table however...
  #you like (optional).
  
  second_table_readm <- reactive({
    readmissions %>% 
      filter(geography2 %in% input$readmissions_location 
             & measure %in% input$readmissions_measure_type_two) %>% 
      select(dataset, year, geography2, value) %>%
      arrange(year, geography2)
  })
  
  ### 33 ---
  
  #Create the actual table.
  #The title of the last column depends on the user's selection in the...
  #SELECT MEASURE filter.
  
  output$second_readm_table <- renderDataTable({
    datatable(second_table_readm(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'), 
              colnames = c("Treatment specialty", "Financial year", 
                           "Location of treatment", 
                           input$readmissions_measure_type_two))
  })
  
  ### 34 ---
  
  #Insert a download button, that allows the user to export the table in...
  #CSV format.
  
  output$second_download_readmissions <- downloadHandler(
    filename = 'readmission_data_chart_two.csv',
    content = function(file) {
      write.table(second_table_readm(), 
                  file, 
                  row.names = FALSE, 
                  col.names = c("Treatment specialty", "Financial year", 
                                "Location of treatment", 
                                input$readmissions_measure_type_two), 
                  sep = ",")
    }
  )
  
  ##############################################.              
  ############## Table tab ----    
  ##############################################.
  
  #On to the final tab, which is the Table tab.
  
  ### 1 ---
  
  #The following piece of syntax tells R to switch between files...
  #based on the user's input in the filter SELECT DATA FILE.
  #The files below are the ones we read into R at the very beginning.
  #However, they require a few transformations before they can be displayed...
  #as a table.
  
  data_table <- reactive({
    switch(input$table_filenames,
           "Trends in diagnoses (Data explorer)" = time_trend %>%
             mutate(measure = fct_rev(measure)) %>%
             mutate(geography1 = fct_rev(geography1)) %>%
             rename(`Treatment specialty` = dataset, 
                    `Financial year` = year, 
                    `Type of location` = geography1, 
                    `Location` = geography2, 
                    `Diagnosis grouping` = diagnosis_groupings, 
                    `Type of measure` = measure, 
                    `Value` = value) %>%
             filter(complete.cases(.)),
           "Geography (Data explorer)" = geography %>%
             mutate(measure = fct_rev(measure)) %>%
             rename(`Treatment specialty` = dataset, 
                    `Financial year` = fyear, 
                    `Council area of residence` = ca, 
                    `Type of measure` = measure),
           "Age/sex (Data explorer)" = age_sex %>%
             mutate(measure = fct_rev(measure)) %>%
             mutate(geography1 = fct_rev(geography1)) %>%
             rename(`Treatment specialty` = dataset, 
                    `Financial year` = year, 
                    `Type of location` = geography1, 
                    `Location` = geography2, 
                    `Age group` = ageband,
                    `Sex` = sex_char, 
                    `Type of measure` = measure, 
                    `Value` = value) %>%
             mutate(`Value` = abs(`Value`)) %>%
             filter(complete.cases(.)),
           "Deprivation - SIMD quintiles (Data explorer)" = deprivation %>%
             mutate(measure = fct_rev(measure)) %>%
             mutate(geography1 = fct_rev(geography1)) %>%
             arrange(dataset, year, geography1, geography2, simd, measure, 
                     value) %>%
             rename(`Treatment specialty` = dataset, 
                    `Financial year` = year, 
                    `Type of location` = geography1, 
                    `Location` = geography2, 
                    `Deprivation quintile (SIMD)` = simd, 
                    `Type of measure` = measure, 
                    `Value` = value) %>%
             filter(complete.cases(.)),
           "Deprivation - Relative Index of Inequality (Data explorer)" = RII %>%
             mutate(measure = fct_rev(measure)) %>%
             rename(`Treatment specialty` = dataset, 
                    `Financial year` = year, 
                    `Location of residence` = geography1, 
                    `Type of measure` = measure, 
                    `Relative Index of Inequality` = value) %>%
             filter(complete.cases(.)),
           "Cross-boundary flow (Data explorer)" = flow %>%
             select(dataset, year, `health board of residence`, 
                    `health board of treatment`, measure, value) %>%
             mutate(measure = fct_rev(measure)) %>%
             rename(`Treatment specialty` = dataset, 
                    `Financial year` = year, 
                    `Health board of residence` = `health board of residence`,
                    `Health board of treatment` = `health board of treatment`,
                    `Type of measure` = measure,
                    `Value` = value) %>%
             filter(complete.cases(.)),
           "Readmissions (Data explorer)" = readmissions %>%
             mutate(measure = fct_rev(measure)) %>%
             mutate(geography1 = fct_rev(geography1)) %>%
             rename(`Treatment specialty` = dataset, 
                    `Financial year` = year,
                    `Type of location` = geography1, 
                    `Location` = geography2, 
                    `Type of measure` = measure,
                    `Value` = value),
           "Activity by hospital (Trend data)" = activity_by_hospital %>%
             rename(`Treatment specialty` = Specialty, 
                    `Financial year` = year, 
                    `Health board of treatment (HBT)` = hbtreat_name, 
                    `Hospital (includes the total for each HBT)` = hospital_name, 
                    `Type of measure` = TrendType, 
                    `Value` = Value),
           "Length of stay (Trend data)" = length_of_stay %>%
             rename(`Financial year` = fyear, 
                    `Health board of treatment` = geography2,
                    `Stay specialty` = Specialty, 
                    `Length of stay category` = LengthOfStay, 
                    `Number of stays` = NumberOfStays)
    )
  })
  
  ### 2 ---
  
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
  
  ### 3 ---
  
  #We also create a download data button.
  
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
  ############## Glossary ----    
  ##############################################.
  
  #We have prepared a glossary to help the user understand the graphs and...
  #tables more easily.
  
  ### 1 ---
  
  #Create a download button for the glossary.
  
  glossary_code_shortcut <- downloadHandler(
    filename = 'glossary.pdf',
    content = function(file) {
      file.copy("www/MHIA glossary.pdf", file)
    }
  )
  
  ### 2 ---
  
  #There must be one button in each tab (including the Introduction tab).
  #Therefore, we need to execute the command eight times.
  
  output$download_glossary_one <- glossary_code_shortcut
  
  output$download_glossary_two <- glossary_code_shortcut
  
  output$download_glossary_three <- glossary_code_shortcut  
  
  output$download_glossary_four <- glossary_code_shortcut 
  
  output$download_glossary_five <- glossary_code_shortcut
  
  output$download_glossary_six <- glossary_code_shortcut
  
  output$download_glossary_seven <- glossary_code_shortcut
  
  output$download_glossary_eight <- glossary_code_shortcut
  
}

#We are now finished with the Server syntax.