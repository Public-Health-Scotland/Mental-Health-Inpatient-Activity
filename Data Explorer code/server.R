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


### SECTION 2: SERVER ----


#A Shiny app needs two things: the Server and the User Interface. 
#We can begin with the Server.
server <- function(input, output, session) {

  ##* ObserveEvent() commands ----
  
  #These observeEvent() commands will be combined with action buttons in...
  #the User Interface to allow the user to navigate to each tab in the Explorer...
  #by clicking on links in the Introduction page (in addition to the classic...
  #way of navigating, which is by clicking on the tab headers).   
  observeEvent(input$link_to_trends_in_diagnoses_tab, {
    updateTabsetPanel(session, "Panels", selected = "Trends in diagnoses")
  })
  observeEvent(input$link_to_geography_tab, {
    updateTabsetPanel(session, "Panels", selected = "Geography")
  })
  observeEvent(input$link_to_age_sex_tab, {
    updateTabsetPanel(session, "Panels", selected = "Age/sex")
  })
  observeEvent(input$link_to_deprivation_tab, {
    updateTabsetPanel(session, "Panels", selected = "Deprivation")
  })
  observeEvent(input$link_to_cross_boundary_flow_tab, {
    updateTabsetPanel(session, "Panels", selected = "Cross-boundary flow")
  })
  observeEvent(input$link_to_readmissions_tab, {
    updateTabsetPanel(session, "Panels", selected = "Readmissions")
  })
  observeEvent(input$link_to_table_tab, {
    updateTabsetPanel(session, "Panels", selected = "Table")
  })
  
  ##* Trends in diagnoses server ----
  
  #The user's selection in the filter SELECT TYPE OF LOCATION determines...
  #which choices appear in the filter SELECT LOCATION.
  #The renderUI() command makes the process of creating these two dynamic... 
  #filters very easy.
  #In the second renderUI() command, we set multiple to TRUE, as we want the...
  #user to be able to select multiple locations. However, we set the limit to...
  #four selections for accessibility reasons.
  output$diagnoses_location_types <- renderUI({
    shinyWidgets::pickerInput("diagnoses_location_type", 
                              label = "Select type of location",
                              choices = diag_location_types, 
                              selected = "Scotland")
  })
  output$diagnoses_locations <- renderUI({
    shinyWidgets::pickerInput( 
      "diagnoses_location",
      label = "Select location (up to four selections allowed)",  
      choices = sort(unique(as.character(
        diagnoses$geography2[diagnoses$geography1 %in% input$diagnoses_location_type]
      ))),
      multiple = TRUE,
      width = '100%',
      options = list("max-options" = 4, `selected-text-format` = "count > 1"),
      selected = if(input$diagnoses_location_type == "Scotland") 
      {print("Scotland")} else{print("NHS Ayrshire & Arran")}
    )
  })
  
  #There are five filters in total: SELECT TREATMENT SPECIALTY, SELECT TYPE... 
  #OF LOCATION, SELECT LOCATION, SELECT DIAGNOSIS GROUPING, and SELECT MEASURE.
  #The reactive() command below creates a subset of the diagnoses dataset...
  #based on the user's selections in these five filters.
  #This subset will then "feed" the diagnoses line chart.
  #Don't forget to drop the unused levels in your factors. This is essential...
  #because, in the graph, we will be matching colours to factor levels.
  diagnoses_new <- reactive({
    diagnoses %>%
      filter(dataset %in% input$diagnoses_dataset
             & geography1 %in% input$diagnoses_location_type 
             & geography2 %in% input$diagnoses_location
             & diagnosis_groupings %in% input$diagnoses_diagnosis_groupings 
             & measure %in% input$diagnoses_measure_type) %>%
      droplevels
  })
  
  #Create the line chart for the diagnoses tab.
  #We create this using the plotly library.
  output$diagnoses_plot <- renderPlotly({
    
    #Insert various IF statements, in case the user makes a combination of...
    #selections that isn't valid.
    
    #If the user selects psychiatric/total specialties + State Hospital +...
    #rates, there needs to be a message saying that no rates are...
    #available for State Hospital.
    if((input$diagnoses_dataset == "Psychiatric" | 
        input$diagnoses_dataset == "Total") & 
       any(input$diagnoses_location == 
           "The State Hospitals Board for Scotland") &
       (input$diagnoses_measure_type == 
        "Rate of patients (per 100,000 population)" |
        input$diagnoses_measure_type == 
        "Rate of discharges (per 100,000 population)" | 
        input$diagnoses_measure_type == 
        "Rate of hospital residents (per 100,000 population)")) 
      
    { #This is the message we are using.
      text_state_hosp_rates <- list(
        x = 5, 
        y = 2,
        font = list(color = "#0072B2", size = 20),
        text = "No rates available for location of treatment 'State Hospital'.", 
        xref = "x", 
        yref = "y",  
        showarrow = FALSE
      ) 
      
      #Visualise an empty graph with the above message in the middle.
      plot_ly() %>% 
        layout(annotations = text_state_hosp_rates, 
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
    else if(input$diagnoses_dataset == "Non-psychiatric" & 
            any(input$diagnoses_location == 
                "The State Hospitals Board for Scotland"))
      
    { #This is the message we are using.
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
    else if(input$diagnoses_dataset == "Psychiatric" & 
            any(input$diagnoses_location == "NHS Shetland" | 
                input$diagnoses_location == "NHS Orkney"))
      
    { #This is the message we are using.
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
    else if(input$diagnoses_dataset == "Total" & 
            any(input$diagnoses_location == "NHS Shetland" | 
                input$diagnoses_location == "NHS Orkney") & 
            (input$diagnoses_measure_type == "Number of hospital residents" |
             input$diagnoses_measure_type == 
             "Rate of hospital residents (per 100,000 population)"))
      
    { #This is the message we are using.
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
    else if(input$diagnoses_dataset == "Non-psychiatric" & 
            (input$diagnoses_measure_type == "Number of hospital residents" |
             input$diagnoses_measure_type == 
             "Rate of hospital residents (per 100,000 population)"))
      
    { #This is the message we are using.
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
    #Otherwise, the user might see an empty graph and think there...
    #is something wrong with it.
    else if(sum(diagnoses_new()$value) == 0 & 
            !is.na(sum(diagnoses_new()$value)) &
            input$diagnoses_location != "") 
      
    { #This is the message we are using.
      text_no_patients_found <- list(
        x = 5, 
        y = 2, 
        font = list(color = "#0072B2", size = 20),
        text = paste0("No ", 
                      if(input$diagnoses_measure_type == 
                         "Number of discharges" |
                         input$diagnoses_measure_type == 
                         "Rate of discharges (per 100,000 population)")
                      {print(c("discharges "))}
                      else if(input$diagnoses_measure_type == 
                              "Number of patients" |
                              input$diagnoses_measure_type == 
                              "Rate of patients (per 100,000 population)") 
                      {print(c("patients "))}
                      else{print(c("hospital residents "))}, 
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
    
    #Now let's create the "normal" version of the diagnoses graph.
    else{
      
      #Create the tooltip, i.e., insert the information that appears when you...
      #hover your mouse over a dot in the line graph.
      #E.g.:
      #"Treatment specialty: Non-psychiatric"
      #"Financial year: 2019/2020" 
      #"Location of treatment: Scotland"
      #"Diagnosis grouping: Disorders of adult behaviour and personality"
      #"Number of discharges: XXXX"
      tooltip_diagnoses <- paste0("Treatment specialty: ",
                                  diagnoses_new()$dataset, "<br>",
                                  "Financial year: ", 
                                  diagnoses_new()$year, "<br>",
                                  "Location of treatment: ", 
                                  diagnoses_new()$geography2, "<br>",
                                  "Diagnosis grouping: ", 
                                  diagnoses_new()$diagnosis_groupings, "<br>",
                                  input$diagnoses_measure_type, ": ", 
                                  diagnoses_new()$value)
      
      #Create the main body of the chart.
      plot_ly(data = diagnoses_new(), 
              
              #Select your variables.
              x = ~year, y = ~value, color = ~geography2,
              
              #Assign a colour to each location of treatment.
              #The user can select a maximum of four health boards, so we...
              #need four colours.
              #We will be using only two colours, but we can make the lines...
              #distinguishable in other ways, i.e., with symbols and different...
              #line types (see below).
              #Both colours here are shades of blue, for accessibility...
              #purposes. 
              colors = c("#4CBEED", "#004785", "#4CBEED", "#004785"),
              text = tooltip_diagnoses, hoverinfo = "text",  
              
              #Select the type of chart you want, in this case a scatter...
              #plot, but set the mode to 'lines+markers' in order to...
              #transform it into a line chart.
              type = 'scatter', mode = 'lines+markers',
              
              #Set the line width, and the line types for the four health boards.
              #Some line types will be repeated, but we will also be using...
              #symbols (see below), so in the end, it will be clear which...
              #line corresponds to which health board.
              line = list(width = 3),
              linetype = ~geography2,
              linetypes = c("solid", "solid", "dot", "dot"),
              
              #Select symbols for the health boards, and set their size.
              symbol = ~geography2, 
              symbols = c("square-open", "x", "circle", "diamond"),
              marker = list(size = 12),
              
              #Set the width and height of the graph.
              #The "name" attribute tells plotly which variable to base the...
              #legend labels on. We use a string wrapper, so that only 10...
              #characters are visualised per line. This is meant to deal with...
              #boards with long names, like NHS Greater Glasgow & Clyde.
              width = 1000, height = 600, 
              name = ~str_wrap(geography2, 10)) %>%
        
        #Write the title of the graph, which must be dynamic.
        layout(title = 
                 paste0(
                   "<b>", 
                   input$diagnoses_measure_type,
                   if(input$diagnoses_measure_type == 
                      "Number of discharges" | 
                      input$diagnoses_measure_type == 
                      "Rate of discharges (per 100,000 population)") 
                   {paste0(" from")}
                   else{paste0(" in")},
                   if(input$diagnoses_dataset == "Psychiatric") 
                   {paste0(" psychiatric specialties")}
                   else if(input$diagnoses_dataset == "Non-psychiatric") 
                   {paste0(" non-psychiatric specialties")}
                   else{paste0(" any treatment specialty")},
                   " with main diagnosis", 
                   "<br>", 
                   "'", input$diagnoses_diagnosis_groupings, "',", 
                   "<br>",
                   first(as.vector(diagnoses_new()$year)), 
                   " - ", 
                   last(as.vector(diagnoses_new()$year)),
                   ", by financial year and location of treatment",
                   "<br>",
                   "</b>"
                 ),
               separators = ".",
               
               #We need to fix the range of the y axis, as R refuses to set...
               #the lower end of this axis to zero.
               #The following "range" command fixes the lower end to...
               #zero, and calculates the upper end as the maximum...
               #number visualised in the graph + 10% of this number.
               yaxis = list(
                 exponentformat = "none",
                 separatethousands = TRUE,
                 range = c(0, max(diagnoses_new()$value, na.rm = TRUE) * 110 / 100), 
                 
                 #Wrap the y axis title in spaces so it doesn't cover the...
                 #tick labels.
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   if(input$diagnoses_measure_type == 
                      "Rate of discharges (per 100,000 population)")
                   {print(c("Rate of discharges"))}
                   else if(input$diagnoses_measure_type == 
                           "Rate of patients (per 100,000 population)")
                   {print(c("Rate of patients"))}
                   else if(input$diagnoses_measure_type == 
                           "Rate of hospital residents (per 100,000 population)")
                   {print(c("Rate of hospital residents"))}
                   else{print(c(input$diagnoses_measure_type))}, 
                   rep("&nbsp;", 20),
                   rep("\n&nbsp;", 3)
                 ), 
                 collapse = ""),
                 showline = TRUE, 
                 ticks = "outside"
               ),
               
               #Set the x axis tick angle to minus 45. It's the only way for...
               #the tick labels (financial years) to display without...
               #covering each other.
               #Also, wrap the x axis title in spaces so it doesn't...
               #cover the tick labels.
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
               
               #Set the graph margins.
               margin = list(l = 90, r = 60, b = 170, t = 90),
               
               #Set the font sizes.
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #Add a legend so that the user knows which colour, line type...
               #and symbol corresponds to which location of treatment.
               #Make the legend background and legend border white.              
               showlegend = TRUE,
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove any buttons we don't need from the modebar.
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                             'zoomOut2d', 'autoScale2d', 
                                             'toggleSpikelines', 
                                             'hoverCompareCartesian', 
                                             'hoverClosestCartesian'), 
               displaylogo = F, collaborate = F, editable = F)
      
    }
    
  })
  
  #This reactive() creates the subset that will be used to populate the table...
  #appearing under the line chart.
  #Select the columns you need for the table, and filter out NAs.
  table_diagnoses <- reactive({
    diagnoses %>% 
      filter(dataset %in% input$diagnoses_dataset 
             & geography1 %in% input$diagnoses_location_type 
             & geography2 %in% input$diagnoses_location
             & diagnosis_groupings %in% input$diagnoses_diagnosis_groupings 
             & measure %in% input$diagnoses_measure_type) %>%
      select(dataset, year, geography1, geography2, diagnosis_groupings, 
             value) %>%
      filter(complete.cases(.))
  })
  
  #We now create the table that will go under the diagnoses line chart.
  #We need to provide clearer column names.
  #The name of the last column, which is the measure column, changes...
  #according to the user's input in the filter SELECT MEASURE.
  output$diagnoses_table <- renderDataTable({
    datatable(table_diagnoses(), 
              style = 'bootstrap', 
              class = 'table-bordered table-condensed', 
              rownames = FALSE, 
              options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'),
              colnames = c("Treatment specialty", "Financial year", 
                           "Type of location", 
                           "Location", "Diagnosis grouping", 
                           input$diagnoses_measure_type))
  })
  
  #We also create a download button that allows users to download the...
  #table in .csv format.
  output$download_diagnoses <- downloadHandler(
    filename = 'diagnosis_data.csv',
    content = function(file) {
      write.table(table_diagnoses(), 
                  file,
                  #Remove row numbers as the .csv file already has row numbers.
                  row.names = FALSE,
                  col.names = c("Treatment specialty", "Financial year", 
                                "Type of location", "Location", 
                                "Diagnosis grouping", 
                                input$diagnoses_measure_type), 
                  sep = ",")
    }
  )
  
  ##* Geography server ----   
  
  #Merge the shapefile with the file containing the rates.
  #Use the column holding the council area names to do this.
  ca_data <- sp::merge(CA_smaller, 
                       geography, 
                       by.x = "NAME", 
                       by.y = "ca", 
                       duplicateGeoms = TRUE)
  
  #Create a subset of the dataset which looks at rate of patients in...
  #psychiatric specialties in 2020/2021.
  #This subset will form the basis of our first map.
  #And then, whenever the user makes a new selection, this initial map will...
  #be overwritten by a new one, using the command leafletProxy().
  #leafletProxy() is great as the map will not need to be redrawn from...
  #scratch every time the user makes a new selection.
  #leafletProxy() will keep the initial map, but will delete the council area...
  #boundaries and the legend, and create new ones.
  ca_data_init_selection <- subset(ca_data,
                                   dataset == "Psychiatric"  
                                   & fyear == "2020/2021"  
                                   & measure == "Rate of patients (per 100,000 population)")
  
  output$mymap <- renderLeaflet({
    
    #Create the initial map.
    leaflet() %>% 
      
      #Choose a map provider.
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      
      #Set the zoom level on initiation.
      setView(lng = -3.9031, lat = 57.8772, zoom = 6) %>%
      
      #Use the council area polygons from the subsetted dataset above.
      addPolygons(data = ca_data_init_selection,
                  stroke = TRUE,
                  weight = 2, 
                  smoothFactor = 0.5, 
                  opacity = 1, 
                  fillOpacity = 1, 
                  highlightOptions = highlightOptions(color = "#000000", 
                                                      weight = 5),
                  color = "#000000", 
                  
                  #Colour the council areas based on the rate of...
                  #patients they have.
                  #This ranges from light blue for the lowest values to... 
                  #dark blue for the highest.
                  fillColor = colorNumeric(palette = "Blues", 
                                           domain = ca_data_init_selection$Value)(ca_data_init_selection$Value),
                  
                  #Create the tooltip, i.e., the information that pops up...
                  #when users click on a council area on the map.
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
      
      #Add a dynamic legend.
      addLegend(position = "topright", 
                
                #The legend palette and legend values are based on rate of...
                #patients, like the colours of the council areas on the map.
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
  
  #Open an observer so that leafletProxy() can react to the user's selections.
  observeEvent(c(input$geography_datasets, 
                 input$geography_financial_years, 
                 input$geography_measure_type), {
                   
                   #Create a new subset of the dataset according to the user's selections...
                   #beyond the initial map.
                   ca_data_new <- subset(ca_data,
                                         dataset == input$geography_datasets  
                                         & fyear == input$geography_financial_years  
                                         & measure == input$geography_measure_type)
                   
                   #leafletProxy() will now build upon/overwrite our initial map.
                   leafletProxy("mymap", session) %>%
                     
                     #Clear all previous polygons.
                     clearShapes() %>%
                     
                     #Clear the previous legend.
                     clearControls() %>%
                     
                     #Add the new polygons and tooltip, which have the same format as...
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
                     
                     #Add the new legend, which, again, has the same format as the...
                     #legend of the initial map.
                     #The map is now finished.
                     addLegend(position = "topright", 
                               pal = colorNumeric("Blues", ca_data_new$Value), 
                               values = ca_data_new$Value,
                               title = paste0(
                                 if(input$geography_measure_type == 
                                    "Rate of discharges (per 100,000 population)")
                                 {paste0("Rate of discharges",
                                         "<br>", 
                                         "(per 100,000 population)",
                                         "<br>",
                                         "from ")}
                                 else{paste0("Rate of patients",
                                             "<br>", 
                                             "(per 100,000 population)",
                                             "<br>",
                                             "in ")},
                                 if(input$geography_datasets == "Psychiatric") 
                                 {paste0("psychiatric specialties",
                                         "<br>",
                                         "in ", 
                                         first(as.vector(ca_data_new$fyear)), 
                                         ", by council area",
                                         "<br>",
                                         "of residence")}
                                 else if(input$geography_datasets == "Non-psychiatric") 
                                 {paste0("non-psychiatric specialties",
                                         "<br>",
                                         "in ",
                                         first(as.vector(ca_data_new$fyear)), 
                                         ", by council area",
                                         "<br>",
                                         "of residence")}
                                 else{paste0("any treatment specialty",
                                             "<br>",
                                             "in ",
                                             first(as.vector(ca_data_new$fyear)), 
                                             ", by council area",
                                             "<br>",
                                             "of residence")}
                               ),
                               opacity = 1) 
                   
                 })
  
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
  
  #We now create the table that will go under the map.
  #We need to provide clearer column names.
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
  
  #We also create a download button that allows users to download the...
  #geography table in .csv format.
  output$download_geography <- downloadHandler(
    filename = 'geography_data.csv',
    content = function(file) {
      write.table(table_geography(), 
                  file,
                  #Remove row numbers as the .csv file already has row numbers.
                  row.names = FALSE,
                  col.names = c("Treatment specialty", "Financial year",
                                "Council area of residence", 
                                input$geography_measure_type), 
                  sep = ",")
    }
  )
  
  ##* Age/sex server ----   
  
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
                              choices = sort(unique(as.character(
                                age_sex$geography2
                                [age_sex$geography1 %in% 
                                    input$age_sex_location_type]
                              ))), 
                              selected = "Scotland")
  })
  
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
    
    #Insert various IF statements, in case the user makes a combination of...
    #selections that isn't valid.
    
    #If the user makes a combination of selections that leads to zero...
    #patients or discharges, there needs to be a message saying no patients/...
    #discharges found. Otherwise the user will see an empty graph and...
    #think the app is not working.
    if(sum(abs(age_sex_new()$value)) == 0 & 
       !is.na(sum(abs(age_sex_new()$value)))) 
      
    { #This is the message we are using.
      text_no_patients_found <- list(
        x = 5, 
        y = 2, 
        font = list(color = "#0072B2", size = 20),
        text = paste0("No ", 
                      if(input$age_sex_measure_type == 
                         "Number of discharges" |
                         input$age_sex_measure_type == 
                         "Rate of discharges (per 100,000 population)")
                      {print(c("discharges "))}
                      else{print(c("patients "))},
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
    
    #If the user selects the location 'Other' AND rates, there...
    #needs to be a message saying that no rates are available for...
    #location 'Other'.
    else if(input$age_sex_location == "Other" &  
            (input$age_sex_measure_type == 
             "Rate of patients (per 100,000 population)" |
             input$age_sex_measure_type == 
             "Rate of discharges (per 100,000 population)"))
      
    { #This is the message we are using.
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
    
    #Now let's create the "normal" version of the graph.
    else{
      
      #Create the tooltip, i.e., insert the information that pops up when...
      #you hover your mouse over a bar in the pyramid.
      #E.g.:
      #"Treatment specialty: Psychiatric"
      #"Financial year: 2019/2020"
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
              
              #Select your variables.
              x = ~value, y = ~ageband, color = ~sex_char,
              
              #Colour palette:
              #Dark blue for males and light blue for females.
              colors = c("#004785", "#4CBEED"),
              text = tooltip_age_sex, hoverinfo = "text", 
              
              #Select the type of chart you want, in this case a bar chart,...
              #and set the orientation to horizontal to achieve the...
              #"pyramid" look. Set the width and height of the graph.
              type = 'bar', orientation = 'h',
              width = 1000, height = 400) %>%
        
        #Write the title of the graph, which should be dynamic.
        layout(title = paste0("<b>", 
                              input$age_sex_measure_type,
                              if(input$age_sex_measure_type == 
                                 "Number of discharges" | 
                                 input$age_sex_measure_type == 
                                 "Rate of discharges (per 100,000 population)") 
                              {paste0(" from")}
                              else{paste0(" in")},
                              if(input$age_sex_dataset == "Psychiatric") 
                              {paste0(" psychiatric specialties")}
                              else if(input$age_sex_dataset == "Non-psychiatric") 
                              {paste0(" non-psychiatric specialties")}
                              else{paste0(" any treatment specialty")},
                              " in ", 
                              input$age_sex_financial_year, 
                              ",", 
                              "<br>",
                              "by age group and sex, ",
                              if(input$age_sex_location == "Other") 
                              {paste0("residents outwith Scotland or with no fixed abode")}
                              else{paste0("residents of ", 
                                          input$age_sex_location)},
                              "</b>"),
               separators = ".",
               
               #Set the gap size between bars, and make sure that the bars...
               #overlay, again to achieve that "pyramid" look.
               bargap = 0.2, barmode = 'overlay',
               
               #Write the y axis title.
               #Wrap the title in empty spaces so it doesn't cover the tick...
               #labels.
               yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                             "Age group",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""), 
                            showline = TRUE, ticks = "outside"),
               xaxis = list(exponentformat = "none",
                            separatethousands = TRUE,
                            tickmode = 'array',
                            
                            #Define the range of the x axis.
                            #This is recalculated every time the user makes a new selection.
                            range = c(
                              -round(max(abs(age_sex_new()$value)) * 110 / 100),
                              round(max(abs(age_sex_new()$value)) * 110 / 100)
                            ),
                            tickangle = 0,
                            
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
                            
                            #Write the title for the x axis.
                            title = 
                              if(input$age_sex_measure_type == 
                                 "Rate of discharges (per 100,000 population)")
                              {print(c("Rate of discharges"))}
                            else if(input$age_sex_measure_type == 
                                    "Rate of patients (per 100,000 population)")
                            {print(c("Rate of patients"))}
                            else{print(c(input$age_sex_measure_type))},
                            showline = TRUE, 
                            ticks = "outside"
               ),
               
               #Set the graph margins.
               margin = list(l = 140, r = 10, b = 70, t = 90),
               
               #Set the font sizes.
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #Add a legend so that the user knows which colour...
               #corresponds to which sex.
               #Make the legend background and legend border white.              
               showlegend = TRUE, 
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove any buttons we don't need from the modebar.
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
      #To achieve the "pyramid" look, the original .csv file had to be...
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
  
  #Create the table that will appear under the pyramid.
  #Provide clearer column names.
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
  
  #Create a download button that allows users to download the table...
  #in .csv format. 
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
  
  ##* Deprivation server ----   
  
  #The Deprivation tab will contain two graphs:
  #The first one will show activity broken down by deprivation quintile.
  #The second one will display the RII as a trend over time.
  #The first graph will be a bar chart, with each bar representing a quintile.
  #The second graph will be a line chart.
  
  #We start with the bar chart.
  
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
                              choices = sort(unique(as.character(
                                deprivation$geography2
                                [deprivation$geography1 %in% 
                                    input$deprivation_location_type]
                              ))), 
                              multiple = TRUE,
                              width = '100%',
                              options = list("max-options" = 4,
                                             `selected-text-format` = "count > 1"),
                              selected = if(input$deprivation_location_type 
                                            == "Scotland") {print("Scotland")} 
                              else{print("NHS Ayrshire & Arran")}
    )
  })
  
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
    
    #Insert various IF statements, in case the user makes a combination of...
    #selections that isn't valid.
    
    #If the user makes a combination of selections that leads to zero...
    #patients/discharges/hospital residents, there needs to be a message...
    #saying no patients/discharges/residents found. Otherwise the user will...
    #see an empty graph and think the app is not working.
    if(sum(deprivation_new()$value) == 0 & 
       !is.na(sum(deprivation_new()$value)) &
       input$deprivation_location != "") 
      
    { #This is the message we are using.
      text_no_patients_found <- list(
        x = 5, 
        y = 2, 
        font = list(color = "#0072B2", size = 20),
        text = paste0("No ", 
                      if(input$deprivation_measure_type == 
                         "Number of discharges" |
                         input$deprivation_measure_type == 
                         "Rate of discharges (per 100,000 population)")
                      {print(c("discharges "))}
                      else if(input$deprivation_measure_type == 
                              "Number of patients" |
                              input$deprivation_measure_type == 
                              "Rate of patients (per 100,000 population)") 
                      {print(c("patients "))}
                      else{print(c("hospital residents "))}, 
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
    else if(input$deprivation_dataset == "Non-psychiatric" & 
            (input$deprivation_measure_type == 
             "Rate of hospital residents (per 100,000 population)" |
             input$deprivation_measure_type == 
             "Number of hospital residents"))
      
    { #This is the message we are using.
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
    
    #Now let's create the "normal" version of the graph.
    else {
      
      #Create the tooltip, i.e., insert the information that pops up when the...
      #user hovers his/her mouse over a bar in the graph.
      #E.g.: 
      #"Treatment specialty: Non-psychiatric"
      #"Financial year: 2019/2020"
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
              
              #Select your variables.
              x = ~simd, y = ~value, color = ~geography2, 
              
              #Assign a colour to each location of residence.
              #The user can select a maximum of four health boards, so we...
              #need four colours.
              #All the colours here are shades of blue, for accessibility...
              #purposes. 
              colors = c("#004785", "#4C7EA9", "#00A2E5", "#99DAF5"),
              
              #Select the type of chart you want, in this case a bar chart.
              type = 'bar',
              text = tooltip_deprivation, hoverinfo = "text",
              
              #Set the width and height of the graph.
              #The "name" attribute tells plotly which variable to base the...
              #legend labels on. We use a string wrapper, so that only 10...
              #characters are visualised per line. This is meant to deal with...
              #boards with long names, like NHS Greater Glasgow & Clyde.
              width = 1000, height = 450, 
              name = ~str_wrap(geography2, 20)) %>%
        
        #Write the graph title.
        layout(title = paste0("<b>",
                              input$deprivation_measure_type,
                              if(input$deprivation_measure_type == 
                                 "Number of discharges" | 
                                 input$deprivation_measure_type == 
                                 "Rate of discharges (per 100,000 population)") 
                              {paste0(" from")}
                              else{paste0(" in")},
                              if(input$deprivation_dataset == "Psychiatric") 
                              {paste0(" psychiatric specialties")}
                              else if(input$deprivation_dataset == 
                                      "Non-psychiatric") 
                              {paste0(" non-psychiatric specialties")}
                              else{paste0(" any treatment specialty")},
                              "<br>",
                              "in ",
                              input$deprivation_financial_year,
                              ", by deprivation quintile and ", 
                              "location of residence",
                              "</b>"),
               separators = ".",
               
               #We need to fix the range of the y axis, as R refuses to set...
               #the lower end of this axis to zero.
               #The following "range" command fixes the lower end to...
               #zero, and calculates the upper end as the maximum...
               #number visualised in the graph + 10% of this number.
               yaxis = list(
                 exponentformat = "none",
                 separatethousands = TRUE,
                 range = c(0, max(deprivation_new()$value, na.rm = TRUE) * 110 / 100),
                 
                 #Wrap the y axis title in blank spaces so it...
                 #doesn't cover the tick labels.
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   if(input$deprivation_measure_type == 
                      "Rate of discharges (per 100,000 population)")
                   {print(c("Rate of discharges"))}
                   else if(input$deprivation_measure_type == 
                           "Rate of patients (per 100,000 population)")
                   {print(c("Rate of patients"))}
                   else if(input$deprivation_measure_type == 
                           "Rate of hospital residents (per 100,000 population)")
                   {print(c("Rate of hospital residents"))}
                   else{print(c(input$deprivation_measure_type))},
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
               
               #Set the graph margins.
               margin = list(l = 80, r = 10, b = 50, t = 90),
               
               #Set the font sizes.
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #Add a legend so that the user knows which colour...
               #corresponds to which location of residence.
               #Make the legend background and legend border white.             
               showlegend = TRUE, 
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove any buttons we won't need from the modebar.
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                             'zoomOut2d', 'autoScale2d', 
                                             'toggleSpikelines', 
                                             'hoverCompareCartesian', 
                                             'hoverClosestCartesian'), 
               displaylogo = F, collaborate = F, editable = F)
      
    }
    
  })
  
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
  
  #Create the table that will appear under the deprivation bar chart.
  #We need clearer column names.
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
  
  #Create a download button that allows the user to download the table...
  #in .csv format.
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
  #changes in the RII over time.
  
  #There will be two filters: SELECT TREATMENT SPECIALTY and SELECT MEASURE.
  #The reactive() command below creates a subset of the RII dataset based on...
  #the user's selections in these two filters.
  #This subset will then "feed" the RII line chart.
  RII_new <- reactive({
    RII %>% 
      filter(dataset %in% input$RII_datasets 
             & measure %in% input$RII_measure_type)
  })
  
  #Create the line chart.
  #We do this using the plotly library.
  output$RII_line_chart <- renderPlotly({
    
    #If the user selects non-psychiatric specialties + the measure 'Hospital...
    #residents', there needs to be a message saying that this is not...
    #a valid measure for non-psychiatric specialties.
    if(input$RII_datasets == "Non-psychiatric" & 
       input$RII_measure_type == "Hospital residents")
      
    { #This is the message we are using.
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
    
    #Now let's create the "normal" version of the RII graph.
    else{
      
      #Create the tooltip, i.e., insert the information that pops up when the...
      #user hovers his/her mouse over a marker in the graph.
      #E.g.: 
      #"Treatment specialty: Non-psychiatric"
      #"Financial year: 2019/2020"
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
              
              #Select your variables.
              x = ~year, y = ~value, color = ~geography1, 
              
              #Specify the colour to be used.
              #We only need one colour, as we are only visualising Scotland.
              #We can go for dark blue.
              colors = c("#004785"),
              text = tooltip_RII, hoverinfo = "text",
              
              #Select the type of chart you want, in this case a scatter...
              #plot, but set the mode to 'lines+markers' in order to...
              #transform it into a line chart.
              #Set the line type and width (optional).
              #Choose the marker symbol and size (again optional), as well as...
              #the height and width of the graph.
              type = 'scatter', mode = 'lines+markers',
              line = list(dash = "dot", width = 3),
              marker = list(symbol = "circle-open", size = 12, opacity = 1),
              width = 1000, height = 600) %>%
        
        #Write the graph title.
        layout(title = paste0("<b>",
                              "Relative Index of Inequality:",
                              if(input$RII_measure_type == "Patients") 
                              {paste0(" Patients in")}
                              else if(input$RII_measure_type == "Discharges") 
                              {paste0(" Discharges from")}
                              else{paste0(" Hospital residents in")},
                              if(input$RII_datasets == "Psychiatric") 
                              {paste0(" psychiatric specialties,")}
                              else if(input$RII_datasets == "Non-psychiatric") 
                              {paste0(" non-psychiatric specialties,")}
                              else{paste0(" any treatment specialty,")},
                              "<br>",
                              first(as.vector(RII_new()$year)), 
                              " - ", 
                              last(as.vector(RII_new()$year)),
                              ", by financial year, residents of Scotland", 
                              "</b>"),
               separators = ".",
               
               #We need to fix the range of the y axis.
               #The following range() command calculates the lower limit as...
               #the minimum number visualised in the graph - 10% of this...
               #number, and defines the upper limit as the maximum number...
               #visualised in the graph + 10% of this number.
               yaxis = list(exponentformat = "none",
                            separatethousands = TRUE,
                            range = c(
                              min(RII_new()$value, na.rm = TRUE) * 90 / 100, 
                              max(RII_new()$value, na.rm = TRUE) * 110 / 100
                            ),
                            
                            #Wrap the y axis title in blank spaces so it...
                            #doesn't cover the tick labels.
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
               #for the tick labels (financial years) to display without...
               #covering each other.
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
               
               #Set the graph margins.
               margin = list(l = 90, r = 60, b = 170, t = 70),
               
               #Set the font sizes.
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #Add a legend in order to make it clear to the user...
               #that the location of residence being visualised is Scotland.
               #Make the legend background and legend border white.             
               showlegend = TRUE, 
               legend = list(x = 1, 
                             y = 1, 
                             bgcolor = 'rgba(255, 255, 255, 0)', 
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove any buttons we won't need from the modebar.
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                             'zoomOut2d', 'autoScale2d', 
                                             'toggleSpikelines', 
                                             'hoverCompareCartesian', 
                                             'hoverClosestCartesian'), 
               displaylogo = F, collaborate = F, editable = F)
      
    }
    
  })
  
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
  
  #Create the table that will appear under the RII line chart.
  #Provide clearer column names.
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
  
  #Create a download button that allows the user to download the RII table...
  #in .csv format.
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
  
  ##* Cross-boundary flow server ----   
  
  #There are three filters here: SELECT TREATMENT SPECIALTY, SELECT FINANCIAL...
  #YEAR and SELECT HEALTH BOARD OF RESIDENCE.
  #The reactive() command below creates a subset of the cross-boundary flow...
  #dataset based on the user's selections in these three filters.
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
             & year %in% input$flow_financial_year 
             & `health board of residence` %in% input$flow_board_of_residence) %>%
      filter(complete.cases(.)) %>%
      filter(measure != "Number of discharges") %>%
      filter(flow == 0) %>%
      filter(value > 0) %>%
      rename(`Number of patients` = value) %>%
      select(`health board of residence`, `health board of treatment`,
             `Number of patients`)
  })
  
  #Calculate the percentages and numbers which will be used in the...
  #dynamic sentence that appears above our Sankey diagram. We need to...
  #calculate 5 different values (listed below).
  output$flow_text <- renderText({
    
    flow_txt <- flow %>% 
      filter(dataset %in% input$flow_dataset
             & `health board of residence` %in% input$flow_board_of_residence 
             & year %in% input$flow_financial_year) %>%
      filter(complete.cases(.)) %>%
      filter(measure != "Number of discharges")
    
    if(sum(flow_txt$flow) == 0) {
      
      # a
      no_intraboard_flow_number <- flow_txt %>%
        summarise(sum(value)) %>%
        pull()
      
    }
    
    else{
      
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
    
    #We can now build our dynamic sentence.
    #This will be done using "if" statements.
    
    #Statement 1: If there are no patients for a given combination of...
    #selections, the user gets the message "No patients found".
    if(sum(flow_txt$value) == 0) {
      
      paste0("<b>",
             "No patients found.",
             "</b>")
      
    }
    
    #Statement 2: If there are patients, but no intra-board flow, the user...
    #gets a message saying that the percentage of patients from NHS X who...
    #were treated inside their board was 0%. The only boards with 0%... 
    #intra-board flow are NHS Orkney and NHS Shetland, in the psychiatric...
    #specialty.
    else if(sum(flow_txt$value) != 0 & 
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
    else if(sum(flow_txt$value) != 0 &
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
             else{paste0(" in any treatment specialty")},
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
    else if(sum(flow_txt$value) != 0 &
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
             else{paste0(" in any treatment specialty")},
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
    
    else {paste0(" ")}
    
  })
  
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
  
  #Create the table that will appear under the visual.
  #Rename each column to something better.
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
  
  #Create a download button to allow users to download the table in .csv format.
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
  
  ##* Readmissions server ----    
  
  #This tab will have two charts: one bar chart and one line chart.
  #They both utilise the same dataset.
  #The bar chart allows you to compare the readmission percentages of...
  #multiple HBs in a single year, whereas the line chart shows you a time...
  #trend of the readmission percentages of a single HB (or multiple HBs, but...
  #that's up to the user).
  #Both graphs allow the user to compare the HB percentage against the...
  #Scotland percentage. 
  
  #Bar chart:
  
  #Filter the dataset according to the user's selections in the filters...
  #SELECT FINANCIAL YEAR and SELECT MEASURE.
  readm_bar_chart_subset <- reactive({
    readmissions %>% 
      filter(year %in% input$readmissions_financial_year 
             & measure %in% input$readmissions_measure_type)
  })
  
  #Start creating the bar chart.
  output$readm_bar_chart <- renderPlotly({
    
    #Create the tooltip, i.e., insert the text that the users see when...
    #they hover their mouse over a bar in the chart.
    #E.g.:
    #"Treatment specialty: Psychiatric"
    #"Financial year: 2019/2020"
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
            
            #Choose your x (geography2), y (value), and grouping (geography2)...
            #variables.
            x = ~geography2, y = ~value, color = ~geography2, 
            
            #Choose colours for the bars.
            #We need to repeat the hexadecimal code for light blue 12 times,...
            #as there are 12 health boards here, and then we specify dark...
            #blue for Scotland. 
            colors = c(rep("#4CBEED", 12), "#004785"), 
            text = tooltip_readm_bar_chart, hoverinfo = "text",
            
            #Choose your chart type, in this case a bar chart, and set the...
            #width and height of the chart.
            type = 'bar', width = 1000, height = 600) %>%
      
      #Write the title of the graph, which will be dynamic, i.e., it will...
      #change according to the user's selections in the filters.
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
               
               #We need to fix the range of the y axis, as R refuses to set...
               #the lower end of this axis to zero.
               #The following "range" command fixes the lower end to...
               #zero, and calculates the upper end as the maximum...
               #number visualised in the graph + 10% of this number.
               exponentformat = "none",
               separatethousands = TRUE,
               range = c(0, max(readm_bar_chart_subset()$value, na.rm = TRUE) 
                         * 110 / 100),
               
               #The y axis title is dynamic, i.e., it changes...
               #according to the user's selection in the SELECT MEASURE filter.
               #However, we need to shorten the title, as it...
               #is too long to display normally, i.e., it doesn't fit the...
               #length of the axis.
               #Also, wrap the title in blank spaces so it...
               #doesn't cover the tick labels.
               title = paste0(c(
                 rep("&nbsp;", 20),
                 if(input$readmissions_measure_type == 
                    "Percentage readmissions within 28 days")
                 {print(c("% readmissions within 28d"))}
                 else{print(c("% readmissions within 133d"))},
                 rep("&nbsp;", 20),
                 rep("\n&nbsp;", 3)
               ),
               collapse = ""),
               showline = TRUE, 
               ticks = "outside"
               
             ),
             
             #Create a title for the x axis.
             #But wrap it in spaces, so it doesn't cover the...
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
             
             #Set the graph margins and font sizes.
             margin = list(l = 100, r = 10, b = 200, t = 70),
             font = list(size = 13),
             titlefont = list(size = 15),
             
             #No legend is needed to understand this graph, so remove it.
             showlegend = FALSE) %>%
      
      #Remove any buttons we don't plan to use from the plotly toolbar.
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                           'zoomOut2d', 'autoScale2d', 
                                           'toggleSpikelines', 
                                           'hoverCompareCartesian', 
                                           'hoverClosestCartesian'), 
             displaylogo = F, collaborate = F, editable = F)
    
  })
  
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
  
  #Finally, insert a download button for downloading the table as a .csv file.
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
  
  #We continue with the second graph, i.e., the line chart:
  
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
    
    #Create the tooltip, i.e., insert the information that the users see when...
    #they hover their mouse over a dot in the line chart.
    #E.g.:
    #"Treatment specialty: Psychiatric"
    #"Financial year: 2019/2020"
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
            
            #Choose your x, y, and grouping variables.
            x = ~year, y = ~value, color = ~geography2, 
            
            #Assign a colour to each location of treatment.
            #The user can select a maximum of four health boards, so we...
            #need four colours.
            #We will only be using two colours, but we can make the lines...
            #distinguishable in other ways, i.e., with symbols and different...
            #line types (see below).
            #Both colours here are shades of blue, for accessibility...
            #purposes.
            colors = c("#4CBEED", "#004785", "#4CBEED", "#004785"),
            text = tooltip_readm_line_chart, hoverinfo = "text",
            
            #Specify the type of chart you want, in this case a scatter plot,...
            #and set the mode to 'lines+markers' to transform it into a line...
            #chart. 
            #Then, set the marker (i.e., dot) size.
            type = 'scatter', mode = 'lines+markers', marker = list(size = 12),
            
            #Set the line width and line types.
            #Some line types will be repeated, but we will also be using...
            #symbols (see below), so in the end, it will be clear which line...
            #corresponds to which HB.
            line = list(width = 3),
            linetype = ~geography2,
            linetypes = c("solid", "solid", "dot", "dot"),
            
            #Set the symbols for the health boards. 
            symbol = ~geography2, 
            symbols = c("square-open", "x", "circle", "diamond"),
            
            #Set the width and height of the graph.
            #The "name" attribute tells plotly which variable to base the...
            #legend labels on. We use a string wrapper, so that only 10...
            #characters are visualised per line. This is meant to deal with...
            #boards with long names, like NHS Greater Glasgow & Clyde.
            width = 1000, height = 600, 
            name = ~str_wrap(geography2, 10)) %>%
      
      #Write the title of the graph.
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
             yaxis = list(exponentformat = "none", 
                          separatethousands = TRUE,
                          
                          #Fix the range of the y axis.
                          #Set the lower limit to zero and the upper limit to the...
                          #largest number visualised plus 10% of this number.
                          range = c(0, 
                                    max(readm_line_chart_subset()$value, 
                                        na.rm = TRUE) * 110 / 100),
                          
                          #Create a title for the y axis and make it dynamic. 
                          #Shorten the descriptions of the measures, as they...
                          #are too long to visualise correctly.
                          title = paste0(c(
                            rep("&nbsp;", 20),
                            if(input$readmissions_measure_type_two == 
                               "Percentage readmissions within 28 days")
                            {print(c("% readmissions within 28d"))}
                            else{print(c("% readmissions within 133d"))},
                            rep("&nbsp;", 20),
                            rep("\n&nbsp;", 3)
                          ),
                          collapse = ""),
                          showline = TRUE, ticks = "outside"
             ),
             
             #Add a title for the x axis, but wrap it in spaces...
             #so it doesn't cover the tick labels. Also, set the angle...
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
             
             #Set the margins of the graph.
             margin = list(l = 90, r = 60, b = 170, t = 70),
             
             #Set font sizes for the different elements of the graph.
             font = list(size = 13),
             titlefont = list(size = 15),
             
             #Add a legend showing the colour, line type and symbol used...
             #for each location of treatment.
             #Set the legend border and legend background colour to white.
             showlegend = TRUE, 
             legend = list(x = 1, 
                           y = 1, 
                           bgcolor = 'rgba(255, 255, 255, 0)', 
                           bordercolor = 'rgba(255, 255, 255, 0)')) %>%
      
      #Remove any unnecessary buttons from the plotly toolbar.
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                           'zoomOut2d', 'autoScale2d', 
                                           'toggleSpikelines', 
                                           'hoverCompareCartesian', 
                                           'hoverClosestCartesian'), 
             displaylogo = F, collaborate = F, editable = F)
    
  })
  
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
  
  #Create the actual table.
  #The name of the last column depends on the user's selection in the...
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
  
  #Add a download button, to allow the user to export the table in...
  #.csv format.
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
  
  ##* Table server ----    
  
  #On to the final tab, which is the Table tab.
  
  #The following piece of syntax tells R to switch between files...
  #based on the user's input in the filter SELECT DATA FILE.
  #The files below are the ones we read into R in the data manipulation section.
  #However, they require a few transformations before they can be displayed...
  #as a table.
  data_table <- reactive({
    switch(input$table_filenames,
           "Trends in diagnoses (Data explorer)" = diagnoses %>%
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
                    `Value` = Value) %>%
             filter(complete.cases(.)),
           "Length of stay (Trend data)" = length_of_stay %>%
             rename(`Financial year` = fyear, 
                    `Health board of treatment` = geography2,
                    `Stay specialty` = Specialty, 
                    `Length of stay category` = LengthOfStay, 
                    `Number of stays` = NumberOfStays)
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
  
  #We also create a download data button.
  output$download_table <- downloadHandler(
    filename = 'table_data.csv', 
    content = function(file) { 
      write.csv(
        #The command "input[["table_tab_rows_all"]]" tells R to create a .csv...
        #file that takes into account the user's input in the filters below...
        #the column names.
        data_table()[input[["table_tab_rows_all"]], ], 
        file, 
        row.names = FALSE
      )
    } 
  )
  
  ##* Glossary ----    
  
  #We have prepared a glossary to help the user understand the graphs and...
  #tables more easily.
  
  #Create a download button for the glossary.
  #The glossary was created in MS Word, and was then converted into a PDF.
  #The PDF was then placed in its own folder inside the working directory.
  #The folder is called "www". 
  glossary_code_shortcut <- downloadHandler(
    filename = 'Mental Health Inpatient Activity glossary.pdf',
    content = function(file) {
      file.copy("www/MHIA glossary.pdf", file)
    }
  )
  
  #There must be one download button in each tab (incl. the Introduction tab).
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
