##* Deprivation server ----   

# The Deprivation tab will contain two graphs:
# The first one will show activity broken down by deprivation quintile.
# The second one will display the RII as a trend over time.
# The first graph will be a bar chart, with each bar representing a quintile.
# The second graph will be a line chart.

# We start with the bar chart.

# The user's selection in the filter SELECT TYPE OF LOCATION determines...
# which choices appear in the filter SELECT LOCATION.
# The renderUI() command makes the process of creating these two dynamic...
# filters very easy.
# In the second filter, we set multiple to TRUE, as we want the user to be...
# able to compare multiple health boards at the same time. However, we set...
# the limit to four selections.
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

# There are five filters in total: SELECT TREATMENT SPECIALTY, SELECT TYPE...
# OF LOCATION, SELECT LOCATION, SELECT FINANCIAL YEAR, and SELECT MEASURE.
# The reactive() command below creates a subset of the deprivation...
# dataset based on the user's selections in these five filters.
# This subset will then "feed" the deprivation bar chart.
# Don't forget to drop the unused levels in your factors. This is essential...
# because, in the graph, we will be matching colours to factor levels.
deprivation_new <- reactive({
  deprivation %>%
    filter(dataset %in% input$deprivation_dataset 
           & geography1 %in% input$deprivation_location_type 
           & geography2 %in% input$deprivation_location 
           & year %in% input$deprivation_financial_year 
           & measure %in% input$deprivation_measure_type) %>%
    droplevels
})

# Create the bar chart for the deprivation tab.
# We create this using the plotly library.
output$deprivation_bar_chart <- renderPlotly({
  
  # Insert various IF statements, in case the user makes a combination of...
  # selections that isn't valid.
  
  # If the user makes a combination of selections that leads to zero...
  # patients/discharges/hospital residents, there needs to be a message...
  # saying no patients/discharges/residents found. Otherwise the user will...
  # see an empty graph and think the app is not working.
  if(sum(deprivation_new()$value) == 0 & 
     !is.na(sum(deprivation_new()$value)) &
     input$deprivation_location != "") 
    
  { # This is the message we are using.
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
    
    # Visualise an empty graph with the above message in the middle.
    plot_ly() %>% 
      layout(annotations = text_no_patients_found, 
             yaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE), 
             xaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE)) %>%  
      config(displayModeBar = FALSE,
             displaylogo = F, editable = F) 
  }
  
  # If the user selects non-psychiatric specialties + any of the 'hospital...
  # residents' measures, there needs to be a message saying that this is not...
  # a valid measure for non-psychiatric specialties.
  else if(input$deprivation_dataset == "Non-psychiatric" & 
          (input$deprivation_measure_type == 
           "Rate of hospital residents (per 100,000 population)" |
           input$deprivation_measure_type == 
           "Number of hospital residents"))
    
  { # This is the message we are using.
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
    
    # Visualise an empty graph with the above message in the middle.
    plot_ly() %>% 
      layout(annotations = text_hosp_res_non_psych_spec_bar_chart, 
             yaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE), 
             xaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE)) %>%  
      config(displayModeBar = FALSE,
             displaylogo = F, editable = F) 
  }
  
  # Now let's create the "normal" version of the graph.
  else {
    
    # Create the tooltip, i.e., insert the information that pops up when the...
    # user hovers his/her mouse over a bar in the graph.
    # E.g.:
    # "Treatment specialty: Non-psychiatric"
    # "Financial year: 2019/2020"
    # "Location of residence: Scotland"
    # "Deprivation quintile (SIMD): 2"
    # "Number of discharges: XXXX"
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
    
    # Create the main body of the bar chart.
    plot_ly(data = deprivation_new(),
            
            # Select your variables.
            x = ~simd, y = ~value, color = ~geography2, 
            
            # Assign a colour to each location of residence.
            # The user can select a maximum of four health boards, so we...
            # need four colours.
            # All the colours here are shades of blue, for accessibility...
            # purposes.
            colors = c("#004785", "#4C7EA9", "#00A2E5", "#99DAF5"),
            
            # Select the type of chart you want, in this case a bar chart.
            type = 'bar',
            text = tooltip_deprivation, hoverinfo = "text",
            textposition = "none", # removed text that appears next to bar
            
            # Set the width and height of the graph.
            # The "name" attribute tells plotly which variable to base the...
            # legend labels on. We use a string wrapper, so that only 10...
            # characters are visualised per line. This is meant to deal with...
            # boards with long names, like NHS Greater Glasgow & Clyde.
            width = 1000, height = 450, 
            name = ~str_wrap(geography2, 20)) %>%
      
      # Write the graph title.
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
             
             # We need to fix the range of the y axis, as R refuses to set...
             # the lower end of this axis to zero.
             # The following "range" command fixes the lower end to...
             # zero, and calculates the upper end as the maximum...
             # number visualised in the graph + 10% of this number.
             yaxis = list(
               exponentformat = "none",
               separatethousands = TRUE,
               range = c(0, max(deprivation_new()$value, na.rm = TRUE) * 110 / 100),
               
               # Wrap the y axis title in blank spaces so it...
               # doesn't cover the tick labels.
               title = list(font = list(size = 13)), paste0(c(
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
             
             # Label the x axis.
             xaxis = list(title = "Deprivation quintile (SIMD)", 
                          showline = TRUE, 
                          ticks = "outside"),        
             
             # Set the graph margins.
             margin = list(l = 80, r = 10, b = 50, t = 90),
             
             # Set the font sizes.
             font = list(size = 13),
             
             
             # Add a legend so that the user knows which colour...
             # corresponds to which location of residence.
             # Make the legend background and legend border white.
             showlegend = TRUE, 
             legend = list(x = 1, 
                           y = 1, 
                           bgcolor = 'rgba(255, 255, 255, 0)', 
                           bordercolor = 'rgba(255, 255, 255, 0)')) %>%
      
      # Remove any buttons we won't need from the modebar.
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                           # 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 
                                           'toggleSpikelines', 
                                           'hoverCompareCartesian', 
                                           'hoverClosestCartesian'), 
             displaylogo = F, editable = F)
    
  }
  
})

# This reactive() creates the subset that will be used to populate the table...
# appearing under the bar chart.
# Select the columns you need for the table and filter out NAs.
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

# Create the table that will appear under the deprivation bar chart.
# We need clearer column names.
# The name of the final column reacts to the user's input in the filter...
# SELECT MEASURE.
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

# Create a download button that allows the user to download the table...
# in .csv format.
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

# We can now move on to the second graph, i.e., the line chart that shows...
# changes in the RII over time.

# There will be two filters: SELECT TREATMENT SPECIALTY and SELECT MEASURE.
# The reactive() command below creates a subset of the RII dataset based on...
# the user's selections in these two filters.
# This subset will then "feed" the RII line chart.
RII_new <- reactive({
  RII %>% 
    filter(dataset %in% input$RII_datasets 
           & measure %in% input$RII_measure_type)
})

# Create the line chart.
# We do this using the plotly library.
output$RII_line_chart <- renderPlotly({
  
  # If the user selects non-psychiatric specialties + the measure 'Hospital...
  # residents', there needs to be a message saying that this is not...
  # a valid measure for non-psychiatric specialties.
  if(input$RII_datasets == "Non-psychiatric" & 
     input$RII_measure_type == "Hospital residents")
    
  { # This is the message we are using.
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
    
    # Visualise an empty graph with the above message in the middle.
    plot_ly() %>% 
      layout(annotations = text_hosp_res_non_psych_spec_line_chart, 
             yaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE), 
             xaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE)) %>%  
      config(displayModeBar = FALSE,
             displaylogo = F, editable = F) 
  }
  
  # Now let's create the "normal" version of the RII graph.
  else{
    
    # Create the tooltip, i.e., insert the information that pops up when the...
    # user hovers his/her mouse over a marker in the graph.
    # E.g.:
    # "Treatment specialty: Non-psychiatric"
    # "Financial year: 2019/2020"
    # "Location of residence: Scotland"
    # "Relative Index of Inequality - Patients: XX"
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
    
    # Create the main body of the line chart.
    plot_ly(data = RII_new(),
            
            # Select your variables.
            x = ~year, y = ~value, color = ~geography1, 
            
            # Specify the colour to be used.
            # We only need one colour, as we are only visualising Scotland.
            # We can go for dark blue.
            colors = c("#004785"),
            text = tooltip_RII, hoverinfo = "text",
            
            # Select the type of chart you want, in this case a scatter...
            # plot, but set the mode to 'lines+markers' in order to...
            # transform it into a line chart.
            # Set the line type and width (optional).
            # Choose the marker symbol and size (again optional), as well as...
            # the height and width of the graph.
            type = 'scatter', mode = 'lines+markers',
            line = list(dash = "dot", width = 3),
            marker = list(symbol = "circle-open", size = 12, opacity = 1),
            width = 1000, height = 600) %>%
      
      # Write the graph title.
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
             
             # We need to fix the range of the y axis.
             # The following range() command calculates the lower limit as...
             # the minimum number visualised in the graph - 10% of this...
             # number, and defines the upper limit as the maximum number...
             # visualised in the graph + 10% of this number.
             yaxis = list(exponentformat = "none",
                          separatethousands = TRUE,
                          range = c(
                            min(RII_new()$value, na.rm = TRUE) * 90 / 100, 
                            max(RII_new()$value, na.rm = TRUE) * 110 / 100
                          ),
                          
                          # Wrap the y axis title in blank spaces so it...
                          # doesn't cover the tick labels.
                          title = list(font = list(size = 13)), paste0(c(
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
             
             # Label the x axis.
             # Also, set the x axis tick angle to minus 45. It's the only way...
             # for the tick labels (financial years) to display without...
             # covering each other.
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
             
             # Set the graph margins.
             margin = list(l = 90, r = 60, b = 170, t = 70),
             
             # Set the font sizes.
             font = list(size = 13),
             
             
             # Add a legend in order to make it clear to the user...
             # that the location of residence being visualised is Scotland.
             # Make the legend background and legend border white.
             showlegend = TRUE, 
             legend = list(x = 1, 
                           y = 1, 
                           bgcolor = 'rgba(255, 255, 255, 0)', 
                           bordercolor = 'rgba(255, 255, 255, 0)')) %>%
      
      # Remove any buttons we won't need from the modebar.
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                           # 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 
                                           'toggleSpikelines', 
                                           'hoverCompareCartesian', 
                                           'hoverClosestCartesian'), 
             displaylogo = F, editable = F)
    
  }
  
})

# This reactive() creates the subset that will be used to populate the table...
# appearing under the line chart.
# Select the columns you need for the table and filter out NAs.
table_RII <- reactive({
  RII %>% 
    filter(dataset %in% input$RII_datasets 
           & measure %in% input$RII_measure_type) %>% 
    select(dataset, year, geography1, value) %>%
    filter(complete.cases(.))
})

# Create the table that will appear under the RII line chart.
# Provide clearer column names.
# The name of the final column reacts to the user's input in the filter...
# SELECT MEASURE.
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

# Create a download button that allows the user to download the RII table...
# in .csv format.
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
