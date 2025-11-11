## [[Trends in diagnoses server]] ----

# The user's selection in the filter SELECT TYPE OF LOCATION determines...
# which choices appear in the filter SELECT LOCATION.
# The renderUI() command makes the process of creating these two dynamic...
# filters very easy.
# In the second renderUI() command, we set multiple to TRUE, as we want the...
# user to be able to select multiple locations. However, we set the limit to...
# four selections for accessibility reasons.
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

# There are five filters in total: SELECT TREATMENT SPECIALTY, SELECT TYPE...
# OF LOCATION, SELECT LOCATION, SELECT DIAGNOSIS GROUPING, and SELECT MEASURE.
# The reactive() command below creates a subset of the diagnoses dataset...
# based on the user's selections in these five filters.
# This subset will then "feed" the diagnoses line chart.
# Don't forget to drop the unused levels in your factors. This is essential...
# because, in the graph, we will be matching colours to factor levels.
diagnoses_new <- reactive({
  diagnoses %>%
    filter(dataset %in% input$diagnoses_dataset
           & geography1 %in% input$diagnoses_location_type 
           & geography2 %in% input$diagnoses_location
           & diagnosis_groupings %in% input$diagnoses_diagnosis_groupings 
           & measure %in% input$diagnoses_measure_type) %>%
    droplevels
})

# Create the line chart for the diagnoses tab.
# We create this using the plotly library.
output$diagnoses_plot <- renderPlotly({
  
  # Insert various IF statements, in case the user makes a combination of...
  # selections that isn't valid.
  
  # If the user selects psychiatric/total specialties + State Hospital +...
  # rates, there needs to be a message saying that no rates are...
  # available for State Hospital.
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
    
  { # This is the message we are using.
    text_state_hosp_rates <- list(
      x = 5, 
      y = 2,
      font = list(color = "#0072B2", size = 20),
      text = "No rates available for location of treatment 'State Hospital'.", 
      xref = "x", 
      yref = "y",  
      showarrow = FALSE
    ) 
    
    # Visualise an empty graph with the above message in the middle.
    plot_ly() %>% 
      layout(annotations = text_state_hosp_rates, 
             yaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE), 
             xaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE)) %>%  
      config(displayModeBar = FALSE,
             displaylogo = F, editable = F) 
  }
  
  # If the user selects non-psychiatric specialties + State Hospital, there...
  # needs to be a message saying that State Hospital is an invalid...
  # selection for non-psychiatric specialties.
  else if(input$diagnoses_dataset == "Non-psychiatric" & 
          any(input$diagnoses_location == 
              "The State Hospitals Board for Scotland"))
    
  { # This is the message we are using.
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
    
    # Visualise an empty graph with the above message in the middle.
    plot_ly() %>% 
      layout(annotations = text_state_hosp_non_psych_spec, 
             yaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE), 
             xaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE)) %>%  
      config(displayModeBar = FALSE,
             displaylogo = F, editable = F) 
  }
  
  # If the user selects psychiatric specialties and then either Orkney or...
  # Shetland, we need to clarify that these boards have no psychiatric...
  # facilities.
  else if(input$diagnoses_dataset == "Psychiatric" & 
          any(input$diagnoses_location == "NHS Shetland" | 
              input$diagnoses_location == "NHS Orkney"))
    
  { # This is the message we are using.
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
    
    # Visualise an empty graph with the above message in the middle.
    plot_ly() %>% 
      layout(annotations = text_orkn_shetl_psych_spec, 
             yaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE), 
             xaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE)) %>%  
      config(displayModeBar = FALSE,
             displaylogo = F, editable = F) 
  }
  
  # If the user selects 'Total' specialty + any of the hospital residents...
  # measures + NHS Orkney/NHS Shetland, we display a message that says it...
  # was not possible to calculate this (a) because these boards have no...
  # psychiatric hospitals and (b) because hospital residents have not...
  # been calculated for non-psychiatric specialties.
  else if(input$diagnoses_dataset == "Total" & 
          any(input$diagnoses_location == "NHS Shetland" | 
              input$diagnoses_location == "NHS Orkney") & 
          (input$diagnoses_measure_type == "Number of hospital residents" |
           input$diagnoses_measure_type == 
           "Rate of hospital residents (per 100,000 population)"))
    
  { # This is the message we are using.
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
    
    # Visualise an empty graph with the above message in the middle.
    plot_ly() %>% 
      layout(annotations = text_orkn_shetl_total_spec_SH, 
             yaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE), 
             xaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE)) %>%  
      config(displayModeBar = FALSE,
             displaylogo = F, editable = F) 
  }
  
  # If the user selects non-psychiatric specialties + hospital...
  # residents, we need to clarify that this type of measure is meaningless...
  # for non-psychiatric specialties.
  else if(input$diagnoses_dataset == "Non-psychiatric" & 
          (input$diagnoses_measure_type == "Number of hospital residents" |
           input$diagnoses_measure_type == 
           "Rate of hospital residents (per 100,000 population)"))
    
  { # This is the message we are using.
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
    
    # Visualise an empty graph with the above message in the middle.
    plot_ly() %>% 
      layout(annotations = text_hosp_res_non_psych_spec, 
             yaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE), 
             xaxis = list(showline = FALSE, 
                          showticklabels = FALSE, 
                          showgrid = FALSE)) %>%  
      config(displayModeBar = FALSE,
             displaylogo = F, editable = F) 
  }
  
  # If the user makes a combination of selections that leads to zero...
  # patients/discharges/residents, insert a message saying zero patients/...
  # discharges/residents found.
  # Otherwise, the user might see an empty graph and think there...
  # is something wrong with it.
  else if(sum(diagnoses_new()$value) == 0 & 
          !is.na(sum(diagnoses_new()$value)) &
          input$diagnoses_location != "") 
    
  { # This is the message we are using.
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
  
  # Now let's create the "normal" version of the diagnoses graph.
  else{
    
    # Create the tooltip, i.e., insert the information that appears when you...
    # hover your mouse over a dot in the line graph.
    # E.g.:
    # "Treatment specialty: Non-psychiatric"
    # "Financial year: 2019/2020"
    # "Location of treatment: Scotland"
    # "Diagnosis grouping: Disorders of adult behaviour and personality"
    # "Number of discharges: XXXX"
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
    
    # Create the main body of the chart.
    plot_ly(data = diagnoses_new(), 
            
            # Select your variables.
            x = ~year, y = ~value, color = ~geography2,
            
            # Assign a colour to each location of treatment.
            # The user can select a maximum of four health boards, so we...
            # need four colours.
            # We will be using only two colours, but we can make the lines...
            # distinguishable in other ways, i.e., with symbols and different...
            # line types (see below).
            # Both colours here are shades of blue, for accessibility...
            # purposes.
            colors = c("#4CBEED", "#004785", "#4CBEED", "#004785"),
            text = tooltip_diagnoses, hoverinfo = "text",  
            
            # Select the type of chart you want, in this case a scatter...
            # plot, but set the mode to 'lines+markers' in order to...
            # transform it into a line chart.
            type = 'scatter', mode = 'lines+markers',
            
            # Set the line width, and the line types for the four health boards.
            # Some line types will be repeated, but we will also be using...
            # symbols (see below), so in the end, it will be clear which...
            # line corresponds to which health board.
            line = list(width = 3),
            linetype = ~geography2,
            linetypes = c("solid", "solid", "dot", "dot"),
            
            # Select symbols for the health boards, and set their size.
            symbol = ~geography2, 
            symbols = c("square-open", "x", "circle", "diamond"),
            marker = list(size = 12),
            
            # Set the width and height of the graph.
            # The "name" attribute tells plotly which variable to base the...
            # legend labels on. We use a string wrapper, so that only 10...
            # characters are visualised per line. This is meant to deal with...
            # boards with long names, like NHS Greater Glasgow & Clyde.
            width = 1000, height = 600, 
            name = ~str_wrap(geography2, 26)) %>%
      
      # Write the title of the graph, which must be dynamic.
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
             
             # We need to fix the range of the y axis, as R refuses to set...
             # the lower end of this axis to zero.
             # The following "range" command fixes the lower end to...
             # zero, and calculates the upper end as the maximum...
             # number visualised in the graph + 10% of this number.
             yaxis = list(
               exponentformat = "none",
               separatethousands = TRUE,
               range = c(0, max(diagnoses_new()$value, na.rm = TRUE) * 110 / 100), 
               
               # Wrap the y axis title in spaces so it doesn't cover the...
               # tick labels.
               title = list(font = list(size = 13)),
               paste0(c(rep("&nbsp;", 20),
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
             
             # Set the x axis tick angle to minus 45. It's the only way for...
             # the tick labels (financial years) to display without...
             # covering each other.
             # Also, wrap the x axis title in spaces so it doesn't...
             # cover the tick labels.
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
             margin = list(l = 90, r = 60, b = 170, t = 90),
             
             # Set the font sizes.
             font = list(size = 13),
             
             # Add a legend so that the user knows which colour, line type...
             # and symbol corresponds to which location of treatment.
             # Make the legend background and legend border white.              
             showlegend = TRUE,
             legend = list(x = 1, 
                           y = 0.8, 
                           bgcolor = 'rgba(255, 255, 255, 0)', 
                           bordercolor = 'rgba(255, 255, 255, 0)')) %>%
      
      # Remove any buttons we don't need from the modebar.
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
# Select the columns you need for the table, and filter out NAs.
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

# We now create the table that will go under the diagnoses line chart.
# We need to provide clearer column names.
# The name of the last column, which is the measure column, changes...
# according to the user's input in the filter SELECT MEASURE.
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

# We also create a download button that allows users to download the...
# table in .csv format.
output$download_diagnoses <- downloadHandler(
  filename = 'MHIA_diagnosis_graph_data.csv',
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