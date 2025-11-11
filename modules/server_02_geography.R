##* Geography server ----

# Similar to in the diagnosis tab but with fewer pickers.
# In the renderUI() command, we set multiple to TRUE, as we want the...
# user to be able to select multiple locations. However, we set the limit to...
# four selections for accessibility reasons.

output$geography_location_input <- renderUI({
  shinyWidgets::pickerInput( 
    "geography_location_input",
    label = "Select location (up to four selections allowed)",  
    choices = sort(unique(as.character(geography$ca))),
    multiple = TRUE,
    width = '100%',
    options = list("max-options" = 4, `selected-text-format` = "count > 1"),
    selected = "Aberdeen City"
  )
})

# There are three filters in total: SELECT TREATMENT SPECIALTY, SELECT LOCATION,
# and SELECT MEASURE.
# The reactive() command below creates a subset of the diagnoses dataset...
# based on the user's selections in these five filters.
# This subset will then "feed" the geography line chart.
# Don't forget to drop the unused levels in your factors. This is essential...
# because, in the graph, we will be matching colours to factor levels.

geography_new <- reactive({
  geography %>%
    filter(dataset %in% input$geography_dataset_input
           & ca %in% input$geography_location_input
           & measure %in% input$geography_measure_input) %>%
    select(dataset, fyear, ca, measure, Value)
})




# Create the line chart for the diagnoses tab.
# We create this using the plotly library.
output$geography_plot <- renderPlotly({
  
    # Create the tooltip, i.e., insert the information that appears when you...
    # hover your mouse over a dot in the line graph.
    # E.g.:
    # "Treatment specialty: Non-psychiatric"
    # "Financial year: 2019/2020"
    # "Location of treatment: Scotland"
    # "Diagnosis grouping: Disorders of adult behaviour and personality"
    # "Number of discharges: XXXX"
    tooltip_geography <- paste0("Treatment specialty: ",
                                geography_new()$dataset, "<br>",
                                "Financial year: ", 
                                geography_new()$fyear, "<br>",
                                # "Council area of residence: ", 
                                # geography_new()$ca, "<br>",
                                "Location: ", "<br>",
                                # geography_new()$diagnosis_groupings, "<br>",
                                input$geography_measure_input, ": ", 
                                geography_new()$Value)
    
    # Create the main body of the chart.
    plot_ly(data = geography_new(), 
            
            # Select your variables.
            x = ~fyear, y = ~Value, color = ~ca,
            
            # The user can select a maximum of four council, so we...
            # need four colours.
            # Both colours here are shades of blue, for accessibility purposes.
            colors = c("#4CBEED", "#004785", "#4CBEED", "#004785"),
            text = tooltip_geography, hoverinfo = "text",  
            
            # Select the type of chart you want, in this case a scatter...
            # plot, but set the mode to 'lines+markers' in order to...
            # transform it into a line chart.
            type = 'scatter', mode = 'lines+markers',
            
            # Set the line width, and the line types
            # Some line types will be repeated, but we will also be using...
            # symbols (see below), so in the end, it will be clear which...
            # line corresponds to which council area.
            line = list(width = 3),
            linetype = ~ca,
            linetypes = c("solid", "solid", "dot", "dot"),
            
            # Select symbols for the health boards, and set their size.
            symbol = ~ca, 
            symbols = c("square-open", "x", "circle", "diamond"),
            marker = list(size = 12),
            
            # Set the width and height of the graph.
            # The "name" attribute tells plotly which variable to base the...
            # legend labels on. We use a string wrapper, so that only 10...
            # characters are visualised per line. This is meant to deal with...
            # boards with long names, like NHS Greater Glasgow & Clyde.
            width = 1000, height = 600, 
            name = ~str_wrap(ca, 26)) %>%
      
      # Write the title of the graph, which must be dynamic.
      layout(title = 
               paste0(
                 "<b>", 
                 input$geography_measure_input,
                 if(input$geography_measure_input == 
                    "Rate of discharges (per 100,000 population)") 
                 {paste0(" from")}
                 else{paste0(" in")},
                 if(input$geography_dataset_input == "Psychiatric") 
                 {paste0(" psychiatric specialties")}
                 else if(input$geography_dataset_input == "Non-psychiatric") 
                 {paste0(" non-psychiatric specialties")}
                 else{paste0(" any treatment specialty")},
                 ",",
                 "<br>", 
                 # " where the council area of residence is", 
                 # "'", input$geography_location_input, "',", 
                 # "<br>",
                 first(as.vector(geography_new()$fyear)), 
                 " - ", 
                 last(as.vector(geography_new()$fyear)),
                 ", by financial year and council area of residence.",
                 # "<br>",
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
               range = c(0, max(geography_new()$Value) * 110 / 100), 
               
               # Wrap the y axis title in spaces so it doesn't cover the...
               # tick labels.
               title = list(font = list(size = 13)),
               paste0(c(rep("&nbsp;", 20),
                        if(input$geography_measure_input == 
                           "Rate of discharges (per 100,000 population)")
                        {print(c("Rate of discharges"))}
                        else if(input$geography_measure_input == 
                                "Rate of patients (per 100,000 population)")
                        {print(c("Rate of patients"))}
                        # else if(input$geography_measure_input == 
                        #         "Rate of hospital residents (per 100,000 population)")
                        # {print(c("Rate of hospital residents"))}
                        else{print(c(input$geography_measure_input))}, 
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
    
  # } #uncomment if using if/else statements again
  
})

# Can remove if everything works with the below section loading table from earlier selection.
# This reactive() creates the subset that will be used to populate the table...
# appearing under the line chart.
# Select the columns you need for the table, and filter out NAs.
# table_diagnoses <- reactive({
#   diagnoses %>% 
#     filter(dataset %in% input$diagnoses_dataset 
#            & geography1 %in% input$diagnoses_location_type 
#            & geography2 %in% input$diagnoses_location
#            & diagnosis_groupings %in% input$diagnoses_diagnosis_groupings 
#            & measure %in% input$geography_measure_type) %>%
#     select(dataset, year, geography1, geography2, diagnosis_groupings, 
#            value) %>%
#     filter(complete.cases(.))
# })

# We now create the table that will go under the diagnoses line chart.
# We need to provide clearer column names.
# The name of the last column, which is the measure column, changes...
# according to the user's input in the filter SELECT MEASURE.
output$geography_table <- renderDataTable({
  datatable(geography_new(), 
            style = 'bootstrap', 
            class = 'table-bordered table-condensed', 
            rownames = FALSE, 
            options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'),
            colnames = c("Treatment specialty", "Financial year", 
                         "Council area of residence", input$geography_measure_input, "Value"))
})

# We also create a download button that allows users to download the...
# table in .csv format.
output$download_geography <- downloadHandler(
  filename = 'MHIA_geography_selection_data.csv',
  content = function(file) {
    write.table(geography_new(), 
                file,
                # Remove row numbers as the .csv file already has row numbers.
                row.names = FALSE,
                col.names = c("Treatment specialty", "Financial year", 
                              "Council area of residence", input$geography_measure_input), "Value", 
                sep = ",")
  }
)