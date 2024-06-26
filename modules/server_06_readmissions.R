##* Readmissions server ----    

# This tab will have two charts: one bar chart and one line chart.
# They both utilise the same dataset.
# The bar chart allows you to compare the readmission percentages of...
# multiple HBs in a single year, whereas the line chart shows you a time...
# trend of the readmission percentages of a single HB (or multiple HBs, but...
# that's up to the user).
# Both graphs allow the user to compare the HB percentage against the...
# Scotland percentage.

# Bar chart:

# Filter the dataset according to the user's selections in the filters...
# SELECT FINANCIAL YEAR and SELECT MEASURE.
readm_bar_chart_subset <- reactive({
  readmissions %>% 
    filter(year %in% input$readmissions_financial_year 
           & measure %in% input$readmissions_measure_type)
})

# Start creating the bar chart.
output$readm_bar_chart <- renderPlotly({
  
  # Create the tooltip, i.e., insert the text that the users see when...
  # they hover their mouse over a bar in the chart.
  # E.g.:
  # "Treatment specialty: Psychiatric"
  # "Financial year: 2019/2020"
  # "Location of treatment: Scotland"
  # "Percentage readmissions within 28 days: XX"
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
          
          # Choose your x (geography2), y (value), and grouping (geography2)...
          # variables.
          x = ~geography2, y = ~value, color = ~geography2, 
          
          # Choose colours for the bars.
          # We need to repeat the hexadecimal code for light blue 12 times,...
          # as there are 12 health boards here, and then we specify dark...
          # blue for Scotland.
          colors = c(rep("#4CBEED", 12), "#004785"), 
          text = tooltip_readm_bar_chart, hoverinfo = "text",
          
          # Choose your chart type, in this case a bar chart, and set the...
          # width and height of the chart.
          type = 'bar', width = 1000, height = 600) %>%
    
    # Write the title of the graph, which will be dynamic, i.e., it will...
    # change according to the user's selections in the filters.
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
             
             # We need to fix the range of the y axis, as R refuses to set...
             # the lower end of this axis to zero.
             # The following "range" command fixes the lower end to...
             # zero, and calculates the upper end as the maximum...
             # number visualised in the graph + 10% of this number.
             exponentformat = "none",
             separatethousands = TRUE,
             range = c(0, max(readm_bar_chart_subset()$value, na.rm = TRUE) 
                       * 110 / 100),
             
             # The y axis title is dynamic, i.e., it changes...
             # according to the user's selection in the SELECT MEASURE filter.
             # However, we need to shorten the title, as it...
             # is too long to display normally, i.e., it doesn't fit the...
             # length of the axis.
             # Also, wrap the title in blank spaces so it...
             # doesn't cover the tick labels.
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
           
           # Create a title for the x axis.
           # But wrap it in spaces, so it doesn't cover the...
           # axis tick marks and tick labels.
           # Also, set the tick angle to -25 degrees, otherwise the HB names...
           # (i.e., the tick labels) will overlap with each other.
           xaxis = list(tickangle = -25, 
                        title = list(font = list(size = 13)), paste0(c(rep("&nbsp;", 20),
                                                                       "<br>",
                                                                       "<br>",
                                                                       "Location of treatment",
                                                                       rep("&nbsp;", 20),
                                                                       rep("\n&nbsp;", 3)),
                                                                     collapse = ""),
                        showline = TRUE, 
                        ticks = "outside"), 
           
           # Set the graph margins and font sizes.
           margin = list(l = 100, r = 10, b = 200, t = 70),
           font = list(size = 13),
           
           
           # No legend is needed to understand this graph, so remove it.
           showlegend = FALSE) %>%
    
    # Remove any buttons we don't plan to use from the plotly toolbar.
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                         # 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian'), 
           displaylogo = F, editable = F)
  
})

# This subset "feeds" the table that appears under the bar chart.
# Select the columns you need for the table and use arrange() to sort the...
# data however you like (optional).
first_table_readm <- reactive({
  readmissions %>% 
    filter(year %in% input$readmissions_financial_year 
           & measure %in% input$readmissions_measure_type) %>% 
    select(dataset, year, geography2, value) %>%
    arrange(year, geography2)
})

# Create the actual table using the subset above.
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

# Finally, insert a download button for downloading the table as a .csv file.
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

# We continue with the second graph, i.e., the line chart:

# Filter the readmissions basefile according to the user's selections in the...
# filters for location of treatment and measure type.
# Don't forget to drop the unused levels in your factors. This is essential...
# because, in the graph, we will be matching colours to factor levels.
readm_line_chart_subset <- reactive({
  readmissions %>% 
    filter(geography2 %in% input$readmissions_location 
           & measure %in% input$readmissions_measure_type_two) %>%
    droplevels
})

# Create the line chart.
output$readm_line_chart <- renderPlotly({
  
  # Create the tooltip, i.e., insert the information that the users see when...
  # they hover their mouse over a dot in the line chart.
  # E.g.:
  # "Treatment specialty: Psychiatric"
  # "Financial year: 2019/2020"
  # "Location of treatment: Scotland"
  # "Percentage readmissions within 28 days: XX"
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
          
          # Choose your x, y, and grouping variables.
          x = ~year, y = ~value, color = ~geography2, 
          
          # Assign a colour to each location of treatment.
          # The user can select a maximum of four health boards, so we...
          # need four colours.
          # We will only be using two colours, but we can make the lines...
          # distinguishable in other ways, i.e., with symbols and different...
          # line types (see below).
          # Both colours here are shades of blue, for accessibility...
          # purposes.
          colors = c("#4CBEED", "#004785", "#4CBEED", "#004785"),
          text = tooltip_readm_line_chart, hoverinfo = "text",
          
          # Specify the type of chart you want, in this case a scatter plot,...
          # and set the mode to 'lines+markers' to transform it into a line...
          # chart.
          # Then, set the marker (i.e., dot) size.
          type = 'scatter', mode = 'lines+markers', marker = list(size = 12),
          
          # Set the line width and line types.
          # Some line types will be repeated, but we will also be using...
          # symbols (see below), so in the end, it will be clear which line...
          # corresponds to which HB.
          line = list(width = 3),
          linetype = ~geography2,
          linetypes = c("solid", "solid", "dot", "dot"),
          
          # Set the symbols for the health boards. 
          symbol = ~geography2, 
          symbols = c("square-open", "x", "circle", "diamond"),
          
          # Set the width and height of the graph.
          # The "name" attribute tells plotly which variable to base the...
          # legend labels on. We use a string wrapper, so that only 10...
          # characters are visualised per line. This is meant to deal with...
          # boards with long names, like NHS Greater Glasgow & Clyde.
          width = 1000, height = 600, 
          name = ~str_wrap(geography2, 10)) %>%
    
    # Write the title of the graph.
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
                        
                        # Fix the range of the y axis.
                        # Set the lower limit to zero and the upper limit to the...
                        # largest number visualised plus 10% of this number.
                        range = c(0, 
                                  max(readm_line_chart_subset()$value, 
                                      na.rm = TRUE) * 110 / 100),
                        
                        # Create a title for the y axis and make it dynamic.
                        # Shorten the descriptions of the measures, as they...
                        # are too long to visualise correctly.
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
           
           # Add a title for the x axis, but wrap it in spaces...
           # so it doesn't cover the tick labels. Also, set the angle...
           # of the tick labels to 45 degrees, so they don't overlap with...
           # each other.
           xaxis = list(tickangle = -45,
                        title = list(font = list(size = 13)), paste0(c(rep("&nbsp;", 20),
                                                                       "<br>",
                                                                       "<br>",
                                                                       "Financial year",
                                                                       rep("&nbsp;", 20),
                                                                       rep("\n&nbsp;", 3)),
                                                                     collapse = ""),
                        showline = TRUE, 
                        ticks = "outside"), 
           
           # Set the margins of the graph.
           margin = list(l = 90, r = 60, b = 170, t = 70),
           
           # Set font sizes for the different elements of the graph.
           font = list(size = 13),
           
           
           # Add a legend showing the colour, line type and symbol used...
           # for each location of treatment.
           # Set the legend border and legend background colour to white.
           showlegend = TRUE, 
           legend = list(x = 1, 
                         y = 1, 
                         bgcolor = 'rgba(255, 255, 255, 0)', 
                         bordercolor = 'rgba(255, 255, 255, 0)')) %>%
    
    # Remove any unnecessary buttons from the plotly toolbar.
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                         # 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian'), 
           displaylogo = F, editable = F)
  
})

# The following reactive() creates the subset that will be used to populate...
# the table appearing under the line chart.
# Select the columns you need for the table and then sort the table however...
# you like (optional).
second_table_readm <- reactive({
  readmissions %>% 
    filter(geography2 %in% input$readmissions_location 
           & measure %in% input$readmissions_measure_type_two) %>% 
    select(dataset, year, geography2, value) %>%
    arrange(year, geography2)
})

# Create the actual table.
# The name of the last column depends on the user's selection in the...
# SELECT MEASURE filter.
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

# Add a download button, to allow the user to export the table in...
# .csv format.
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