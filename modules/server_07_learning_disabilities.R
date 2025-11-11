## [[Learning disabilities server]] ----

# Measure Selector
output$LD_measure_output <- renderUI({
  shinyWidgets::pickerInput( 
    "LD_measure_input",
    label = "Select measure",  
    choices = LD_measures,
    multiple = TRUE,
    width = '100%',
    options = list("max-options" = 3, `selected-text-format` = "count > 1"),
    selected = "Discharges")
})

LD_new <- reactive({
  LD_activity %>%
    filter(measure %in% input$LD_measure_input) %>%
    droplevels
})

# Create the line chart for the diagnoses tab.
# We create this using the plotly library.
output$LD_plot <- renderPlotly({
  
  # # Insert various IF statements, in case the user makes a combination of...
  # # selections that isn't valid.
  # 
  # # If the user makes a combination of selections that leads to zero...
  # # insert a message saying zero.
  # # Otherwise, the user might see an empty graph and think there...
  # # is something wrong with it.
  # if(sum(LD_new()$value) == 0 & 
  #         !is.na(sum(LD_new()$value)) &
  #         input$LD_measure_input != "") 
  #   
  # { # This is the message we are using.
  #   text_no_data_found <- list(
  #     x = 5, 
  #     y = 2, 
  #     font = list(color = "#0072B2", size = 20),
  #     text = paste0("No ", 
  #                   if(input$LD_measure_input == 
  #                      "Discharges")
  #                   {print(c("discharges"))}
  #                   else if(input$LD_measure_input == 
  #                           "Patients") 
  #                   {print(c("patients"))}
  #                   else{print(c("Stays"))}, 
  #                   " found."),
  #     xref = "x", 
  #     yref = "y",  
  #     showarrow = FALSE
  #   )
  #   
  #   # Visualise an empty graph with the above message in the middle.
  #   plot_ly() %>% 
  #     layout(annotations = text_no_data_found, 
  #            yaxis = list(showline = FALSE, 
  #                         showticklabels = FALSE, 
  #                         showgrid = FALSE), 
  #            xaxis = list(showline = FALSE, 
  #                         showticklabels = FALSE, 
  #                         showgrid = FALSE)) %>%  
  #     config(displayModeBar = FALSE,
  #            displaylogo = F, editable = F) 
  # }
  # 
  # # Now let's create the "normal" version of the diagnoses graph.
  # else{
    
    # Create the tooltip, i.e., insert the information that appears when you...
    # hover your mouse over a dot in the line graph.
    # E.g.:
    # "Financial year: 2019/2020"
    # "Number of discharges: XXXX"
    tooltip_LD <- paste0("Financial year: ", 
                                LD_new()$fyear, "<br>",
                                input$LD_measure_input, ": ", 
                                LD_new()$value)
    
    # Create the main body of the chart.
    plot_ly(data = LD_new(), 
            
            # Select your variables.
            x = ~fyear, y = ~value, color = ~measure,
            colors = c("#4CBEED", "#004785", "#4CBEED", "#004785"),
            text = tooltip_LD, hoverinfo = "text",  

            type = 'scatter', mode = 'lines+markers',

            line = list(width = 3),
            linetype = ~measure,
            linetypes = c("solid", "solid", "dot", "dot"),
            
            # Select symbols for the health boards, and set their size.
            symbol = ~measure, 
            symbols = c("square-open", "x", "circle", "diamond"),
            marker = list(size = 12),
            
            # Set the width and height of the graph.
            # The "name" attribute tells plotly which variable to base the...
            # legend labels on. We use a string wrapper, so that only 10...
            # characters are visualised per line. This is meant to deal with...
            # boards with long names, like NHS Greater Glasgow & Clyde.
            width = 1000, height = 600, 
            name = ~str_wrap(measure, 26)) %>%
      
      # Write the title of the graph, which must be dynamic.
      layout(title = 
               paste0(
                 "<b>", 
                 "Learning Disabilities", 
                 "<br>",
                 first(as.vector(LD_new()$fyear)), 
                 " - ", 
                 last(as.vector(LD_new()$fyear)),
                 ", by financial year",
                 "<br>",
                 "</b>"
               ),
             separators = ".",
             
             
             ## y-axis ----
             yaxis = list(
               # Wrap the y axis title in spaces so it doesn't cover the...
               # tick labels.
               title = "Number",
                 
                 # paste0(c(rep("&nbsp;", 20),
                 #                "Number", 
                 #                rep("&nbsp;", 20),
                 #                rep("\n&nbsp;", 3))),
               # list(font = list(size = 13)),
               exponentformat = "none",
               separatethousands = TRUE,
               # y-axis range
               range = c(0, max(LD_new()$value, na.rm = TRUE) * 110 / 100), 
               collapse = "",
               showline = TRUE, 
               ticks = "outside"
             ),
             

             ## x-axis ----
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
    
  # }
  
})

# Create table under graph
table_LD <- reactive({
  LD_activity %>%
    filter(measure %in% input$LD_measure_input) %>%
    select(fyear, value)
})

# Create the table that will go under the diagnoses line chart.
output$LD_table <- renderDataTable({
  datatable(table_LD(), 
            style = 'bootstrap', 
            class = 'table-bordered table-condensed', 
            rownames = FALSE, 
            options = list(pageLength = 16, autoWidth = TRUE, dom = 'tip'),
            colnames = c("Financial year", 
                         input$LD_measure))
})

# We also create a download button that allows users to download the...
# table in .csv format.
output$download_LD <- downloadHandler(
  filename = 'MHIA_learning_disability_graph_data.csv',
  content = function(file) {
    write.table(table_LD(), 
                file,
                #Remove row numbers as the .csv file already has row numbers.
                row.names = FALSE,
                col.names = c("Financial year",
                              "Measure"),
                              #input$LD_measure),
                sep = ",")
  }
)


##* Learning Disabilities Supplementary Workbook Download ----
output$LD_supplement <- downloadHandler(
  filename = 'Learning Disabilities 1997-2023.xlsx',
  content = function(file) {
    file.copy("www/Learning Disabilities Outputs 1997 - 2023.xlsx", file)
  }
)

















