##* Age/sex server ----   

# Moving on to the age/sex data.
# The user's selection in the filter SELECT TYPE OF LOCATION determines...
# which choices appear in the filter SELECT LOCATION.
# The renderUI() command makes the process of creating these two dynamic... 
# filters very easy. 
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

# There are five filters in total: SELECT TREATMENT SPECIALTY, SELECT TYPE...
# OF LOCATION, SELECT LOCATION, SELECT FINANCIAL YEAR, and SELECT MEASURE.
# The reactive() command below creates a subset of the age/sex...
# dataset based on the user's selections in these five filters.
# This subset will then "feed" the age/sex pyramid.
age_sex_new <- reactive({
  age_sex %>% 
    filter(dataset %in% input$age_sex_dataset 
           & geography1 %in% input$age_sex_location_type 
           & geography2 %in% input$age_sex_location 
           & year %in% input$age_sex_financial_year 
           & measure %in% input$age_sex_measure_type)
})

# Create the pyramid chart for the age/sex tab.
# We create this using the plotly library.
output$age_sex_pyramid <- renderPlotly({
  
  # Insert various IF statements, in case the user makes a combination of...
  # selections that isn't valid.
  
  # If the user makes a combination of selections that leads to zero...
  # patients or discharges, there needs to be a message saying no patients/...
  # discharges found. Otherwise the user will see an empty graph and...
  # think the app is not working.
  if(sum(abs(age_sex_new()$value)) == 0 & 
     !is.na(sum(abs(age_sex_new()$value)))) 
    
  { # This is the message we are using.
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
  
  # If the user selects the location 'Other' AND rates, there...
  # needs to be a message saying that no rates are available for...
  # location 'Other'.
  else if(input$age_sex_location == "Other" &  
          (input$age_sex_measure_type == 
           "Rate of patients (per 100,000 population)" |
           input$age_sex_measure_type == 
           "Rate of discharges (per 100,000 population)"))
    
  { #- This is the message we are using.
    text_other <- list(
      x = 5, 
      y = 2, 
      font = list(color = "#0072B2", size = 20),
      text = "No rates available for location of residence 'Other'.", 
      xref = "x", 
      yref = "y",  
      showarrow = FALSE
    )
    
    # Visualise an empty graph with the above message in the middle.
    plot_ly() %>% 
      layout(annotations = text_other, 
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
  else{
    
    # Create the tooltip, i.e., insert the information that pops up when...
    # you hover your mouse over a bar in the pyramid.
    # E.g.:
    # "Treatment specialty: Psychiatric"
    # "Financial year: 2019/2020"
    # "Location of residence: Scotland"
    # "Age group: Ages 65+"
    # "Sex: Female"
    # "Number of discharges: XXXX"
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
    
    # Create the main body of the chart.
    plot_ly(data = age_sex_new(),
            
            # Select your variables.
            x = ~value, y = ~ageband, color = ~sex_char,
            
            # Colour palette:
            # Dark blue for males and light blue for females.
            colors = c("#004785", "#4CBEED"),
            text = tooltip_age_sex, 
            hoverinfo = "text", 
            textposition = "none", # removed text that appears next to bar
            
            # Select the type of chart you want, in this case a bar chart,...
            # and set the orientation to horizontal to achieve the...
            # "pyramid" look. Set the width and height of the graph.
            type = 'bar', orientation = 'h',
            width = 1000, height = 400) %>%
      
      # Write the title of the graph, which should be dynamic.
      layout(title = list(font = list(size = 13)), paste0("<b>", 
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
             
             # Set the gap size between bars, and make sure that the bars...
             # overlay, again to achieve that "pyramid" look.
             bargap = 0.2, barmode = 'overlay',
             
             # Write the y axis title.
             # Wrap the title in empty spaces so it doesn't cover the tick...
             # labels.
             yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                           "Age group",
                                           rep("&nbsp;", 20),
                                           rep("\n&nbsp;", 3)),
                                         collapse = ""), 
                          showline = TRUE, ticks = "outside"),
             xaxis = list(exponentformat = "none",
                          separatethousands = TRUE,
                          tickmode = 'array',
                          
                          # Define the range of the x axis.
                          # This is recalculated every time the user makes a new selection.
                          range = c(
                            -round(max(abs(age_sex_new()$value)) * 110 / 100),
                            round(max(abs(age_sex_new()$value)) * 110 / 100)
                          ),
                          tickangle = 0,
                          
                          # Insert breaks and labels for the x axis.
                          # These are recalculated with every new selection.
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
                          
                          # Write the title for the x axis.
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
             
             # Set the graph margins.
             margin = list(l = 140, r = 10, b = 70, t = 90),
             
             # Set the font sizes.
             font = list(size = 13),
             
             
             # Add a legend so that the user knows which colour...
             # corresponds to which sex.
             # Make the legend background and legend border white.              
             showlegend = TRUE, 
             legend = list(x = 1, 
                           y = 1, 
                           bgcolor = 'rgba(255, 255, 255, 0)', 
                           bordercolor = 'rgba(255, 255, 255, 0)')) %>%
      
      # Remove any buttons we don't need from the modebar.
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                           # 'zoomIn2d', 'zoomOut2d', 
                                           # 'autoScale2d', 
                                           'toggleSpikelines', 
                                           'hoverCompareCartesian', 
                                           'hoverClosestCartesian'), 
             displaylogo = F, editable = F)
    
  }
  
})

# This reactive() creates the subset that will be used to populate the table...
# appearing under the pyramid.
table_age_sex <- reactive({
  age_sex %>% 
    filter(dataset %in% input$age_sex_dataset
           & geography1 %in% input$age_sex_location_type
           & geography2 %in% input$age_sex_location 
           & year %in% input$age_sex_financial_year 
           & measure %in% input$age_sex_measure_type) %>% 
    
    # Create a new version of the numeric variable, where all...
    # the numbers become positive numbers.
    # To achieve the "pyramid" look, the original .csv file had to be...
    # created in such a way that males were given negative values and...
    # females were given positive values.
    # With abs(), we ensure that both males and females display as positive...
    # values on the table.
    mutate(numbers_v2 = abs(value)) %>%
    
    # Select the columns you need for the table, and filter out NAs.
    select(dataset, year, geography1, geography2, ageband, sex_char, 
           numbers_v2) %>%
    filter(complete.cases(.)) 
})

# Create the table that will appear under the pyramid.
# Provide clearer column names.
# The name of the last column depends on the user's input in the...
# filter SELECT MEASURE.
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

# Create a download button that allows users to download the table...
# in .csv format. 
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