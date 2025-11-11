##* Cross-boundary flow server ----   

# There are three filters here: SELECT TREATMENT SPECIALTY, SELECT FINANCIAL...
# YEAR and SELECT HEALTH BOARD OF RESIDENCE.
# The reactive() command below creates a subset of the cross-boundary flow...
# dataset based on the user's selections in these three filters.
# This subset will then "feed" the cross-boundary flow chart.
# Additional transformations:
# Filter out NAs. We have quite a few NAs, as there are certain...
# combinations of values that don't make sense, e.g., State Hospital and...
# non-psychiatric specialties.
# In the measure column, we filter out 'Number of discharges', as we want the...
# cross-boundary flow diagram to show only number of patients.
# Moreover, we are only visualising inter-board flow, so we need to filter...
# out intra-board flow. To do this, we execute 'filter(flow == 0)'.
# Also, prevent R from visualising flows with zero patients.
# Rename the 'value' variable to 'Number of patients'.
# Finally, keep only the columns you need for the chart. This last step is...
# very important, as the Sankey diagram only works with three...
# variables: 1) origin, 2) destination, and 3) a numeric variable that flows...
# from origin to destination.
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


# Calculate the percentages and numbers which will be used in the...
# dynamic sentence that appears above our Sankey diagram. We need to...
# calculate 5 different values (listed below).
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
  
  # We can now build our dynamic sentence.
  # This will be done using "if" statements.
  
  # Statement 1: If there are no patients for a given combination of...
  # selections, the user gets the message "No patients found".
  if(sum(flow_txt$value) == 0) {
    
    paste0("<b>",
           "No patients found.",
           "</b>")
    
  }
  
  # Statement 2: If there are patients, but no intra-board flow, the user...
  # gets a message saying that the percentage of patients from NHS X who...
  # were treated inside their board was 0%. The only boards with 0%...
  # intra-board flow are NHS Orkney and NHS Shetland, in the psychiatric...
  # specialty.
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
  
  # Statement 3:
  # If there are patients, and there is intra-board flow, but the...
  # intra-board flow accounts for 100% of the activity, inform the user that...
  # the percentage of patients from NHS X who were treated inside their...
  # board was 100%.
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
  
  # Statement 4:
  # Finally, if there are patients, and there is intra-board flow, and...
  # the intra-board activity does not account for 100% of the activity,...
  # insert a sentence saying here's how many patients were treated...
  # inside their board, and here's how many were treated outside their board.
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

# Create the Sankey diagram.
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

# # This reactive() creates the subset that will be used to populate the table...
# # appearing under the diagram.
# # Filter out NAs and discharges.
# # Select the columns you need for the table.
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

# Create the table that will appear under the visual.
# Rename each column to something better.
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

# Create a download button to allow users to download the table in .csv format.
output$download_flow <- downloadHandler(
  filename = 'cross_boundary_flow_data.csv',
  content = function(file) {
    #write.table(flow_new(),
    write.table(table_flow(), 
                file, 
                row.names = FALSE, 
                col.names = c("Treatment specialty", 
                              "Financial year", 
                              "Health board of residence", 
                              "Health board of treatment", 
                              "Number of patients"), 
                sep = ",")
  }
)

