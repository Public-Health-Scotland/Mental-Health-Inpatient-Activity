##* Table server ----    

# On to the final tab, which is the Table tab.

# The following piece of syntax tells R to switch between files...
# based on the user's input in the filter SELECT DATA FILE.
# The files below are the ones we read into R in the data manipulation section.
# However, they require a few transformations before they can be displayed...
# as a table.
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

# Create the actual table for the Table tab.
output$table_tab <- renderDataTable({
  datatable(data_table(), 
            style = 'bootstrap', 
            class = 'table-bordered table-condensed',
            rownames = FALSE, 
            options = list(
              pageLength = 20, 
              autoWidth = TRUE, 
              dom = 'tip'),
            # Insert filters at the top of each column.
            filter = list(position = 'top'))
})

# We also create a download data button.
output$download_table <- downloadHandler(
  filename = 'table_data.csv', 
  content = function(file) { 
    write.csv(
      # The command "input[["table_tab_rows_all"]]" tells R to create a .csv...
      # file that takes into account the user's input in the filters below...
      # the column names.
      data_table()[input[["table_tab_rows_all"]], ], 
      file, 
      row.names = FALSE
    )
  } 
)