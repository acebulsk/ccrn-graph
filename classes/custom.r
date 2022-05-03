#### custom graphs ####

output$header2 <- renderUI({
  req(input$custom_site)
  str1 <- paste0("<h2>", input$custom_site, "</h2>")
  HTML(paste(str1))
})

available_timeseries <- reactive({
  req(input$custom_site)
  data <- WISKIr::findWISKItimeseries(input$custom_site)
})

# get available variables for selected station
output$varSelection1 <- renderUI({
  req(available_timeseries())
  
  df <- available_timeseries()
  
  # get colnames from reactive dataset
  
  stnVars <- unique(df$stationparameter_name)
  
  selectInput(inputId = "custom_var1", label = "Select a Variable:", choices = stnVars, multiple = F, selected = stnVars[1])
  # checkboxGroupInput(inputId = "custom_var_subset", label = "Select Subset:", choices = stnSubset, inline = FALSE)
})

# output$varSelection2 <- renderUI({
#   req(available_timeseries())
#   
#   df <- available_timeseries()
#   
#   # get colnames from reactive dataset
#   
#   stnVars <- unique(df$stationparameter_name)
#   
#   selectInput(inputId = "custom_var2", label = "Select Second Variable:", choices = c("", as.character(stnVars)), multiple = F)
#   # checkboxGroupInput(inputId = "custom_var_subset", label = "Select Subset:", choices = stnSubset, inline = FALSE)
# })

# get available variables for selected station
output$varSelectionSubset <- renderUI({
  req(available_timeseries())
  
  df <- available_timeseries()
  
  # get colnames from reactive dataset
  usrVar <- input$custom_var1
  
  stnSubset <- df[df$stationparameter_name == usrVar, ]$ts_name
  
  selectInput(inputId = "custom_var_subset", label = "Select Subset:", choices = stnSubset)
  
})

output$customYearMin <- renderUI({
  req(input$custom_var1)
  
  numericInput(inputId = "custom_year_min", 
               label = "Select Min Year:", 
               min = 1800, 
               max = year(Sys.Date()), 
               value = year(Sys.Date())-1)
  
})

output$customYearMax <- renderUI({
  req(input$custom_var1)
  
  numericInput(inputId = "custom_year_max", 
               label = "Select Max Year:", 
               min = 1800, 
               max = year(Sys.Date()), 
               value = year(Sys.Date()))
  
})

# pull data from mysql db based on user station and year input
custom_data_query <- reactive({
  req(available_timeseries())
  req(input$custom_var1)
  req(input$custom_var_subset)
  req(input$custom_year_min)
  
  df <- available_timeseries()
  
  # what does the user want?
  
  ts_id1 <- df[df$stationparameter_name == input$custom_var1 & df$ts_name == input$custom_var_subset, ]$ts_id
  
  data1 <- getWISKIvalues(timeSeries = ts_id1, startDate = paste0(input$custom_year_min, '-01-01'), endDate = paste0(paste0(input$custom_year_max, '-12-31')), timezone = 'MST') %>% 
    mutate(time = as.POSIXct(time, tz = "MST"))
  
  # if(nchar(input$custom_var2)>1){
  #   ts_id2 <- df[df$stationparameter_name == input$custom_var2 & df$ts_name == input$custom_var_subset, ]$ts_id
  #   data2 <- getWISKIvalues(timeSeries = ts_id2, startDate = paste0(input$custom_year_min, '-01-01'), endDate = paste0(paste0(input$custom_year_max, '-12-31')), timezone = 'MST') %>% 
  #     mutate(time = as.POSIXct(time, tz = "MST"))
  #   return(left_join(data1, data2, by = 'time'))
  # } else {
  #   return(data1)
  # }


  
})

output$cleanSnowButton <- renderUI({
  req(input$custom_var1)
  if("Snow_Depth" %in% input$custom_var){
    radioButtons("cleanSnowCstm", "Preform automated spike correction on Snow Depth?:", inline = T,
                 c("Yes" = "yes",
                   "No" = "no"),
                 selected = "no"
    )
  }
  
})

output$slider <- renderUI({
  req(custom_data_query())
  
  sliderInput(inputId = "sliderTimeRange", label = "",
              min = min(custom_data_query()$time),
              max = max(custom_data_query()$time),
              value = c(min(custom_data_query()$time),
                        max(custom_data_query()$time)),
              step = 3600,
              width = '85%',
              height )
})

#filter preset data query
customDataFilter <-  reactive({
  req(input$sliderTimeRange)
  df <- custom_data_query()
  df %>%  filter(time >= input$sliderTimeRange[1] & time <= input$sliderTimeRange[2])
})

# final data set

finalData <- reactive({
  req(customDataFilter())
  req(input$custom_var1)
  
  df <- customDataFilter()
  
  if("Snow_Depth" %in% input$custom_var) {
    req(input$cleanSnowCstm)
    if(input$cleanSnowCstm == "yes"){
      flag  <- ("Snow_Depth" %in% input$custom_var)
      clean <- input$cleanSnow
      df <- cleanSnowData(data = df, spike_th = 10, roc_hi_th = 40, roc_low_th = 75)
    }
    else{return(df)}
  }
  return(df)
})


# plot two variables on one chart
output$plot1 <- renderPlotly({
  req(input$custom_site)
  req(input$custom_var1)
  req(input$custom_var_subset)
  req(finalData())
  
  df <- finalData() 
  
  
  
    plot_ly(df) %>%
      add_lines(x = ~time,
                y = ~(df)[[2]],
                name = input$custom_var1,
                line = list(color = lineGraphColour$colOne, width = 1),
                #hovertemplate = paste('%{x}<br>%{yaxis.title.text}: %{y:.2f}<extra></extra>')
                hoverinfo = 'text',
                text = ~paste(time, '</br></br><b>',input$custom_var1,': ', round(get(names(df)[2]), 2))
      ) %>%
      layout(
        xaxis = c(generalAxLayout, list(title = "")),
        yaxis = c(generalAxLayout, list(title=paste0("<b>",input$custom_var1,"</b>"),titlefont = list(color = lineGraphColour$colOne))),
        margin = marg,
        showlegend = F,
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "#f5f5f5"
      )
  
})
