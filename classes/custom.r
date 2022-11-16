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
  
  selectInput(inputId = "custom_var1", label = "Select a Variable:", choices = stnVars, multiple = T, selected = stnVars[1])
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
  selectInput(inputId = "custom_var_subset", label = "Variable Subset:", choices = stnSubset)
})

output$custom_date_min <- renderUI({
  dateInput(inputId = "custom_date_min", 
            label = "Select Min Date:",
            value = Sys.Date()-365,
            min = '1900-01-01',
            max = Sys.Date())
  
})

output$custom_date_max <- renderUI({
  dateInput(inputId = "custom_date_max", 
            label = "Select Max Date:",
            min = '1900-01-01',
            max = Sys.Date()
            )
  
})

# pull data from mysql db based on user station and year input
custom_data_query <- reactive({
  
  req(available_timeseries())
  req(input$custom_var1)
  req(input$custom_var_subset)
  req(input$custom_date_min)
  
  avail_ts <- available_timeseries()
  
  # what does the user want?
  ts_ids <- avail_ts[avail_ts$stationparameter_name %in% input$custom_var1 & avail_ts$ts_name %in% input$custom_var_subset, ]$ts_id
  ts_names <- avail_ts[avail_ts$stationparameter_name %in% input$custom_var1 & avail_ts$ts_name %in% input$custom_var_subset, ]$stationparameter_name
  
  params <- list(
    ts_ids,
    ts_names
  )
  
  mlti_df <- purrr::pmap_dfr(
    params, 
    ~get_wiski_value_long(..1, 
                          input$custom_site, ..2, 
                          paste(input$custom_date_min), 
                          paste(input$custom_date_max), 
                          'GMT-6'))

  return(mlti_df)
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
output$plot_multi <- renderPlotly({
  
  req(input$custom_site)
  req(input$custom_var1)
  req(input$custom_var_subset)
  req(finalData())
  
  df <- finalData() 
  
  
  p <- ggplot(data = df, aes(x = time, y = value)) +
    geom_line() +
    facet_grid(rows = vars(ts_name), scales = "free_y") +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
  plotly::ggplotly(p)
})


output$plot_multi_ui <- renderUI({
  plotlyOutput('plot_multi', height = length(input$custom_var1)*input$fig_height)
})

updateSelectizeInput(session, 'custom_site', choices = stations$station_name, selected = cur_stn, server = T)

# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$custom_site, "_wiskir_download.csv", sep = "")
  },
  content = function(file) {
    write.csv(finalData(), file, row.names = FALSE)
  }
)


# output$plot1 <- renderPlotly({
#   req(input$custom_site)
#   req(input$custom_var1)
#   req(input$custom_var_subset)
#   req(finalData())
#   
#   df <- finalData() 
#   
#   
#   
#     plot_ly(df) %>%
#       add_lines(x = ~time,
#                 y = ~(df)[[2]],
#                 name = input$custom_var1,
#                 line = list(color = lineGraphColour$colOne, width = 1),
#                 #hovertemplate = paste('%{x}<br>%{yaxis.title.text}: %{y:.2f}<extra></extra>')
#                 hoverinfo = 'text',
#                 text = ~paste(time, '</br></br><b>',input$custom_var1,': ', round(get(names(df)[2]), 2))
#       ) %>%
#       layout(
#         xaxis = c(generalAxLayout, list(title = "")),
#         yaxis = c(generalAxLayout, list(title=paste0("<b>",input$custom_var1,"</b>"),titlefont = list(color = lineGraphColour$colOne))),
#         margin = marg,
#         showlegend = F,
#         plot_bgcolor = "#f5f5f5",
#         paper_bgcolor = "#f5f5f5"
#       )
#   
# })
