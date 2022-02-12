
#### user interface ####
ui <- function(request) {
  dashboardPage(title = "CCRN Hydromet Stations",
    dashboardHeader(title = 'CCRN Hydromet Stations'),
    dashboardSidebar(
      collapsed = T,
      uiOutput("sidebarControls"),
      sidebarMenu(id = "smenu",
                  menuItem("Station Map", tabName = "map", icon = icon("fas fa-map")),
                  menuItem("Custom Graphs", tabName = "cstm_graph", icon = icon("fas fa-chart-line")),
                  # menuItem("Annual Comparisons", tabName = "ann_compare", icon = icon("fas fa-chart-line")),
                  # menuItem("Station Comparisons", tabName = "stn_compare", icon = icon("fas fa-chart-line")),
                  menuItem("About Us", icon = icon("info"), href = "https://ccrnetwork.ca/index.php")
      )
    ),

    dashboardBody(

      # add styling for graph formating
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "viu.css")
      ),
      tabItems(
        tabItem("map",
                div(class="outer",
                    tags$head(
                      # Include our custom CSS
                      includeCSS("www/mapstyles.css")
                    ),
                    
                    leafletOutput("map", width="100%", height="100%")
                    
                )
                
        ),

        tabItem("cstm_graph",

                fluidRow(
                  column(2,
                         selectInput("custom_site",
                                        label = "Choose a Weather Station:",
                                     choices = stations$station_name,
                                     selected = cur_stn,
                                        selectize = F

                         ),
                         # selectInput("custom_year", "Select Water Year", "",  selectize = F),
                         uiOutput("varSelection1"),
                         uiOutput("varSelection2"),
                         uiOutput("varSelectionSubset"),
                         uiOutput("customYearMin"),
                         uiOutput("customYearMax"),
                         uiOutput("cleanSnowButton")
                  ),
                  column(10,
                         htmlOutput('header2'),
                         wellPanel(
                           plotlyOutput("plot1", height = "40vh"),
                           chooseSliderSkin('Flat',color = "#99ccff"),
                           div(style = "margin-top:-3.5em; margin-bottom: -2em",
                               fluidRow(uiOutput("slider"), align = 'center'))
                         )
                  )

                )
        )
      )
        )

      )


}


