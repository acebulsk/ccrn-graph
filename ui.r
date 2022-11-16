
#### user interface ####
ui <- function(request) {
  dashboardPage(title = "CCRN Hydromet Stations",
    dashboardHeader(title = 'CCRN Hydromet Stations'),
    dashboardSidebar(
      collapsed = T,
      uiOutput("sidebarControls"),
      sidebarMenu(id = "smenu",
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

        tabItem("cstm_graph",
                fluidRow(
                  column(12,
                         h1("Custom Graphs", align = "center")
                  )
                ),
                fluidRow(
                  column(2,
                         selectizeInput("custom_site",
                                        label = "Choose a Weather Station:",
                                     choices = NULL

                         ),
                         # selectInput("custom_year", "Select Water Year", "",  selectize = F),
                         uiOutput("varSelection1"),
                         uiOutput("varSelectionSubset"),
                         uiOutput("custom_date_min"),
                         uiOutput("custom_date_max"),
                         uiOutput("cleanSnowButton"),
                         numericInput(
                           inputId = 'fig_height',
                           label = 'Figure Height in Pixels:',
                           value = 300
                         ),
                         downloadButton("downloadData", "Download")
                  ),
                  column(10,
                         htmlOutput('header2'),
                         wellPanel(
                           uiOutput('plot_multi_ui'),
                           chooseSliderSkin('Flat',color = "#99ccff"),
                           div(style = "margin-top:-3.5em; margin-bottom: -2em",
                               fluidRow(uiOutput("slider"), align = 'center'))
                         )
                  )

                )
        )
        )

      )


}


