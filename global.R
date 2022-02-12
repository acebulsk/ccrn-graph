library(dplyr)
library(DBI)
library(plotly)
library(lubridate)
library(leaflet)
library(tsibble)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(WISKIr)


Sys.setenv(TZ = 'UTC')

stations <- readRDS('data/ccrn_sites.rds')

# set initial staiton
cur_stn = "Powerline"


##### graph presets ####
lineGraphColour <- list(
  colOne = "rgb(0,119,187)",
  colTwo = "rgb(204,51,17)"
)

# plotly layout lists
generalAxLayout <- list(
  zeroline = FALSE,
  showline = TRUE
)

# and set standard margins for plotly
marg <- list(b = 0, r = 50, l = 50, t = 10)

# Set Vars for Date range
wk_min_dt <- paste0(now()-604800)

#### functions #####

# set year of posix datetime using lubridate for if else statement
setYr <- function(DATE_TIME, YEAR) {
  year(DATE_TIME) <- YEAR

  DATE_TIME
}

# calculate water year by date
wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

# cleans snow depth data
cleanSnowData <- function(data, spike_th, roc_hi_th, roc_low_th) {
  data <- as_tsibble(data, index = DateTime) %>%
    fill_gaps() %>%
    mutate(
      Snow_Depth =
        case_when(
          (Snow_Depth - lag(Snow_Depth)) < -spike_th & (lead(Snow_Depth) - lag(Snow_Depth)) < -spike_th & (lead(Snow_Depth, n = 2) - Snow_Depth) > spike_th ~ lag(Snow_Depth), #  low spike
          (Snow_Depth - lag(Snow_Depth)) > spike_th & (lead(Snow_Depth) - Snow_Depth) < -spike_th ~ lag(Snow_Depth), #  hi spike
          (Snow_Depth - lag(Snow_Depth)) < -spike_th & (lead(Snow_Depth) - Snow_Depth) > spike_th ~ lag(Snow_Depth), #  low spike
          TRUE ~ Snow_Depth # else set to raw
        )
    )

  # while the dataset does not have a rate of change higher than the threshold
  while(TRUE %in% (data$Snow_Depth - lag(data$Snow_Depth) > roc_hi_th)){
    data <- data %>%
      mutate(
        Snow_Depth =
          case_when(

            (Snow_Depth - lag(Snow_Depth)) > roc_hi_th ~ lag(Snow_Depth), # rate of change
            (Snow_Depth - lead(Snow_Depth)) > roc_low_th ~ lead(Snow_Depth), # rate of change
            TRUE ~ Snow_Depth # else set to raw
          )
      )
  }
  return(data)
}


# creates a plotly wind rose using data from viu cleaned df tables time is number of hours
plotWindRose <- function(data, hrs = 168, plotTitle = "", dirres = 15){

      if(all(data$Wind_Speed == 0) | length(data$Wind_Speed) < (hrs / 2)){ # account for stations that dont have ws sensors and if there have been gaps for over half the data period
        plot <- plotly_empty(type = "barpolar") %>%
          layout(
            margin = list(t = 60, l = 20, r = 20),
            annotations = list(text = plotTitle, xanchor = "centre",
                               yref="paper",y=1,yshift=50,showarrow=FALSE,
                               font=list(size=18,color='rgb(0,0,0)')),
            xaxis = list(
              title = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
            ),
            legend = list(orientation = "h"),
            polar = list(
              radialaxis = list(
                nticks = 7,
                angle = 45,
                tickangle = 45,
                ticksuffix = " %"
              ),
              angularaxis = list(
                tickmode = "array",
                tickvals = c(0 , 45, 90, 135, 180, 225, 270, 315),
                ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
                direction = "clockwise"
              )
            )
          )
      } else {
      dirres <- dirres
      dir.breaks <- c(-dirres/2,
                      seq(dirres/2, 360-dirres/2, by = dirres),
                      360+dirres/2)
      dir.labels <- c(seq(0, 360, by = dirres))

      startTime <- (Sys.time() - hours(8))-hours(hrs)

      wind <- data %>%
        filter(DateTime >= startTime) %>%
        select(DateTime, Wind_Speed, Wind_Dir) %>%
        mutate(ws_bin = cut(Wind_Speed, breaks=c(-Inf, 5, 10, 20, 30, 40, 50, Inf), labels = c("< 5 km/h", "5-10 km/h", "10-20 km/h", "20-30 hm/h", "30-40 km/h", "40-50 km/h",">50 km/h"))) %>%
        mutate(wd_bin = cut(Wind_Dir, breaks = dir.breaks, labels = dir.labels)) %>%
        group_by(wd_bin, ws_bin) %>%
        summarise(Freq=(n()/hrs)*100) %>%
        arrange(ws_bin)

      plot <- plot_ly(wind, type = "barpolar", hovertemplate = paste('Freq (%): %{r:.2f}<br>Dir: %{theta}\u00b0;'), colors = c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027")) %>%
        add_trace(r = ~Freq, theta = ~wd_bin, color = ~ws_bin) %>%
      layout(
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "#f5f5f5",
        autosize = TRUE,
        margin = list(l = 10, r = 10),
        annotations = list(text = plotTitle, xanchor = "centre",
                           yref="paper",y=1,yshift=50,showarrow=FALSE,
                           font=list(size=18,color='rgb(0,0,0)')),
        xaxis = list(
          title = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        ),
        legend = list(orientation = "h"),
        polar = list(
          radialaxis = list(
            nticks = 7,
            angle = 45,
            tickangle = 45,
            ticksuffix = " %"
          ),
          angularaxis = list(
            tickmode = "array",
            tickvals = c(0 , 45, 90, 135, 180, 225, 270, 315),
            ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
            direction = "clockwise"
          )
        )
      )
      }
  plot
}

#### dashboard theme use https://nik01010.shinyapps.io/dashboardThemeDesigner/ to create ####
customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#2D2D2D"
  ,primaryFontColor = "#0F0F0F"
  ,infoFontColor = "#0F0F0F"
  ,successFontColor = "#0F0F0F"
  ,warningFontColor = "#0F0F0F"
  ,dangerFontColor = "#0F0F0F"
  ,bodyBackColor = "#FFFFFF"

  ### header
  ,logoBackColor = "#F8F8F8"

  ,headerButtonBackColor = "#F8F8F8"
  ,headerButtonIconColor = "#808080"
  ,headerButtonBackColorHover = "#E7E7E7"
  ,headerButtonIconColorHover = "#9E9E9E"

  ,headerBackColor = "#F8F8F8"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"

  ### sidebar
  ,sidebarBackColor = "#7C7F80"
  ,sidebarPadding = "0"

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"

  ,sidebarUserTextColor = "#737373"

  ,sidebarSearchBackColor = "#F0F0F0"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"

  ,sidebarTabTextColor = "#E6E6E6"
  ,sidebarTabTextSize = "14"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"

  ,sidebarTabBackColorSelected = "#E6E6E6"
  ,sidebarTabTextColorSelected = "#000000"
  ,sidebarTabRadiusSelected = "0px"

  ,sidebarTabBackColorHover = "#F5F5F5"
  ,sidebarTabTextColorHover = "#000000"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"

  ### boxes
  ,boxBackColor = "#FFFFFF"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#E1E1E1"
  ,boxPrimaryColor = "#5F9BD5"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#ED7D31"
  ,boxDangerColor = "#E84C22"

  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#F8F8F8"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "5"

  ### inputs
  ,buttonBackColor = "#D7D7D7"
  ,buttonTextColor = "#2D2D2D"
  ,buttonBorderColor = "#969696"
  ,buttonBorderRadius = "5"

  ,buttonBackColorHover = "#BEBEBE"
  ,buttonTextColorHover = "#000000"
  ,buttonBorderColorHover = "#969696"

  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#767676"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#F5F5F5"
  ,textboxBorderColorSelect = "#6C6C6C"

  ### tables
  ,tableBackColor = "#FFFFFF"
  ,tableBorderColor = "#CCC6C6"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)


