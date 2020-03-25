# Load libraries ---------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(tigris)
library(RColorBrewer)

# setwd('/home/jungho/deep_learning/covid19/')

# Load data polygons -----------------------------------------------

cty <- counties(cb = TRUE, resolution = "20m")

url <- 'https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv'
cv_us <- read_csv(url(url))

today <- as.Date(Sys.Date())
string_today <- paste(str_sub(today,6,7), str_sub(today,9,10), year(today), sep = '-')

url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/', string_today, '.csv')
cv_us_today <- tryCatch(read_csv(url(url)), warn = F,
                        error = function(e) NULL)

cv_us <- cv_us %>%
  mutate(countyFIPS = str_pad(countyFIPS,5,'left', '0')) %>%
  distinct(countyFIPS, .keep_all = T) %>% # Distinct county codes
  mutate(countyFIPS = ifelse(countyFIPS == '46031', '45031', countyFIPS)) # Darlington county FIPS code fixed

if(!is.null(cv_us_today)){
  cv_us_today <- cv_us_today %>%
    rename(countyFIPS = FIPS,
           today = Confirmed,
           last_update = Last_Update) %>%
    select(countyFIPS, today, last_update) %>%
    distinct(countyFIPS, .keep_all = T)
}
  
## combine historical and today

if(!is.null(cv_us_today)){
  cv_us_today <- cv_us_today %>%
  left_join(cv_us, by = 'countyFIPS')
} else{
  cv_us_today <- cv_us
}
  
cty_data <- cty@data
cty_data <- cty_data %>%
  mutate(countyFIPS = paste0(STATEFP, COUNTYFP)) %>%
  left_join(cv_us_today, by = c('countyFIPS' = 'countyFIPS')) %>%
  mutate_if(is.numeric, function(x) replace_na(x, 0)) # Replace all NA to zero

cty_data <- cty_data %>%
  select(1:10,12:ncol(cty_data),11)

col_index <- grep('^[0-9]|^today',names(cty_data))

cty_data_gather <- cty_data %>%
  tidyr::gather(date,value,col_index) 

cty_data_gather <- cty_data_gather %>%
  mutate(date = ifelse(date == 'today', paste(month(today), day(today), year(today), sep = '/'), date))

cty_data_gather <- cty_data_gather %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'))

cty@data <- cty_data

# Palette for color coding
bins <- c(-Inf, 0, 10, 100, 500, Inf) # create color bins
pal <- colorBin("BuGn", domain = cty_data[, ncol(cty_data)-1], bins = bins)

# State name append

state_name <- cty_data_gather %>%
  distinct(STATEFP, State) %>%
  filter(!is.na(State))

cty@data <- cty@data %>%
  left_join(state_name, by = 'STATEFP') %>%
  rename(State = State.y) %>%
  select(-State.x)

cty_data_gather <- cty_data_gather %>%
  left_join(state_name, by = 'STATEFP') %>%
  rename(State = State.y) %>%
  select(-State.x)

# Load worldwide data ------------------------------------------------

url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv'
covid <- read_csv(url(url))

covid_by_ctry <- covid %>%
  group_by(`Country/Region`) %>%
  summarise_if(is.numeric, sum, na.rm = T) %>%
  rename(country = `Country/Region`)

covid_by_ctry <- covid_by_ctry[covid_by_ctry[, ncol(covid_by_ctry)] > 200,]

covid_tidy <- covid_by_ctry %>%
  tidyr::gather(date,value,4:ncol(covid_by_ctry))

# Data frame for COVID19 days since 100

covid_tidy_days_since_100 <- covid_tidy %>%
  filter(value > 100) %>%
  group_by(country) %>%
  mutate(days = 1) %>%
  mutate(days_since_100 = cumsum(days))

# Data frame for COVID19 today

cv_today <- covid_tidy %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  filter(date == max(date)) %>%
  arrange(-value)

# annotation_df <- covid_tidy_days_since_100 %>%
#   group_by(country) %>%
#   filter(days_since_100 == max(days_since_100))
# 
# covid_tidy_days_since_100 %>%
#   group_by(country) %>%
#   plot_ly(x = ~days_since_100) %>%
#   add_lines(y = ~log(value), color = ~factor(country)) %>%
#   layout(xaxis = x, yaxis = y) %>%
#   add_annotations(x = annotation_df$days_since_100,
#                   y = log(annotation_df$value),
#                   color = ~factor(annotation_df$country),
#                   text = annotation_df$country,
#                   showarrow = F,
#                   ax = 30,
#                   font = list(family = 'sans serif',
#                               size = 14))
  

# Country Case plot function -----------------------------------------

country_cases_plot = function(cv_cases, type = 'cumulative') {
  ## Font 
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Date",
    titlefont = f
  )
  y <- list(
    title = "Cumulative Infected Cases",
    titlefont = f
  )
  
  ## Plot
  if(type == 'cumulative'){
    plot = cv_cases %>%
      mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
      group_by(country) %>%
      plot_ly(x = ~date) %>%
      add_lines(y = ~value, color = ~factor(country)) %>%
      layout(xaxis = x, yaxis = y)
  } 
  
  if(type == 'new'){
    plot = cv_cases %>%
      mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
      group_by(country) %>%
      arrange(date) %>%
      mutate(new_cases = value - lag(value, default = first(value))) %>%
      group_by(country) %>%
      plot_ly(x = ~date) %>%
      add_lines(y = ~new_cases, color = ~factor(country)) %>%
      layout(xaxis = x, yaxis = y)
    
  }
  
  if(type == 'log_cumulative'){
    plot = cv_cases %>%
      mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(log_cum = ifelse(value == 0, 0, log(value))) %>%
      group_by(country) %>%
      plot_ly(x = ~date) %>%
      add_lines(y = ~log_cum, color = ~factor(country)) %>%
      layout(xaxis = x, yaxis = y)
  }
  
  if(type == 'days_since_100'){
    annotation_df <- cv_cases %>%
      group_by(country) %>%
      filter(days_since_100 == max(days_since_100))
    
    plot = cv_cases %>%
      group_by(country) %>%
      plot_ly(x = ~days_since_100) %>%
      add_lines(y = ~log(value), color = ~factor(country)) %>%
      layout(xaxis = x, yaxis = y) 
  }
  plot
}

# Miscellaneous variable definitions ------------------------------------------------

min_date = min(as.Date(cty_data_gather$date,"%m/%d/%y"))
max_date = max(as.Date(cty_data_gather$date,"%m/%d/%y"))
current_date = max_date

# Create covid base map -------------------------------------------------------------

basemap = leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(-98,40, zoom = 4) %>%  # Set View to US 
  addLegend(colors = brewer.pal(5, 'BuGn'), opacity = 0.7, 
            position = 'bottomright',
            labels = c('0', '1-10', '11-100', '100-499', '500+'),
            title = '<small>Number of Confirmed Cases</small>')

# Shiny UI --------------------------------------------------------------------------

ui <- navbarPage(theme = shinytheme("darkly"), collapsible = TRUE,
                 "COVID-19 NYL", id="nav",
                 
                 tabPanel("COVID-19 US Map by County",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 20, width = 350, fixed=TRUE,
                                            draggable = TRUE, height = 'auto',
                                            h3(textOutput("reactive_case_count"), align = "center", style="color:#006d2c"),
                                            h5(textOutput("reactive_last_update"), align = "center", style="color:#006d2c"),
                                            h5('Select date for mapping', style='color:#000000'),
                                            sliderInput("plot_date",
                                                        label = 'Select date for mapping',
                                                        min = min_date,
                                                        max = max_date,
                                                        value = current_date,
                                                        timeFormat = "%d %b", 
                                                        animate=animationOptions(interval = 2000, loop = FALSE)),
                                            h5('Select at least one state', style='color:#000000'),
                                            pickerInput("state_select", "State:",
                                                        choices = sort(unique(cty_data_gather$State)),
                                                        options = list(`actions-box` = TRUE),
                                                        selected = 'NY',
                                                        multiple = TRUE),
                              ),
                          )
                 ),
                 
                 tabPanel("Country plots",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              pickerInput("country_select", "Country:",   
                                          choices = cv_today$country, 
                                          options = list(`actions-box` = TRUE),
                                          selected = cv_today$country,
                                          multiple = TRUE), 
                              "Select outcome and countries from drop-down menues to update plots."
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("New", plotlyOutput("country_plot", height = '800px')),
                                tabPanel("Cumulative", plotlyOutput("country_plot_cumulative", height = '800px')),
                                tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log", height = '800px')),
                                tabPanel("Days sine 100", plotlyOutput("country_plot_days_since_100", height = '800px'))
                              )
                            )
                          )
                 ),
                 
                 
                 tabPanel("About this site",
                          tags$div(
                            tags$h4("Last update"),
                            h6(paste0('2020-03-22')),
                            "This site is updated once daily. At this time of rapid escalation of the COVID-19 pandemic, the following resources offer the latest numbers of known cases:",tags$br(),
                          )
                 )
)

# Shiny Server ----------------------------------------------------------------------

server = function(input, output) {
  
  
  # covid US country map
  # reactive by date 
  reactive_covid = reactive({
    cty_data_gather = cty_data_gather %>% filter(date == input$plot_date)
    cty@data <- cty_data_gather
    cty@data$INFECTED <- cty@data$value
    
    if(is.null(input$state_select)){
      cty@data$INFECTED = 0
      return(cty)
    } else{
      cty = subset(cty, State %in% input$state_select)
      return(cty)
    }
  })
  
  # Count in Selected State
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_covid()@data$INFECTED), big.mark=","), " cases")
  })
  
  # Last Update
  output$reactive_last_update <- renderText({
    paste0("Last Updated: ", unique(reactive_covid()@data$last_update)[1])
  })
  
  # Base leaflet map
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  # Reactive Label
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%g Confirmed Cases",
      reactive_covid()@data$NAME, reactive_covid()@data$INFECTED
    ) %>% lapply(htmltools::HTML)
  })
  
  # Observe Event
  observeEvent(reactive_covid(), {
    leafletProxy("mymap") %>% 
      clearShapes() %>%
      addPolygons(data = reactive_covid(),
                  color = '#444444', weight = 1, smoothFactor = 0.5,
                  opacity = 0.5, fillOpacity = 0.9,
                  fillColor = ~pal(INFECTED),
                  highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = T),
                  label = labels(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal"),
                    textsize = "15px",
                    direction = "auto"))
    
  })
  
  # covid by country
  
  covid_by_ctry = reactive({
    covid_tidy %>% filter(country %in% input$country_select)
  })
  
  # covid US days since 100
  # reactive 
  reactive_annot = reactive({
    covid_tidy_days_since_100 %>%
      filter(country %in% input$country_select) %>%
      group_by(country) %>%
      filter(days_since_100 == max(days_since_100))  
  })
  
  # reactive 
  reactive_days_100 = reactive({
    covid_tidy_days_since_100 %>%
      filter(country %in% input$country_select)
  })
  
  
  # Plotly New/Cumlative/Cumulative Log Cases by Country
  
  output$country_plot_cumulative_log <- renderPlotly({
    country_cases_plot(covid_by_ctry(), 'log_cumulative')
  })
  output$country_plot <- renderPlotly({
    country_cases_plot(covid_by_ctry(), 'new')
  })
  output$country_plot_cumulative <- renderPlotly({
    country_cases_plot(covid_by_ctry(), 'cumulative')
  })
  output$country_plot_days_since_100 <- renderPlotly({
    country_cases_plot(reactive_days_100(), 'days_since_100') %>%
      add_annotations(x = reactive_annot()$days_since_100,
                      y = log(reactive_annot()$value),
                      color = ~factor(reactive_annot()$country),
                      text = reactive_annot()$country,
                      showarrow = F,
                      ax = 30,
                      font = list(family = 'sans serif',
                                  size = 14))
  })
  
}

shinyApp(ui, server)

library(rsconnect)



deployApp('/Users/junghoyeom/Desktop/projects/covid19/')



