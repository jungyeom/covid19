# Load libraries ---------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(tigris)
library(RColorBrewer)
library(DT)

# Load data polygons -----------------------------------------------

cty <- counties(cb = TRUE, resolution = "20m")

url_confirm <- 'https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv'
cv_us_confirm <- read_csv(url(url_confirm))
url_deaths <- 'https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv'
cv_us_deaths <- read_csv(url(url_deaths))

names(cv_us_confirm)[5:ncol(cv_us_confirm)] <- paste0(names(cv_us_confirm)[5:ncol(cv_us_confirm)], '20')
names(cv_us_deaths)[5:ncol(cv_us_deaths)] <- paste0(names(cv_us_deaths)[5:ncol(cv_us_deaths)], '20')

cv_us_confirm$type <- "CONFIRM"
cv_us_deaths$type <- "DEATH"

# cv_us_confirm <- cv_us_confirm %>% select(-(ncol(cv_us_confirm)-1))
names(cv_us_confirm) <- names(cv_us_deaths)

cv_us <- bind_rows(cv_us_confirm, cv_us_deaths)
# cv_us <- cv_us %>% select(-(ncol(cv_us)-1))

today <- as.Date(Sys.Date())
string_today <- paste(str_sub(today,6,7), str_sub(today,9,10), year(today), sep = '-')

url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/', string_today, '.csv')
cv_us_today <- NULL
tryCatch(cv_us_today <- read_csv(url(url)), 
         error = function(e) "error")

cv_us <- cv_us %>%
  mutate(countyFIPS = str_pad(countyFIPS,5,'left', '0')) %>%
  distinct(countyFIPS, type, .keep_all = T) %>% # Distinct county codes
  mutate(countyFIPS = ifelse(countyFIPS == '46031', '45031', countyFIPS)) # Darlington county FIPS code fixed

if(!is.null(cv_us_today)){
  cv_us_today <- cv_us_today %>%
    rename(countyFIPS = FIPS,
           CONFIRM = Confirmed,
           DEATH = Deaths,
           last_update = Last_Update) %>%
    select(countyFIPS, last_update, CONFIRM, DEATH) %>%
    distinct(countyFIPS, .keep_all = T) %>%
    gather(type, value, 3:4)
}

## combine historical and today

if(!is.null(cv_us_today)){
  cv_us_today <- cv_us_today %>%
    left_join(cv_us, by = c('countyFIPS', 'type'))
} else{
  cv_us_today <- cv_us
}

cty_data <- cty@data
cty_data <- cty_data %>%
  mutate(countyFIPS = paste0(STATEFP, COUNTYFP)) %>%
  left_join(filter(cv_us_today, type == 'CONFIRM'), by = c('countyFIPS' = 'countyFIPS')) %>%
  mutate_if(is.numeric, function(x) replace_na(x, 0)) # Replace all NA to zero

cty_data <- cty_data %>%
  select(1:12,14:ncol(cty_data),13)

col_index <- grep('^[0-9]|^value',names(cty_data))

cty_data_gather <- cty_data %>%
  tidyr::gather(date,value,all_of(col_index))

cty_data_gather <- cty_data_gather %>%
  mutate(date = ifelse(date == 'value', paste(month(today), day(today), year(today), sep = '/'), date))

cty_data_gather <- cty_data_gather %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'))

cty@data <- cty_data

# Death 

cv_death_gather <- cv_us_today %>%
  select(1:3,5:ncol(cv_us_today),4)

col_index <- grep('^[0-9]|^value',names(cv_death_gather))

cv_death_gather <- cv_death_gather %>%
  filter(type == 'DEATH') %>%
  tidyr::gather(date,value,col_index) 

cv_death_gather <- cv_death_gather %>%
  mutate(date = ifelse(date == 'value', paste(month(today), day(today), year(today), sep = '/'), date))

cv_death_gather <- cv_death_gather %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'))

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

# Collect state level data

state_coord <- read_csv('state_coord.csv')

state_data_gather <- cty_data_gather %>%
  group_by(State, stateFIPS, date, type) %>%
  summarise(value = sum(value))
  
state_data_gather <- state_data_gather %>%
  left_join(select(state_coord,1:3), by = c('State' = 'state'))


# Load worldwide data ------------------------------------------------

url_confirm <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
url_death <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
covid_confirm <- read_csv(url(url_confirm))
covid_death <- read_csv(url(url_death))

covid_confirm$type = 'CONFIRM'
covid_death$type = 'DEATH'

covid <- bind_rows(covid_confirm, covid_death)

col_index <- grep('^[0-9]', names(covid))
names(covid)[col_index] <- paste0(names(covid)[col_index], '20')

covid_by_ctry <- covid %>%
  group_by(`Country/Region`, type) %>%
  summarise_if(is.numeric, sum, na.rm = T) %>%
  rename(country = `Country/Region`)

# identify countries with more than 200 confirmed cases
ctry_with_200plus <- covid_by_ctry %>% dplyr::filter(type == 'CONFIRM')
ctry_with_200plus <- ctry_with_200plus[ctry_with_200plus[, ncol(ctry_with_200plus)] > 200, ]
ctry_with_200plus <- ctry_with_200plus$country

covid_tidy <- covid_by_ctry %>%
  filter(country %in% ctry_with_200plus) %>%
  tidyr::gather(date,value,5:ncol(covid_by_ctry))

# Data frame for COVID19 today

cv_today <- covid_tidy %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  filter(date == max(date)) %>%
  arrange(-value)


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
    title = "Cases",
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
                                            h3(textOutput("reactive_death_count"), align = "center", style="color:#FF0000"),
                                            h5(tags$i(textOutput("reactive_last_update")), align = "center", style="color:#000000")
                              ),
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 480, left = 20, width = 350, fixed=TRUE,
                                            draggable = TRUE, height = 'auto',
                                            h4('List of top counties', align = 'left', style="color:#000000"),
                                            dataTableOutput("reactive_top10_county")

                              ),
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, right = 20, width = 350, fixed=TRUE,
                                            draggable = TRUE, height = 'auto',
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
                                                        multiple = TRUE)
                                            
                              )
                              
                          )
                 ),
                 
                 tabPanel("Country plots",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              pickerInput("country_select", "Country:",   
                                          choices = cv_today$country[cv_today$type == 'CONFIRM'], 
                                          options = list(`actions-box` = TRUE),
                                          selected = cv_today$country,
                                          multiple = TRUE), 
                              "Select outcome and countries from drop-down menues to update plots.",
                              radioButtons("type2", h3("Confirmed or Death"),
                                           choices = list("Confirmed" = 'CONFIRM', "Deaths" = 'DEATH'), selected = 'CONFIRM')
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("New", plotlyOutput("country_plot", height = '800px')),
                                tabPanel("Cumulative", plotlyOutput("country_plot_cumulative", height = '800px')),
                                tabPanel("Cumulative (log(e))", plotlyOutput("country_plot_cumulative_log", height = '800px')),
                                tabPanel("Days since 100 (log(e))", plotlyOutput("country_plot_days_since_100", height = '800px'))
                                # tabPanel("table", tableOutput("reactive_top10_county"))
                              )
                            )
                          )
                 ),
                 
                 
                 tabPanel("About this site",
                          tags$div(
                            tags$h4("Last update"),
                            h6(paste0(today)),
                            "This site",tags$br(),
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
  
  # Death in Selected State
  output$reactive_death_count <- renderText({
    df <- cv_death_gather %>%
      filter(State %in% input$state_select) %>%
      filter(date == input$plot_date) %>%
      summarise(death = sum(value, na.rm = T))
    
    paste0(prettyNum(df$death, big.mark=","), " deaths")
  })
  
  # Top 10 counties 
  output$reactive_top10_county <- renderDataTable({
    confirm <- cty_data_gather %>% filter(date == input$plot_date) %>% filter(State %in% input$state_select)
    death <- cv_death_gather %>% filter(date == input$plot_date) %>% filter(State %in% input$state_select)
    
    confirm <- confirm %>% select(countyFIPS, NAME, value) %>% rename(confirmed = value)
    death <- death %>% select(countyFIPS, value) %>% rename(deaths = value)
    
    df <- confirm %>% left_join(death, by = 'countyFIPS') %>% arrange(-confirmed) %>% select(-countyFIPS) %>% 
      rename(County = NAME, Confirmed = confirmed, Deaths = deaths) %>%
      mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
      mutate(Confirmed = as.integer(Confirmed), Deaths = as.integer(Deaths))
    df
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '100px', targets = "_all")),
    lengthChange = FALSE
  ), rownames = F)
  
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
    covid_tidy %>% 
      filter(type == input$type2) %>%
      filter(country %in% input$country_select)
  })
  
  # covid US days since 100
  # reactive 
  reactive_annot = reactive({
    
    # Data frame for COVID19 days since 100
    
    covid_tidy_days_since_100 <- covid_tidy %>%
      filter(type == input$type2) %>%
      filter(value > 100) %>%
      group_by(country) %>%
      mutate(days = 1) %>%
      mutate(days_since_100 = cumsum(days))
    
    covid_tidy_days_since_100 %>%
      filter(country %in% input$country_select) %>%
      group_by(country) %>%
      filter(days_since_100 == max(days_since_100))  
  })
  
  # reactive 
  reactive_days_100 = reactive({
    covid_tidy_days_since_100 <- covid_tidy %>%
      filter(type == input$type2) %>%
      filter(value > 100) %>%
      group_by(country) %>%
      mutate(days = 1) %>%
      mutate(days_since_100 = cumsum(days)) %>%
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





