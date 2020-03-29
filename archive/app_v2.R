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

# Load data ----------------------------------------------------------
load('data/covid_data.RData')


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
  setView(-98,40, zoom = 4) 

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
                                            h2("US COVID-19 Tracker", align = 'center', style='color:#000000'),
                                            h3(textOutput("reactive_case_count"), align = "center", style="color:#006d2c"),
                                            h3(textOutput("reactive_death_count"), align = "center", style="color:#FF0000"),
                                            h5('Select date for mapping', style='color:#000000'),
                                            sliderInput("plot_date",
                                                        label = 'Select date for mapping',
                                                        min = min_date,
                                                        max = max_date,
                                                        value = current_date,
                                                        timeFormat = "%d %b", 
                                                        animate=animationOptions(interval = 2000, loop = FALSE)),
                                            plotlyOutput('us_trend', height = '250px'),
                                            h4('List of States', align = 'left', style="color:#000000"),
                                            dataTableOutput("reactive_top_states")
                              ),
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, right = 20, width = 350, fixed=TRUE,
                                            draggable = TRUE, height = 'auto',
                                            h2('State Specific COVID19', align = 'center', style='color:#000000'),
                                            plotlyOutput('state_trend_confirm', height = '250px'),
                                            plotlyOutput('state_trend_death', height = '250px'),
                                            dataTableOutput('reactive_top10_county')
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
  
  # covid US state
  # reactive by date
  reactive_covid_state = reactive({
    state_df = state_gather %>% filter(date == input$plot_date) %>%
      filter(type == 'CONFIRM')
    state_df
  })
  
  reactive_covid_state2 = reactive({
    state_df = state_gather %>% filter(date == input$plot_date) %>%
      filter(type == 'DEATH')
    state_df
  })
  
  # Confirmed cases in US
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(filter(reactive_covid_state(), type == 'CONFIRM')$value), big.mark=","), " cases")
  })
  
  # Death in US
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(filter(reactive_covid_state2(), type == 'DEATH')$value), big.mark=","), " deaths")
  })
  
  # US trend
  output$us_trend <- renderPlotly({
    us_gather %>%
      filter(type == 'CONFIRM') %>%
      top_n(14) %>%
      plot_ly(x = ~date) %>%
      add_bars(y = ~value, color = I('darkgreen')) %>%
      config(displayModeBar = F) 
  })
  
  # Reactive State data
  reactive_single_state = reactive({
    click <- input$mymap_shape_click
    
    if(is.null(click)){
      state_df = NULL
    } else{
      state_df = state_gather %>%
        filter(State == click$id)
    }
    state_df
  })
  
  # State trend (Confirm and Death)
  output$state_trend_confirm <- renderPlotly({
    
    ax <- list(
      title = ""
    )
    
    if(is.null(reactive_single_state())){
      plot_ly()
    } else{
      as.data.frame(reactive_single_state()) %>%
        filter(type == 'CONFIRM') %>%
        # arrange(~date) %>%
        top_n(14, date) %>%
        plot_ly(x = ~date) %>%
        add_bars(y = ~value, color = I('darkgreen')) %>%
        config(displayModeBar = F) %>%
        layout(
          title = "Confirmed Cases",
          xaxis = ax)
      }
  })
  
  output$state_trend_death <- renderPlotly({
    
    ax <- list(
      title = ""
    )
    
    if(is.null(reactive_single_state())){
      plot_ly()
    } else{
    
      as.data.frame(reactive_single_state()) %>%
        filter(type == 'DEATH') %>%
        # arrange(~date) %>%
        top_n(14, date) %>%
        plot_ly(x = ~date) %>%
        add_bars(y = ~value, color = I('darkred')) %>%
        config(displayModeBar = F) %>%
        layout(
          title = "Death Cases",
          xaxis = ax)
    }
  })
  
  # Top 10 counties 
  output$reactive_top10_county <- renderDataTable({
    click <- input$mymap_shape_click
    confirm <- cty_data_gather %>% filter(date == input$plot_date) %>% filter(State %in% click$id)
    death <- cv_death_gather %>% filter(date == input$plot_date) %>% filter(State %in% click$id)
    
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
  
  # Top states
  output$reactive_top_states <- renderDataTable({
    confirm <- state_gather %>% filter(date == input$plot_date) %>% filter(type == 'CONFIRM')
    death <- state_gather %>% filter(date == input$plot_date) %>% filter(type == 'DEATH')
    
    confirm <- as.data.frame(confirm) %>% select(State, value) %>% rename(confirmed = value)
    death <- as.data.frame(death) %>% select(State, value) %>% rename(deaths = value)
    
    df <- confirm %>% 
      left_join(death, by = 'State') %>% 
      left_join(select(state_coord, state, name), by = c('State' = 'state')) %>%
      select(name, confirmed, deaths) %>%
      rename(State = name, Confirmed = confirmed, Deaths = deaths) %>%
      mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
      mutate(Confirmed = as.integer(Confirmed), Deaths = as.integer(Deaths)) %>%
      arrange(-Confirmed)
    df
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '100px', targets = "_all")),
    lengthChange = FALSE
  ), rownames = F)

  
  
  # Last Update
  # output$reactive_last_update <- renderText({
  #   paste0("Last Updated: ", unique(reactive_covid()@data$last_update)[1])
  # })
  # <font size="3" color="red">This is some text!</font>
  # Reactive Label
  labels_state <- reactive({
    sprintf(
      "<strong><u>%s</u></strong><br/><font color='green'>%s Confirmed Cases </font><br/><font color='red'>%s Deaths</font>",
      reactive_covid_state()$State, prettyNum(reactive_covid_state()$value,big.mark=","), prettyNum(reactive_covid_state2()$value,big.mark=",")
    ) %>% lapply(htmltools::HTML)
  })
  
  # Base leaflet map
  output$mymap <- renderLeaflet({ 
    basemap %>%
      addCircles(data = reactive_covid_state(),
                 lng = ~longitude, lat = ~latitude, weight = 1,
                 radius = ~value^(1/3) * 2500, color = 'red',
                 layerId = ~State,
                 highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = T),
                 label = labels_state(),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal"),
                   textsize = "15px",
                   direction = "auto"))
  })
  
  # Reactive Label
  # labels <- reactive({
  #   sprintf(
  #     "<strong>%s</strong><br/>%g Confirmed Cases",
  #     reactive_covid()@data$NAME, reactive_covid()@data$INFECTED
  #   ) %>% lapply(htmltools::HTML)
  # })
  
  # Observe Event

  observe({

    # covid_state <- state_gather %>% filter(date == plot_date) %>%
    #   filter(type == 'CONFIRM')
    #
    click <- input$mymap_shape_click
    cty_data_gather = cty_data_gather %>% filter(date == input$plot_date)
    cty@data <- cty_data_gather

    # if(is.null(input$state_select)){
    #   cty@data$INFECTED = 0
    #   return(cty)
    # } else{
    #   cty = subset(cty, State %in% input$state_select)
    #   return(cty)
    # }
    
    if(is.null(click)){
      return()}
    else{
      
      new_data_confirm <- reactive_covid_state() %>% filter(!State %in% click$id)
      new_data_deaths  <- reactive_covid_state2() %>% filter(!State %in% click$id)
      
      labels_state <- sprintf(
          "<strong><u>%s</u></strong><br/><font color='green'>%s Confirmed Cases </font><br/><font color='red'>%s Deaths</font>",
          new_data_confirm$State, prettyNum(new_data_confirm$value,big.mark=","), prettyNum(new_data_deaths$value,big.mark=",")
      ) %>% lapply(htmltools::HTML)
      
      cty = subset(cty, State %in% click$id)
      
      labels_cty <- sprintf(
        "<strong><u>%s</u></strong><br/><font color='green'>%s Confirmed Cases",
        cty@data$`County Name`, prettyNum(cty@data$value,big.mark=",")
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy('mymap') %>%
      setView(lng = click$lng, lat = click$lat, zoom = 7) %>%
        clearShapes() %>%
        addCircles(data = reactive_covid_state() %>% filter(!State %in% click$id),
                   lng = ~longitude, lat = ~latitude, weight = 1,
                   radius = ~value^(1/3) * 2500, color = 'red',
                   layerId = ~State,
                   highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = T),
                   label = labels_state,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal"),
                     textsize = "15px",
                     direction = "auto")) %>%
        addPolygons(data = cty,
                    color = '#444444', weight = 1, smoothFactor = 0.5,
                    opacity = 0.5, fillOpacity = 0.5,
                    fillColor = ~pal(value),
                    highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = T),
                    label = ~labels_cty,
                    # options = pathOptions(interactive = FALSE),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal"),
                      textsize = "15px",
                      direction = "auto"))
    }
    
  })

  
  # observe({
  #   # event <- input$map_shape_click
  #   cty_data_gather = cty_data_gather %>% filter(date == input$plot_date)
  #   cty@data <- cty_data_gather
  #   cty@data$INFECTED <- cty@data$value
  # 
  #   if(is.null(input$map_shape_click)){
  #     cty@data$INFECTED = 0
  #   } else{
  #     cty = subset(cty, State == input$map_shape_click)
  #   }
  # 
  #   leafletProxy("mymap") %>%
  #     clearShapes() %>%
  #     addPolygons(data = cty,
  #                 color = '#444444', weight = 1, smoothFactor = 0.5,
  #                 opacity = 0.5, fillOpacity = 0.9,
  #                 fillColor = ~pal(INFECTED),
  #                 highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = T),
  #                 label = labels(),
  #                 labelOptions = labelOptions(
  #                   style = list("font-weight" = "normal"),
  #                   textsize = "15px",
  #                   direction = "auto"))
  # })
  
  
  
  # click on a marker
  # click on a marker
  # observe({ 
  #   event <- input$map_marker_click
  #   reactive_covid <- 
  #   
  #   message <- paste("widgets sold in", points$name[points$uid == event$id],":", points$widgets[points$uid == event$id]) 
  #   output$widgets <- renderText(message)
  # })
  
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





