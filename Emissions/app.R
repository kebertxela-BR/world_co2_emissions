#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#things to add, panels with other data to look at, add multiple countries to a graphic, table that shows the countries selected, population, co2 max and current levels, and co2 goals
library(shiny)
library(plotly)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(DT)
library(shinythemes)
library(bslib)



emit=read_csv("owid-co2-data.csv")%>% 
  filter(year>1920) %>% select(country,year,iso_code,population,co2,co2_per_capita) %>% mutate(co2 = coalesce(co2, 0),
                                                                                               co2_per_capita = coalesce(co2_per_capita, 0))



accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme=bs_theme(version=5,bootswatch="zephyr"),
  
  # Application title
  titlePanel("World CO2 Emissions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "checkboxes",
        label= "Select Countries",
        choices = unique(emit$country),
        selected="United States",
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          delimiter = ",",
          create =FALSE,
          persist = FALSE,
          highlight = FALSE,
          selectOnTab= TRUE,
          searchfield= list("placeholder"="Search...")
        )),
      selectInput("year",
                  h4("Select a year:"),
                  choices = unique(emit$year)))  ,
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Emissions per Capita", plotlyOutput("distPlot"),br(),dataTableOutput("distable")),
                  tabPanel("Annual Emissions", plotlyOutput("distPlot2"),br(),dataTableOutput("distable2")))
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    thisCountry=input$checkboxes
    
    f=emit %>% accumulate_by(~year) %>% filter(country %in% c(thisCountry))
    
    f <- f %>%
      plot_ly(
        x = ~year, 
        y = ~co2_per_capita,
        split = ~country,
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F)
      )
    
    f <- f %>% layout(
      xaxis = list(
        title = "Year",
        zeroline = F
      ),
      yaxis = list(
        title = "Annual CO2 per Capita",
        zeroline = F
      )
    ) 
    f <- f %>% animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE
    )
    f <- f %>% animation_slider(
      hide = F
    )
    f <- f %>% animation_button(
      x = 1.1, xanchor = "right", y = 0, yanchor = "bottom"
    )
    
    f
  })
  
  output$distPlot2 <- renderPlotly({
    thisCountry=input$checkboxes
    
    fig=emit %>% accumulate_by(~year) %>% filter(country %in% c(thisCountry))  
    fig <- fig %>%
      plot_ly(
        x = ~year, 
        y = ~co2,
        split = ~country,
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F)
      )
    
    fig <- fig %>% layout(
      xaxis = list(
        title = "Year",
        zeroline = F
      ),
      yaxis = list(
        title = "Annual CO2 (tonnes)",
        zeroline = F
      )
    ) 
    fig <- fig %>% animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE
    )
    fig <- fig %>% animation_slider(
      hide = F
    )
    fig <- fig %>% animation_button(
      x = 1.1, xanchor = "right", y = 0, yanchor = "bottom"
    )
    
    fig
  })
  
  output$distable <- renderDT({
    
    thisCountry=input$checkboxes 
    whatyear=input$year
    selection_table= emit %>% select(country,year,population,co2_per_capita) %>% filter(year==whatyear,country %in% thisCountry) 
    
    callback <- c(
      "$('#DataTables_Table_0_length select').css('color', '#000');",
      "$('#DataTables_Table_0_filter input').css('color', '#000');"
    )
    
    
    
    datatable(selection_table, callback=JS(callback),colnames = c("Country", "Year", "Population", "CO2 (tonnes) Per Capita "), options = list(
      initComplete = JS(
      )))
    
  })
  output$distable2 <- renderDT({
    
    thisCountry=input$checkboxes 
    whatyear=input$year
    selection_table= emit %>% select(country,year,population,co2) %>% filter(year==whatyear,country %in% thisCountry) 
    
    
    callback <- c(
      "$('#DataTables_Table_0_length select').css('color', '#000');",
      "$('#DataTables_Table_0_filter input').css('color', '#000');"
    )
    
    
    
    datatable(selection_table, callback=JS(callback),colnames = c("Country", "Year", "Population", "Annual CO2 (tonnes)"), options = list(
      initComplete = JS(
      )))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
