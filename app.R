
library(shiny)
library(plotly)
library(tidyverse)

# Load and preprocess data
df <- read.csv("ess_allyr_multi.csv")

countries_full <- c("Belgium", "Switzerland", "Germany", "Spain", "Finland", "France", "United Kingdom", "Hungary", "Ireland", "Netherlands", "Norway", "Poland", "Portugal", "Sweden", "Slovenia")


df <- df %>%
  mutate(name = case_when(
    name == "ESS1e06_7" ~ "2002",
    name == "ESS2e03_6" ~ "2004",
    name == "ESS3e03_7" ~ "2006",
    name == "ESS4e04_6" ~ "2008",
    name == "ESS5e03_5" ~ "2010",
    name == "ESS6e02_6" ~ "2012",
    name == "ESS7e02_3" ~ "2014",
    name == "ESS8e02_3" ~ "2016",
    name == "ESS9e03_2" ~ "2018",
    name == "ESS10e03_2" ~ "2020",
    name == "ESS10SCe03_1" ~ "2020",
    name == "ESS11e02" ~ "2023"
  ))


dfhappy <- df %>%
  select(name, cntry, happy) %>%
  filter(happy <= 10) %>%
  group_by(name, cntry, happy) %>%
  summarise(qty = n(), .groups = "drop") %>%
  group_by(name, cntry) %>%
  mutate(perc = round((qty / sum(qty)) * 100, 1))

# UI
ui <- fluidPage(
  #div(
  #  titlePanel("Happiness Level in European Countries"),
  #  style = "text-align: center;"
  #),

    sidebarLayout(
    sidebarPanel(
      style = "background-color: #f8f9fa; border-radius: 10px; padding: 15px; margin-top: 57px;", #57px
      selectInput(
        inputId = "ess_yr",
        label = "Year:",
        choices = unique(dfhappy$name),
        selected = unique(dfhappy$name)[11] # Default selection (first year in the list)
      ),
      checkboxGroupInput(
        inputId = "selected_countries",
        label = "Countries:",
        choiceValues = unique(dfhappy$cntry),
        choiceNames = countries_full,
        selected = unique(dfhappy$cntry)[c(10,13)]
      ),
      sliderInput(
        inputId = "slideracc", 
        label = "Cumulative line by level of happiness:", 
         min = 0, max = 10, value = 5, step = 1)

    ),
    
    mainPanel(
      HTML('<h3> <span style="text-decoration: underline;">Happiness Level in European Countries</span></h3>'),
      div( style = "background-color: #f8f9fa; border-radius: 10px; padding: 15px;
           display: inline-block; max-width: 600px; margin: auto;",
      HTML(
        "
  <h4><strong>How to Use:</strong></h4>
  <ul style='line-height: 1.8;'>
    <li><strong>Select a year:</strong> Use the dropdown to pick a specific year.</li>
    <li><strong>Choose countries:</strong> Select one or more countries from the checkbox list.</li>
    <li><strong>Adjust the slider:</strong> Filter the cumulative percentage up to a happiness level.</li>
    <li><strong>Hover over the graph:</strong> View detailed statistics for each data point.</li>
  </ul>
  "
      )),
      plotlyOutput("happinessPlot")
    )
  
)
)




# Server
server <- function(input, output, session) {
  # Reactive filtering based on selected countries and year
  
  filteredData <- reactive({
    req(input$selected_countries) # Ensure at least one country is selected
    dfhappy %>%
      #filter(cntry %in% input$selected_countries & name == input$ess_yr) %>%
      filter(cntry %in% input$selected_countries) %>%
      filter(name == input$ess_yr) %>%
      group_by(name, cntry) %>%
      mutate(accum_freq = cumsum(perc)) %>%
      mutate(accum_freq = ifelse(row_number() == n(), 100, accum_freq))}) # Ensure the last value = 100%
    
  filteredDataAcc <- reactive({
      req(input$selected_countries) # Ensure at least one country is selected
      dfhappy %>%
        filter(cntry %in% input$selected_countries) %>%
        filter(name == input$ess_yr) %>%
        group_by(name, cntry) %>%
        mutate(accum_freq = cumsum(perc)) %>% 
        filter(happy <= input$slideracc)
      })
  
  
  
  
  # Render the plotly plot
  output$happinessPlot <- renderPlotly({
    plot_ly() %>%
      add_trace(
        data = filteredData(),
        x = ~happy,
        y = ~perc,
        color = ~cntry,
        type = 'bar',
        text = ~paste0(
          "Country: ", cntry,
          "<br>Happiness Score: ", happy,
          "<br>Percentage: ", perc, "%",
          "<br>Accumulated Frequency: ", accum_freq, "%"
        ),
        hoverinfo = 'text',
        textposition = 'none'
      ) %>%
      add_trace(
        data = filteredDataAcc(),
        x = ~happy,
        y = ~accum_freq,
        color = ~cntry,
        type = 'scatter',
        mode = 'lines+markers',
        text = ~paste(
          "Country: ", cntry,
          "<br>Happy Level: ", happy,
          "<br>Percentage: ", perc, "%",
          "<br>Accumulated Frequency: ", accum_freq, "%"
        ),
        hoverinfo = 'text',
        textposition = 'none'
      ) %>%
      layout(
        #title = "Happiness with Cumulative Line",
        xaxis = list(title = "Happiness Scores", tickvals = unique(filteredData()$happy)),
        yaxis = list(title = "Percentage"),
        barmode = "group",  
        legend = list(title = list(text = "Country"))
        
      )
  })



  
  
}

# Run the application
shinyApp(ui = ui, server = server)
