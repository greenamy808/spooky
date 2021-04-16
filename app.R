#R Shiny tutorial

# Attach necessary packages
library(tidyverse)
library(shiny)
library(shinythemes)

spooky <- read_csv("spooky_data.csv")

# Create the user interface:
ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Spooky Shiny App!"),
  sidebarLayout(
    sidebarPanel("Widgets!",
                 selectInput(inputId = "state_select",
                             label = "Choose a state:",
                             choices = unique(spooky$state)),
                 radioButtons(inputId = "region_select",
                              label = "Choose region:",
                              choices = unique(spooky$region_us_census))),
    mainPanel("Outputs!",
              p("State's top candies:"),
              tableOutput(outputId = "candy_table"),
              p("Region's top costumes:",
                plotOutput(outputId = "costume_graph")))
  )
)

# Create the server function:
server <- function(input, output) {
  
  state_candy <- reactive({
    spooky %>%
      filter(state == input$state_select) %>%
      select(candy, pounds_candy_sold)
  })
  
  output$candy_table <- renderTable({
    state_candy()
  })
  region_costume <- reactive({
    spooky %>%
      filter(region_us_census == input$region_select) %>%
      count(costume, rank)
  })
  output$costume_graph <- renderPlot({
    ggplot(region_costume(), aes(x = costume, y = n)) +
      geom_col(aes(fill = rank)) +
      coord_flip() +
      scale_fill_manual(values = c("black", "purple", "orange")) +
      theme_minimal()
  })
}


# Combine them into an app:
shinyApp(ui = ui, server = server)
