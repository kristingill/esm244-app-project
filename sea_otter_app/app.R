# sea otter app

library(tidyverse)
library(shiny)
library(here)
library(janitor)

ui <- fluidPage(theme = "ocean.css",

    navbarPage("Southern Sea Otters",
               tabPanel("Thing 1",
                        sidebarLayout(
                            sidebarPanel("Sea Otter Strandings",
                                         checkboxGroupInput(inputId = "pick_life_stage",
                                                            label = "Choose Life Stage:",
                                                            choices = unique(sea_otter_data$life_stage))),
                            mainPanel(plotOutput("ls_plot"))
                        )
                        ),
               tabPanel("Thing 2"),
               tabPanel("Thing 3")
               )
)

server <- function(input, output) {

  ls_reactive <- reactive({

    sea_otter_data %>%
      filter(life_stage %in% input$pick_life_stage)
  })

  output$ls_plot <- renderPlot(
    ggplot(data = ls_reactive(), aes(x = year,
                                     y = count)) +
      geom_col(aes(fill = life_stage)) +
      theme_minimal()
  )

}

shinyApp(ui = ui, server = server)

