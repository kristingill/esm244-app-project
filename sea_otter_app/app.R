# Sea otter app

library(tidyverse)
library(shiny)
library(here)
library(janitor)

ui <- fluidPage(includeCSS("www/ocean.css"),

    navbarPage("Southern Sea Otters",
               tabPanel("Strandings by Life Stage",
                        sidebarLayout(
                            sidebarPanel("Sea Otter Stranding Life Stage",
                                         checkboxGroupInput(inputId = "pick_life_stage",
                                                            label = "Choose Life Stage:",
                                                            selected = "pups",
                                                            choices = unique(sea_otter_data$life_stage))),
                            mainPanel(plotOutput("ls_plot"))
                        )
                        ),
               tabPanel("Strandings by Location",
                        sidebarLayout(
                          sidebarPanel("Sea Otter Stranding Location",
                                       sliderInput(inputId = "pick_year",
                                                   label = "Slide Over Time",
                                                   min = 1985,
                                                   max = 2014,
                                                   value = 1985,
                                                   animate = TRUE,
                                                   sep = "")),
                          mainPanel(plotOutput("geog_plot"))
                        )
                        ),
               tabPanel("Strandings Map",
                        sidebarLayout(
                          sidebarPanel("Sea Otter Linear Density",
                                       selectInput(inputId = "pick_density",
                                                   label = "Select Linear Density",
                                                   multiple = TRUE,
                                                   selected = 13.9,
                                                   choices = levels(factor(locations_sea_otters$lin_dens))
                                                   )),
                          mainPanel(tmapOutput("density_plot"))
                        )
                        ),
               tabPanel("Widget 4"),
               tabPanel("Summary")
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
      theme_minimal() +
      labs(x = "Time (Years)",
           y = "Number of Sea Otter Strandings",
           title = "Number of Sea Otter Strandings Over Time (1985-2017)") +
      guides(fill=guide_legend(title="Life Stage"))
  )

  geog_reactive <- reactive({

    geog_sea_otter %>%
      filter(year %in% input$pick_year)

  })

  output$geog_plot <- renderPlot(
    ggplot(data = geog_reactive(), aes(x = reorder(location, count),
                                     y = count)) +
      geom_col(color = "darkslategrey", fill = "darkslategray3") +
      labs(x = "Location", y = "Number of Stranded Sea Otters") +
      coord_flip() +
      theme_minimal()
  )

  density_reactive <- reactive({

    locations_sea_otters %>%
      filter(lin_dens %in% input$pick_density)
  })

  output$density_plot <- renderTmap(
    tm_shape(density_reactive()) +
      tm_polygons("lin_dens", border.alpha = 0) +
      tm_fill("lin_dens") +
      tm_borders(alpha = 0)
  )

}

shinyApp(ui = ui, server = server)

