# Sea otter app

library(tidyverse)
library(shiny)
library(here)
library(janitor)

ui <- fluidPage(includeCSS("www/ocean.css"),

    navbarPage("Southern Sea Otters",
               tabPanel("Summary",
                        sidebarLayout(
                          mainPanel(h2("About the App"),
                                    p("This app helps visualize Southern Sea Otter stranding and linear density data. The data is from the U.S. Geological Survey."),
                                    h2("Introduction"),
                                    p("The Southern Sea Otter (Enhydra lutris nereis) is a threatened and depleted species. Sea otters play an important role in the ecosystem. Sea otters consume sea urchins, which means that the urchins cannot feed on kelp as much. In places where sea otters no longer roam, sea urchins have taken over and eliminated kelp forests, depleting coastal productivity. Below is a map of the Southern sea otter current range."),
                                    h3("Data Citation"),
                                    p("Data Citation Annual Sea Otter Census: Yee, J.L., and Tinker, M.T., 2018, Annual California Sea Otter Census, 1985-2014:U.S. Geological Survey data release."),
                                    p("Data Citation Sea Otter Strandings: Hatfield, B.B., Harris, M.D., Young, C., Ames, J.A., and Tinker, M.T., 2018, Summary of stranded southern sea otters, 1985-2017 (ver. 2.0, September 2018): U.S. Geological Survey data release, https://doi.org/10.5066/F71J98P4")),
                          sidebarPanel(img(src = "Sea_Otter_Population_Range.png", height = 470, width = 350),
                                       fluid = TRUE,
                                       img(src = "otters.jpg", height = 172, width = 350),
                                       fluid = TRUE),

                        )),
               tabPanel("Strandings by Life Stage",
                        sidebarLayout(
                            sidebarPanel("Sea Otter Stranding Life Stage",
                                         selectInput(inputId = "pick_life_stage",
                                                     label = "Choose Life Stage:",
                                                     selected = "Pups",
                                                     multiple = TRUE,
                                                     choices = unique(sea_otter_data$life_stage)),
                                         style = "background-color: azure;"),
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
                                                   sep = ""),
                                       style = "background-color: azure;"),
                          mainPanel(plotOutput("geog_plot"))
                        )
                        ),
               tabPanel("Population Density Map",
                        sidebarLayout(
                          sidebarPanel("Sea Otter Linear Density",
                                       sliderInput("range",
                                                   inputId = "range",
                                                   label = "Pick a Range",
                                                   min  = 1,
                                                   max = 14,
                                                   value = c(1,14),
                                                   step = 1,
                                                   round = TRUE,
                                                   animate = FALSE,
                                                   dragRange = TRUE),
                                       style = "background-color: azure;"),
                          mainPanel(tmapOutput("density_plot"))
                        )
                        ),
               tabPanel("Census",
                        sidebarLayout(
                          sidebarPanel("Population Over Time by Zone",
                                       checkboxGroupInput(inputId = "pick_year_range",
                                                   label = "Select Year Range",
                                                   selected = 1985:2014,
                                                   choices = unique(sea_otter_pop$year)
                                                   ),
                                       style = "background-color: azure;"),
                          mainPanel(plotOutput("census_plot"))
                        ))
))

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
      scale_fill_manual(values = c("#33FFCC", "lightblue", "blue4", "blue2","#66CCCC", "blue3")) +
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
      filter(lin_dens %in% input$range[1]:input$range[2])
  })

  output$density_plot <- renderTmap(
    tm_shape(density_reactive()) +
      tm_polygons("lin_dens", border.alpha = 0) +
      tm_fill("lin_dens", palette = "BuGn") +
      tm_borders(alpha = 0)

  )

  year_range_reactive <- reactive({

    sea_otter_pop %>%
      filter(year %in% input$pick_year_range)
  })

  output$census_plot <- renderPlot(
    ggplot(data = year_range_reactive(), aes(x = year, y = n)) +
      geom_line(color = "darkseagreen4") +
      labs(x = NULL, y = "Sea Otter Count") +
      theme_minimal()
  )

}

shinyApp(ui = ui, server = server)

