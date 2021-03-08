# Sea otter app

library(tidyverse)
library(shiny)
library(here)
library(janitor)

ui <- fluidPage(includeCSS("www/ocean.css"),

    navbarPage("Southern Sea Otters",
               tabPanel("Summary",
                        sidebarLayout(
                          mainPanel(h1(strong("About the App")),
                                    p("This app helps visualize Southern Sea Otter stranding data, population data, and linear density data. The data is from the U.S. Geological Survey."),
                                    h1(strong("Introduction")),
                                    p("The Southern Sea Otter (Enhydra lutris nereis) is a threatened species. Sea otters play an important role in the ecosystem by consuming sea urchins. If left uncontrolled, sea urchins would consume kelp to the point of depleting kelp forests. In places where sea otters no longer roam, sea urchins have taken over and eliminated kelp forests, depleting coastal productivity. Below is a map of the Southern Sea Otter current range. The Southern Sea Otter range used to extend from Alaska to Baja California, but their population was decimated due to the fur trade. Conservationists and researchers have been working to restore sea otter populations and re-expand their range. Now, sea otters face threats like entanglement, oil spills, and chemical pollution."),
                                    img(src = "otters.jpg", height = 172, width = 350, style = "display:block; margin-left: auto; margin-right: auto;"),
                                    fluid = TRUE,
                                    h2(strong("Data Citation")),
                                    p("Data Citation Annual Sea Otter Census: Yee, J.L., and Tinker, M.T., 2018, Annual California Sea Otter Census, 1985-2014:U.S. Geological Survey data release."),
                                    p("Data Citation Sea Otter Strandings: Hatfield, B.B., Harris, M.D., Young, C., Ames, J.A., and Tinker, M.T., 2018, Summary of stranded southern sea otters, 1985-2017 (ver. 2.0, September 2018): U.S. Geological Survey data release, https://doi.org/10.5066/F71J98P4")),
                          sidebarPanel(img(src = "Sea_Otter_Population_Range.png", height = 470, width = 350, style = "display:block; margin-left: auto; margin-right: auto;"),
                                       fluid = TRUE,
                                       style = "background-color: azure"),
                        )
                        ),
               tabPanel("Strandings by Life Stage & Sex",
                        sidebarLayout(
                            sidebarPanel("Sea Otter Strandings",
                                         selectInput(inputId = "pick_life_stage",
                                                     label = "Choose Life Stage",
                                                     selected = "Pups",
                                                     multiple = TRUE,
                                                     choices = unique(sea_otter_data$life_stage)),
                                         checkboxGroupInput(inputId = "pick_sex",
                                                            label = "Pick Sex",
                                                            choices = unique(sea_otter_data$sex),
                                                            selected = "female"),
                                         style = "background-color: lightcyan;
                                                  box-shadow: 2px 4px teal;
                                                  border: 1.5px solid darkslategrey"),
                            mainPanel(plotOutput("ls_plot"))
                        )
                        ),
               tabPanel("Strandings by Location",
                        sidebarLayout(
                          sidebarPanel("Sea Otter Stranding Locations",
                                       sliderInput(inputId = "pick_year",
                                                   label = "Slide Over Time",
                                                   min = 1985,
                                                   max = 2014,
                                                   value = 1985,
                                                   animate = TRUE,
                                                   sep = ""),
                                       p("Press play to animate the graph and see the changes over time. The area with the most strandings will always be at the top of the graph. You can also click on the circle and drag along the time frame to pick a specific year to visualize."),
                                       style = "background-color: lightcyan;
                                                box-shadow: 2px 4px teal;
                                                border: 1.5px solid darkslategrey"),
                          mainPanel(plotOutput("geog_plot"),
                                    p("Figure 2: The number of stranded sea otters by geographic location over time. Below is a map with many of the locations shown."),
                                    img(src = "Sea_Otter_Population_Range.png", height = 600, width = 500, style = "display:block; margin-left: auto; margin-right: auto;"))
                        )
                        ),
               tabPanel("Census",
                        sidebarLayout(
                          sidebarPanel("Sea Otter Population Over Time",
                                       sliderInput("range",
                                                   inputId = "range_2",
                                                   label = "Select Year Range",
                                                   min = 1985,
                                                   max = 2014,
                                                   value = c(1985,2014),
                                                   step = 1,
                                                   dragRange = TRUE,
                                                   sep = ""),
                                       style = "background-color: lightcyan;
                                                box-shadow: 2px 4px teal;
                                                border: 1.5px solid darkslategrey"),
                          mainPanel(plotOutput("census_plot"))
                        )
                        ),
               tabPanel("Density Map",
                        sidebarLayout(
                          sidebarPanel("Sea Otter Linear Density (Otters/km of Coastline)",
                                       checkboxGroupInput(inputId = "range",
                                                   label = "Choose Linear Density",
                                                   selected = 8:14,
                                                   choices = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)),
                                       p("Check and Uncheck boxes to visualize sea otter linear densities along the coast. A higher number indicates that more sea otters are present there."),
                                       style = "background-color: lightcyan;
                                                box-shadow: 2px 4px teal;
                                                border: 1.5px solid darkslategrey"),
                          mainPanel(tmapOutput("density_plot"),
                                    p("Figure 4: A map of sea otter linear densities. The darker colors indicate higher linear denisites. Hovering over the polygons shows the areas and clicking on the polygons shows the linear densities."))
                        )
                        )
))

server <- function(input, output) {

  ls_reactive <- reactive({

    sea_otter_data %>%
      filter(life_stage %in% input$pick_life_stage) %>%
      filter(sex %in% input$pick_sex)
  })

  output$ls_plot <- renderPlot(
    ggplot(data = ls_reactive(), aes(x = year,
                                     y = count)) +
      geom_col(aes(fill = life_stage)) +
      theme_minimal() +
      labs(x = NULL,
           y = "Number of Stranded Sea Otters") +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Set2") +
      guides(fill=guide_legend(title="Life Stage")) +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold"))
  )

  geog_reactive <- reactive({

    geog_sea_otter %>%
      filter(year %in% input$pick_year)

  })

  output$geog_plot <- renderPlot(
    ggplot(data = geog_reactive(), aes(x = reorder(location, count),
                                     y = count)) +
      geom_col(color = "darkslategrey", fill = "darkslategray3") +
      labs(x = NULL,
           y = "Number of Stranded Sea Otters") +
      coord_flip() +
      theme_minimal() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold"))

  )

  year_range_reactive <- reactive({

    sea_otter_pop %>%
      filter(year %in% input$range_2[1]:input$range_2[2])
  })

  output$census_plot <- renderPlot(
    ggplot(data = year_range_reactive(), aes(x = year,
                                             y = n)) +
      geom_line(color = "darkslategray4", size = 1.5) +
      labs(x = NULL,
           y = "Sea Otter Count") +
      facet_wrap(~zone_code, scales = "free") +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold"))
  )

  density_reactive <- reactive({

    locations_sea_otters %>%
      filter(lin_dens %in% input$range)
  })

  output$density_plot <- renderTmap(
    tm_shape(density_reactive()) +
      tm_polygons("lin_dens", border.alpha = 0) +
      tm_fill("lin_dens", palette = "BuGn") +
      tm_borders(alpha = 0) +
      tm_basemap("Esri.WorldTopoMap")

  )
}

shinyApp(ui = ui, server = server)

