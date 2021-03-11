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
                                    p("Southern Sea Otters (Enhydra lutris nereis) have been listed as threatened since 1977 under the Endangered Species Act. Sea otters are a keystone species and play an important role in kelp forest ecosystems by consuming sea urchins. When sea urchin populations grow unchecked, they over-consume kelp and deplete kelp forests. In places where sea otters no longer roam, sea urchins have taken over and eliminated kelp forests, depleting coastal productivity. To the right is a map of the Southern Sea Otter's current range. Historically, their range extended from Alaska to Baja California, but their population was decimated due to the fur trade. Conservationists and researchers have been working to restore sea otter populations and re-expand their range. Now, sea otters face threats like entanglement, oil spills, and chemical pollution. Population data was collected by two-person teams using binoculars and spotting scopes to count individuals from accessible stretches of coastline and from fixed-wing aircraft in the remaining areas."),
                                    img(src = "otter.jpg", height = 172, width = 350, style = "display:block; margin-left: auto; margin-right: auto;"),
                                    fluid = TRUE,
                                    h2(strong("Data Citations")),
                                    p(strong("Annual Sea Otter Census Data (Census tab):")),
                                    p("Yee, J.L., and Tinker, M.T., 2018, Annual California Sea Otter Census, 1985-2014:U.S. Geological Survey data release, https://doi.org/10.5066/P9B2KNB3."),
                                    p(strong("Sea Otter Stranding Data (Strandings by Life Stage & Sex + Location tabs):")),
                                    p("Hatfield, B.B., Harris, M.D., Young, C., Ames, J.A., and Tinker, M.T., 2018, Summary of stranded southern sea otters, 1985-2017 (ver. 2.0, September 2018): U.S. Geological Survey data release, https://doi.org/10.5066/F71J98P4."),
                                    p(strong("Linear Density Data (Density map tab):")),
                                    p("Hatfield, B.B., Yee, J.L., Kenner, M.C., and Tomoleoni, J.A., 2019, Annual California sea otter census - 2019 spring census summary: U.S. Geological Survey data release, https://doi.org/10.5066/P9B2KNB3.")),
                          sidebarPanel(img(src = "fig-1-full.png", height = 470, width = 300, style = "display:block; margin-left: auto; margin-right: auto;"),
                                       fluid = TRUE,
                                       style = "background-color: azure",
                                       p("Rudebusch et al. 2020. https://doi.org/10.7717/peerj.10241.")),
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
                                         p("Pick a life stage to see that group represented by different colors on the graph. You also have the option to choose otter gender (note: this will only change the count reflected on the graph)."),
                                         checkboxGroupInput(inputId = "pick_sex",
                                                            label = "Pick Sex",
                                                            choices = unique(sea_otter_data$sex),
                                                            selected = "female"),
                                         style = "background-color: lightcyan;
                                                  box-shadow: 2px 4px teal;
                                                  border: 1.5px solid darkslategrey"),
                            mainPanel(plotOutput("ls_plot"),
                                      p(strong("Figure 1."), "The number of stranded sea otters by life stage and sex over time (1985-2017)."))
                        )
                        ),
               tabPanel("Strandings by Location",
                        sidebarLayout(
                          sidebarPanel("Sea Otter Stranding Locations",
                                       sliderInput(inputId = "pick_year",
                                                   label = "Slide Over Time",
                                                   min = 1985,
                                                   max = 2017,
                                                   value = 1985,
                                                   animate = TRUE,
                                                   sep = ""),
                                       p("Press play to animate the graph and see the changes over time. The area with the most strandings will always be at the top of the graph. You can also click on the circle and drag along the time frame to pick a specific year to visualize."),
                                       style = "background-color: lightcyan;
                                                box-shadow: 2px 4px teal;
                                                border: 1.5px solid darkslategrey"),
                          mainPanel(plotOutput("geog_plot"),
                                    p(strong("Figure 2."), "The number of stranded sea otters by geographic location over time (1985-2017). Below is a map with many of the locations shown."),
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
                                       p("Move the slider bar to select the year range of choice."),
                                       style = "background-color: lightcyan;
                                                box-shadow: 2px 4px teal;
                                                border: 1.5px solid darkslategrey"),
                          mainPanel(plotOutput("census_plot"),
                                    p(strong("Figure 3."), "Sea otter population over time (1985-2014)."))
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
                                    p(strong("Figure 4."), "A map of sea otter linear density in 2019. The darker colors indicate higher linear denisites. Hovering over the polygons shows the areas and clicking on the polygons shows the linear densities."))
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
      scale_x_continuous(breaks = integer_breaks()) +
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
      scale_x_continuous(breaks = integer_breaks()) +
      theme_minimal() +
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

