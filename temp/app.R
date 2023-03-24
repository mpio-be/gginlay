library(shiny)
library(ggplot2)
library(gridlayout)
library(bslib)


ui <- grid_page(
  layout = c(
    ".       .        ",
    "sidebar linePlots",
    ".       .        "
  ),
  row_sizes = c(
    "70px",
    "1fr",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  
  grid_card(
    area = "sidebar",
    card_body_fill(
      sliderInput(
        inputId = "numChicks",
        label = "Number of Chicks",
        min = 1,
        max = 15,
        value = 5,
        step = 1,
        width = "100%"
      )
    )
  ),
  grid_card_plot(area = "linePlots")
)


server <- function(input, output) {
   
  output$linePlots <- renderPlot({
    obs_to_include <- as.integer(ChickWeight$Chick) <= input$numChicks
    chicks <- ChickWeight[obs_to_include, ]
  
    ggplot(
      chicks,
      aes(
        x = Time,
        y = weight,
        group = Chick
      )
    ) +
      geom_line(alpha = 0.5) +
      ggtitle("Chick weights over time")
  })
  
  output$dists <- renderPlot({
    ggplot(
      ChickWeight,
      aes(x = weight)
    ) +
      facet_wrap(input$distFacet) +
      geom_density(fill = "#fa551b", color = "#ee6331") +
      ggtitle("Distribution of weights by diet")
  })
}

shinyApp(ui, server)
  
