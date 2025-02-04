library(shiny)
library(ggplot2)
library(gridlayout)
library(bslib)


ui <- grid_page(
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
  layout = c(
    "header header",
    "sidebar linePlots",
    "dists dists"
  ),
  grid_card(
    area = "sidebar",
    card_header("Settings"),
    card_body(
      selectInput(
        inputId = "mySelectInput",
        label = "Select Input",
        choices = list("choice a" = "a", "choice b" = "b")
      ),
      selectInput(
        inputId = "mySelectInput",
        label = "Select Input",
        choices = list("choice a" = "a", "choice b" = "b")
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Chick Weights",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card_plot(area = "dists"),
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
  

