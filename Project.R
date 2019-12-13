library(shiny)
library(semantic.dashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboard_header(color = "blue", title = "Migration Statistics", inverted = TRUE),
  dashboardSidebar(
    size = 'thin', color = 'teal',
    sidebarMenu(
      menuItem(tabName = "main", "Main"),
      menuItem(tabName = 'extra', "Extra")
    )
  ),
  dashboard_body(
    tabItems(
      selected = 1,
      tabItem(
        tabName = 'main',
        fluidRow(
          box(width = 8,
              title = 'Graph 1',
              color = 'green', ribbon = TRUE, title_side = 'top right',
              column(8,
                     plotOutput("boxplot1")
                     )
              ),
          box(width = 8,
              title = 'Graph 2',
              color = "red", ribbon = TRUE, title_side = 'top right')
        )
      ),
      tabItem(
        tabName = "extra",
        fluidRow(
          h1("extra")
        )
      )
    )
  )
)

server <- shinyServer(function(input,output,session) {
  population <- read.csv("PopulationStatistics.csv")
  population$direction <- factor(population$direction, levels = c("Arrivals", "Departures"))
  
  output$boxplot1 <- renderPlot({
    ggplot(population, aes(x = direction, y = estimate)) +
      geom_boxplot(fill = semantic_palette[["green"]]) +
      xlab("Direction") + ylab("Estimate")
  })
})


shinyApp(ui, server)
