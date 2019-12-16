library(shiny)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(reshape2)

ui <- dashboardPage(
  dashboard_header(color = "blue", title = "Migration Statistics", inverted = TRUE),
  dashboardSidebar(
    size = 'thin', color = 'teal',
    sidebarMenu(
      menuItem(tabName = "migration", "Migration"),
      menuItem(tabName = 'population', "Population")
    )
  ),
  dashboard_body(
    tabItems(
      selected = 1,
      tabItem(
        tabName = 'migration',
        fluidRow(
          box(width = 16,
              title = 'Arrivals and Departures',
              color = 'blue', ribbon = TRUE, title_side = 'top right',
              column(width = 16,
                     plotOutput("boxplot1")
                     )
              ),
          box(width = 16,
              title = 'Arrivals and Departures based on Age',
              color = "blue", ribbon = TRUE, title_side = 'top right',
              column(width = 16,
                     plotlyOutput("dotplot1")
                     )
              ),
          box(width = 16,
              title = 'Arrivals and Departures based on Sex',
              color = "blue", ribbon = TRUE, title_side = 'top right',
              column(width = 16,
                     plotlyOutput("boxplot2")
              )
          )
          
        )
      ),
      tabItem(
        tabName = "population",
        fluidRow(
          box(width = 16,
              title = 'Arrivals and departures each year',
              ribbon = TRUE, title_side = 'top right',
              column(width = 16,
                     plotlyOutput("dotplot3")
              )
          ),
          
          box(width = 16,
              title = 'Net gain per year',
              color = "blue", ribbon = TRUE, title_side = 'top right',
              column(width = 16,
                     plotlyOutput("dotplot2")
              )
          )
        )
      )
    )
  )
)

server <- shinyServer(function(input,output,session) {
  population <- read.csv("PopulationStatistics2.csv")
  population$direction <- factor(population$direction, levels = c("Arrivals", "Departures"))
  
  migration <- read.csv("MigrationStatistics.csv")
  migration$Date <- factor(migration$Date, levels = c("2001", "2002", "2003", "2004", "2005",
                                                        "2006", "2007", "2008", "2009", "2010",
                                                      "2011", "2012", "2013", "2014", "2015", 
                                                      "2016", "2017", "2018", "2019"))
  
  output$boxplot1 <- renderPlot({
    ggplot(population, aes(x = direction, y = Estimate)) +
      geom_boxplot(fill = semantic_palette[["teal"]]) +
      xlab("Direction") + ylab("Estimated number of people arriving")
  })
  
  output$dotplot1 <- renderPlotly({
    ggplotly(ggplot(population, aes(x = Age, y = Estimate, fill = direction)) 
             + geom_bar(stat = "identity", width = 0.5) + theme_minimal()
             )
  })
  
  output$boxplot2 <- renderPlotly({
    ggplotly(ggplot(population, aes(x = sex, y = Estimate, fill = Age))
             + geom_bar(stat = "identity", width = 0.5) + theme_minimal()
    )
  })
  
  output$dotplot3 <- renderPlotly({
    ggplotly(ggplot(migration, aes(x = Date, y = Migration_arrivals, label = Migration_departures)) 
             + geom_line(linetype = 'dashed') + geom_point()
             + theme_minimal()
    )
  })
  
  output$dotplot2 <- renderPlotly({
    ggplotly(ggplot(migration, aes(x = Date, y = Migration_netgain))
             + geom_bar(stat = "identity", width = 0.5) + theme_minimal()
             )
  })
  
})


shinyApp(ui, server)
