library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)

# Define UI for application
ui <- fluidPage(
  titlePanel("Winter-run Current and Historic Cumulative Salvage"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("year", "Select Year:", choices = unique(cumulativeLAD$WY), options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3", `count-selected-text` = "{0} years selected")),
      selectInput("type", "Select Type:", choices = unique(cumulativeLAD$Type2), multiple = TRUE),
      selectInput("type", "Select BiOp grouping:", choices = c("Pre-2009 BiOp", "2009 & 2019 BiOp"), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    req(input$year)
    req(input$type)
    
    data <- cumulativeLAD %>% 
      filter(WY %in% input$year, Type2 %in% input$type, Status %in% input$Status )
    
    yearsLAD <- data %>% group_by(WY, Status, TYPE, Type2) %>% summarize(Day = max(wDay), Loss = max(cumloss))
    max2024LAD <- data %>% filter(WY == 2024 & cumloss == max(cumloss)) %>% pull(cumloss)
    loss2024LAD <- data %>% filter(WY == 2024) %>% select(10, 11)
    
    
    WR <- ggplot(data = data) +
      geom_line(filter(data, WY < 2024), 
                mapping = aes(x = wDay, y = cumloss, group = factor(WY)), color = 'grey', linewidth = 1)+
      geom_line(loss2024LAD, 
                mapping = aes(x = wDay, y = cumloss), color = 'steelblue2', linewidth = 1.5)+
      geom_text_repel(filter(yearsLAD, Loss > max2024LAD), mapping = aes(x = Day+1, y = Loss, label = WY), 
                      size = 3, fontface = 'bold', alpha = 0.75, min.segment.length = .1) +
      facet_grid(Status ~ Type2) +
      labs(x = 'Date', y = 'Cumulative Loss', title = 'Winter-run Current and Historic Cumulative Salvage') +
      scale_x_continuous(breaks = c(93,152,213,274), labels = c('Jan', 'Mar', 'May', 'Jul')) +
      theme(plot.margin = margin(0.5,0.5,0.25,0.25, unit = 'cm'),
            axis.title.x = element_text(margin=margin(t=10)),
            axis.title.y = element_text(margin=margin(r=10)))
    WR
  })
}

# Run the application 
shinyApp(ui = ui, server = server)