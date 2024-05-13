library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "STARS Survival Plot"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        width = 12,
        column(width = 4,
        selectInput(
        inputId = "select_yeartype",
        label = "Select year type:", 
        choices = c("Water Year", "Calendar Year"),
        multiple = FALSE)
        ),
        column(
          width = 4,
          selectInput(
                  inputId = "select_metric", 
                  label = "Select Probability:", 
                  choices = c("idRoute", "idsurv", "surv"),
                  multiple = FALSE)
        ),
        column(
          width = 4,
          selectInput(
            inputId = "select_years", 
            label = "Select years of interest:", 
            choices = 2017:2024,
            multiple = TRUE,
            selected = c(2017:2024)
        )
      )
      ),
    box(
      width = 12,
      plotlyOutput("plot")
      )
    )
    )
  )

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    # Load data
    files <- list.files(path = here::here('track-a-cohort/shared files/STARS/'))
    df_stars <- read_csv(here::here(paste0('track-a-cohort/shared files/STARS/',files))) %>% 
      bind_rows() %>% 
      select(1, surv = 2, survL80 = 3, survU80 = 4, idsurv = 17, idsurvL80 = 18, idsurvU80 = 19, idRoute = 32, idRouteL80 = 33, idRouteU80 = 34) %>% 
      arrange(Date) %>% 
      mutate(WY = year(Date) + (month(Date) >= 10),
             wDay = if_else(month(Date) >= 10, yday(Date) - 273, yday(Date) + 92),
             doy = yday(Date),
             CY = year(Date),
             wDate = if_else(month(Date) >= 10, Date + years(1), Date))
    
    # Filter data
    filtered_data <- df_stars %>% 
      filter(WY %in% input$select_years,
             CY %in% input$select_years)
    
    if(input$select_yeartype == "Water Year") {
      p<-fct_stars_survival_plot(filtered_data, x_var = "wDay", year = "WY", input$select_metric)
    } else {
      p<-fct_stars_survival_plot(filtered_data, x_var = "doy", year = "CY", input$select_metric)
    }
    
   return(p)
  })
}

shinyApp(ui, server)