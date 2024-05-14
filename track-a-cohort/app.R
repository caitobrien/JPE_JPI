library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(xts)

# Function to convert DOY to month name
doy_to_month <- function(doy, year_type) {
  # Adjust the DOY based on the year type
  adjusted_doy <- if (year_type == "WY") {
    (doy + 273) %% 365 + 1
  } else {
    doy
  }
  # Convert the adjusted DOY to a date
  date <- as.Date(paste(2000, adjusted_doy), format = "%Y %j")
  # Return the month name
  return(format(date, "%b"))
}

# Function to plot figures 
fct_stars_survival_plot <- function(data, x_var, year, metric) {
  
  # Define y variable and ribbon variables based on metric
  y_var <- metric
  ymin_var <- paste0(metric, "L80")
  ymax_var <- paste0(metric, "U80")
  
  # # Define title based on metric
  # rct_title <- switch(metric,
  #                     surv = 'STARS model - Overall Survival',
  #                     idsurv = 'STARS model -Interior Delta Route-specific Survival Probability',
  #                     idRoute = 'STARS model - Interior Delta Route-specific Probability',
  #                     'STARS model - Survival')
  
  # # Define title based on metric
  # rct_caption <- switch(metric,
  #                       surv = "The solid line shows median survival of daily cohorts of winter run Chinook Salmon\n through the Delta from Knights Landing to Chipps Island for all routes combined.",
  #                       idsurv = "Route-specific survival of daily cohorts of winter run Chinook Salmon\nthrough the Delta from Knights Landing to Chipps Island.",
  #                       idRoute = "Proportion of daily cohorts of winter run Chinook Salmon\nthrough the Delta (Knights Landing to Chipps Island) using the Interior Delta route.")
  
  # Define title based on metric
  rct_x_var <- switch(x_var,
                      wDay =  'Month\n(Water Year: Oct-Dec of year [t-1], Jan-Sep of year [t])',
                      doy =  'Month\n(Calendar Year: Jan-Dec of year [t])')
  
  # Generate the labels for the x-axis
  labels <- sapply(seq(1, 365, by = 60), function(doy) doy_to_month(doy, year))
  
  
  
  # Plot
  p <- ggplot(data, aes_string(x = x_var, group = year)) +
    geom_line(aes_string(y = y_var)) +
    labs(x = rct_x_var,
         y = 'Probability',
         title =  NULL,#rct_title,
         subtitle = "Years of data : 2018 to 2024"
         #losing caption with plotly--see plotly annotation instead
         # caption = rct_caption
         ) +
    gghighlight(use_direct_label = FALSE) +
    facet_wrap(as.formula(paste0("~", year))) +
    geom_ribbon(aes_string(ymin = ymin_var, ymax = ymax_var), alpha = 0.5) +
    scale_x_continuous(breaks = seq(1, 365, by = 60), labels = labels) +
    # scale_x_continuous(breaks = seq(1, 365, by = 60), labels = day_to_month) +
    theme_minimal()
  
  p <- plotly::ggplotly(p)
  
  
  p <- p %>%
    plotly::layout(
      autosize = F,
      margin = list(
        l = 50, # left margin
        r = 50, # right margin
        b = 100, # bottom margin
        t = 50, # top margin
        pad = 4 # padding
      )
      
    ) 
  #annotation needs work -- if shiny or rmark easier to do outside plot
  # %>% plotly::add_annotations(
  #     x = .65,
  #     y = -0.31,
  #     text = rct_caption,
  #     showarrow = F,
  #     xref = 'paper',
  #     yref = 'paper',
  #     xanchor = 'right',
  #     yanchor = 'auto',
  #     xshift = 0,
  #     yshift = 0,
  #     font = list(size = 10)
  #   )
  
  return(p)
}

ui <- dashboardPage(
  dashboardHeader(title = "STARS Survival Plot"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        width = 12,
        fluidRow(
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
        )
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
      uiOutput("plot_caption"),
      plotlyOutput("plot")
      )
    )
    )
  )

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    # Load data
    load(here::here("track-a-cohort/STARS.shinyinputs.Rdata")) #COB removed verbose=T to run with here::here
    
    # Subset the data and convert to tibble
    df_stars <- as_tibble(WR_xts[,c("Survival Interior Delta Est", 
                                    "Survival Interior Delta LCL 80", 
                                    "Survival Interior Delta UCL 80",
                                    "Routing Probability Interior Delta Est",    
                                    "Routing Probability Interior Delta LCL 80",
                                    "Routing Probability Interior Delta UCL 80",
                                    "Survival Overall Est", 
                                    "Survival Overall LCL 80",
                                    "Survival Overall UCL 80")]) %>%
      # Add the first date as a new column
      mutate(date = index(WR_xts)) %>%
      # Make date the first column
      select(date, everything()) %>%
      rename( surv =  "Survival Overall Est", survL80 =  "Survival Overall LCL 80", survU80 =  "Survival Overall UCL 80", 
              idsurv = "Survival Interior Delta Est", idsurvL80 = "Survival Interior Delta LCL 80", idsurvU80 = "Survival Interior Delta UCL 80", 
              idRoute = "Routing Probability Interior Delta Est", idRouteL80 =  "Routing Probability Interior Delta LCL 80", idRouteU80 =  "Routing Probability Interior Delta UCL 80") %>%
      # convert date to WY, wday
      arrange(date) %>% 
      mutate(WY = year(date) + (month(date) >= 10),
             wDay = if_else(month(date) >= 10, yday(date) - 273, yday(date) + 92),
             doy = yday(date),
             CY = year(date),
             wDate = if_else(month(date) >= 10, date + years(1), date))
    
    # Add this reactive expression
    output$plot_caption <- renderUI({
      HTML(switch(input$select_metric,
             surv = "<b>STARS model - Overall Survival:</b><br>The solid line shows median survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) for all routes combined.",
             idsurv = "<b>STARS model -Interior Delta Route-specific Survival Probability:</b><br> Route-specific survival of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island).",
             idRoute = "<b>STARS model - Interior Delta Route-specific Probability:</b> <br>Proportion of daily cohorts of winter run Chinook Salmon through the Delta (Knights Landing to Chipps Island) using the Interior Delta route."
             )
           )
    })
    
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