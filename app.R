library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
library(mgcv)
library(forecast)

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Global Temperature Analysis"),
 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Average Temperature Rise", tabName = "tab1", icon = icon("chart-line")),
      menuItem("Surface Warming 1950-2022", tabName = "tab2", icon = icon("thermometer-half")),
      menuItem("Warming Estimates", tabName = "tab3", icon = icon("globe")),
      menuItem("Information", tabName = "tab4", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # Tab 1: Global Average Temperature Rise
      tabItem(tabName = "tab1",
              fluidRow(
                box(
                  title = "Temperature Anomaly (Have we reached 1.5°C yet?)", status = "warning", solidHeader = TRUE, width = 12,
                  plotOutput("anomalyPlot", height = "50vh")
                )
              ),
              fluidRow(
                box(
                  wellPanel(
                    div(style = "text-align: center;", htmlOutput("tempAnomalyMessage"))
                  ),
                  title = "Options", status = "primary", solidHeader = TRUE, width = 12, 
                  selectInput("dataName", "Choose a dataset", 
                              choices = c("NASA_GISS", "HADCRUT5", "CRUTEM5", "HADSST", 
                                          "NOAA_NCEI", "ERA_5", "HAD_CRUT4_Krig", "Berkeley")),
                  uiOutput("yearInput"),
                  pickerInput("method", "Choose a method", 
                              choices = c("LOESS", "Spline", "OSMA10", "OSMA20", 
                                          "COR", "ARIMA", "30yrlt", "20yrlt"), 
                              options = list(`style` = "btn-info")),
                  actionButton("info_button_tab1", icon("info-circle"))
                )
              )
      ),
      
      # Tab 2: Indicators of Global Surface Warming
      tabItem(tabName = "tab2",
              fluidRow(
                box(
                  title = "Global Surface Warming from 1950-2022", status = "warning", solidHeader = TRUE, width = 12,
                  plotOutput("indicatorPlot", height = "50vh")
                )
              ),
              fluidRow(
                box(
                  title = "Global Surface Warming Datasets", status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("indicator", "Choose an Indicator", 
                              choices = c("LOESS" = "LOESS.csv", 
                                          "Last 2010-yr 20avg" = "Last_2010-yr_20avg.csv", 
                                          "Last-yr 20avg 20GWL" = "Last-yr_20avg_20GWL.csv", 
                                          "End of 30-yr Trend C3S" = "End_of_30-yr_trend_C3S.csv", 
                                          "End of 30-yr Trend" = "End_of_30-yr_trend.csv", 
                                          "Forecast RCP4.5 CGWL" = "Forecast_RCP4.5_CGWL.csv", 
                                          "Human Induced Warming" = "Human_induced_warming.csv", 
                                          "UKCP18 RCP4.5 CGWL" = "UKCP18_RCP4.5_CGWL.csv")),
                  radioButtons("showAll", "Display Options", 
                               choices = c("Show Selected Dataset" = "single", 
                                           "Show All Datasets" = "all"), selected = "single"),
                  actionButton("info_button_tab2", icon("info-circle"))
                )
              )
      ),
      
      # Tab 3: Current Global Surface Warming
      tabItem(tabName = "tab3",
              fluidRow(
                box(
                  title = "Current Global Surface Warming Estimates", status = "warning", solidHeader = TRUE, width = 12,
                  plotOutput("warmingPlot", height = "50vh")
                )
              ),
              fluidRow(
                box(
                  title = NULL, status = "primary", solidHeader = TRUE, width = 12,
                  actionButton("info_button_tab3", icon("info-circle"))
                )
              )
      ),
      
      # Tab 4: Climate Information
      tabItem(tabName = "tab4",
              fluidRow(
                box(
                  title = "Global Warming Information", status = "primary", solidHeader = TRUE, width = 12,
                  uiOutput("rmdContent")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  ### Tab 1: Global Average Temperature Rise
  
  output$yearInput <- renderUI({
    startYears <- c(NASA_GISS = 1880, HADCRUT5 = 1850, CRUTEM5 = 1857, 
                    HADSST = 1850, NOAA_NCEI = 1880, ERA_5 = 1950, 
                    HAD_CRUT4_Krig = 1850, Berkeley = 1850)
    
    startYear <- startYears[input$dataName]
    endYear <- startYear + 20
    
    tagList(
      numericInput("minYear", "Start of pre-industrial period", 
                   value = startYear, min = startYear, max = 2024),
      numericInput("maxYear", "End of pre-industrial period", 
                   value = endYear, min = startYear, max = 2024)
    )
  })
  
  calcValues <- reactive({
    req(input$dataName, input$method)
    data_name <- input$dataName
    method <- input$method
    
    data <- read_csv(paste0("data/", data_name, ".csv"), show_col_types = FALSE) %>% na.omit()
    data$Anomaly <- data$Anomaly - mean(data$Anomaly[data$Year >= input$minYear & data$Year <= input$maxYear])
    
    if (method == "Spline") {
      model_fit <- gam(Anomaly ~ s(Year_num), data = data)
      data$Smooth <- model_fit$fitted.values
    } else if (method == "AR1") {
      ts_data <- ts(data$Anomaly, start = min(data$Year_num), frequency = 1)
      model_fit <- Arima(ts_data, order = c(1,0,0))
      data$Smooth <- model_fit$fitted
    } else if (method == "ARIMA") {
      ts_data <- ts(data$Anomaly, start = min(data$Year_num), frequency = 1)
      model_fit <- forecast::auto.arima(ts_data)
      data$Smooth <- model_fit$fitted
    } else if (method == "COR") {
      model_fit <- lm(Anomaly ~ poly(Year_num, 3), data = data)
      data$Smooth <- model_fit$fitted.values
    } else if (method == "OSMA20") {
      model_fit <- stats::filter(data$Anomaly, rep(1 / 240, 240), sides = 1)
      data$Smooth <- model_fit
    } else if (method == "OSMA10") {
      model_fit <- stats::filter(data$Anomaly, rep(1 / 120, 120), sides = 1)
      data$Smooth <- model_fit
    } else if (method == "LOESS") {
      model_fit <- stats::loess(Anomaly ~ Year_num, data = data)
      data$Smooth <- model_fit$fitted
    } else if (method == "30yrlt") {
      data$Smooth <- rep(NA, nrow(data))
      for (i in 30:nrow(data)) {
        data$Smooth[i] <- lm(Anomaly[(i-29):i] ~ Year_num[(i-29):i], data = data)$fitted.values[30]
      }
    } else if (method == "20yrlt") {
      data$Smooth <- rep(NA, nrow(data))
      for (i in 20:nrow(data)) {
        data$Smooth[i] <- lm(Anomaly[(i-19):i] ~ Year_num[(i-19):i], data = data)$fitted.values[20]
      }
    }
    
    method_desc <- data.frame(
      names = c("LOESS", "Spline", "AR1", "OSMA10", "OSMA20", "COR", 
                "ARIMA", "30yrlt", "20yrlt"),
      detail = c("Local polynomial regression",
                 "Penalized cubic regression spline", 
                 "Auto-regressive model with order 1", 
                 "One-sided moving average over 10 years", 
                 "One-sided moving average over 20 years", 
                 "Cubic orthogonal regression",
                 "Best fit ARIMA model",
                 "Last point of 30-year linear trend",
                 "Last point of 20-year linear trend")
    )
    
    curr_pred_temp <- data$Smooth[nrow(data)]
    curr_year <- data$Year[nrow(data)]
    curr_month <- data$Month[nrow(data)]
    
    message <- paste0('Smoothed line = ', method_desc$detail[method_desc$names == method])
    
    message_2 <- paste0("The current temperature anomaly is <b>", round(curr_pred_temp, 2), 
                        "°C in ", month.abb[curr_month], '-', curr_year, "</b> above pre-industrial levels.")
    
    list(data = data, message = message, message_2 = message_2)
  })
  
  # Plot the temperature anomaly and smoothing method on Tab 1
  output$anomalyPlot <- renderPlot({
    values <- calcValues()
    data <- values$data
    message <- values$message
    data_name <- input$dataName
    method <- input$method
    
    # Create the plot
    data %>% pivot_longer(cols = c(Anomaly, Smooth), 
                          names_to = "Type", values_to = "Value") %>% 
      mutate(Type = factor(Type, levels = c("Anomaly", "Smooth"), ordered = TRUE)) %>%
      na.omit() %>% 
      ggplot(aes(x = Year_num, y = Value, colour = Type)) +
      annotate("text", x = mean(range(data$Year)), y = mean(range(data$Anomaly)),
               label = paste0(round(data$Smooth[nrow(data)], 2), "°C"), size = 160/.pt, alpha = 0.05) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line(alpha = 0.7) + 
      scale_colour_manual(values = c("black", "red")) +
      theme_bw() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) + 
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
      labs(title = paste0(data_name," with ", method, " smoothing"), 
           subtitle = message,
           y = "Temperature anomaly (°C)", x = "Year") + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,0,-10,-10))
  })
  
  # Used to display the temperature anomaly message on Tab 1
  output$tempAnomalyMessage <- renderUI({
    values <- calcValues()
    HTML(values$message_2)
  })
  
  ### Tab 2: Indicators of Global Surface Warming
  
  # Load the observation data
  observations <- reactive({
    read_csv("data/indicator_data/Observations.csv", show_col_types = FALSE)
  })
  
  # Load all indicators
  allIndicators <- reactive({
    files <- c("End_of_30-yr_trend_C3S.csv", 
               "End_of_30-yr_trend.csv", 
               "Forecast_RCP4.5_CGWL.csv", 
               "Human_induced_warming.csv", 
               "Last_2010-yr_20avg.csv", 
               "Last-yr_20avg_20GWL.csv", 
               "LOESS.csv", 
               "UKCP18_RCP4.5_CGWL.csv")
    
    indicator_data <- lapply(files, function(file) {
      read_csv(paste0("data/indicator_data/", file), show_col_types = FALSE) %>%
        mutate(Indicator = gsub("\\.csv", "", file))
    })
    
    bind_rows(indicator_data)
  })
  
  # Load the selected indicator data
  indicatorData <- reactive({
    req(input$indicator)
    read_csv(paste0("data/indicator_data/", input$indicator), show_col_types = FALSE)
  })
  
  # Render the plot for Tab 2
  output$indicatorPlot <- renderPlot({
    obs_data <- observations()
    
    if (input$showAll == "all") {
      indicator_data <- allIndicators()
      
      ggplot() +
        geom_line(data = obs_data, aes(x = Year, y = Temperature, color = "Observations"), size = 1) +
        geom_line(data = indicator_data, aes(x = Year, y = Temperature, color = Indicator), size = 1) +
        ylab("Temperature (°C)") +
        xlab("Year") +
        theme_bw() +
        scale_color_manual(values = c("Observations" = "black", 
                                      setNames(rainbow(length(unique(indicator_data$Indicator))), unique(indicator_data$Indicator)))) +
        scale_x_continuous(breaks = seq(min(obs_data$Year), max(obs_data$Year), by = 10)) +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(title = NULL)) +
        ggtitle("Global Surface Warming: Observations and All Indicators")
      
    } else {
      indicator_data <- indicatorData()
      indicator_name <- gsub("\\.csv", "", input$indicator)
      
      ggplot() +
        geom_line(data = obs_data, aes(x = Year, y = Temperature, color = "Observations"), size = 1) +
        geom_line(data = indicator_data, aes(x = Year, y = Temperature, color = indicator_name), size = 1) +
        ylab("Temperature (°C)") +
        xlab("Year") +
        theme_bw() +
        scale_color_manual(values = setNames(c("black", "red"), c("Observations", indicator_name))) +
        scale_x_continuous(breaks = seq(min(obs_data$Year), max(obs_data$Year), by = 10)) +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(title = NULL)) +
        ggtitle(paste("Global Surface Warming: Observations and", indicator_name))
    }
  })
  
  ### Tab 3: Current Global Surface Warming
  # Load the climate uncertainty data for the third tab
  climate_unc <- reactive({
    read_csv("data/indicator_data/climate_unc.csv", show_col_types = FALSE)
  })
  
  # Render the plot for Tab 3
  output$warmingPlot <- renderPlot({
    data <- climate_unc()
    
    ggplot(data, aes(x = Indicator, y = Estimate)) +
      geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7, width = 0.5) +
      geom_rect(aes(xmin = as.numeric(as.factor(Indicator)) - 0.25, 
                    xmax = as.numeric(as.factor(Indicator)) + 0.25, 
                    ymin = Lower, ymax = Upper, fill = Indicator), 
                alpha = 0.4) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "black") +
      geom_hline(yintercept = 1.5, linetype = "solid", color = "red", size = 1) +
      geom_hline(yintercept = 2.0, linetype = "solid", color = "red", size = 1) +
      annotate("text", x = 8.5, y = 1.45, label = "1.5°C", color = "red", hjust = 1, size = 5) +
      annotate("text", x = 8.5, y = 1.95, label = "2.0°C", color = "red", hjust = 1, size = 5) +
      scale_y_continuous(name = "°C", limits = c(0.8, 2.0)) +
      scale_fill_manual(values = c("Last 20-yr avg GWL" = "lightgreen", "Last 10-yr avg" = "darkgreen",
                                   "LOESS" = "lightcoral", "End of 30-yr trend" = "lightblue",
                                   "End of 30-yr trend C3S" = "skyblue", "Human-induced warming" = "lightpink",
                                   "UKCP18 RCP4.5 CGWL" = "yellow", "Forecast RCP4.5 CGWL" = "sandybrown")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle("Current Global Surface Warming")
  })
  
  ### Info Button Modals for Tabs 1, 2, and 3
  
  observeEvent(input$info_button_tab1, {
    info_text_tab1 <- readLines("text/tab1.txt")
    showModal(modalDialog(
      title = "Have we reached 1.5°C yet?",
      HTML(paste(info_text_tab1, collapse = "<br/>")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_button_tab2, {
    info_text_tab2 <- readLines("text/tab2.txt")
    showModal(modalDialog(
      title = "Global Surface Warming Indicators",
      HTML(paste(info_text_tab2, collapse = "<br/>")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_button_tab3, {
    info_text_tab3 <- readLines("text/tab3.txt")
    showModal(modalDialog(
      title = "Current Global Surface Warming",
      HTML(paste(info_text_tab3, collapse = "<br/>")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  ### Tab 4: Climate Information
  
  # Render the RMarkdown file to HTML and display it in the fourth tab
  output$rmdContent <- renderUI({
    rmarkdown::render("climate_info.Rmd", output_format = "html_document", output_file = "climate_info.html")
    includeHTML("climate_info.html")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
