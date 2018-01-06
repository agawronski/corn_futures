library(forecast)
library(Quandl)
library(shiny)


data <- Quandl("CHRIS/CME_C1")
to_forecast <- names(data)[-1]

title <- "Arima Forecast: Corn Futures, Continuous Contract #1 (C1) (Front Month)"

ui <- fluidPage(
  titlePanel(title, windowTitle = title),
  a("Click here for original data from Quandl", 
  href = "https://www.quandl.com/data/CHRIS/CME_C1-Corn-Futures-Continuous-Contract-1-C1-Front-Month", target="_blank"),
  tags$div(tags$br()),
  a("Click Here for information about the 'forecast' R package used to create this", 
    href = "http://pkg.robjhyndman.com/forecast/", target="_blank"),
  tags$div(tags$br()),
  a("Click here to view the R code on GitHub", 
    href = "https://github.com/agawronski/", target="_blank"),
  tags$div(tags$br()),
  dateRangeInput("daterange", "Date range:",
                 start = min(data$Date),
                 end   = max(data$Date)),
  selectInput(inputId = "series_to_forecast", "Choose what to forecast", as.list(to_forecast)),
  sliderInput(inputId = "num_days",
              label = "Choose the number of days to forecast",
              value = 25, min = 1, max = 365),
  
  plotOutput("time_series"),
  verbatimTextOutput("model_summary"),
  plotOutput("forecast"),
  verbatimTextOutput("forecast_details")
)


server <- function(input, output) {
  dataSub <- reactive({
    dataS <- data[data$Date >= input$daterange[1] &
                  data$Date <= input$daterange[2],]
    dataS <- dataS[,c("Date", input$series_to_forecast)]
  })
  
  output$time_series <- renderPlot({
    plot(dataSub()$Date, 
         dataSub()[,2], 
         type = "l", 
         col = "darkBlue",
         main = paste("Corn Futures -", input$series_to_forecast),
         xlab = "Date", 
         ylab = "Corn Futures")
  })
  
  model_fit <- reactive({
    auto.arima(dataSub()[,2])
  })

  output$model_summary <- renderPrint({
    print(summary(model_fit()))
  })

  yhat <- reactive({
    forecast(model_fit(), h = input$num_days)
  })

  output$forecast <- renderPlot({
    plot(yhat())
  })

  output$forecast_details <- renderPrint({
    y_out <- data.frame(yhat())
    y_out <- apply(y_out, 2, function(x) round(x, 1))
    cols <- c("Point.Forecast", "Lo.95", "Lo.80", "Hi.80", "Hi.95")
    y_out <- y_out[,cols]
    y_out
  })
}

shinyApp(ui = ui, server = server)











