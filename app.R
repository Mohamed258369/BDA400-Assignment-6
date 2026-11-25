# BDA400 Assignment 6
# Technical Analysis Dashboard

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

ui <- fluidPage(
  titlePanel("Stock Market Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol", "AAPL"),
      dateRangeInput("dates", "Date Range",
                     start = "2023-01-01",
                     end = "2023-07-01"),
      selectInput("timeframe", "Time Frame",
                  c("Daily", "Weekly", "Monthly")),
      selectInput("chart", "Chart Type",
                  c("Line", "Candlestick", "Area")),
      checkboxGroupInput("indicators",
                         "Technical Indicators",
                         c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages"),
      numericInput("short", "Short Moving Average", 20, min = 2),
      numericInput("long", "Long Moving Average", 50, min = 3),
      checkboxInput("signals", "Show Trading Signals", TRUE)
    ),
    mainPanel(
      plotOutput("stock_chart", height = "650px"),
      tableOutput("signal_table")
    )
  )
)

server <- function(input, output) {
  
  # Step 1: Collect stock data
  stock <- reactive({
    req(input$symbol, input$dates)
    
    x <- tryCatch(
      getSymbols(
        toupper(input$symbol),
        src = "yahoo",
        from = input$dates[1] - 3650,
        to = input$dates[2] + 1,
        auto.assign = FALSE
      ),
      error = function(e) NULL
    )
    
    validate(need(!is.null(x),
                  "Unable to download stock data."))
    
    x <- na.omit(x)
    
    if (input$timeframe == "Weekly")
      x <- to.weekly(x)
    
    if (input$timeframe == "Monthly")
      x <- to.monthly(x)
    
    x
  })
  
  # Step 2: Calculate technical indicators
  results <- reactive({
    x <- stock()
    
    validate(
      need(input$short < input$long,
           "Short MA must be smaller than Long MA."),
      need(nrow(x) > max(input$long, 35),
           "Not enough historical data.")
    )
    
    close <- as.numeric(Cl(x))
    
    d <- data.frame(
      Date = as.Date(index(x)),
      Open = as.numeric(Op(x)),
      High = as.numeric(Hi(x)),
      Low = as.numeric(Lo(x)),
      Close = close
    )
    
    d$ShortMA <- SMA(close, input$short)
    d$LongMA <- SMA(close, input$long)
    d$RSI <- RSI(close, 14)
    
    m <- MACD(close)
    d$MACD <- m[, 1]
    d$MACDSignal <- m[, 2]
    
    # Step 3: Trading crossover rules
    difference <- d$ShortMA - d$LongMA
    previous <- c(NA, head(difference, -1))
    
    d$Signal <- "Hold"
    
    d$Signal[which(difference > 0 &
                     previous <= 0)] <- "Buy"
    
    d$Signal[which(difference < 0 &
                     previous >= 0)] <- "Sell"
    
    d <- subset(d, Date >= input$dates[1] &
                  Date <= input$dates[2])
    
    validate(need(nrow(d) > 0,
                  "No data in selected date range."))
    
    d
  })
  
  # Step 4: Visualizations
  output$stock_chart <- renderPlot({
    d <- results()
    selected <- input$indicators
    
    p <- ggplot(d, aes(Date, Close))
    
    if (input$chart == "Line")
      p <- p + geom_line(color = "steelblue")
    
    if (input$chart == "Area")
      p <- p + geom_area(fill = "skyblue", alpha = 0.6)
    
    if (input$chart == "Candlestick") {
      d$Direction <- ifelse(d$Close >= d$Open,
                            "Up", "Down")
      
      width <- switch(input$timeframe,
                      Daily = 0.7,
                      Weekly = 4,
                      Monthly = 20)
      
      p <- ggplot(d, aes(Date, Close)) +
        geom_linerange(aes(ymin = Low, ymax = High)) +
        geom_tile(aes(y = (Open + Close) / 2,
                      height = pmax(abs(Close - Open), 0.01),
                      fill = Direction),
                  width = width) +
        scale_fill_manual(values = c(Up = "green3",
                                     Down = "red3"))
    }
    
    if ("Moving Averages" %in% selected) {
      p <- p +
        geom_line(aes(y = ShortMA), color = "orange") +
        geom_line(aes(y = LongMA), color = "purple")
    }
    
    if (input$signals) {
      p <- p +
        geom_text(
          data = subset(d, Signal != "Hold"),
          aes(label = Signal, color = Signal),
          vjust = -1
        ) +
        scale_color_manual(values = c(Buy = "darkgreen",
                                      Sell = "red"))
    }
    
    p <- p +
      labs(title = toupper(input$symbol),
           y = "Stock Price", x = "Date") +
      theme_minimal()
    
    # Add RSI and MACD indicator panels
    plots <- list(p)
    
    if ("RSI" %in% selected) {
      plots <- c(plots, list(
        ggplot(d, aes(Date, RSI)) +
          geom_line(color = "purple") +
          geom_hline(yintercept = c(30, 70),
                     linetype = "dashed") +
          theme_minimal()
      ))
    }
    
    if ("MACD" %in% selected) {
      plots <- c(plots, list(
        ggplot(d, aes(Date, MACD)) +
          geom_line(color = "blue") +
          geom_line(aes(y = MACDSignal), color = "red") +
          theme_minimal()
      ))
    }
    
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(
      layout = grid::grid.layout(length(plots), 1)
    ))
    
    for (i in seq_along(plots)) {
      print(plots[[i]], vp = grid::viewport(
        layout.pos.row = i,
        layout.pos.col = 1
      ))
    }
  })
  
  output$signal_table <- renderTable({
    d <- results()
    d$Date <- format(d$Date, "%Y-%m-%d")
    tail(d[, c("Date", "Close", "Signal")], 10)
  })
}

shinyApp(ui, server)