# Get time with millisecond accuracy
options(digits.secs=6)
library(shinyGridster)
require(PerformanceAnalytics)
require(robustbase)
require(quantmod)
source("data.r")
shinyServer(function(input, output, session) {

  all_values <- 100  # Start with an initial value 100
  max_length <- 80   # Keep a maximum of 80 values

  # Collect new values at timed intervals and adds them to all_values
  # Returns all_values (reactively)
  values <- reactive({
    # Set the delay to re-run this reactive expression
    invalidateLater(input$delay, session)

    # Generate a new number
    new_value <- isolate(last(all_values) * 
      (1 +  runif(1, min = -input$volatility, max = input$volatility)))

    # Append to all_values
    all_values <<- c(all_values, new_value)

    # Trim all_values to max_length (dropping values from beginning)
    all_values <<- last(all_values, n = max_length)

    all_values
  })


  # Generate histogram
  output$plotout <- renderPlot({
    portret<-portret*100
    hist(portret, col = "#cccccc",
      main = paste("Value-at-Risk"),
      xlab = paste("VaR =", round(100), 1)
    )
    var<- -qnorm(input$rate)*input$volatility *100
    abline(v = var, col = "red", lwd = 2, lt = 2)
  })

  # Set the value for the gauge
  # When this reactive expression is assigned to an output object, it is
  # automatically wrapped into an observer (i.e., a reactive endpoint)
  output$live_gauge <- reactive({
    running_mean <- mean(last(values(), n = 10))
    round(running_mean, 1)
  })

  # Output the status text ("OK" vs "Past limit")
  # When this reactive expression is assigned to an output object, it is
  # automatically wrapped into an observer (i.e., a reactive endpoint)
  output$status <- reactive({
    running_mean <- mean(last(values(), n = 10))
    if (running_mean < qnorm(input$rate)*input$volatility *100)
      list(text="Past limit", gridClass="alert")
    else if (running_mean < qnorm(input$rate)*input$volatility*2*100)
      list(text="Warn", subtext = "VaR approaching threshold",
           gridClass="warning")
    else
      list(text="OK", subtext="VaR within Risk Limit")
  })


  # Update the latest value on the graph
  # Send custom message (as JSON) to a handler on the client
  observe({
    session$sendCustomMessage(
      type = "updateHighchart", 
      message = list(
        # Name of chart to update
        name = "live_highchart",
        # Send UTC timestamp as a string so we can specify arbitrary precision
        # (large numbers get converted to scientific notation and lose precision)
        x = sprintf("%15.3f", as.numeric(Sys.time()) * 1000),
        # Most recent value
        y0 = last(values()),
      
        # Smoothed value (average of last 10)
        
        y1 =qnorm(input$rate)*input$volatility *100
      )
    )
  })

  # Generate histogram
  output$mvarout <- renderPlot({
    p=input$rate
    barplot(VaR(dfxdata,p=p,weights=pwt, clean="none", portfolio_method="component")$pct_contrib_MVaR,horiz=T,las=2,main="Marginal Risk")
    
  })
  
})
pwt<-c(0.4,0.2,0.3,0.1)


fxdata<-testdata()

dfxdata<-diff(log(fxdata))
dfxdata<-na.locf(dfxdata,na.rm=T)
portret<-t(t(pwt) %*% t(dfxdata))


# Return the last n elements in vector x
last <- function(x, n = 1) {
  start <- length(x) - n + 1
  if (start < 1)
    start <- 1

  x[start:length(x)]
}

