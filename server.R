library(shiny)
library(quantmod)
library(xtable)
library(PerformanceAnalytics)
library(rugarch)

#Function to capitalize the first letter of a string
proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # acquiring data
  dataInput <- reactive({
    if (input$get == 0)
      return(NULL)
    
    return(isolate({
      getSymbols(input$symb,src="yahoo", auto.assign = FALSE)
    }))
  })
  
  datesInput <- reactive({
    if (input$get == 0)
      return(NULL)
    
    return(isolate({
      paste0(input$dates[1], "::",  input$dates[2])
    }))
  })
  
  
  # tab based controls
  output$newBox <- renderUI({
    switch(input$tab,
           "Prices" = chartControls,
           "Returns" = returnControls,
           "VaR" = varControls
    )
  })
  
  # Charts tab
  chartControls <- div(
    wellPanel(
      selectInput("chart_type",
                  label = "Chart type",
                  choices = c("Candlestick" = "candlesticks", 
                              "Matchstick" = "matchsticks",
                              "Bar" = "bars",
                              "Line" = "line"),
                  selected = "Line"
      ),
      checkboxInput(inputId = "log_y", label = "log y axis", 
                    value = FALSE)
    ),
    
    wellPanel(
      p(strong("Technical Analysis")),
      checkboxInput("ta_vol", label = "Volume", value = FALSE),
      checkboxInput("ta_sma", label = "Simple Moving Average", 
                    value = FALSE),
      checkboxInput("ta_ema", label = "Exponential Moving Average", 
                    value = FALSE),
      checkboxInput("ta_wma", label = "Weighted Moving Average", 
                    value = FALSE),
      checkboxInput("ta_bb", label = "Bolinger Bands", 
                    value = FALSE),
      checkboxInput("ta_momentum", label = "Momentum", 
                    value = FALSE),
      
      br(),
      
      actionButton("chart_act", "Add Technical Analysis")
    )
  )
  
  TAInput <- reactive({
    if (input$chart_act == 0)
      return("NULL")
    
    tas <- isolate({c(input$ta_vol, input$ta_sma, input$ta_ema, 
                      input$ta_wma,input$ta_bb, input$ta_momentum)})
    funcs <- c(addVo(), addSMA(), addEMA(), addWMA(), 
               addBBands(), addMomentum())
    
    if (any(tas)) funcs[tas]
    else "NULL"
  })
  
  output$chart <- renderPlot({
    chartSeries(dataInput(),
                name = input$symb,
                type = input$chart_type,
                subset = datesInput(),
                log.scale = input$log_y,
                theme = "white",
                TA = TAInput())
  })
  
  
  # Returns tab
  returnControls <- div(
    wellPanel(
      selectInput("period",
                  label = "Periodicity",
                  choices = c("Daily" = "daily", 
                              "Weekly" = "weekly",
                              "Monthly" = "monthly",
                              "Quarterly" = "quarterly",
                              "Yearly" = "yearly"),
                  selected = "Daily"
      ),
      selectInput("type",
                  label = "Arithmetic/Log Return",
                  choices = c("Arithmetic" = "arithmetic", 
                              "Log" = "log"),
                  selected = "Log"
      )
    ))
  returns <- reactive({ 
    if (input$get == 0)
      return(NULL)
    
    periodReturn(dataInput(), period = input$period, type = input$type)
  })

  
  output$chart2 <- renderPlot({
    chartSeries(returns(),
                name = paste(paste(paste(input$symb,"'s",sep=""),proper(input$period),sep=" "),"Returns", sep=" "),
                type = "line",
                subset = datesInput(),
                theme = "white",
                TA = NULL)
  })
  output$summary <- renderTable({
    table.Stats(returns())
  },caption = "Summary Statistics",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  # VaR tab
  varControls <- div(
    wellPanel(
      selectInput("var_act",
                  label = "Confidence Level",
                  choices = c("90%" = 0.1, 
                              "95%" = 0.05,
                              "99%" = 0.01)
      )
    )
  )
#   confInput <- reactive({
#     if (input$var_act == 0)
#       return("NULL")
#     
#     tas <- isolate({c(input$ta_90, input$ta_95, input$ta_99)})
#     funcs <- c(0.1,0.05,0.01)
#     
#     if (any(tas)) funcs[tas]
#     else "NULL"
#   })
  
  var <- reactive({
    spec <- ugarchspec(mean.model = list(armaOrder = c(1,0)), 
                       variance.model = list(garchOrder = c(1,1), 
                                             model = "gjrGARCH", 
                                             variance.targeting=FALSE), 
                       distribution.model = "sstd")
    fit <- ugarchfit(spec, returns())
    quantile(fit, input$var_act)
  })
  output$chart3 <- renderPlot({
    chartSeries(var(),
                name = paste(paste(paste(input$symb,"'s",sep=""), proper(input$period),sep=" "),"VaR", sep=" "),
                type = "line",
                subset = datesInput(),
                theme = "white",
                TA = NULL)
  })
  output$text3 <- renderText({paste0(input$symb, " 3: ", input$tab)})
  
})