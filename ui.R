library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  headerPanel("Stock Explorer"),
  
  sidebarPanel(
    
    helpText("Select a stock by typing in the box below. All data are sourced from Yahoo Finance."),
    
    textInput("symb", "Symbol", placeholder = 'Type a ticker symbol, e.g. GOOG'),
#     selectizeInput("symb", label = "Symbol", choices = NULL, options = list(
#       placeholder = 'Type a ticker symbol, e.g. GOOG', maxOptions = 5)
#     ),
    dateRangeInput("dates", 
                   "Dates",
                   start = "2013-01-01", end = Sys.Date()),
    
    actionButton("get", "Get Stock"),
    
    br(),
    br(),
    
    uiOutput("newBox")
    
    ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Prices", plotOutput("chart")), 
      tabPanel("Returns", plotOutput("chart2"), tableOutput("summary")), 
      tabPanel("VaR", plotOutput("chart3")),
      tabPanel("How To",
               HTML("<p> This Shiny application can be used to collect historical stock prices and perform relevant technical analyses. The application depends on the following packages to function properly: quantmod, xtable, PerformaceAnalytics, and rugarch. Below is a description of how this application works, organized by tabs.</p>
                
                    <b>Prices</b>
                    <p>This tab provides basic tools to visualize the price movement of a particular stock over time.</p>
                    <li>Symbol:</li> Type in the ticker symbol of the stock you want to examine e.g. MSFT(Microsoft). 
                    <li>Dates:</li> Select the starting and ending dates of the period you want to examine. Ending date is set as system date by default.
                    <p>Hit 'Get Stock' to apply your selection.</p>
                    <li>Chart type:</li> Different chart types (line, candlestick, matchstick, and bar) are available from the drop-down menu. 
                    <li>Log y axis:</li> Select this option if you want the y-axis to be log scaled. When the increment on the y-axis is small it may be helpful to check this box.
                    <li>Technical analysis:</li> Add or overlay additional plots of technical indicators that are useful for making trading decisions. 
                    <p>When you are satisfied with your selection, hit 'Add Technical Analysis' to see the result.</p>
                    <b>Returns</b>
                    <p>This tab allows you to convert prices to returns of your chosen method and periodicity. A table of summary statistics is also displayed.</p>
                    <li>Periodicity:</li> Determine the time period of your returns such as daily/weekly/etc.
                    <li>Arithmetic/Log return:</li> Whether the arithmetic (discrete) or log (continuous) transformation is used.
                    <p></p>
                    <b>VaR</b>
                    <p>This tab computes the Value-at-Risk (VaR) at a given confidence level using parametric method by fitting an ARMA(1,0)-GJR-GARCH(1,1) model with skewed Student-t innovations to the returns that you have calculated.</p>
                    <li>Confidence level:</li> Select a confidence level. Available options are 90%, 95%, and 99%.
                    ")),

      id = "tab"
    )
  )
))