
source('dashwidgets.r', local=TRUE)

shinyUI(bootstrapPage(

  tabsetPanel(
    tabPanel("VaR",
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),

    # For JustGage, http://justgage.com/
    tags$script(src = 'js/raphael.2.1.0.min.js'),
    tags$script(src = 'js/justgage.1.0.1.min.js'),

    # For Highcharts, http://www.highcharts.com/
    tags$script(src = 'js/highcharts.js'),

    # For the Shiny output binding for status text and JustGage
    tags$script(src = 'shiny_status_binding.js'),
    tags$script(src = 'justgage_binding.js')
   
    
    
  ),

  h1("FX Portfolio Hedging"),

  gridster(width = 250, height = 250,
    gridsterItem(col = 1, row = 1, sizex = 1, sizey = 1,

      sliderInput("rate", "Value-at-Risk:",
        min = 0.9, max = .99, value = .95, step = .01),

      sliderInput("volatility", "Risk(Volatility):",
        min = 0, max = .3, value = .1, step = .01),

      sliderInput("delay", "Delay (ms):",
        min = 250, max = 5000, value = 3000, step = 250),

      tags$p(
        tags$br(),
        tags$a(href = "http://quantie.com", "Risk")
      )
    ),
    gridsterItem(col = 2, row = 1, sizex = 2, sizey = 1,
      tags$div(id = "live_highchart",
        style="min-width: 200px; height: 230px; margin: 0 auto"
      )
    ),
    gridsterItem(col = 1, row = 2, sizex = 1, sizey = 1,
      #justgageOutput("live_gauge", width=250, height=200)
                 plotOutput("mvarout", height = 250)
    ),
    gridsterItem(col = 2, row = 2, sizex = 1, sizey = 1,
      tags$div(class = 'grid_title', 'Status'),
      statusOutput('status')
    ),
    gridsterItem(col = 3, row = 2, sizex = 1, sizey = 1,
      plotOutput("plotout", height = 250)
    )
  ),

  # Can read Javascript code from a separate file
  # This code initializes the dynamic chart
  tags$script(src = "initchart.js")
#   tags$script(src = 'jquery.min.js'),
 # tags$script(src = 'jquery.hipchat.js'),
  #tags$script(src = "initchat.js")
), #end tabpanel
    tabPanel("Hedging", HTML('<iframe src="http://glimmer.rstudio.com/kriskumar/test/" style="border: 1px solid #CCC; width: 1000px; height: 1000px"> </ iframe>')
                  )
  )  #end tabsetpanel
  
))
