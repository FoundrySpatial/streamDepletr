## app.R
#' Script for shiny app implementing streamflow depletion solutions in user-friendly manner.

# packages
require(shiny)
require(ggplot2)
require(Rmpfr)
require(magrittr)

# source functions
source(file.path("..", "R", "hunt.R"))
source(file.path("..", "R", "hantush.R"))
source(file.path("..", "R", "glover.R"))
source(file.path("..", "R", "streambed_conductance.R"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Analytical Streamflow Depletion Models"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Checkboxes for different analytical solutions
      checkboxGroupInput("checkMethod", label = "Method", 
                         choices = list("Glover & Balmer (1954)" = "glover", 
                                        "Hantush (1965)" = "hantush", 
                                        "Hunt (1999)" = "hunt"),
                         selected = "glover"),
      
      # Input: Select length units
      radioButtons("lengthUnits", label = "Units",
                   choices = list("meters" = "m", "feet" = "ft"), 
                   selected = "m", inline=T),
      
      
      # Input: Slider for distance from well to stream ----
      numericInput(inputId = "distToStream",
                   label = "Distance to Stream [m]:",
                   value = 100),
      
      # Input: Slider for Kh ----
      sliderInput(inputId = "Kh",
                  label = "Horizontal Hydraulic Conductivity (Kh) [m/d]:",
                  min = 0.1,
                  max = 10,
                  value = 1),
      
      # Input: Slider for Kh ----
      sliderInput(inputId = "b",
                  label = "Aquifer Saturated Thickness [m]:",
                  min = 1,
                  max = 100,
                  value = 10),
      
      # Input: Slider for storage coefficienc ----
      sliderInput(inputId = "S",
                  label = "Storage Coefficient",
                  min = 0.01,
                  max = 0.3,
                  value = 0.1),
      
      # Input: Numeric box for start of plot
      numericInput("tStart", 
                   h3("Start Time"), 
                   value = 0),
      
      # Input: Numeric box for end of plot
      numericInput("tEnd", 
                   h3("End Time"), 
                   value = 100)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot of Qf vs time ----
      plotOutput(outputId = "depletion",
                 click = "plot_click"),
      verbatimTextOutput("info")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Reactive expression to create data frame and calculate Qf ----
  df <- reactive({
    rbind(
      data.frame(times=seq(input$tStart,input$tEnd), 
                 Qf = glover(t = seq(input$tStart,input$tEnd), 
                             d = input$distToStream, 
                             S = input$S, 
                             Tr= input$Kh*input$b),
                 method = "glover"), 
      data.frame(times=seq(input$tStart,input$tEnd), 
                 Qf = hunt(t = seq(input$tStart,input$tEnd), 
                           d = input$distToStream, 
                           S = input$S, 
                           Tr= input$Kh*input$b, 
                           lmda = streambed_conductance(w=10, Kriv=1, briv=1)),
                 method = "hunt"), 
      data.frame(times=seq(input$tStart,input$tEnd), 
                 Qf = hantush(t = seq(input$tStart,input$tEnd), 
                              d = input$distToStream, 
                              S = input$S,
                              Kh= input$Kh, 
                              b = input$b, 
                              Kriv = 1, 
                              briv = 1),
                 method = "hantush")
    )
    
  })
  
  # Reactive expression to figure out where click was ----
  df.click <- reactive({
    if (is.null(input$plot_click)) return()
    nearPoints(df(), input$plot_click, threshold=20, maxpoints=1)
  })
  
  # Make a plot ----
  
  output$depletion <- renderPlot({
    ggplot(subset(df(), method %in% input$checkMethod), aes(x=times, y=Qf, color=method)) +
      geom_line() + 
      annotate("segment", x=-Inf, xend=df.click()$times, y=df.click()$Qf, yend=df.click()$Qf, color="red") +
      annotate("segment", x=df.click()$times, xend=df.click()$times, y=-Inf, yend=df.click()$Qf, color="red") +
      geom_point(data=df.click(), aes(times, Qf), color="red", size=2) +
      scale_x_continuous(name="Time",
                         expand=c(0,0)) +
      scale_y_continuous(name="Capture Fraction", 
                         limits=c(0,1), 
                         breaks=seq(0,1,0.25),
                         expand=c(0,0),
                         labels = scales::percent) + 
      scale_color_manual(name="Method", values=c("glover"="black", "hantush"="green", "hunt"="red"),
                         labels=c("glover"="Glover & Balmer", "hantush"="Hantush", "hunt"="Hunt"), drop=F) +
      theme_bw() +
      theme(legend.position=c(1,0),
            legend.justification=c(1,0))
  })
  
  output$info <- renderPrint({
    # With ggplot2, no need to tell it what the x and y variables are.
    # threshold: set max distance, in pixels
    # maxpoints: maximum number of rows to return
    # addDist: add column with distance, in pixels
    nearPoints(df(), input$plot_click, threshold = 10, maxpoints = 1,
               addDist = TRUE)
  })
  
}

shinyApp(ui = ui, server = server)