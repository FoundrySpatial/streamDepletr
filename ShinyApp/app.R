## app.R
#' Script for shiny app implementing streamflow depletion solutions in user-friendly manner.

# packages
require(shiny)
require(ggplot2)
require(Rmpfr)

# source functions
source(file.path("..", "R", "hunt.R"))
source(file.path("..", "R", "glover.R"))

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
      
      # Input: Slider for transmissivity ----
      sliderInput(inputId = "Tr",
                  label = "Transmissivity [m/d]:",
                  min = 5,
                  max = 500,
                  value = 100),
      
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
      plotOutput(outputId = "depletion")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Reactive expression to create data frame and calculate Qf ----
  df <- reactive({
    df <- data.frame(times=seq(input$tStart,input$tEnd), 
                     Qf = glover(t=seq(input$tStart,input$tEnd), d=input$distToStream, S=input$S, input$Tr))
  })
  
  # Make a plot ----
  
  output$depletion <- renderPlot({
    ggplot(df(), aes(times, Qf)) + 
      geom_line() + 
      scale_x_continuous(name="Time",
                         expand=c(0,0)) +
      scale_y_continuous(name="Capture Fraction", 
                         limits=c(0,1), 
                         breaks=seq(0,1,0.25),
                         expand=c(0,0),
                         labels = scales::percent) + 
      theme_bw()
  })
  
}

shinyApp(ui = ui, server = server)