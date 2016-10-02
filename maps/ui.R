# Define UI 
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mariculture 2011"),
  
  # Show a plot of the generated distribution
  mainPanel(
    htmlOutput("gvis")
  )
))