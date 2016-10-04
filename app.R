suppressPackageStartupMessages({
  library(readr)
  library(knitr)
  library(dplyr)
  library(shiny)
  library(googleVis)
})

data1 <- read.csv('data_normalized_092616.csv',stringsAsFactors = F)
data2 <- read.csv('gVis_name_conversion.csv',stringsAsFactors = F)
data3 <- data1 %>% left_join(data2,by=c("country"="MASTER")) %>% filter(gVisname!="") %>% distinct(gVisname)

vars <- names(data3)[4:24]
names(vars) <- c("Trade Balance (Q)","Trade Balance (V)", "Energy Adequacy","GDP","Fatty Acids","Calories","Protein",
                 "Vitamin A","Thiamin","Niacin","Riboflavin","B6","Iron","Calcium","Zinc","Native","Fishmeal","Habitat",
                 "Species Farmed","Production Ratio","All Vitamins")

data4 <- data3 %>% select(gVisname,one_of(vars))

shinyApp(
  
  ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel(
      textOutput(outputId = "title")
    ),
    
    # Choose variable
    selectInput("variable",label="Choose Variable",choices=vars),
    
    # Choose Region
    selectInput("region",label="Region",choices=c("World"="world","Africa"="002","Europe"="150","North America"="021",
                                                     "Caribbean"="029","Central America"="013","South America"="005",
                                                     "Asia"="142","Southeast Asia"="035","Oceania"="009")),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("gvis"),
      width=1500
    )
  )),
  
  
  server <- shinyServer(function(input, output) {
    
    variable <- reactive({input$variable})
    
    output$title <- renderText(names(vars)[match(variable(),vars)])
    
    output$gvis <- renderGvis({
      
      dat <- data4[,c("gVisname",variable())]
      
      # Plot countries
      Sys.sleep(0.3)
      
      gvisGeoChart(dat, "gVisname", variable(), hovervar="gVisname",
                   options=list(colorAxis="{colors:['#F7FCFD', '#6E016B']}",
                                region=input$region,width=1500,height=500))
    })

  })
)