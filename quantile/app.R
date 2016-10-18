suppressPackageStartupMessages({
  library(readr)
  library(knitr)
  library(dplyr)
  library(shiny)
  library(googleVis)
  library(ggplot2)
  library(DT)
})

data1 <- read.csv('data/data_quantized_101716.csv',stringsAsFactors = F)
data2 <- read.csv('data/gVis_name_conversion.csv',stringsAsFactors = F)
data3 <- data1 %>% left_join(data2,by=c("country"="MASTER")) %>% 
  filter(gVisname!="") %>% distinct(gVisname,.keep_all=T) %>% select(-X)

vars <- names(data3)[3:36]
names(vars) <- c("Trade Balance (Q)","Trade Balance (V)", "Energy Adequacy","GDP","Calories","Protein","Fat","Vitamin A",
                 "Calcium","Iron","Zinc","Thiamin","Riboflavin","Niacin","B6","Magnesium","Fatty Acids",
                 "Fatty Acids from Seafood","Calories from Seafood","Protein from Seafood",
                 "Vitamin A from Seafood","Thiamin from Seafood","Niacin from Seafood",
                 "Riboflavin from Seafood","B6 from Seafood","Iron from Seafood","Calcium from Seafood",
                 "Zinc from Seafood","Native","Fishmeal","Habitat",
                 "Species Farmed","Production Ratio","Aquaculture as Percent of GDP")
quantvars <- names(data3)[c(37:77)]
names(quantvars) <- c("Trade Balance (Q)","Trade Balance (V)","Production Ratio","Species Farmed",
                      "Energy Adequacy","GDP","Calories",
                      "Protein","Fat","Vitamin A",
                      "Calcium","Iron","Zinc","Thiamin","Riboflavin","Niacin","B6","Magnesium","Fatty Acids",
                      "Fatty Acids from Seafood","Calories from Seafood","Protein from Seafood",
                      "Vitamin A from Seafood","Thiamin from Seafood","Niacin from Seafood",
                      "Riboflavin from Seafood","B6 from Seafood","Iron from Seafood","Calcium from Seafood",
                      "Zinc from Seafood","All Vitamins","Native","Fishmeal","Habitat",
                      "Aquaculture as Percent of GDP",
                      "All Trade","All FS","All Reliance","All Nutri","All Ecol","Total Score")

data4 <- data3 %>% select(gVisname,one_of(vars),one_of(quantvars))

shinyApp(
  
  ui <- shinyUI(fluidPage(
    verticalLayout(
      # Application title
      titlePanel(
        textOutput(outputId = "title")
      ),
      fluidRow(
        column(width=4,
               wellPanel(
                 # Choose variable
                 selectInput("variable",label="Choose Variable",choices=quantvars),
                 
                 # Choose Region
                 selectInput("region",label="Region",choices=c("World"="world","Africa"="002","Europe"="150","North America"="021",
                                                               "Caribbean"="029","Central America"="013","South America"="005",
                                                               "Asia"="142","Southeast Asia"="035","South Asia"="034","Oceania"="009","Melanesia"="054",
                                                               "Micronesia"="057","Polynesia"="061"))
               )
        ),
        column(width=4,
               dataTableOutput("top")
        ),
        column(width=4,
               # histogram of data in question
               plotOutput("varhist")
        )
      ),
      
      # Show a plot of the generated distribution
      htmlOutput("gvis")
      
    )
  )),
  
  
  server <- shinyServer(function(input, output) {
    
    variable <- reactive({input$variable})
    
    rawvar <- reactive({gsub("_q","",input$variable)})
    
    df <- reactive({
      df <- as.data.frame(data4[,c("gVisname",rawvar())])
      names(df) <- c("Country","Raw Value")
      df
    })
    
    
    output$title <- renderText(names(quantvars)[match(variable(),quantvars)])
    
    output$gvis <- renderGvis({
      
      dat <- data4[,c("gVisname",variable())]
      
      # Plot countries
      Sys.sleep(0.3)
      
      gvisGeoChart(dat, "gVisname", variable(), hovervar="gVisname",
                   options=list(colorAxis="{minValue:1, maxValue:5,colors: ['#CD8162','#CDCD00','#8FBC8F']}",
                                region=input$region,width=1000,height=700,
                                datalessRegionColor="#B8B8B8",defaultColor="#B8B8B8"))
    })
    
    output$top <- renderDataTable(df(),options=list(pageLength=5))
    
    output$varhist <- renderPlot({
      hist(data4[,rawvar()],main="Distribution of Raw Variable",xlab="value",ylab="frequency")
    })
    
  })
)