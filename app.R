suppressPackageStartupMessages({
  library(readr)
  library(knitr)
  library(dplyr)
  library(shiny)
  library(googleVis)
  library(ggplot2)
  library(DT)
})

data1 <- read.csv('final_country_scores.csv',stringsAsFactors = F)
data2 <- read.csv('gVis_name_conversion.csv',stringsAsFactors = F)
data3 <- data1 %>% left_join(data2,by=c("country_name"="MASTER")) %>% 
  filter(gVisname!="") %>% distinct(gVisname,.keep_all=T)

vars <- names(data3)[c(38:42,50:61,64:66,68)]
names(vars) <- c("Production Ratio","Trade Balance (Q)","Trade Balance (V)","Willingness to Pay","GDP per capita",
                 "Energy Adequacy","Protein","Fatty Acids","Vitamin A","Zinc","Iron","Calories from Seafood",
                 "Protein from Seafood","Fatty Acids from Seafood","Vitamin A from Seafood",
                 "Zinc from Seafood","Iron from Seafood","Reliance Score","Economic Score","Nutrition Score",
                 "Total Mariculture Opportunity Score")

data4 <- data3 %>% select(gVisname,one_of(vars))

shinyApp(
  
  ui <- shinyUI(fixedPage(
    verticalLayout(
      # Application title
      titlePanel(
        textOutput(outputId = "title")
      ),
      fixedRow(
        column(width = 3,  
               wellPanel(
                 # Choose variable
               selectInput("variable",label="Choose Variable",choices=vars),
               
               # Choose Region
               selectInput("region",label="Region",choices=c("World"="world","Africa"="002","Europe"="150","North America"="021",
                                                             "Caribbean"="029","Central America"="013","South America"="005",
                                                             "Asia"="142","Southeast Asia"="035","South Asia"="034","Oceania"="009","Melanesia"="054",
                                                             "Micronesia"="057","Polynesia"="061"))
        )),
        # Show a plot of the generated distribution
        column(width = 7, 'YEAR 2011', htmlOutput("gvis")) 
      ),
        
        fixedRow(
          column(width=4,
               dataTableOutput("top5")
               ),
        column(width=4,
                # histogram of data in question
                 plotOutput("varhist")
               
              )
      )
      
    )
  )),
  
  
  server <- shinyServer(function(input, output) {
    
    variable <- reactive({input$variable})
    df <- reactive({
      df <- as.data.frame(data4[,c("gVisname",variable())])
      names(df) <- c("Country","Score")
      df$Score <- round(df$Score,2)
      df
    })
    
    output$title <- renderText(c(names(vars)[match(variable(),vars)]," Normalized"))
    
    output$gvis <- renderGvis({
      
      dat <- data4[,c("gVisname",variable())]
      
      # Plot countries
      Sys.sleep(0.3)
      
      gvisGeoChart(dat, "gVisname", variable(), hovervar="gVisname",
                   options=list(colorAxis="{minValue: 0, maxValue:1, colors: ['#E2E2E2', '#4A6FE3', '#D33F6A']}",
                                region=input$region,width=600,height=300,
                                datalessRegionColor="#B8B8B8",defaultColor="#B8B8B8"))
    })
    
    output$top5 <- renderDataTable(df(),options=list(pageLength=5))
    
    output$varhist <- renderPlot({
      hist(data4[,variable()],main="Distribution of Normalized Variable",xlim=c(0,1),xlab="value",ylab="frequency",ylim=c(0,70))
    })

  })
)