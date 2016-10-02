      library(shiny)
      library(dplyr)
      library(tidyr)
      library(googleVis)
      library(readr)
 
      ui <- shinyUI(fluidPage(
        
        # Application title
        titlePanel("Mariculture 2011"),
        
        # Show a plot of the generated distribution
        mainPanel(
          htmlOutput("gvis")
        )
      ))
      
      server <- shinyServer(function(input, output) {
        
        output$gvis=renderGvis({
          ##### MARICULTURE VARIABLES STARTS HERE
          ## Prepare data to be displayed
          #reading data
          
          x <- read_csv('data/data_normalized_092616.csv')
          
          #This is the file with the matching column "FAO_Country"
          
          conversion <- read_csv('data/CountryConversion.csv')
          
          gdp <- x %>% 
            filter(!is.na(gdp)) %>%
            rename(FAO_Country=country) %>%        #rename match column FAO_Country
            left_join(                             #joining harvest_countries with conversion data 
              conversion, 
              by = c("FAO_Country")) %>%
            select(FAO_Country, gdp) 
         
          
          # Plot countries
          
          gvisGeoChart(gdp, "FAO_Country", "gdp", hovervar="FAO_Country",
                       options=list(gvis.editor="GDP", colorAxis="{colors:['#FFFFFF', '#0000FF']}"))
          
          
        })
      })
      
      shinyApp(ui = ui, server = server)
      
      