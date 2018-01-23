#Name: Project 8
#Authoer: Juan P. Garces
#Date: 12/07/2016
library(dplyr)
library(shiny)

# Define UI for application
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Project VIII"),
   
   # Sidebar with 3 Select Inputs 
   sidebarLayout(
      sidebarPanel(
          selectInput("Selector", "Select the Category of Choice", choices =  c("Economic", "Health & Safety")),
          selectInput("Type", "Choose Between Fatalities or Injuries", choices = c("Both", "Fatalities", "Injury")),
          selectInput("State", "Choose a State", choices = c("ALL", "IL", "KY", "FL", "GA"))
      ),
      
      #Plot the Data
      mainPanel(
         plotOutput("Plot")
      )
   )
))
server <- shinyServer(function(input, output) {

#Data and Load
DATA <- read.csv("WeatherEvents.csv")

First <- DATA %>% select(EVENT, CROPDMG, PROPDMG, INJURIES, FATALITIES , STATE) %>% group_by(EVENT)

#one function to handle the processes  
Process <- function(x,y=1){
  
    #selecting and choosing a category
    if(x == "Economic"){
      T <- Bonus(input$State) %>% summarize(TOTAL = sum(PROPDMG, CROPDMG)) %>% arrange(desc(TOTAL)) %>% top_n( 3, TOTAL) 
      return(T)
    }
    else if(y=="Both"){
            #Weight for the decision of tops
      T <- Bonus(input$State) %>% summarize(TOTAL = sum(FATALITIES*.8, INJURIES*.2)) %>% arrange(desc(TOTAL)) %>% top_n(3, TOTAL)
      return(T)
    }
    else if(y=="Fatalities"){
      T <- Bonus(input$State) %>% summarize(FATALITIES = sum(FATALITIES)) %>% arrange(desc(FATALITIES)) %>% top_n(3, FATALITIES)
      return(T)
    }
    else{ 
      T <- Bonus(input$State) %>% summarize(INJURIES = sum(INJURIES)) %>% arrange(desc(INJURIES)) %>% top_n(3, INJURIES)
      return(T)
    }

}
#BONUS SECTION
Bonus <- function(x){
  
  if(x == "IL"){
    return(First %>% filter(STATE == "IL") )
  }
  else if(x == "KY"){
    return(First %>% filter(STATE == "KY"))
  }
  else if(x == "FL"){
    return(First %>% filter(STATE == "FL"))
  }
  else if(x == "GA"){
    return(First %>% filter(STATE == "GA"))
  }
  else{ 
    return(First)
  }
}
#Plot Render
output$Plot <- renderPlot({
  
     if(input$Selector == "Economic"){
       barplot(height = Process(input$Selector)$TOTAL, width = 1, xlab = "Top 3 Events", ylab = "Cost of Damages", names.arg = Process(input$Selector)$EVENT, col = "green")
     }
     else if(input$Type == "Injury"){
       barplot(height = Process(input$Selector,input$Type)$INJURIES, width = 1, xlab = "Top 3 Events", ylab = "Amount of Injuries", names.arg = Process(input$Selector)$EVENT, col = "green")
     }
     else if(input$Type == "Fatalities"){
       barplot(height = Process(input$Selector,input$Type)$FATALITIES, width = 1, xlab = "Top 3 Events", ylab = "Amount of Fatalities", names.arg = Process(input$Selector)$EVENT, col = "green")
     }
     else{
       barplot(height = Process(input$Selector,input$Type)$TOTAL, width = 1, xlab = "Top 3 Events", ylab = "Total Fatalities/Injuries", names.arg = Process(input$Selector)$EVENT, col = "green")
     }
     
   })
   
})

# Run the application 
shinyApp(ui = ui, server = server)

