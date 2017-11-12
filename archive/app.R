#install.packages("shiny")
#install.packages("DT")
#install.packages("stringr")
library(shiny)
library(DT)
library(stringr)
db <- read.csv("E:\\USF Database\\db11-9.csv",fill = TRUE)



ui <- fluidPage(
  #textInput("Drug_1", label = "Drug 1", value = "Enter text..."),
  
  #textInput("Drug_2",label =  "Drug 2", value = "Enter text..."),
  
  selectInput("Drug_1b",label = "Drug 1b", choices = db$Drug),
  
  selectInput("Drug_2b",label = "Drug 2b", choices = db$Drug),
  dataTableOutput("table1"),
  dataTableOutput("table2")
  
  
  
)



server <- function(input, output) {
  
  found <- reactive({
    
    if(sum(str_detect(db$Drug, input$Drug_1b)) > 0)
    {
      tb1 <- db[db$Drug == input$Drug_1b,]
    }
    
    else if(sum(str_detect(db$CYP..., input$Drug_1b)) > 0)
    {
      tb1 <- db[db$CYP... == input$Drug_1b,]
    }
    
    if(sum(str_detect(db$Drug, input$Drug_2b)) > 0)
    {
      tb2 <- db[db$Drug == input$Drug_2b,]
    }
    
    else if(sum(str_detect(db$CYP..., input$Drug_2b)) > 0)
    {
      tb2 <- db[db$CYP... == input$Drug_2b,]
    }
    
    
    inhibitor1 <- sum(str_detect(tb1$Action,"inhibitor"))
    substrate1 <- sum(str_detect(tb1$Action,"substrate"))
    inducer1 <-sum(str_detect(tb1$Action,"inducer"))
    
    inhibitor2 <- sum(str_detect(tb2$Action,"inhibitor"))
    substrate2 <- sum(str_detect(tb2$Action,"substrate"))
    inducer2 <-sum(str_detect(tb2$Action,"inducer"))
    
    if(inhibitor1 >= substrate1 && inhibitor1 >= inducer1)
    {
      action1 = "inhibitor"
    }
    
    else if(substrate1 >= inhibitor1 && substrate1 >= inducer1)
    {
      action1 = "substrate"
    }
    
    else
    {
      action1 = "inducer"
    }
    
    if(inhibitor2 >= substrate2 && inhibitor2 >= inducer2)
    {
      action2 = "inhibitor"
    }
    
    else if(substrate2 >= inhibitor2 && substrate2 >= inducer2)
    {
      action2 = "substrate"
    }
    
    else
    {
      action2 = "inducer"
    }
    
    pt1 <- max(inducer1,substrate1,inhibitor1)
    pt2 <- max(inducer2,substrate2,inhibitor2)
    score <- sqrt(pt1*pt2)
    
    #row1 <- c(input$Drug_1b,input$Drug_2b,"Drug Score")
    row2 <- c(paste(action1,pt1),paste(action2,pt2), score)
    
    #table <- rbind(row1,row2)
    #mytable <- data.frame(row)
    mytable <- matrix(row2,ncol = 3,byrow = TRUE)
    colnames(mytable) <- c(input$Drug_1b,input$Drug_2b,"Drug Score")
    mytable <- data.frame(mytable)
  })
  
  check_me <- reactive({
    test <- db[db$Drug %in% input$Drug_1b | db$CYP... %in% input$Drug_1b |
                 db$Drug %in% input$Drug_2b | db$CYP... %in% input$Drug_2b,]
    
  })
  
  output$table1 <-renderDataTable({
    found()
    
    
  })
  
  output$table2 <-renderDataTable({ 
    
    check_me()
    
    
  })
  
  
  
  
  
  
  
  
  
}

shinyApp(ui,server)
