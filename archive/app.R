#install.packages("shiny")
#install.packages("DT")
#install.packages("stringr")
library(shiny)
library(DT)
library(stringr)
library(stringi)

db <- read.csv("db11-10.csv",fill = TRUE)
#former color for .well background: rgb(216, 31, 31)


ui <- fluidPage(
  #textInput("Drug_1", label = "Drug 1", value = "Enter text..."),
  
  #textInput("Drug_2",label =  "Drug 2", value = "Enter text..."),
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      imageOutput("image"),
      selectInput("search",label = "Search Type:", choices = c('Drug_Name','CYP')),
      conditionalPanel("input.search == 'Drug_Name' ",
      selectInput("Drug_1",label = "Drug 1", choices = unique(db$Drug),selectize = FALSE, size = 5),
      selectInput("Drug_2",label = "Drug 2", choices = unique(db$Drug),selectize = FALSE,size = 5)
      ),
      conditionalPanel("input.search == 'CYP' ",
      selectInput("CYP_1",label = "CYP 1", choices = unique(db$CYP...)),
      selectInput("CYP_2",label = "CYP 2", choices = unique(db$CYP...))
      ),
      actionButton("GO","Search",icon("refresh"))
          
    ),
    
    mainPanel(
      tags$style(type="text/css"," .dataTables_wrapper .dataTables_length .dataTables_info
                 .dataTables_filter .dataTables_paginate  {font-size: 10vh; width: 50vw;}",
                 "."),
      tags$style(type="text/css"," .well {background-color: rgb(139, 8, 8) ; max-width: 300px; height: 100vh;} "),
      tags$style(type="text/css"," .form-group {color: #FFF ;}"),
      tags$style(type="text/css"," table.dataTable { padding-bottom: 60px; font-size:1.5vh ;}"),
      tags$style(type="text/css"," .col-sm-3 {max-width: 300px;}"),
      tags$style(type="text/css"," a {color:rgb(216, 31, 31) ;}"),
      tags$style(type="text/css"," .nav-tabs>li.active>a {color:rgb(139, 8, 8) ;}"),
      tags$style(type="text/css"," .nav-tabs>li>a {background-color:rgb(139, 8, 8) ; color:#FFF ;}"),
      tags$style(type="text/css",".dataTables_wrapper .dataTables_paginate .paginate_button {color: #FFF;}"),
      tabsetPanel(id = "tabs", 
                  tabPanel("Home",value="home",
                           h3(textOutput("home_header1")),
                           p(textOutput("home_body1"))
                            ),
                  tabPanel("DDI",value="DDI",
                  
        
                  h3(textOutput("DDI_header1")),
                  dataTableOutput("table1"),
                  h3(textOutput("DDI_header2")),
                  dataTableOutput("table2")
                          )
                  )
              )
    )
    
 )



server <- function(input, output,session) {
   
  observeEvent(input$GO, {
    updateTabsetPanel(session, "tabs",
      selected = "DDI"
    )
  })
  found <- reactive({
    choice <- input$search
    
    test <- db[db$Drug %in% input$Drug_1 | db$Enzyme %in% input$Drug_1 |
           db$Drug %in% input$Drug_2 | db$Enzyme %in% input$Drug_2,]

    cyps <- unique(test$Enzyme)

    
    holding <- NULL
    for(i in cyps)
    {
        tb1 <- db[db$Drug %in% input$Drug_1 & db$Enzyme %in% i,]
      #}
      
      #if(sum(str_detect(db$Drug, input$Drug_2)) > 0)
      #{
        tb2 <- db[db$Drug %in% input$Drug_2 & db$Enzyme %in% i,]
      #}
      
    #else
    #{
      #if(sum(str_detect(db$CYP..., input$CYP_1)) > 0)
      #{
     #   tb1 <- db[db$CYP... %in% input$CYP_1,]
      #}
   
      #if(sum(str_detect(db$CYP..., input$CYP_2)) > 0)
      #{
      #  tb2 <- db[db$CYP... %in% input$CYP_2,]
      #}
    #}
    
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
#    
      pt1 <- max(inducer1,substrate1,inhibitor1)
      pt2 <- max(inducer2,substrate2,inhibitor2)
      score <- sqrt(pt1*pt2)
      if(pt1 == 0)
      {
         action1 = "No Matches"
      }
      if(pt2 == 0)
      {
         action2 = "No Matches"
      }
      if((action1 == "substrate" && action2 == "substrate") | (action1 != "substrate" && action2 != "substrate"))
      {
         score = 0
      }
      
      
    #row1 <- c(input$Drug_1b,input$Drug_2b,"Reliability score")
      row2 <- c(paste(action1,pt1),paste(action2,pt2), score)
      if(score > 0)
      {
        holding <- rbind(holding,row2)
      }
    }
    #table <- rbind(row1,row2)
    #mytable <- data.frame(row)
     mytable <- matrix(holding,ncol = 3)#,byrow = TRUE)
    #if(choice == "Drug_Name")
    #{
      colnames(mytable) <- c(input$Drug_1,input$Drug_2,"R_Score")
      rownames(mytable) <- cyps
    #}
    #else
    #{
    # colnames(mytable) <- c(input$CYP_1,input$CYP_2,"Drug Score")

   # }
    #mytable <- mytable[mytable[,3] > 0,]
    mytable <- data.frame(mytable)
    #mytable <- mytable[which(mytable[,3]>0),]
   
  })
  
  check_me <- reactive({
    
    choice <- input$search
    if(choice == "Drug_Name")
    {
      test <- db[db$Drug %in% input$Drug_1 | db$Drug %in% input$Drug_2,]
      #test2 <- db[db$Drug %in% input$Drug_2,]
                
    }
    fulldt <- NULL
   # else
   # {
   #   test <- db[db$CYP... %in% input$CYP_1 | db$CYP... %in% input$CYP_2,]
   # }
    #stri_enc_tonative("\u2713")
    
    if(sum(str_detect(test$Database,"DrugBank")>0))
    {
    #link_db <- paste0("https://www.drugbank.ca/drugs/",test$DrugID)
    
      db <- test[test$Database == "DrugBank",]
      db$Extra <- paste0("https://www.drugbank.ca/drugs/",db$DrugID)
      db$Database <- paste0("<a href='",db$Extra,"'>DrugBank</a>")
      fulldt <- rbind(fulldt,db)
      
    }
      
    if(sum(str_detect(test$Database,"SuperCYP")>0))
    {
      sc <- test[test$Database == "SuperCYP",]
      sc$Extra <- paste0("http://bioinformatics.charite.de/transformer/index.php?site=drug_search")
      sc$Database <- paste0("<a href='",sc$Extra,"'>SuperCYP</a>")
      fulldt <- rbind(fulldt,sc)
    }
    
    if(sum(str_detect(test$Database,"KEGG")>0))
    {
      k <- test[test$Database == "KEGG",]
      k$Extra <- paste0("http://www.kegg.jp/kegg-bin/search_pathway_text?map=map&keyword=",k$Drug,"&mode=1&viewImage=true")
      k$Database <- paste0("<a href='",k$Extra,"'>KEGG</a>")
      fulldt <- rbind(fulldt,k)
    }
    
    if(sum(str_detect(test$Database,"Indiana University")>0))
    {  
      iu <- test[test$Database == "Indiana University",]
      iu$Extra <- "http://medicine.iupui.edu/clinpharm/ddis/main-table/"
      iu$Database <- paste0("<a href='",iu$Extra,"'>Indiana University</a>")
      fulldt <- rbind(fulldt,iu)
    }  
      
    if(sum(str_detect(test$Database,"ildcare")>0))
    {  
      ild <- test[test$Database == "ildcare",]
      ild$Extra <- "http://www.ildcare.eu/Downloads/artseninfo/CYP450_drug_interactions.pdf"
      ild$Database <- paste0("<a href='",ild$Extra,"'>ildcare</a>")
      fulldt <- rbind(fulldt,ild)
      
    }  
      
    
     
    fulldt <- data.frame(fulldt[1:5],row.names = NULL)
    
  })
  
  
  output$image <- renderImage({
  list(src = "www/pills.png",contentType = "image/png",width= "100%" )  
    
  },deleteFile = FALSE)
  
  output$table1 <-renderDataTable({
    input$GO
    found()
   
    
    
  })
  
  output$table2 <-renderDataTable({ 
    input$GO
    check_me()
    
    
  },escape=FALSE)
  
  output$DDI_header1 <- renderText({
    
    "Results of search: "
  })
  
  output$DDI_header2 <- renderText({
    
    "Full list of results:"
  })
  
  output$home_header1 <- renderText({
    
    "Welcome to the USF webserver! " 
  
})
  
  output$home_body1 <- renderText({
    
    "With the help of this tool it is possible to search for a drug-cocktail to check whether 
the metabolisms of the drugs interact with each other. By typing in the first few letters of the drug or utilizing the drop down selection
, you can choose 2 drugs from our database and determine a Reliability score. If you have chosen your drugs, click on the *Search* button."

  })  
    
}
shinyApp(ui,server)
