#install.packages("shiny")
#install.packages("DT")
#install.packages("stringr")
library(shinyBS)
library(shiny)
library(DT)
library(stringr)
library(stringi)

db <- read.csv("db12-4.csv",fill = TRUE)
db$Drug <- tolower(db$Drug)
db$Drug <- trimws(db$Drug )

drug_info <- read.csv("drugbankid_info.csv",fill = TRUE)
drug_info$Name <- tolower(drug_info$Name)
drug_info$Name <- trimws(drug_info$Name)
modal_made = 0
modal_view <- "www.google.com"
modal_name <- NULL

kegg_info <- read.csv("keggid_info.csv",fill = TRUE)
kegg_info$DrugName <- tolower(kegg_info$DrugName)
kegg_info$DrugName <- trimws(kegg_info$DrugName)

#former color for .well background: rgb(216, 31, 31)


ui <- fluidPage(
  #textInput("Drug_1", label = "Drug 1", value = "Enter text..."),
  
  #textInput("Drug_2",label =  "Drug 2", value = "Enter text..."),
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      imageOutput("image",height = "35vh"),
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
      tags$style(type="text/css"," .dataTables_wrapper .dataTables_paginate .paginate_button {color: #FFF;}"),
     
      tabsetPanel(id = "tabs", 
                  tabPanel("Home",value="home",
                           h3(textOutput("home_header1")),
                           p(textOutput("home_body1")),
                           imageOutput("ddi_home1",height = "80vh")
                            ),
                  tabPanel("DDI",value="DDI",
                  
        
                  h3(textOutput("DDI_header1")),
                  dataTableOutput("table1"),
                  textOutput("verbose"),
                  h3(textOutput("DDI_header2")),
                  dataTableOutput("table2"),
                  uiOutput("view_struct_pt1")
                  #uiOutput("view_struct_pt2")
                  #bsModal("modalExample", "Your plot", "struct", size = "medium",htmlOutput("pic"))
                          )
                  )
              )
    )
    
 )



server <- function(input, output,session) {
   
  shinyInput <- function(FUN, len, id,rn, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, rn[i]), ...))
      }
      inputs
    }
  
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
    
      #one outcome
      if(inhibitor1 > substrate1 && inhibitor1 > inducer1)
      {
        action1 = c("inhibitor")
      }
    
      else if(substrate1 > inhibitor1 && substrate1 > inducer1)
      {
        action1 = c("substrate")
      }
    
      else if(inducer1 > substrate1 && inducer1 > inhibitor1)
      {
        action1 = c("inducer")
      }
        
      #two outcomes  
      else if(inhibitor1 == substrate1 && inhibitor1 > inducer1)
      {
        action1 = c("inhibitor","substrate")
      }
    
      else if(inhibitor1 == inducer1 && inhibitor1 > substrate1)
      {
        action1 = c("inhibitor","inducer")
      }
    
      else if(inducer1 == substrate1 && inducer1 > inhibitor1)
      {
        action1 = c("inducer","substrate")
      }
      
      #three outcomes
      else if(inducer1 == substrate1 && inducer1 == inhibitor1)
      {
        action1 = c("inducer","substrate","inhibitor")
      }
    
      #one outcome
      if(inhibitor2 > substrate2 && inhibitor2 > inducer2)
      {
        action2 = c("inhibitor")
      }
    
      else if(substrate2 > inhibitor2 && substrate2 > inducer2)
      {
        action2 = c("substrate")
      }
    
      else if(inducer2 > substrate2 && inducer2 > inhibitor2)
      {
        action2 = c("inducer")
      }
        
      #two outcomes  
      else if(inhibitor2 == substrate2 && inhibitor2 > inducer2)
      {
        action2 = c("inhibitor","substrate")
      }
    
      else if(inhibitor2 == inducer2 && inhibitor2 > substrate2)
      {
        action2 = c("inhibitor","inducer")
      }
    
      else if(inducer2 == substrate2 && inducer2 > inhibitor2)
      {
        action2 = c("inducer","substrate")
      }
      
      #three outcomes
      else if(inducer2 == substrate2 && inducer2 == inhibitor2)
      {
        action2 = c("inducer","substrate","inhibitor")
      }
      
        
        
      #pt1 <- max(inducer1,substrate1,inhibitor1)
      # pt2 <- max(inducer2,substrate2,inhibitor2)
      # score <- sqrt(pt1*pt2)
      
      #appending rows to table
      if(length(action1) < length(action2))
      {
        
        for(j in action1)
        {
          for(k in action2)
          {
            if(j == "inducer")
            {
              pt1 <- inducer1
            }
            else if(j == "substrate")
            {
              pt1 <- substrate1 
            }
            else
            {
              pt1 <- inhibitor1
            }
            if(k == "inducer")
            {
              pt2 <- inducer2
            }
            else if(k == "substrate")
            {
              pt2 <- substrate2 
            }
            else
            {
              pt2 <- inhibitor2
            }
            
            
            score <- sqrt(pt1*pt2)
            tk = k
            s = score
            if(pt1 == 0)
            {
              tk = "No Matches"
            }
            if(pt2 == 0)
            {
              tk = "No Matches"
            }
            if((j == "substrate" && k == "substrate") | (j != "substrate" && k != "substrate"))
            {
              s = 0
            }
     # enz <- tb1[tb1$Enzyme %in% tb2$Enzyme]$Enzyme
            
    #row1 <- c(input$Drug_1b,input$Drug_2b,"Reliability score")
            row2 <- c(i,paste(j,pt1),paste(tk,pt2), s)
            print(row2)
            holding <- rbind(holding,row2)
          }
        }
      }
      else
      {
        for(j in action2)
        {
          for(k in action1)
          {
            if(k == "inducer")
            {
              pt1 <- inducer1
            }
            else if(k == "substrate")
            {
              pt1 <- substrate1 
            }
            else
            {
              pt1 <- inhibitor1
            }
            if(j == "inducer")
            {
              pt2 <- inducer2
            }
            else if(j == "substrate")
            {
              pt2 <- substrate2 
            }
            else
            {
              pt2 <- inhibitor2
            }
            
            score <- sqrt(pt1*pt2)
            tk = k
            s = score
            if(pt1 == 0)
            {
              tk = "No Matches"
            }
            if(pt2 == 0)
            {
              tk = "No Matches"
            }
            if((k == "substrate" && j == "substrate") | (k != "substrate" && j != "substrate"))
            {
              s = 0
            }
     # enz <- tb1[tb1$Enzyme %in% tb2$Enzyme]$Enzyme
      
    #row1 <- c(input$Drug_1b,input$Drug_2b,"Reliability score")
            row2 <- c(i,paste(k,pt1),paste(j,pt2), s)
            print(row2)
            holding <- rbind(holding,row2)
          }
        }   
      }  
    }
    #table <- rbind(row1,row2)
    #mytable <- data.frame(row)
     mytable <- matrix(holding,ncol = 4)#,byrow = TRUE)
    #if(choice == "Drug_Name")
    #{
      colnames(mytable) <- c("Enzyme",input$Drug_1,input$Drug_2,"R_Score")
     # rownames(mytable) <- cyps
      #cyp <- rownames(mytable[mytable[,4] > 0,])
    #}
    #else
    #{
    # colnames(mytable) <- c(input$CYP_1,input$CYP_2,"Drug Score")

   # }
    mytable <- mytable[mytable[,4] > 0,]
    #cyp <- rownames(mytable)
    mytable <- matrix(mytable,ncol = 4)
    colnames(mytable) <- c("Enzyme",input$Drug_1,input$Drug_2,"R_Score")
    #rownames(mytable) <- cyp
    mytable <- data.frame(mytable)
    mytable <- mytable[(order(mytable$R_Score, decreasing = TRUE)),]
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
    
      dbank <- test[test$Database == "DrugBank",]
      dbank$Extra <- paste0("https://www.drugbank.ca/drugs/",dbank$DrugID)
      dbank$Database <- paste0("<a href='",dbank$Extra,"'>DrugBank</a>")
      dbank$Extra2 <- paste0("https://www.drugbank.ca/structures/",dbank$DrugID,"/image.svg")
      
      #####I WORK FINE####### ->  dbank$Structure <- paste0("<a href='",dbank$Extra2,"'>structure</a>") #actionLink(paste0("dbstruct_",rownames(dbank)),label = "structure")#urlModal(paste0("https://www.drugbank.ca/structures/",dbank$DrugID,"/image.svg"), title = "Bookmarked application link", subtitle = NULL)
      dbank$Structure <- shinyInput(actionButton,nrow(dbank),"dbstruct_",rownames(dbank),label = "structure",onclick = 'Shiny.onInputChange(\"select_button1\",  this.id)' )#$#HTML(readLines(paste0("https://www.drugbank.ca/structures/",dbank$DrugID,"/image.svg")))
      holddb <- NULL
      print("These are the rows associated with dbank")
      print(rownames(dbank))
      fulldt <- rbind(fulldt,dbank)
      
    }
      
    if(sum(str_detect(test$Database,"SuperCYP")>0))
    {
      sc <- test[test$Database == "SuperCYP",]
      sc$Extra <- paste0("http://bioinformatics.charite.de/transformer/index.php?site=drug_search")
      sc$Database <- paste0("<a href='",sc$Extra,"'>SuperCYP</a>")
      sc$Extra2 <- "Not Available"
      sc$Structure <- shinyInput(actionButton,nrow(sc),"kstruct_",rownames(sc),label = "structure",onclick = 'Shiny.onInputChange(\"select_button2\",  this.id)' )#$#HTML(readLines(paste0("https://www.drugbank.ca/structures/",dbank$DrugID,"/image.svg")))

      fulldt <- rbind(fulldt,sc)
    }
    
    if(sum(str_detect(test$Database,"KEGG")>0))
    {
      k <- test[test$Database == "KEGG",]
      
      #k$DrugID <- kids
      k$Extra <- paste0("http://www.kegg.jp/kegg-bin/search_pathway_text?map=map&keyword=",k$Drug,"&mode=1&viewImage=true")
      k$Database <- paste0("<a href='",k$Extra,"'>KEGG</a>")
      k$Extra2 <- paste0("http://www.kegg.jp/Fig/drug/",kegg_info[kegg_info$DrugName == k$Drug,]$DrugID,".gif")
      k$Structure <- shinyInput(actionButton,nrow(k),"kstruct_",rownames(k),label = "structure",onclick = 'Shiny.onInputChange(\"select_button3\",  this.id)' )#$#HTML(readLines(paste0("https://www.drugbank.ca/structures/",dbank$DrugID,"/image.svg")))
      for(i in k$Drug)
      {
        print(i)
        print(length(k$Drug))
        print(kegg_info[kegg_info$DrugName == i,])
        print(kegg_info[kegg_info$DrugName == i,]$DrugID)
        print(unique(kegg_info[kegg_info$DrugName == i,]$DrugID))
        k_temp <- NULL
        k_temp <- c(k_temp,unique(kegg_info[kegg_info$DrugName == i,]$DrugID))
      }
      k$DrugID <- k_temp
      print("KEGG info TABLE!!!!")
      print(k_temp)
      print(k$DrugID)
      #print(k)
      #print(kegg_info[kegg_info$DrugName == k$Drug,])
      fulldt <- rbind(fulldt,k)
    }
    
    if(sum(str_detect(test$Database,"Indiana University")>0))
    {  
      iu <- test[test$Database == "Indiana University",]
      iu$Extra <- "http://medicine.iupui.edu/clinpharm/ddis/main-table/"
      iu$Database <- paste0("<a href='",iu$Extra,"'>Indiana University</a>")
      iu$Extra2 <- "Not Available"
      iu$Structure <- "Not Available"
      fulldt <- rbind(fulldt,iu)
    }  
      
    if(sum(str_detect(test$Database,"ildcare")>0))
    {  
      ild <- test[test$Database == "ildcare",]
      ild$Extra <- "http://www.ildcare.eu/Downloads/artseninfo/CYP450_drug_interactions.pdf"
      ild$Database <- paste0("<a href='",ild$Extra,"'>ildcare</a>")
      ild$Extra2 <- "Not Available"
      ild$Structure <- "Not Available"
      fulldt <- rbind(fulldt,ild)
      
   }  
      
    
     
    #fulldt <- data.frame(fulldt[1:5])
    fulldt <- data.frame(fulldt)                          
    fulldt <- fulldt[(order(fulldt$Enzyme)),]
    list(drugs = fulldt)
    
  })
  #mod <- observe({
  mod <- reactive({
    myModal = modalDialog(title=paste(modal_name),HTML(readLines(modal_view)),easyClose=TRUE,footer=paste("source:",modal_view))    
    })  
      
  modal_stuff <- reactiveValues()
   
  observeEvent(input$select_button1, {
      selectedRow <- as.numeric(strsplit(input$select_button1, "_")[[1]][2])
      
      modal_view <<- check_me()$drugs[paste(selectedRow),11]
      modal_name <<- check_me()$drugs[paste(selectedRow),1]
      
      print("selected Row DrugBank")
      print(selectedRow)
      print("link to row")
      print(modal_view)
      print("Drug Name")
      print(modal_name)
      showModal(modalDialog(title=paste(modal_name),HTML(readLines(modal_view)),easyClose=TRUE,footer=paste("source:",modal_view))    
)
      modal_name <<- NULL
      modal_view <<- NULL
  
    
    })
    observeEvent(input$select_button2, {
      selectedRow <- as.numeric(strsplit(input$select_button2, "_")[[1]][2])
     
      modal_view <<- check_me()$drugs[paste(selectedRow),11]
      modal_name <<- check_me()$drugs[paste(selectedRow),1]
     
      print("selected Row for SuperCYP")
      print(selectedRow)
      print("link to row")
      print(modal_view)
      print("Drug Name")
      print(modal_name)
      showModal(modalDialog(title=paste(modal_name),HTML(readLines(modal_view)),easyClose=TRUE,footer=paste("source:",modal_view))    
)
      modal_name <<- NULL
      modal_view <<- NULL
  
    
    })
    observeEvent(input$select_button3, {
      selectedRow <- as.numeric(strsplit(input$select_button3, "_")[[1]][2])
   
      modal_view <<- check_me()$drugs[paste(selectedRow),11]
      modal_name <<- check_me()$drugs[paste(selectedRow),1]
      
      print("selected Row for kegg")
      print(selectedRow)
      print("link to row")
      print(modal_view)
      print("Drug Name")
      print(modal_name)
      showModal(modalDialog(title=paste(modal_name),HTML(read.gif(modal_view)),easyClose=TRUE,footer=paste("source:",modal_view))    
)
      modal_name <<- NULL
      modal_view <<- NULL
  
    
    })  
  

  output$pic <- renderUI({
    
      HTML(readLines(modal_stuff$modal_view))
  })
      
  output$image <- renderImage({
  list(src = "www/pills.png",contentType = "image/png",width= "100%" )  
    
  },deleteFile = FALSE)
      
  output$ddi_home1 <- renderImage({
  list(src = "www/ddi_home.png",contentType = "image/png",width= "100%" )  
    
  },deleteFile = FALSE)
  
  output$table1 <-renderDataTable({
    input$GO
    found()
 })
      
 
#  check_me()$drugs <- newtb
  print("This is the number of modals made!!!")
  print(modal_made)
  print("This is the type for output$")
  
      
    
  
      
  output$table2 <-renderDataTable({ 
    input$GO
 #   newtb
    check_me()$drugs[c(1,2,3,4,6,12)]
 #   print("Type received from check_me()")
 #   print(typeof(check_me()))
    
 #   a <- data.frame(matrix(unlist(check_me()$dt), ncol = 11),stringsAsFactors=FALSE)
    #a <- ldply (check_me()$dt, data.frame)
 #   a <- as.data.frame(a)
 #   b <- data.frame(c(a[1:11]))
  #  print("converted dt to a")
  #  print("type of a")
  #  print(typeof(a))
  #  print(typeof(b))
  ### print("Got from check_me()")
  #  print("This is dt type")
  #  print(typeof(check_me()$dt))
  #  print("This is dt")
  #  print(check_me()$dt)
  #  print("This is dbank type") 
  #  print(typeof(check_me()$dbank))
    
    
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
  
  output$verbose <- renderText({
    found()
    
    
    })
  output$home_body1 <- renderText({
    
    "With the help of this tool it is possible to search for a drug-cocktail to check whether 
the metabolisms of the drugs interact with each other. By typing in the first few letters of the drug or utilizing the drop down selection
, you can choose 2 drugs from our database and determine a Reliability score. If you have chosen your drugs, click on the *Search* button."

  })  
    
}
shinyApp(ui,server)
