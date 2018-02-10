#install.packages("shiny")
#install.packages("DT")
#install.packages("stringr")
#install.packages("shinythemes")
library(shinyBS)
library(shiny)
library(DT)
library(stringr)
library(stringi)
library(shinythemes)
db <- read.csv("db2-10.csv",fill=TRUE)
#DR.CHENG COPY BELOW
#db <- read.csv("db12-4.csv",fill = TRUE)
db$Drug <- tolower(db$Drug)
db$Drug <- trimws(db$Drug)
db$Database <- trimws(db$Database)

#drug_info <- read.csv("drugbankid_info.csv",fill = TRUE)
#drug_info$Name <- tolower(drug_info$Name)
#drug_info$Name <- trimws(drug_info$Name)

#kegg_info <- read.csv("keggid_info.csv",fill = TRUE)
#kegg_info$DrugName <- tolower(kegg_info$DrugName)
#kegg_info$DrugName <- trimws(kegg_info$DrugName)

#supcyp_info <- read.csv("Supercyp_1-9.csv",fill = TRUE)
#supcyp_info$DrugName <- tolower(supcyp_info$DrugName)
#supcyp_info$DrugName <- trimws(supcyp_info$DrugName,which=c("both"))
#supcyp_info$DrugName <- trimws(supcyp_info$DrugName,which=c("r"))
modal_made = 0
modal_view <- "www.google.com"
modal_name <- NULL
cocktail <- NULL
mytext <- NULL
current <- NULL
#former color for .well background: rgb(216, 31, 31)


ui <- fluidPage(
  #textInput("Drug_1", label = "Drug 1", value = "Enter text..."),
  
  #textInput("Drug_2",label =  "Drug 2", value = "Enter text..."),
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      imageOutput("image",height = "35vh"),
      selectInput("search",label = "Search Type:", choices = c('DDI_Basic','DDI_Advanced')),
      conditionalPanel("input.search == 'DDI_Basic' ",
      #selectInput("Drug_1",label = "Drug 1", choices = unique(db$Drug),selectize = FALSE, size = 5),
      #selectInput("Drug_2",label = "Drug 2", choices = unique(db$Drug),selectize = FALSE,size = 5),
      actionButton("side_basic","Begin",icon("refresh"))
      ),
      conditionalPanel("input.search == 'DDI_Advanced' ",
      actionButton("side_advanced","Begin",icon("refresh"))
      )
      
          
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
     
      navbarPage(id="menu",title="Welcome to the USF Web Server!",theme = shinytheme("simplex"),
	      tabPanel("Home",value="home",
                           h3(textOutput("home_header1")),
                           p(textOutput("home_body1")),
                           imageOutput("ddi_home1",height = "80vh")
                            ),
                  navbarMenu("Basic",
			   tabPanel("Basic_Search",value="DDI Basic",
				textOutput("basic_drug_1"),
				selectInput("Drug_1",label = NULL, choices = unique(db$Drug),selectize = FALSE, size = 5),
				textOutput("basic_drug_2"),    
      				selectInput("Drug_2",label = NULL, choices = unique(db$Drug),selectize = FALSE,size = 5),
      				actionButton("bs_tb1","Search",icon("refresh"))
				),
                           tabPanel("DDI_Basic1",value="R score (Basic)",
                                h3(textOutput("DDI_header1")),
                                dataTableOutput("table1"),
				actionButton("bs_tb2","Resources",icon("refresh")),
				actionButton("bs_hm1","Home",icon("refresh")) 
                                ),
                           tabPanel("DDI_Basic2",value="Resources (Basic)",
                 # textOutput("verbose"),
    
                                  h3(textOutput("DDI_header2")),
                                  dataTableOutput("table2"),
                                  uiOutput("view_struct_pt1"),
				  actionButton("bs_tb1a","Search",icon("refresh")),
				  actionButton("bs_hm2","Home",icon("refresh"))
				  
                                  )
                            ),
                  navbarMenu("Advanced",
                            tabPanel("DDI_Advanced", value="DDI Advanced",
                                  textOutput("advanced_1"),
                                  textInput("ADrug_1",label = NULL, value = "warfarin"),
                                  textOutput("advanced_2"),
                                  uiOutput("AText_1"),
                           #uiOutput("AText_2"),
                           #selectInput("ADrug_2",label = "Select the drug name from the list", choices = unique(db$Drug),selectize = FALSE,size = 5),
                                  textOutput("advanced_3"),
                                  verbatimTextOutput("Atext"), 
                           #selectInput("ADrug_2",label = "Select the drug name from the list", choices = unique(db$Drug),selectize = FALSE,size = 5),
                           #selectInput("ADrug_",label = "Drug 2", choices = unique(db$Drug),selectize = FALSE,size = 5),
                                  actionButton("GO2","Cocktail",icon("refresh")),
                                  actionButton("GO3","Clear",icon("ban"))
	                                ),
                            tabPanel("Advancedt1", value="DDI Advanced table 1",
				  h3(textOutput("advance_header1")),   
                                  dataTableOutput("advance_table1"),
				  actionButton("as_tb2","Resources",icon("refresh")), 
				  actionButton("as_hm1","Home",icon("refresh")) 
	                                ),
                            tabPanel("Advancedt2", value="DDI Advanced table 2",
				  h3(textOutput("advance_header2")),   
                                  dataTableOutput("advance_table2"),
				  actionButton("as_tb1","R Score",icon("refresh")),    
				  actionButton("as_hm2","Home",icon("refresh")) 
                                  )
                            ),
		 tabPanel("About_Us",value="About US",
                           h3(textOutput("about_header1")),
                           p(textOutput("about_body1"))
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
	
  #BUTTONS FOR SIDE PANEL
	
  observeEvent(input$side_basic, {
    updateNavbarPage(session, "menu",
      selected = "DDI Basic"
    )
  })
  
  observeEvent(input$side_advanced, {
    updateNavbarPage(session, "menu",
      selected = "DDI Advanced"
    )
  })

	
  #BUTTONS FOR DDI BASIC TABS
	
   observeEvent(input$bs_tb1, {
    updateNavbarPage(session, "menu",
      selected = "R score (Basic)"
    )
  })

 observeEvent(input$bs_tb1a, {
    updateNavbarPage(session, "menu",
      selected = "R score (Basic)"
    )
  })
	
  observeEvent(input$bs_tb2, {
    updateNavbarPage(session, "menu",
      selected = "Resources (Basic)"
    )
  })

 observeEvent(input$bs_hm1, {
    updateNavbarPage(session, "menu",
      selected = "DDI Basic"
    )
  })	

 observeEvent(input$bs_hm2, {
    updateNavbarPage(session, "menu",
      selected = "DDI Basic"
    )
  })	
	
	
	
#Advanced Buttons
observeEvent(input$GO2,{
    updateNavbarPage(session, "menu",
      selected = "DDI Advanced table 1"
    )
  })
	     
	     
	     
observeEvent(input$as_tb1, {
    updateNavbarPage(session, "menu",
      selected = "DDI Advanced table 1"
    )
  })

  observeEvent(input$as_tb2, {
    updateNavbarPage(session, "menu",
      selected = "DDI Advanced table 2"
    )
  })

 observeEvent(input$as_hm1, {
    updateNavbarPage(session, "menu",
      selected = "DDI Advanced"
    )
  })	

 observeEvent(input$as_hm2, {
    updateNavbarPage(session, "menu",
      selected = "DDI Advanced"
    )
  })	
	
	
  observeEvent(input$GO3, {
    mytext <<- NULL
  })
	
 
  
  output$AText_1 <- renderUI ({
   #  print("ALL REACTIVE VALUES")
  #   print(str(reactiveValuesToList(input)))
     text <- paste0("^",input$ADrug_1)
  #   print("my text is")
  #   print(text)
  #   print(unique(db$Drug[grep(text, db$Drug)]))
     picks <- unique(db$Drug[grep(text, db$Drug)])
     tagList(
     selectInput("ADrug_2", label = NULL,choices = c(picks),multiple = TRUE,selectize = FALSE,size = 5)
     #checkboxGroupInput("ADrug_2",label = "Select the drug name from the list", choices = c(picks))
     )                     
  })
  
  observe({ 
    at()
  })
  
  at <- reactive({
  
 
  mytext <<- unique(c(mytext,input$ADrug_2))
  
  
  
  })
  
  output$Atext <- renderText ({
    #print("made it to ATExt")
    #mytext <- c(mytext,text)
    #print(mytext)
    #paste(mytext)
    #input$ADrug_2
    at()
  
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
        #    print(row2)
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
        #    print(row2)
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
    print(input$ADrug_1)
    choice <- input$search
    if(choice == "DDI_Basic")
    {
      test <- db[db$Drug %in% input$Drug_1 | db$Drug %in% input$Drug_2,]
      #test2 <- db[db$Drug %in% input$Drug_2,]
                
    }
    else{
      test<-NULL
      for(i in mytext)
      {
        temp <- db[db$Drug %in% i,]
        test <- rbind(temp,test)
      }
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
      dbank$Structure <- shinyInput(actionLink,nrow(dbank),"dbstruct_",rownames(dbank),icon("expand"),label = "View Structure",onclick = 'Shiny.onInputChange(\"select_button1\",  this.id)' )#$#HTML(readLines(paste0("https://www.drugbank.ca/structures/",dbank$DrugID,"/image.svg")))
      holddb <- NULL
   #   print("These are the rows associated with dbank")
   #   print(rownames(dbank))
      fulldt <- rbind(fulldt,dbank)
   #   print(dbank)
      
    }
   # print("after DB")  
    if(sum(str_detect(test$Database,"SuperCYP")>0))
    {
      sc <- test[test$Database == "SuperCYP",]
      sc$DrugID <- paste0("000000000",sc$DrugID)
      sc$DrugID <- str_sub(sc$DrugID,-9)
     # sc_temp <- NULL
     # scd <- sc$Drug
   #   print(length(scd))
   #   print(scd)
     # for(i in scd)
     # {
      #  temp <- paste(unique(supcyp_info[supcyp_info$DrugName == i,]$CASNumber))
      #  sc_temp <- c(sc_temp,temp)
      #}
      #sc$DrugID <- sc_temp
   #   print("Made it to SuperCYP")
      
   #   print("This is supercyp")
   #   print(sc)
      #sc$Extra <- paste0("http://bioinformatics.charite.de/transformer/index.php?site=drug_search")
      sc$Extra <- paste0("http://bioinformatics.charite.de/supercyp/index.php?site=fullinfo&cas=",sc$DrugID)
    #  print("before database")
      sc$Database <- paste0("<a href='",sc$Extra,"'>SuperCYP</a>")
   #   print("before extra2")
      sc$Extra2 <- paste0("http://bioinformatics.charite.de/supercyp/img//jpeg_ohne_h//",sc$DrugID,".jpeg")#supcyp_info[supcyp_info$DrugName == sc$Drug,]$CASNumber,".jpeg")
      sc$Structure <- shinyInput(actionLink,nrow(sc),"scstruct_",rownames(sc),icon("expand"),label = "View Structure",onclick = 'Shiny.onInputChange(\"select_button2\",  this.id)' )#$#HTML(readLines(paste0("https://www.drugbank.ca/structures/",dbank$DrugID,"/image.svg")))
      
      
      #print("sc test table")
      #print(sc)
      #print("sc info table")
      #print(supcyp_info[supcyp_info$DrugName == "warfarin",])
      #rint("sc drug names")
      #print(supcyp_info$DrugName[631])
      fulldt <- rbind(fulldt,sc)
    }
  #  print("after SUP")
    if(sum(str_detect(test$Database,"KEGG")>0))
    {
      k <- test[test$Database == "KEGG",]
     # k_temp <- NULL
    #  print("KEGG DRUG IDS!!!!")
     # kd <- k$Drug
   #   print(length(kd))
    #  for(i in kd)
     # {
    #    temp <- paste(unique(kegg_info[kegg_info$DrugName == i,]$DrugID))
    #    k_temp <- c(k_temp,temp)
    #  }
    #  k$DrugID <- k_temp
      #k$DrugID <- trimws(k$DrugID)
   #   print("made it to KEGG")
   #   print(k)
      k$Extra <- paste0("http://www.kegg.jp/kegg-bin/search_pathway_text?map=map&keyword=",k$Drug,"&mode=1&viewImage=true")
      k$Database <- paste0("<a href='",k$Extra,"'>KEGG</a>")
      k$Extra2 <- paste0("http://www.kegg.jp/Fig/drug/",k$DrugID,".gif")
      k$Structure <- shinyInput(actionLink,nrow(k),"kstruct_",rownames(k),icon("expand"),label = "View Structure",onclick = 'Shiny.onInputChange(\"select_button3\",  this.id)' )#$#HTML(readLines(paste0("https://www.drugbank.ca/structures/",dbank$DrugID,"/image.svg")))
      #k$DrugID[kegg_info$DrugName == k$Drug,] <- paste(unique(kegg_info[kegg_info$DrugName == k$Drug,]$DrugID))
     
      #print(k)
      #print("KEGG info TABLE!!!!")
      #print(kegg_info[kegg_info$DrugName == k$Drug,])
      fulldt <- rbind(fulldt,k)
    }
    #print("after KEGG")
    if(sum(str_detect(test$Database,"Indiana University")>0))
    {  
      iu <- test[test$Database == "Indiana University",]
      iu$Extra <- "http://medicine.iupui.edu/clinpharm/ddis/main-table/"
      iu$Database <- paste0("<a href='",iu$Extra,"'>Indiana University</a>")
      iu$Extra2 <- "Not Available"
      iu$Structure <- "Not Available"
      fulldt <- rbind(fulldt,iu)
    }  
    #print("after Indi")
    if(sum(str_detect(test$Database,"ildcare")>0))
    {  
      ild <- test[test$Database == "ildcare",]
      ild$Extra <- "http://www.ildcare.eu/Downloads/artseninfo/CYP450_drug_interactions.pdf"
      ild$Database <- paste0("<a href='",ild$Extra,"'>ildcare</a>")
      ild$Extra2 <- "Not Available"
      ild$Structure <- "Not Available"
      fulldt <- rbind(fulldt,ild)
      
   }  
      
    #print("I made it here")
    #print(fulldt)
     
    #fulldt <- data.frame(fulldt[1:5])
    fulldt <- data.frame(fulldt)                          
    fulldt <- fulldt[(order(fulldt$Enzyme)),]
    list(drugs = fulldt)
    
  })
      
      
      
   #####ADVANCED TABS#####
   afound <- reactive({
    choice <- input$search
   # print("This is what was chosen")
  #  print(mytext)
  #  print("TRYING TO MAKE IT REACT!!")
    print(input$ADrug_2)
    test<-NULL
    for(i in mytext)
    {
        temp <- db[db$Drug %in% i,]
        test <- rbind(temp,test)
    }
     

    cyps <- unique(test$Enzyme)

    
    holding <- NULL
    for(i in cyps)
    {
      count = 0
      limit = 0
      for(m in unique(test$Drug))
      {
	count = 0
        for(n in unique(test$Drug))
        {
	  if(count >= limit)
	  {
          tb1 <- db[db$Drug %in% m & db$Enzyme %in% i,]
      #}
      
      #if(sum(str_detect(db$Drug, input$Drug_2)) > 0)
      #{
          tb2 <- db[db$Drug %in% n & db$Enzyme %in% i,]
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
              row2 <- c(i,paste(n,j,pt2),paste(m,tk,pt1), s)
              
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
              row2 <- c(i,paste(m,k,pt1),paste(n,j,pt2), s)
              #print(row2)
              holding <- rbind(holding,row2)
	      print("Second case loop")
              holding <- rbind(holding,row2)
	      print("This is action 1")
       	      print(action1)
              print("This is action 2")
              print(action2)
	      print("Drug 1")
	      print(n)
	      print("Drug 2")
	      print(m)
		}
            }
          }   
        }
	count = count + 1
      }
      limit = limit + 1
        }
  #        print("INSIDE LOOP")
        }
  #  print("This is hold after loop")
   # print(holding)
    #table <- rbind(row1,row2)
    #mytable <- data.frame(row)
    mytable <- matrix(holding,ncol = 4)#,byrow = TRUE)
    #print("STEP 1")
   # print(mytable)
    #if(choice == "Drug_Name")
    #{
      colnames(mytable) <- c("Enzyme","Drug_1","Drug_2","R_Score")
     # rownames(mytable) <- cyps
      #cyp <- rownames(mytable[mytable[,4] > 0,])
    #}
    #else
    #{
    # colnames(mytable) <- c(input$CYP_1,input$CYP_2,"Drug Score")

   # }
    mytable <- mytable[mytable[,4] > 0,]
    #cyp <- rownames(mytable)
 #   print("STEP 2")
 #   print(mytable)
    mytable <- matrix(mytable,ncol = 4)
    colnames(mytable) <- c("Enzyme","Drug_1","Drug_2","R_Score")
    #rownames(mytable) <- cyp
 #   print("STEP 3")
  #  print(mytable)      
    mytable <- data.frame(mytable)
  #  print("STEP 4")
   # print(mytable) 
    mytable <- mytable[(order(mytable$R_Score, decreasing = TRUE)),]
   
#Remove Duplicates
    toDelete <- seq(0, nrow(mytable),2)

    mytable <- mytable[ -toDelete,]    
    
  #  print("STEP 5")
   # print(mytable) 
  #  mytable <- mytable[!(duplicated(mytable[,2:3])),]
    #print("THIS IS THE FINAL TABLE")
    #print(mytable)
       
  })
      
      
      
      
      
      
      
  #mod <- observe({
  mod <- reactive({
    myModal = modalDialog(title=paste(modal_name),HTML(readLines(modal_view)),easyClose=TRUE,footer=paste("source:",modal_view))    
    })  
      
  modal_stuff <- reactiveValues()
   
  observeEvent(input$select_button1, {
      selectedRow <- as.numeric(strsplit(input$select_button1, "_")[[1]][2])
      
      modal_view <<- check_me()$drugs[paste(selectedRow),7]
      modal_name <<- check_me()$drugs[paste(selectedRow),1]
      
    #  print("selected Row DrugBank")
    #  print(selectedRow)
   #   print("link to row")
   #   print(modal_view)
   #   print("Drug Name")
    #  print(modal_name)
      showModal(modalDialog(title=paste(modal_name),HTML(readLines(modal_view)),easyClose=TRUE,footer=paste("source:",modal_view))    
)
      modal_name <<- NULL
      modal_view <<- NULL
  
    
    })
    observeEvent(input$select_button2, {
      selectedRow <- as.numeric(strsplit(input$select_button2, "_")[[1]][2])
     
      modal_view <<- check_me()$drugs[paste(selectedRow),7]
      modal_name <<- check_me()$drugs[paste(selectedRow),1]
     
   #   print("selected Row for SuperCYP")
    #  print(selectedRow)
   #   print("link to row")
    #  print(modal_view)
    #  print("Drug Name")
    #  print(modal_name)
      showModal(modalDialog(title=paste(modal_name),tags$img(src = modal_view,style="padding-left:200px;"),easyClose=TRUE,footer=paste("source:",modal_view))    
)
      modal_name <<- NULL
      modal_view <<- NULL
  
    
    })
    observeEvent(input$select_button3, {
      selectedRow <- as.numeric(strsplit(input$select_button3, "_")[[1]][2])
   
      modal_view <<- check_me()$drugs[paste(selectedRow),7]
      modal_name <<- check_me()$drugs[paste(selectedRow),1]
      
    #  print("selected Row for kegg")
    #  print(selectedRow)
    #  print("link to row")
    #  print(modal_view)
     # print("Drug Name")
     # print(modal_name)
      
      showModal(modalDialog(title=paste(modal_name),tags$img(src = modal_view,style="padding-left:200px;"),easyClose=TRUE,footer=paste("source:",modal_view))    
)
      modal_name <<- NULL
      modal_view <<- NULL
  
    
    })  
  

  #output$pic <- renderUI({
    
  #    HTML(readLines(modal_view))
  #})
      
  output$pic <- renderImage({ 
    plot(as.raster(modal_view))
    },deleteFile = FALSE)
      
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
      

  
      
    
  
      
  output$table2 <-renderDataTable({ 
    input$GO
 #   newtb
    
    check_me()$drugs[c(1,2,3,4,5,8)]
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
  
        
        
        
  output$advance_table2 <- renderDataTable({
  input$ADrug_1
  check_me()$drugs[c(1,2,3,4,5,8)]
  
  },escape=FALSE) 
        
  output$advance_table1 <- renderDataTable({
  afound()
  afound()
  })
      
  output$DDI_header1 <- renderText({
    
    "R Score Results: "
  })
  
  output$DDI_header2 <- renderText({
    
    "Full list of resources:"
  })
  
  output$advance_header1 <- renderText({
    
    "R Score Results: "
  })
  
  output$advance_header2 <- renderText({
    
    "Full list of resources:"
  })
	      
  output$home_header1 <- renderText({
    
    "Pharmocokinetic DDIs" 
  
})
  output$advanced_1 <- renderText({
    
    "STEP 1.)    Type in the name of the drug:" 
  
})
  output$advanced_2 <- renderText({
    
    "STEP 2.)    Select the drug name from the list:" 
  
})
 output$advanced_3 <- renderText({
    
    "Verify selected drugs below:" 
  
})
        
        
	      
  output$basic_drug_1 <- renderText({
    
    "Select Drug 1:" 
  
})
 output$basic_drug_2 <- renderText({
    
    "Select Drug 2:" 
  
})	    
	      
	      
  output$verbose <- renderText({
    found()
    
    
    })
  output$home_body1 <- renderText({
    
    "With the help of this tool it is possible to search for a drug-cocktail to check whether 
the metabolisms of the drugs interact with each other. By typing in the first few letters of the drug or utilizing the drop down selection
, you can choose 2 or more drugs from our database and determine a Reliability score. Choose one of the options on the left side bar panel to begin."

  }) 
	     
	      output$about_header1 <- renderText({
    
    "Reliability Computation for Pharmocokinetic DDI" 
  
})
	      
    output$about_body1 <- renderText({
    
    "Identifying drug-drug interaction (DDI) is an important topic for the development of safe
Pharmaceutical drugs and for the optimization of multidrug regimens for complex diseases. Poly-pharmacy increases the risk of DDIs. A pharmacokinetic interaction may occur if one drug affects the absorption, distribution, metabolism, or excretion (ADME) of another drug when two drugs given together. In this project, we construct a web server for predicting and showing possible pharmacokinetic DDIs between two drugs. We also introduced a reliability score to evaluate the probability of these DDIs. Our web server has the potential to help users better understand the mechanism of a pharmacokinetic DDI.
" 
  
})
}
shinyApp(ui,server)
