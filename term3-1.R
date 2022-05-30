library(shiny)
library(dplyr)
library(tableHTML)
library(magrittr) # for the %<>% pipe
library(stringr)
library(tidyverse)
library(tools)
library(devtools)
library(DT)

train <- read.delim("train.txt",header=FALSE, blank.lines.skip = FALSE,col.names = c('Word','BIO'))
train2<- train

#Cada caracter "nulo" ("") es el fin de un abstract -> introducimos un EOL
train2$Word <- replace(train$Word, train$Word  == "", "\n\n")

# Contar el Nº de abstracts 
nabs <- length(which(train2$Word == "\n\n"))
ids <- 1:nabs

# Etiquetar con el abstractID
l <- length(train2$Word)
vec <- 1:l
train2$AbstractID <- vec 

temp = 1
j = 1
for(i in train2$Word){
  train2[j, "AbstractID"] <- temp
  if(i=="\n\n"){
    temp = temp + 1
  }
  if(is.na(i)){
    break
  }
  j = j + 1
}



highlight <- '
function getSelectionText() {
  var text = "";
  if (window.getSelection) {
      text = window.getSelection().toString();
    } else if (document.selection) {
        text = document.selection.createRange().text;
    }
  return text;
}

document.onmouseup = document.onkeyup = document.onselectionchange = function() {
  var selection = getSelectionText();
  Shiny.onInputChange("mydata", selection);
};

'

coded_text <- character(0)

ui3 <- bootstrapPage(
  tags$script(highlight),
  #Fondo blanco al seleccionar fila
  tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
  fluidRow(
    column(8,
           tags$h1("Train file editor for terminology extraction"),
           tags$h3("Abstracts from the train file:"),
           #htmlOutput("table")
           DT::dataTableOutput("table")
    ),
    column(3,
           tags$h2("File editor"),
           actionButton("upload", "Upload Data"),
           actionButton("code1", "Assign word as B-KEY"),
           actionButton("code2", "Assign word as I-KEY"),
           verbatimTextOutput("selected_text"),
           verbatimTextOutput("key"),
           verbatimTextOutput("key2"),
           downloadButton("download","Download train file"),
           DT::dataTableOutput("table2")
           
    )
  )
)
p <- data.frame(AbstractID = ids, 
                Abstract = strsplit(do.call(paste, c(train2$Word, list(collapse=","))), "\n\n"), 
                Ntokens = 0, 
                NChanges = 0)

colnames(p) = c("ID","Abstract","NTokens","NChanges")

markkeys <- function(){
  cont=1
  for(i in train2$Word){
    if(train2$BIO[cont]=="B-KEY"){
      train2$Word[cont]<-paste0('<span style="background-color:yellow">',train2$Word[cont],'</span>')
    }
    if(train2$BIO[cont]=="I-KEY"){
      train2$Word[cont]<-paste0('<span style="background-color:#D6EEEE">',train2$Word[cont],'</span>')
    }
    cont = cont + 1
  }
  p <- data.frame(AbstractID= ids, 
                  Content = strsplit(do.call(paste, c(train2$Word, list(collapse=","))), "\n\n"), 
                  Ntokens =  str_count(p$Abstract, '\\s+')+1, 
                  NChanges = 0)
  colnames(p) = c("ID","Abstract","NTokens","NChanges")
  return(p)
}




p <- markkeys()
p
#pd <- datatable(p,escape = FALSE,rownames= FALSE,selection = 'single', options = list(pageLength = 3))

server3 <- function(input, output) {
  
  RV <- reactiveValues(data = train2)
  RP <- reactiveValues(data = p)
  
  output$table <- DT::renderDataTable(RP$data,selection = "single",escape = FALSE, options = list(pageLength = 3),rownames = FALSE)
  output$table2 = DT::renderDataTable({
    RV$data
  })
  

  coded <- eventReactive(input$code1, {
    coded_text <<- c(coded_text, input$mydata)
    coded_text
  })
  
  output$selected_text <- renderPrint({
    coded()
  })
  
  keyed <- eventReactive(input$code1,{
    keyed_text <<- c(train2$BIO[which((train2$Word == input$mydata)&(train2$AbstractID == input$table_rows_selected))])
    index <- which((train2$Word == input$mydata)&(train2$AbstractID == input$table_rows_selected) , arr.ind = TRUE)
    absid <- input$table_rows_selected
    msb <- "Success B-KEY"
    
    if(RV$data[index[1],"BIO"] == "B-KEY"){
      msb <- "Key is already B-KEY"
    }
    ifelse(index == NULL){
      msb("Word not found, please try again")
    }
    else{
      for(i in index){
        RV$data[i,"BIO"] <- "B-KEY"
      }
      RP$data[absid,"NChanges"] <- RP$data[absid,"NChanges"]+1
    }
    
    msb
  })
  
  keyed2 <- eventReactive(input$code2,{
    keyed_text <<- c(train2$BIO[which((train2$Word == input$mydata)&(train2$AbstractID == input$table_rows_selected))])
    index <- which((train2$Word == input$mydata)&(train2$AbstractID == input$table_rows_selected) , arr.ind = TRUE)
    absid <- input$table_rows_selected
    msi <- "Success I-KEY"
    for(i in index){
      RV$data[i,"BIO"] <- "I-KEY"
    }
    RP$data[absid,"NChanges"] <- RP$data[absid,"NChanges"]+1
    msi
  })
  
  output$key <- renderPrint({
    keyed()
  })
  output$key2 <- renderPrint({
    keyed2()
  })

  
  
  output$download <- downloadHandler(
    filename = "BIOFile.csv",
    content = function(file) {
      write.csv(RV$data, file,row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui = ui3, server = server3)
