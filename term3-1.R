#Librerías necesarias
library(shiny)
library(dplyr)
library(tableHTML)
library(magrittr) # for the %<>% pipe
library(stringr)
library(tidyverse)
library(tools)
library(devtools)
library(DT)

#Función Javascript para marcar el texto

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

# --------- INTERFAZ DE USUARIO ---------------
ui3 <- bootstrapPage(
  tags$script(highlight),
  #Fondo rosa al seleccionar fila
  tags$style(HTML('table.dataTable tr.selected td, 
                  table.dataTable td.selected 
                  {background-color: #FFE7D9 !important;}')),
  titlePanel("File editor for terminology extraction"),
  sidebarLayout(
    sidebarPanel(width = 3, align = "center",
      fileInput("file1", "Choose File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      actionButton("Bkey", "Tag word as B-KEY", 
                   style='width: 150px; background-color: #FDFF82;display:inline-block'),
      br(),
      br(),
      actionButton("Ikey", "Tag word as I-KEY", 
                   style='width: 150px; background-color: #D6EEEE;display:inline-block'),
      br(),
      br(),
      actionButton("Okey", "Tag word as O",
                   style='width: 150px'),
      br(),
      br(),
      span(textOutput("message"), 
           style="font-weight: bold; font-size: 14px"),
      tags$hr(),
      downloadButton("download","Download file", style='width: 150px'),
      #DT::dataTableOutput("table2")
    ),
  mainPanel(width = 9,
           DT::dataTableOutput("table")
    )
  )
)

# --------- SERVIDOR ---------------

server3 <- function(input, output){
  
  # -------- Carga y preprocesado del documento --------
  vals <- reactiveValues(x = NULL)
  msg <- reactiveVal()
  observe({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tryCatch(
      {
    d <- read.delim(input$file1$datapath,
                    header=FALSE,
                    blank.lines.skip = FALSE,
                    col.names = c('Word','BIO'))
    
    d$Word <- replace(d$Word, d$Word  == "", "\n\n")
    l <- length(d$Word)
    vec <- 1:l
    d$AbstractID <- vec 
    
    temp = 1
    cond <- c(FALSE, (d[-nrow(d),1] == "\n\n"))
    r <- d[,3]
    for(i in 1:length(d$AbstractID)){
      r[i] <- temp
      if (cond[i]){
        temp = temp + 1
      }
    }
    
    d[,3]  <- r
    d_disp <- d
    nabs <- length(which(d_disp$Word == "\n\n"))
    ids <- 1:nabs
    p <- data.frame(AbstractID = ids, 
                    Content = strsplit(do.call(
                      paste, c(d_disp$Word, list(collapse=","))), "\n\n"
                    ), 
                    Ntokens = 0, 
                    NChanges = 0)
    colnames(p) = c("ID","Content","NTokens","NChanges")

 
    
    # ----- Highlight Keys ------
    cont=1
    for(i in d_disp$Word){
      if(d_disp$BIO[cont]=="B-KEY"){
        d_disp$Word[cont]<-paste0(
          '<span style="background-color:#FDFF82">',d_disp$Word[cont],'</span>'
        )
      }
      if(d_disp$BIO[cont]=="I-KEY"){
        d_disp$Word[cont]<-paste0(
          '<span style="background-color:#D6EEEE">',d_disp$Word[cont],'</span>'
        )
      }
      cont = cont + 1
    }
    
    p <- data.frame(AbstractID= ids, 
                    Content = strsplit(
                      do.call(paste, c(d_disp$Word, list(collapse=","))), "\n\n"), 
                    Ntokens =  str_count(p$Content, '\\s+')+1, 
                    NChanges = 0)
    colnames(p) = c("ID","Content","NTokens","NChanges")
    
    # Guardamos los dataframes resultantes en una lista:
    # A -> Dataframe con los datos, el que se guardará (sin tags html)
    # B -> Dataframe con los abstracts preprocesados (lo que verá el usuario) 
    # C -> Dataframe para el display que lleva las etiquetas html (no se descarga)
    combo <- list(a = d, b = p, c = d_disp)
    vals$x <- combo
    
    
      },
    error=function(cond){
      msg("Error in file format")
    })
  })
  
  # ------- Mostrar la tabla ------------
  # TABLA PROVISIONAL PARA VER LOS CAMBIOS EN EL FICHERO
  # output$table2 = DT::renderDataTable({
  #   DT::datatable(vals$x[['a']],escape = FALSE)
  # })
  
  output$table <- DT::renderDataTable({
    DT::datatable(vals$x[['b']],
                  selection = "single",
                  escape = FALSE, 
                  options = list(pageLength = 3),
                  rownames = FALSE)
    }
  )
  
  # ------ BOTÓN B-KEY ----------------
  observeEvent(input$Bkey,{
    tryCatch(
      {
        keyed_text <<- c(vals$x[['a']]$BIO[
          which(
            (vals$x[['a']]$Word == input$mydata)&(vals$x[['a']]$AbstractID == input$table_rows_selected)
          )])
        index <- which(
          (vals$x[['a']]$Word == input$mydata)&(vals$x[['a']]$AbstractID == input$table_rows_selected),
          arr.ind = TRUE)
        absid <- input$table_rows_selected
        if(vals$x[['a']]$BIO[index[1]]=="B-KEY"){
          msg("Token already tagged as B-KEY")
        }
        else{
          for(i in index){
            vals$x[['a']]$BIO[i] <- "B-KEY"
            vals$x[['c']]$Word[i]<-paste0(
              '<span style="background-color:#FDFF82">',vals$x[['a']]$Word[i],'</span>'
            )
          }
          vals$x[['b']]$Content[absid] <- strsplit(do.call(paste, c(vals$x[['c']]$Word[vals$x[['c']]$AbstractID == absid], list(collapse=","))), "\n\n") 
          vals$x[['b']]$NChanges[absid] <- vals$x[['b']]$NChanges[absid] + 1
          
          msg("Success!")
        }
      },
      error=function(cond) {
        msg("ERROR: Word not found, please try again")
      }
    ) 
  })
  
  # -------- BOTÓN I-KEY ---------
  observeEvent(input$Ikey,{
    tryCatch(
    {
      keyed_text <<- c(vals$x[['a']]$BIO[
      which(
        (vals$x[['a']]$Word == input$mydata)&(vals$x[['a']]$AbstractID == input$table_rows_selected)
      )])
    index <- which(
      (vals$x[['a']]$Word == input$mydata)&(vals$x[['a']]$AbstractID == input$table_rows_selected),
      arr.ind = TRUE)
    
    absid <- input$table_rows_selected
    if(vals$x[['a']]$BIO[index[1]]=="I-KEY"){
      msg("Token already tagged as I-KEY")
    }
    else{
      for(i in index){
        if(vals$x[['a']]$BIO[i]=="I-KEY" || vals$x[['a']]$BIO[i]=="B-KEY")
        vals$x[['a']]$BIO[i] <- "I-KEY"
        vals$x[['c']]$Word[i]<-paste0(
          '<span style="background-color:#D6EEEE">',vals$x[['a']]$Word[i],'</span>')
      }
      
      vals$x[['b']]$Content[absid] <- strsplit(do.call(paste, c(vals$x[['c']]$Word[vals$x[['c']]$AbstractID == absid], list(collapse=","))), "\n\n") 
      vals$x[['b']]$NChanges[absid] <- vals$x[['b']]$NChanges[absid] + 1
      
      msg("Success!")
    }
    
    },
    error=function(cond){
      msg("ERROR: Word not found, please try again")
    }
    ) 
  })
  
  
  # -------- BOTÓN O-KEY ---------
  observeEvent(input$Okey,{
    tryCatch(
      {
      keyed_text <<- c(vals$x[['a']]$BIO[
      which(
        (vals$x[['a']]$Word == input$mydata)&(vals$x[['a']]$AbstractID == input$table_rows_selected)
      )])
      index <- which(
        (vals$x[['a']]$Word == input$mydata)&(vals$x[['a']]$AbstractID == input$table_rows_selected),
        arr.ind = TRUE)
    
      absid <- input$table_rows_selected
      if(vals$x[['a']]$BIO[index[1]]=="O"){
        msg("Token already tagged as O")
      }
      else{
        for(i in index){
          vals$x[['a']]$BIO[i] <- "O"
          vals$x[['c']]$Word[i] <- paste0(vals$x[['a']]$Word[i])
        }
        vals$x[['b']]$Content[absid] <- strsplit(do.call(paste, c(vals$x[['c']]$Word[vals$x[['c']]$AbstractID == absid], list(collapse=","))), "\n\n") 
        vals$x[['b']]$NChanges[absid] <- vals$x[['b']]$NChanges[absid] + 1
        
        msg("Success!")
      }
    },
    error=function(cond){
      msg("ERROR: Word not found, please try again")
    }
  ) 
  })
  
  #Mensaje de feedback al usuario
  output$message <- renderText({
    msg()
  })
  
  #Botón de descarga
  output$download <- downloadHandler(
    filename = "BIOFile.txt",
    content = function(filed) {
      write.table(vals$x[['a']][, c("Word", "BIO")], 
                filed,
                row.names = FALSE,
                col.names = FALSE)
    }
  )
}

shinyApp(ui = ui3, server = server3)
