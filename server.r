

source("wordprediction.R", local=TRUE)

shinyServer(  
  function(input, output, session) {    
   
     predictions <- reactive({
       #predict the next word if the last character entered is a space 
       if(substring(input$inputtext, nchar(input$inputtext))==" ") {
        a <- c(input$inputtext , substring(input$inputtext, nchar(input$inputtext)), "abc" ) 
        n <- c(1,1,1)
        data.frame(a,n)
        #nextword(input$inputtext, n=20, currenttword=FALSE)
         nextword(input$inputtext)
       }
       #else predict the word that is being typed.
      else{
        word <- unlist(strsplit(input$inputtext, split=" "))[length(unlist(strsplit(input$inputtext, split=" ")))]
        nextword(word, n=20, currenttword=TRUE)
        }

              
    })
    
    #output$prediction1 <- renderText(predictions()[1,1])
    #output$prediction2 <- renderText(predictions()[2,1])
    #output$prediction3 <- renderText(predictions()[3,1])
    #output$newtext <- renderText(paste(input$inputtext, input$Prediction1, sep=" "))
    
    
    
      observeEvent(input$inputtext, {
      p <- unique(predictions()[,1])  
      updateActionButton(session, "Button1", label= p[1]) # predictions()[1,1])
      updateActionButton(session, "Button2", label= p[2]) # predictions()[2,1])
      updateActionButton(session, "Button3", label= p[3])  #predictions()[3,1])
    })
    
    
    
    observeEvent(input$Button1, {
      #currentword<-FALSE
      if(substring(input$inputtext, nchar(input$inputtext))==" ") {currentword<-FALSE} else {currentword<-TRUE}
      
      if(currentword) {
        pwords <- unlist(strsplit(input$inputtext, split=" "))
        newtext <- paste( paste(pwords[1:length(pwords)-1], collapse=" "), predictions()[1,1], "",   sep=" ") 
      }
      else {
        newtext <- paste( input$inputtext, predictions()[1,1], " ",  sep="")
      }
      updateTextInput(session, "inputtext", value=newtext)
      })
    
    
    
    
    observeEvent(input$Button2, {
      if(substring(input$inputtext, nchar(input$inputtext))==" ") {currentword<-FALSE} else {currentword<-TRUE}
      
      if(currentword) {
        pwords <- unlist(strsplit(input$inputtext, split=" "))
        newtext <- paste( paste(pwords[1:length(pwords)-1], collapse=" "), predictions()[2,1], "",   sep=" ") 
      }
      else {
        newtext <- paste( input$inputtext, predictions()[2,1], " ",  sep="")
      }
      updateTextInput(session, "inputtext", value=newtext)
    })
    
    observeEvent(input$Button3, {
      if(substring(input$inputtext, nchar(input$inputtext))==" ") {currentword<-FALSE} else {currentword<-TRUE}
      
      if(currentword) {
        pwords <- unlist(strsplit(input$inputtext, split=" "))
        newtext <- paste( paste(pwords[1:length(pwords)-1], collapse=" "), predictions()[3,1], "",   sep=" ") 
      }
      else {
        newtext <- paste( input$inputtext, predictions()[3,1], " ",  sep="")
      }
      updateTextInput(session, "inputtext", value=newtext)
    })
      
    
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
      #req(predictions()[,1])
      wordcloud_rep(predictions()[,1], predictions()[,2], scale=c(8,2),
                    min.freq = 2, max.words=15,
                    colors=brewer.pal(8, "Dark2"), fixed.asp=FALSE, rot.per=0)
    })
    
    freq_rep <- repeatable(barplot)
    output$plot1 <- renderPlot({
      #req(predictions()[,1])
      freq_rep (predictions()[,2], names.arg=predictions()[,1]) 
    })
    
    
     }
)