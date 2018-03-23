##install.packages('shiny')
library(shiny)
library(markdown)

shinyUI(
  
  navbarPage("Word Prediction Model",
             tabPanel("Application", 
                      
             ###############################################################################
             fluidPage(  
               fluidRow (
                column(12,align="center",
                        textAreaInput("inputtext"
                                      ,label = "Enter text here"
                                      ,value =""  
                                      ,width="300px"
                                      ,resize = "both"
                        )
                 )
               ),
               
               fluidRow (
                 column(12,align="center",
                        tags$body("Top Predictions")
                        ,tags$br(),tags$br()
                 )
               ),  
               
               
               fluidRow (
                 column(5,align="right",
                        actionButton("Button1"
                                     , label="Loading..."
                                     , width = 100
                                     , style ="color: #fff; background-color: #337ab7; border-color: #2e6da4" )
                 ),
                 column(2,align="center",
                        actionButton("Button2"
                                     ,label="Loading..."
                                     , width = 100
                                     , style ="color: #fff; background-color: #337ab7; border-color: #2e6da4" )
                 ),
                 column(5,align="left",
                        actionButton("Button3"
                                     , label="Loading..."
                                     , width = 100
                                     , style ="color: #fff; background-color: #337ab7; border-color: #2e6da4" )
                 )
               ), 
               
               fluidRow (
                 column(12,align="center",
                        tags$hr()
                 )
               ),
               
               fluidRow(
                 
                 column(12,align="center",
                        plotOutput("plot")
                 )
                 
               ),
               
               fluidRow(
                 column( 12
                         ,align="center"
                         ,tags$div("Developed by Jacob Paracka; March 2018 ", style="color: grey; font-size:small; align:center") 
                         ,tags$br()
                         ,tags$div("Coursera: Data Science Specialization - John Hopkins University", style="color: grey; font-size:small; align:center") 
                         
                 )
               )
               
             )
             ##############################################################################
                      
                               
             ),
             tabPanel("Documentation",
                      fluidPage(
                        fluidRow(
                          column(12, includeHTML("Prediction_Model_Notes.html"))
                        )
                      ) 
                              
             
             )
  )
  
  
)

  



