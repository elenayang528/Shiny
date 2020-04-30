#Load the Library
library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(ggplot2)
library(reshape2)
library(forecast)
library(data.table)
library(DT)
library(dplyr)
library(stringi)
library(magrittr)
library(ggcorrplot)
load(url("https://raw.githubusercontent.com/elenayang528/Shiny/master/ncreif_return.RData"))
#ui----------------------------------------------------------------------------------------------------------
# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  titlePanel("NCREIF Historical Data Analysis"),
  # Sidebar layout
  sidebarLayout(
    #Inputs: Select which inputs from the data we want to display
    sidebarPanel(
      selectizeInput(inputId = "Reg",
                     label = "Choose 2 Index Types",
                     choices = c("National","East","West","South","Middle","Hotel","Apartment","Retail","Industrial","Office"), 
                     multiple = TRUE,
                     options = list(maxItems = 2),
                     selected = c("National","East")),
      tags$hr(style="border-color: black;"), 
      
      selectInput(inputId = "min", 
                  label = "From Date:", 
                  choices = t$Date,
                  selected= t$Date[1]),
      
      selectInput(inputId = "max",
                  label = "To Date:", 
                  choices = t$Date,
                  selected= t$Date[-1]),
      tags$hr(style="border-color: black;"), 
      selectInput(inputId = "color_1", 
                  label = "Trend: Plot_1 Color:",
                  choices = c("Red", "Blue", "Black", "Green"), 
                  selected = "Black"),
      selectInput(inputId = "color_2", 
                  label = "Trend: Plot_2 Color",
                  choices = c("Red", "Blue", "Black", "Green"), 
                  selected = "Red"),
      sliderInput(inputId = "size", 
                  label = "Trend Line Size:", 
                  min = 0.5, max = 3.5, 
                  value = 1),
      tags$hr(style="border-color: black;"), 
      sliderInput(inputId = "h", 
                  label = "ARIMA Forecast Period:", 
                  min = 1, max = 12, 
                  value = 5)
      
    ),
    
    #Output: Type of plot
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",verbatimTextOutput("sum"),fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), plotOutput("box1"), plotOutput("box2")))),
        tabPanel("Correlation",verbatimTextOutput("corr"),plotOutput("corrplot")),
        tabPanel("Trend",fluidRow(splitLayout(plotOutput("graph1"))),
                 fluidRow(splitLayout(plotOutput("graph2")))),
        tabPanel("ARIMA Forecast",fluidRow(splitLayout(plotOutput("arima1"))),
                 fluidRow(splitLayout(plotOutput("arima2")))),
        tabPanel("Data",  DT::dataTableOutput(outputId="datasheet"))
      )
    )
  )
)
#server-------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
  dat1 <- reactive({
    ds1 <- t[which(t$Date== input$min):which(t$Date== input$max), ]%>%
      select(input$Reg)
    return(ds1)
  })
  output$sum <- renderPrint({
    d1<-subset(dat1())
    summary(d1)
  })
  output$corr <- renderPrint({
    d1<-subset(dat1())
    round(cor(d1),3)
  })
  output$corrplot <- renderPlot({
    d1<-subset(dat1())
    ggcorrplot(round(cor(d1),3),method = "circle")
  })    
  
  
  output$box1 <- renderPlot({
    d2<- subset(dat1())%>%pull(input$Reg[1])
    x1 <- summary(d2)
    boxplot(x1,col="red",border="black",xlab= input$Reg[1], ylab="Returns(%)")
  }) 
  output$box2 <- renderPlot({
    d3<- subset(dat1())%>%pull(input$Reg[2])
    x2 <- summary(d3)
    boxplot(x2,col="red",border="black",xlab= input$Reg[2], ylab="Returns(%)")
  }) 
  
  dat2 <- reactive({
    ds2 <- t[which(t$Date== input$min):which(t$Date== input$max), ]%>%
      select("Date",input$Reg)
      return(ds2)
  })
  
  output$graph1 <- renderPlot({
    d4<-subset(dat2())
    y_input <- d4 %>%pull(input$Reg[1])
    date_input <- as.Date(d4$Date, "%m/%d/%Y")
    ggplot(data = d4, aes(x=date_input,y=y_input, group=1)) + geom_line(color=input$color_1,size=input$size)+
      theme(axis.text.x=element_text(angle=+90, hjust=0.001))+
      scale_x_date("Date", breaks = "2 years")+ylab(input$Reg[1])
  })
  output$graph2 <- renderPlot({
    d5<-subset(dat2())
    y_input <- d5 %>% pull(input$Reg[2])
    date_input <- as.Date(d5$Date, "%m/%d/%Y")
    ggplot(data = d5, aes(x=date_input,y=y_input, group=1)) + geom_line(color=input$color_2,size=input$size)+
      theme(axis.text.x=element_text(angle=+90, hjust=0.001))+
      scale_x_date("Date", breaks = "2 years")+ylab(input$Reg[2])
  })
  
  output$arima1 <- renderPlot({
    d6<-subset(dat2()) %>%
      pull(input$Reg[1])
    M21=auto.arima(d6)
    M2F1=forecast(M21,h=input$h)
    plot(M2F1,main="ARIMA Forecast: Var_1")
  })
  output$arima2 <- renderPlot({
    d7<-subset(dat2()) %>%
      pull(input$Reg[2])
    M22=auto.arima(d7)
    M2F2=forecast(M22,h=input$h)
    plot(M2F2,main="ARIMA Forecast: Var_2")
  })
  
  output$datasheet<-DT::renderDataTable({
    DT::datatable(data=t[,1:5],
                  options=list(pageLength= 20),
                  rownames=FALSE)
  })
  
}
#--------------------------------------------------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)