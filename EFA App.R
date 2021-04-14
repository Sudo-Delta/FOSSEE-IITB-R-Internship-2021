library(shiny)
library(readxl)
library(tidyverse)
library(REdaS)
library(psych)
library(nFactors)

#Cleaned data
cleaned <- read_excel("D:/R scripts for internship/cleaned.xlsx")

#Making group / subset of question
spoken<-cleaned %>%
    select(contains("spoken"))

howTo<-cleaned %>%
    select(contains("how to use"))

experience<-cleaned[,c(29:43)]


#lists to be displayed in app
data.subset<-c("howTo","spoken","experience")
rotations<-c( "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", "cluster")

#ui of the app
ui<-fluidPage(
    titlePanel("App for Exploratory factor analysis"),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput("dataset","Choose Dataset",choices = data.subset),
            
            textOutput("Scree_text"),
            actionButton("Scree","Plot", class = "btn-block btn-success"),
            
            numericInput("nfactor", label = "Number for factors", value = 3),
            selectInput("rotation", "Choose Rotation", choices = rotations),
            
            textOutput("factor"),
            actionButton("factor.diagram", "Create Factor Diagram",class ="btn-block btn-success"),
            
            textOutput("bartletts"),
            verbatimTextOutput("bartletts.test") #area for bartletts test
        ),
        mainPanel(
            verbatimTextOutput("KMO"),#area for kmo test
            plotOutput("plot",width = 700, height = 500) #plot area
            
        )
    )
)

server<- function(input, output, session){
    
    #simple text outputs for ui
    output$bartletts<-renderText("bartletts shphere test")
    output$Scree_text<-renderText("Generate Scree Plot")
    output$factor<-renderText("Make factor Diagram")
    
    #KMO test
    output$KMO<-renderPrint({
        x<-get(input$dataset)
        KMO(x)
    })
    
    #bartletts test
    output$bartletts.test<-renderPrint({
        x<-get(input$dataset)
        bart_spher(x)
    })
    
    #Plotting function for Scree_plot -> plot(id)
    observeEvent(
        input$Scree, {
            df<-get(input$dataset)
            ev <- eigen(cor(df))
            ap <- parallel(subject=nrow(df),var=ncol(df),rep=100,cent=.05)
            nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
            output$plot<-renderPlot(plotnScree(nS))
        }
    )
    
    #plotting function for factor diagram -> plot(id)
    observeEvent(
        input$factor.diagram, {
            df<-get(input$dataset)
            m1<-fa(df, nfactors = input$nfactor, rotate = input$rotation)
            output$plot<-renderPlot(fa.diagram(m1))
        }
    )
    
    
}

#starting the app
shinyApp(ui, server)