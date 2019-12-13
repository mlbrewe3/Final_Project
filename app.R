

#load needed libraries
library(shiny)
library(dplyr)
library(Cairo)
library(ggplot2)
library(caret)
library(cluster)
library(rpart)
library(gbm)



#get data set up
finaldata<-read.csv("finaldata.csv")
colnames(finaldata)[1] <- "Brand"
colnames(finaldata)[5] <- "Sharpener"
onclick ="window.open('http://google.com', '_blank')"

#create linear model
model1<-glm(Cuts~Brand + Sharpener + Steel:Edge.angle + Brand:Edge.angle + Brand:Steel,data= finaldata)

#create tree model
set.seed(115)
boostFit <- gbm(Cuts ~ Brand + Sharpener + Steel + Edge.angle , data = finaldata, distribution = 'multinomial', n.trees = 400, shrinkage = 0.45, interaction.depth = 5)

#set up data for clustering
selectedData<-finaldata[,c('Edge.angle','Cuts')]
selectedData<-na.omit(selectedData)

ui <- (fluidPage(
    navbarPage("Final Project",
        tabPanel('Test Information',  textOutput("testinfo")), 
        tabPanel("Data and Modeling Information", textOutput("datainfo"), 
                 uiOutput('ex1')),
        tabPanel('Data Exploration', sidebarLayout(
            sidebarPanel(
                h3("Select the knife Brand:"),
                selectizeInput("Brand", "Brand", selected = "Spyderco", choices = levels(as.factor(finaldata$Brand))),
                checkboxInput("Steel", h4("Color Code by Steel Type")),
                h3("Download the shown plot:"),
                downloadButton("export", "Download"),),
                mainPanel(plotOutput("KnifePlot", click = "plot1_click"), fluidRow(
                    column(width = 6,
                           h4("Points near click"),
                           verbatimTextOutput("click_info"),
                           h4("Average Cut for Brand"),
                           verbatimTextOutput("averagecuts")
                    ))
                    
        )))
              ,
        tabPanel('Cluster Analysis', sidebarLayout(
          sidebarPanel(
            h3("Select number of Clusters"),
            sliderInput("Clusters", "Clusters", 1, 10, value = 4)),
            mainPanel(plotOutput("Clusterplot")
          
          )
        )),
        tabPanel('Linear Modeling', sidebarLayout(
            sidebarPanel(
                h3("Select the knife Brand:"),
                selectizeInput("Knife_Brand", "Knife Brand", selected = " ", choices = levels(as.factor(finaldata$Brand))),
                selectizeInput("Blade_Steel", "Blade Steel", selected = " ", choices = levels(as.factor(finaldata$Steel))),
                selectizeInput("Sharpener", "Sharpener", selected = " ", choices = levels(as.factor(finaldata$Sharpener))),
                sliderInput("Edge_Angle", "Edge Angle", 5, 40, value = 17)
               ), mainPanel(
                             h3("Predicted Number of Cuts"),
                             h2(textOutput("fit1")),
                             plotOutput("plot2")))),
        tabPanel('Tree Modeling', sidebarLayout(
          sidebarPanel(
            h3("Select the knife Brand:"),
            selectizeInput("Knife_Brand2", "Knife Brand", selected = " ", choices = levels(as.factor(finaldata$Brand))),
            selectizeInput("Blade_Steel2", "Blade Steel", selected = " ", choices = levels(as.factor(finaldata$Steel))),
            selectizeInput("Sharpener2", "Sharpener", selected = " ", choices = levels(as.factor(finaldata$Sharpener))),
            sliderInput("Edge_Angle2", "Edge Angle", 5, 40, value = 17)
          ), mainPanel(
            h3("Predicted Number of Cuts")
            ,
            h2(textOutput("boostpred"))
            )
            ) 
          ),
        tabPanel('Data Table',  sidebarLayout(
            sidebarPanel(
                h3("Download the shown table:"),
                downloadButton("downloadData", "Download"),
                tags$div(class="header", checked=NA,
                  tags$p("If you would like to look at all the data then,"),
                  tags$a(href="https://docs.google.com/spreadsheets/d/1b_rNfdJnL9oyn-JoL9yUHhUmDLAP1hJ1dN_0q5G4tug/htmlview#", "Click Here!")
                )
            ),mainPanel( 
            tableOutput("table"))))
        )
        )
)
  



server <- function(input, output, session) {
    
#create reactive data based on brand filter
getData <- reactive({
        newData<- finaldata%>% filter(Brand == input$Brand)
})
    
#create table of filtered data
output$table<-renderTable({
      getData()
})
    
# create plot for filtered data
output$KnifePlot <- renderPlot({
        newData <-getData()
        
        g <- ggplot(newData, aes(x = Edge.angle, y = Cuts))
        if(input$Steel){
            g + geom_point(aes(col = Steel))
        } else {
            g + geom_point()
        }
  }
)

#create fitting for clustering
fit <- reactive({
      kmeans(selectedData, input$Clusters)
})
 
#create cluster plot   
output$Clusterplot <- renderPlot({
      fit<-fit()
      clusplot(selectedData, fit$cluster)
})
    
#text for model info
output$testinfo<- renderText({
  paste("The purpose of this is app is to allow users to investigate cut test from various knife brands.  The data used in this app has been collected from a spreadsheet created by YouTube channel creator Cedric and Ada, there is a link to his data on the Data Table tab.  To create this data Pete (owner of Cedric and Ada channel) uses a knife to cut 10mm twisted sisal rope.  Every so often stop to test the edge holding on a sheet of printer paper.  He does this until it no longer reliably cuts a sheet of printed paper.  This test is intended to display the edge holding of the steel the edge of the knife is made of.")})

#text for data info
output$datainfo<- renderText({  
  ("There are many variables that go into the edge holding of a steel and Pete does a good job of documenting most of these variables, like brand, knife, steel type, type of sharpener used, the style of sharpening, edge angle, number of cuts and many other jvariables.  For this app I will only be using a few of them though.  They will be brand, knife, steel type, edge angle, sharpener and number of cuts.  These were chosen for now because they are consistently available for most tests and do a good job of explaining the data.  I would like to include variables like HRC and behind the edge thickness, but since they are not available for many of the knives, I will leave it out.  In this app users will be able to look at the data and graphs for a given brand of knife and look at two different types of predictive modeling for the number of cuts a blade will do based on the given variables earlier.  The first model is a glm model with Cuts=Brand + Sharpener + Steel:Edge.angle + Brand:Edge.angle + Brand:Steel.  The second is a boosted tree model with using just Brand, Sharpener, Steel and Edge Angle to predict Cuts.")
})

#set up click info
output$click_info <- renderPrint({
    newData<- finaldata%>% filter(Brand == input$Brand)
     nearPoints(newData, input$plot1_click, addDist = TRUE)
 })

#download button for dataset
output$downloadData <- downloadHandler(
    filename = function() {
        paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
    }
)

#mathjax for mse
output$ex1 <- renderUI({
  withMathJax(helpText('The purpose of the Boosted Tree is to minimize Mean Square Error:  $$\\frac{1}{n}\\, \\sum_{i}^n\\, \\left(\\hat{y}_i - y_i\\right)^2$$'))
})

#set up data for models
data<-reactive({
data.frame(Brand=input$Brand,Knife=input$Knife,Steel=input$Steel,Sharpener=input$Sharpener,Edge.angle=input$Edge.angle)})

#create reactive boosted model
boostpred<-reactive({
  BrandInput2 <- input$Knife_Brand2
  SharpenerInput2 <- input$Sharpener2
  SteelInput2 <- input$Blade_Steel2
  Edge_angleInput2 <- input$Edge_Angle2
  
  predict(boostFit, newdata= 
                               data.frame(Brand= BrandInput2,
                                          Sharpener = SharpenerInput2,
                                          Steel = SteelInput2,
                                          Edge.angle = Edge_angleInput2
                               ), n.trees = 400)
})

#text for boosted prediction
output$boostpred<-renderText(colnames(boostpred())[apply(boostpred(), 1, which.max)])

#avg cut for brand
output$averagecuts<-renderPrint({
  newData <-getData()
  mean(newData$Cuts, na.rm = TRUE)
})

#create reactive glm model
fit1pred <- reactive({
BrandInput <- input$Knife_Brand
SharpenerInput <- input$Sharpener
SteelInput <- input$Blade_Steel
Edge_angleInput <- input$Edge_Angle

predict(model1, newdata= 
                     data.frame(Brand= BrandInput,
                     Sharpener = SharpenerInput,
                     Steel = SteelInput,
                     Edge.angle = Edge_angleInput
          ))
})

#text for glm model prediction
output$fit1 <- renderText({
  fit1pred()
})

#create plot with glm
output$plot2 <- renderPlot({
  BrandInput <- input$Knife_Brand
  SharpenerInput <- input$Sharpener
  SteelInput <- input$Blade_Steel
  Edge_angleInput <- input$Edge_Angle
  
  plot(finaldata$Edge.angle, finaldata$Cuts, xlab = "Edge Angle", ylab = "Number of Cuts", bty="n", pch=16)
  
  abline(model1, col="red", lwd=2)
  
})

#download button for plot
output$export = downloadHandler(
  filename = function() {"plots.pdf"},
  content = function(file) {
    ggsave(file, device = "pdf", width=11, height=8.5)
    
  }
)

}

# Run the application 
shinyApp(ui = ui, server = server)
