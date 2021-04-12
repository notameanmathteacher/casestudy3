library(shiny)
library(ggplot2)
library(leaflet)


ui = fluidPage(
  titlePanel("Audio Features from MARSYAS and Geographic Origin"),
  navbarPage("Case Study 3: Machine Learning",
             tabPanel(icon('home'),
                      fluidRow(column(
                        br(),
                        p("In the data a correlation between audio features and latitude/longitude is show by looking at the regression modeling tab and selecting all. The multiple R squared results in 0.3769, which yields a correlation coefficient of around 0.5. Which shows there is a slight positive correlation between all audio features and the geographic location. Other predictor variables can be assigned to see which ones have a stronger/weaker correlation relationship with location.

Lastly, an attempt at making a predictor for audio tracks that are processes through MARSYAS is included at the end. Uploading a .txt file with the audio feature data, and selecting which linear regression model you want to use can predict where geographically the audio track is from.

The algorithm selected was regression because the data is continuous (both audio features and latitude/longitude) and the data is supervised since the outputs (location are known and is data given to us in the data set.

Regression is a way to estimate the relationship between predictors and outputs. In this example the predictors were the audio features and the output was the location. Estimators for the mean response of the predictors (in other words, how change in 1 unit of the audio features changes the response/output on average). These predictors are the parameters for a fitted regression line they are not exact, as it does not match the data exactly, but it gives a general idea. This equation can be used to make predictions from data, in this example a set of audio feature data from MARSYAS can be inputted, and the regression model can predict where in the world the song is from.", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"), width=10
                      ))),
             tabPanel("Initial Visualization",
                      fluidRow(column(width=12,
                                 sliderInput(
                                   inputId = "num",
                                   label = "Choose an audio feature",
                                   value = 1,
                                   min = 1,
                                   max = 116,
                                   step = 1
                                 ),
                                 radioButtons(
                                   inputId = "dir",
                                   label = "choose latitutde or longitude",
                                   choices = c('lat', 'long')
                                 ),
                                 plotOutput('out'),
                               ),
                               )
                      
                      ),
             tabPanel("Regression Modeling",
                      fluidRow(column(width=12,
                                      selectInput(
                                        inputId = 'af',
                                        label = 'Choose which audio feature to model (or all)',
                                        choices = c("all", 1:116),
                                        multiple = TRUE
                                      ),
                                      radioButtons(
                                        inputId = "dir1",
                                        label = "choose latitutde or longitude",
                                        choices = c('lat', 'long')
                                      ),
                                      plotOutput('reg'),
                                      verbatimTextOutput('text')
                                      ),
                        
                      )
               
             ),
             tabPanel("Prediction",
                      fluidRow(column(width=12,
                                      selectInput(
                                        inputId = 'af2',
                                        label = 'Choose which audio feature to model (or all)',
                                        choices = c("all", 1:116),
                                        multiple = TRUE
                                      ),
                                      radioButtons(
                                        inputId = "dir2",
                                        label = "choose latitutde or longitude",
                                        choices = c('lat', 'long')
                                      ),
                                      uiOutput('auft'),
                                      verbatimTextOutput('pred')
                                      )))
             ),
  
)

server = function(input, output, session) {
  #import data and clean it
  data = read.table(file="default_plus_chromatic_features_1059_tracks.txt", sep=",", quote="", comment.char="")
  names(data)[ncol(data)] <- "long"
  names(data)[ncol(data)-1] <- "lat"

  #scatter plot output
  output$out = renderPlot({
    if(input$dir == "lat"){
      l = 117
      ylabel = "Latitude"
    } else{
      l = 118
      ylabel = "Longitude"
    }
    xlabel = paste("Audio Feature ", input$num)
    plot(data[,input$num], data[,l], main=paste(xlabel, " vs. ", ylabel), xlab= xlabel, ylab= ylabel)
  })
  
  #regression output
  output$reg = renderPlot({
    if(input$dir1 == "lat"){
      l = 117
      ylabel = "Latitude"
    } else{
      l = 118
      ylabel = "Longitude"
    }
    xlabel = paste("Audio Feature ", input$af)
    afs = as.numeric(as.vector(input$af))
    if(input$af=='all'){
        plot(data)
    } else if(length(afs)>1){
        df = data.frame(matrix(vector(mode='numeric', length = 1), nrow=1059, ncol=1))
        j=1
          for (i in afs) {
            df[,j] = data[,i]
            colnames(df)[j] = paste("audio feature ", as.character(i))
            j = j+1
          }
      df$lat = data$lat
      df$long = data$long
      plot(df)
  }
     else{
        plot(data[, afs], data[, l], main=paste(xlabel, " vs. ", ylabel), xlab= xlabel, ylab= ylabel)
        SLR = lm(data[,l]~data[,afs])
        abline(summary(SLR)$coefficients[1,1], summary(SLR)$coefficients[2,1])
      }
  })
  
  #output print
  output$text = renderPrint({
    afs = as.numeric(as.vector(input$af))
    if(input$dir1 == "lat"){
      l = 117
      ylabel = "Latitude"
    } else{
      l = 118
      ylabel = "Longitude"
    }
    if(input$af=='all'){
      MLR = lm(lat+long~., data = data)
      summary(MLR)
    }
    else if(length(afs)>1){
      df = data.frame(matrix(vector(mode='numeric', length = 1), nrow=1059, ncol=1))
      j=1
      for (i in afs) {
        df[,j] = data[,i]
        colnames(df)[j] = paste("audio feature ", as.character(i))
        j = j+1
      }
      df$lat = data$lat
      df$long = data$long
      MLR1 = lm(lat+long~., data = df)
      summary(MLR1)
    }
    else{
      SLR = lm(data[,l]~data[,afs])
      summary(SLR)
    }

  })
  
  #uioutput
  output$auft = renderUI({
    fileInput(
      inputId = 'file',
      label = 'upload a .txt file of audio features',
      multiple = F,
      buttonLabel = 'Browse...'
    )
  })
  #prediction output
  newdata = eventReactive(input$file, {
    newdata = input$file
    read.table(newdata$datapath, header = TRUE)
  })
  output$pred = renderPrint({
    colnames(newdata()) = c(1:length(newdata()))
    colnames(data) = c(1:length(data))
    afs2 = as.numeric(as.vector(input$af2))
    if(input$af2=='all'){
      MLR2 = lm(lat+long~., data = data)
      predict(MLR2, newdata())
    }
    else if(length(afs2)>1){
      df = data.frame(matrix(vector(mode='numeric', length = 1), nrow=1059, ncol=1))
      newdf = data.frame(matrix(vector(mode='numeric', length = 1), nrow=1059, ncol=1))
      
      j=1
      for (i in afs2) {
        df[,j] = data[,i]
        newdf[,j] = newdata[,i]
        colnames(df)[j] = paste("audio feature ", as.character(i))
        j = j+1
      }
      df$lat = data$lat
      df$long = data$long
      MLR2 = lm(lat+long~., data = df)

      predict(MLR2, newdf)
    }
    else{
      SLR = lm(data[,l]~data[,afs2])
      predict(SLR, newdata()[afs2])
    }
    
  })
}
shinyApp(ui = ui, server = server)