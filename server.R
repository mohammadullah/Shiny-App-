
library(shiny)
library(ggplot2)
library(data.table)
library(caret)
library(rpart)
library(gridExtra)
library(lattice)
library(e1071)
library(randomForest)

shinyServer(function(input, output) {

  ## convert sona cars1 data variables to factor
    
  cars1 <- mtcars
  cars1$cyl <- as.factor(cars1$cyl)
  cars1$vs <- as.factor(cars1$vs)
  cars1$am <- as.factor(cars1$am)
  cars1$gear <- as.factor(cars1$gear)
  cars1$carb <- as.factor(cars1$carb)

  ## Input data from the first panel by reactive function
  
  selecteddata <- reactive({
    cars1[, c(input$var1, input$var2, input$var3)]
  }) 
  
  ## Output function for the plot in first panel
  
  output$plot1 <- renderPlot({
    df1 <- selecteddata()
    p <- ggplot(df1, aes(x = df1[,1], y = df1[,2], color = df1[,3]))
    p <- p + geom_point(size = 5) + geom_smooth(method = "lm")
    p <- p + labs(x = names(df1)[1], y = names(df1)[2], color = names(df1)[3])
    p
  })  
  
  ## Input data from the second panel by reactive function
  ## divide the data frame into test and train set
  
  cardata <- reactive({
    df2 <- cars1[, c(input$var4, input$var5)]
    set.seed(2376)
    inTrain <- createDataPartition(y = df2[,1], p = 0.75, list = FALSE)
    traincar <- df2[inTrain,]
    testcar <- df2[-inTrain,]
    list(traincar = traincar, testcar = testcar)
  })
  
  ## Reactive function to fit prediction model
  
  preddata <- reactive({
    traincar <- cardata()$traincar
    testcar <- cardata()$testcar
    pred1 <- 0
    pred2 <- 0
    
    ## rpart 
    
    if(input$showModel1) {
      formula <- as.formula(paste(input$var4, '~ .' ))
      set.seed(2376)
      modelfit <- rpart(formula, method = "class", data = traincar)
      pred1 <- predict(modelfit, testcar, type = "class")
    }
    
    ## random Forest
    
    if(input$showModel2) {
      x <- traincar[,-1]
      y <- traincar[,1]
      set.seed(2376)
      modelfit2 <- randomForest(x, y, importance = TRUE, proximity = TRUE, ntree = 100)
      pred2 <- predict(modelfit2, testcar)
    }
    
    list(pred1 = pred1, pred2 = pred2)
  })
  
  ## Function to render plot
  
  output$plot2 <- renderPlot({
    
    traincar <- cardata()$traincar
    testcar <- cardata()$testcar
    p1 <- ggplot()
    p2 <- ggplot()
    
    if(input$showModel1) {
      pred1 <- preddata()$pred1
      p1 <- ggplot(testcar, aes(x = testcar[,1], y = pred1))
      p1 <- p1 + geom_point(size = 5, color = "red", alpha = 0.1)
      p1 <- p1 + labs(x = names(testcar)[1], y = "Prediction",
                      title = "Model 1/rpart")
    }
    
     
    if(input$showModel2) {
      pred2 <- preddata()$pred2
      p2 <- ggplot(testcar, aes(x = testcar[,1], y = pred2))
      p2 <- p2 + geom_point(size = 5, color = "blue", alpha = 0.1)
      p2 <- p2 + labs(x = names(testcar)[1], y = "Prediction",
                      title = "Model 2/Random Forest")
    }
      
      return(grid.arrange(p1, p2, ncol = 2))
    
  })

  ## Table to print confusion matrix
  
  output$table <- renderTable({
    
    traincar <- cardata()$traincar
    testcar <- cardata()$testcar
    
    df3 <- data.frame(rpart = sample(0, 7, replace = TRUE), 
                      values = sample(0, 7, replace = TRUE))

    df4 <- data.frame(RandomForest = sample(0, 7, replace = TRUE), 
                      values = sample(0, 7, replace = TRUE))
    
    if(input$showModel1) {
      pred1 <- preddata()$pred1
      matrix1 <- confusionMatrix(pred1, testcar[,1])
      df3 <- data.frame(values = matrix1$overall)
      df3 <- setDT(df3, keep.rownames = TRUE)[]
      names(df3)[1] <- "rpart"
    }
    
    if(input$showModel2) {
      pred2 <- preddata()$pred2
      matrix2 <- confusionMatrix(pred2, testcar[,1])
      df4 <- data.frame(values = matrix2$overall)
      df4 <- setDT(df4, keep.rownames = TRUE)[]
      names(df4)[1] <- "RandomForest"
    }
    
    cbind(df3, df4)
  })
  
})
