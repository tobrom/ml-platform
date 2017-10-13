library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
library(wordcloud)
library(rmarkdown)
library(stringr)
library(MASS) #lda & qda
library(tree) #Decision Tree
library(class) #Knn
library(e1071) #Naive Bayes


Sys.setlocale(category = "LC_TIME", locale="us")


# Shiny Server ------------------------------------------------------------


shinyServer(function(input, output) {
   
  readData <- reactive({

    input$uploaddata
    
    df <- isolate({
      
      
      if (input$sample == TRUE) {
        
        iris %>%
          dplyr::select(Species, 
                        Sepal.Length, 
                        Sepal.Width, 
                        Petal.Length, 
                        Petal.Width) %>%
          filter(Species %in% sample(unique(iris$Species), 2)) %>%
          mutate(Species = as.character(Species))
       
      
      } else if (!is.null(input$file1) && input$acode == "test") {
       
        
      inFile <- input$file1
      
      read.csv(inFile$datapath, 
               header = input$header, 
               sep = input$sep, 
               quote = input$quote)
      } else {
        
         
        return(NULL)
        
        }
        
        
    
   
     }) 
        
          df    

    })
  
  
  splitIndex <- reactive({
  
    input$splitdata
    
    req(readData())
   
    dataIndex <- isolate({
    
    sample(nrow(readData()), nrow(readData())*(input$testsize/100))
      
        })
    
    dataIndex
 
     })
  
  
  trainData <- reactive({
    
    req(readData())
    
    data.frame(readData()[-splitIndex(),])
    
  }) 
  
  
  testData <- reactive({
    
    req(readData())
    
    data.frame(readData()[splitIndex(),])
    
  })  
  
  
  output$splittext <- renderText({
    
     req(readData())

    paste("The data set has been splitted into a training set consisting of", 
          nrow(trainData()), "observations and and test set consisting of the remaining", 
          nrow(testData()), "observations.")   
            
  }) 
  
  
  output$tablehead <- renderTable({
    
    req(readData())
    
    if ((sum(apply(readData()[,2:ncol(readData())], 2, is.numeric) == FALSE) == 0) & 
        (length(levels(as.factor(readData()[,1]))) == 2)) {
      
      head(readData()) } else {
        
      classTest <- ifelse(length(levels(as.factor(readData()[,1]))) > 2, "Error: More than two levels!", 
                            ifelse(length(levels(as.factor(readData()[,1]))) > 2, "Error: Only one level!", "OK"))
          
      numericTest <- ifelse(sum(apply(readData()[,2:ncol(readData())], 2, is.numeric) == FALSE) == 0, 
                            "OK", "Error: Non-numeric feature variable(s)") 
        
        data.frame('Check' = c("Target", "Features"), Status = c(classTest, numericTest)) 
        
      }
  })
  
  
  output$plotxUI <- renderUI({
    
    req(input$file1)
    
    if (input$scatterplot) { 
    
    selectInput('xcol', 'X Variable', names(trainData()[-1]))
      
    } else {return(NULL)}
    
  }) 
  
  
  output$plotyUI <- renderUI({
    
    req(input$file1)
    
    if (input$scatterplot) { 
    
    selectInput('ycol', 'Y Variable', names(trainData()[-1]), selected = names(trainData()[-1])[2])
      
    } else {return(NULL)}
    
  }) 
  

  output$pcaplot <- renderPlotly({
    
    req(input$file1)
    req(input$pca)

     prComps <- trainData() %>%
     dplyr::select(c(2:ncol(trainData()))) %>%
     prcomp(scale = TRUE) 
    
     df <- data.frame(prComps$x[,1:3], Class = trainData()[,1])
    
     plot_ly(df, x = ~ PC1, 
            y = ~ PC2, 
            z = ~ PC3, 
            color = ~ as.factor(Class),
           type = "scatter3d",
            mode = "markers",
            colors = c("#009EC2", "#000000")) 
    
  })
  
 
   output$scatterplot <- renderPlot({
    
    req(input$file1)
    req(input$xcol)
    req(input$ycol)
    req(input$scatterplot)
  
    ggplot(trainData(), aes(x = trainData()[,match(paste(input$xcol), colnames(trainData()))], 
                           y = trainData()[,match(paste(input$ycol), colnames(trainData()))],
                           color = trainData()[,1])) + 
      geom_point() +
      labs(x = paste(input$xcol), y = paste(input$ycol), color = "") +
      scale_color_manual(values = c("#949494", "#009EC2")) +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            legend.key = element_blank(),
            panel.grid.major.x = element_line(colour = "grey", size = 0.1, linetype = 20),
            panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = 20))
  })
  
  
  output$varUI <- renderUI({
    
  req(input$file1)
    
  variables <- colnames(trainData()[-1])
  varCount <- length(variables)
  
  w = ""
    
  for (i in 1:varCount) {
       w = paste0(w, textInput(paste0("var",i), label= paste(variables[i])))
    }
   
    HTML(w)

    })

  
  output$modelUI <- renderUI({
    
    if (is.null(testedModels())) {
      return(NULL)
    }
    
     selectInput("predictionmodel", "Select Model", choices = paste(testedModels()$Model))
  
     }) 
  
  
  
  
  output$knncompare <- renderUI({
    
    req(input$file1)
    
    if (input$knn_2 == TRUE) { 
      
      sliderInput("knnNumberComp", "K", 
                  min = 1, max = 20, step = 1, value =  5)
      
    } else {return(NULL)}
    
    
  }) 
  
  
  
  output$svmcompare <- renderUI({
    
    req(input$file1)
    
    if (input$svm_2 == TRUE) { 
      
      selectInput("svmkernel", "Kernel", choices = c("linear", "polynomial", "radial"))
      
    } else {return(NULL)}
    
    
  }) 
  
  output$svmcomparedegree <- renderUI({
    
    req(input$file1)
    req(input$svmkernel)
    
    if (input$svm_2 == TRUE && input$svmkernel == "polynomial") { 
      
      radioButtons("svmdegree", "Degree",
                   choices = paste(degrees), inline = TRUE)
      
    } else {return(NULL)}
    
    
  })
  


  output$svmcomparegamma <- renderUI({
  
    req(input$file1)
    req(input$svmkernel)

    if (input$svm_2 == TRUE && input$svmkernel %in% c("polynomial", "radial")) { 
    
      radioButtons("svmgamma", "Gamma",
                 choices = paste(gammas), inline = TRUE)
    
    } else {return(NULL)}
  
  
  })
  
  
  output$svmcomparecost <- renderUI({
  
    req(input$file1)
    req(input$svmkernel)
  
    if (input$svm_2 == TRUE) { 
    
      radioButtons("svmcost", "Cost",
                  choices = paste(costs), inline = TRUE)
    
   } else {return(NULL)}
  
  
  }) 
  
  
  
   output$plotlogisticcv <- renderPlot({
      
      if (is.null(trainData())) {
        return(NULL)
      }
      
      if (input$logistic == FALSE) {
        return(NULL)
      } 
      
        cvData <- trainData()
        
        positive <- paste(unique(cvData[,1])[2])
        negative <- paste(unique(cvData[,1])[1])
        
        kk10FCV <- data.frame()
     
        kFolds <- 10
        index <- kFoldIndex(kFolds, nrow(cvData))
        
        withProgress({
          
          setProgress(message = "Cross-Validating Logistic Regression...")
     
        for(i in 1:kFolds) {
          
          incProgress(1/kFolds)
        
          df.train <- cvData[!(index == i),]
          df.test <- cvData[index == i,]
     
          fit <- glm(formula = paste0(colnames(df.train[1]), " ~."), data = df.train, family = "binomial")
     
          probs_in <- predict(fit, df.train, type = "response")
          probs_out <- predict(fit, df.test, type = "response")
     
          Train <- sum(df.train[,1] == ifelse(probs_in > 0.5, positive, negative)) / nrow(df.train)
          Test <- sum(df.test[,1] == ifelse(probs_out > 0.5, positive, negative)) / nrow(df.test)
     
          results <- data.frame(fold = i, Train, Test)
    
          kk10FCV <- rbind(kk10FCV, results) 
     
        } 
          
      })
     
      kk10FCV %>% 
         gather(sample, accuracy, Train:Test) %>%
       ggplot(aes(x = factor(sample, levels = c("Train", "Test")), y = accuracy*100, fill = sample)) + 
         geom_boxplot(fill =c("#BCBCBC", "#009EC2")) +
         labs(x = "", y = "Accuracy (%)") +
         theme(panel.background = element_blank(),
               plot.background = element_blank(),
               panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
               panel.grid.major.x = element_line(colour = "grey", size = 0.1)) +
         scale_y_continuous(breaks = seq(0, 100, 1))
      
        })

    
     output$plotldacv <- renderPlot({
     
       if (is.null(trainData())) {
         return(NULL)
       }
       
       if (input$lda == FALSE) {
         return(NULL)
       } 
       
           cvData <- trainData()
       
           kk10FCV <- data.frame()
           nVariables <- ncol(cvData)
           kFolds <- 10
         
           index <- kFoldIndex(kFolds, nrow(cvData))
           
           withProgress({
             
             setProgress(message = "Cross-Validating Linear Discriminant Analysis...")
       
           for(i in 1:kFolds) {
             
              incProgress(1/kFolds)
             
              df.train <- cvData[!(index == i),]
              df.test <- cvData[index == i,]
       
              fit <- lda(df.train[,2:nVariables], df.train[,1])
       
              probs_in <- predict(fit, df.train[,2:nVariables])
              probs_out <- predict(fit, df.test[,2:nVariables])
       
       
              Train <- sum(df.train[,1] == probs_in$class) / nrow(df.train)
              Test <- sum(df.test[,1] == probs_out$class) / nrow(df.test)
       
              results <- data.frame(fold = i, Train, Test)
       
              kk10FCV <- rbind(kk10FCV, results) 
       
           } 
             
        })
       
           kk10FCV %>% 
             gather(sample, accuracy, Train:Test) %>%
             ggplot(aes(x = factor(sample, levels = c("Train", "Test")), y = accuracy*100, fill = sample)) + 
             geom_boxplot(fill =c("#BCBCBC", "#009EC2")) +
             labs(x = "", y = "Accuracy (%)") +
             theme(panel.background = element_blank(),
                   plot.background = element_blank(),
                   panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
                   panel.grid.major.x = element_line(colour = "grey", size = 0.1)) +
             scale_y_continuous(breaks = seq(0, 100, 1))
   }) 

    
     
     output$plotqdacv <- renderPlot({
       
       if (is.null(trainData())) {
         return(NULL)
       }
       
       if (input$qda == FALSE) {
         return(NULL)
       } 
       
       cvData <- trainData()
       
       kk10FCV <- data.frame()
       nVariables <- ncol(cvData)
       kFolds <- 10
       
       index <- kFoldIndex(kFolds, nrow(cvData))
       
       withProgress({
         
         setProgress(message = "Cross-Validating Quadratic Discriminant Analysis...")
       
       for(i in 1:kFolds) {
         
         incProgress(1/kFolds)
         
         df.train <- cvData[!(index == i),]
         df.test <- cvData[index == i,]
         
         fit <- qda(df.train[,2:nVariables], df.train[,1])
         
         probs_in <- predict(fit, df.train[,2:nVariables])
         probs_out <- predict(fit, df.test[,2:nVariables])
         
         Train <- sum(df.train[,1] == probs_in$class) / nrow(df.train)
         Test <- sum(df.test[,1] == probs_out$class) / nrow(df.test)
         
         results <- data.frame(fold = i, Train, Test)
         
         kk10FCV <- rbind(kk10FCV, results) 
         
       } 
         
      })   
       
       kk10FCV %>% 
         gather(sample, accuracy, Train:Test) %>%
         ggplot(aes(x = factor(sample, levels = c("Train", "Test")), y = accuracy*100, fill = sample)) + 
         geom_boxplot(fill =c("#BCBCBC", "#009EC2")) +
         labs(x = "", y = "Accuracy (%)") +
         theme(panel.background = element_blank(),
               plot.background = element_blank(),
               panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
               panel.grid.major.x = element_line(colour = "grey", size = 0.1)) +
         scale_y_continuous(breaks = seq(0, 100, 1))
     })
  

     output$plottreecv <- renderPlot({
       
       if (is.null(trainData())) {
         return(NULL)
       }
       
       if (input$tree == FALSE) {
         return(NULL)
       } 
       
       cvData <- trainData()
       kk10FCV <- data.frame()
       kFolds <- 10
       
       index <- kFoldIndex(kFolds, nrow(cvData))
       
       withProgress({
         
         setProgress(message = "Cross-Validating Decision Tree...")
       
       for(i in 1:kFolds) {
         
         incProgress(1/kFolds)
        
         df.train <- cvData[!(index == i),]
         df.test <- cvData [index == i,]
         
         fit <- tree(formula = paste0(colnames(df.train[1]), " ~."), data = df.train)
       
         probs_in <- predict(fit, df.train, type = "class")
         probs_out <- predict(fit, df.test, type = "class")
         
         Train <- sum(df.train[,1] == probs_in) / nrow(df.train)
         Test <- sum(df.test[,1] == probs_out) / nrow(df.test)
         
         results <- data.frame(fold = i, Train, Test)
         
         kk10FCV <- rbind(kk10FCV, results) 
         
       } 
       
     }) 
       
       kk10FCV %>% 
         gather(sample, accuracy, Train:Test) %>%
         ggplot(aes(x = factor(sample, levels = c("Train", "Test")), y = accuracy*100, fill = sample)) + 
         geom_boxplot(fill =c("#BCBCBC", "#009EC2")) +
         labs(x = "", y = "Accuracy (%)") +
         theme(panel.background = element_blank(),
               plot.background = element_blank(),
               panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
               panel.grid.major.x = element_line(colour = "grey", size = 0.1)) +
         scale_y_continuous(breaks = seq(0, 100, 1))
     })
     
    
     
     output$plotknncv <- renderPlot({
       
       if (is.null(trainData())) {
         return(NULL)
       }
       
       if (input$knn == FALSE) {
         return(NULL)
       } 
       
       cvData <- trainData()
       
       kk10FCV <- data.frame()
       
       kFolds <- 10
       dataCols <- ncol(cvData)
       index <- kFoldIndex(kFolds, nrow(cvData))
       
       withProgress({
         
         setProgress(message = "Cross-Validating K-Nearest Neighbours...")
       
        for (k in 1:25) {
         
         incProgress(1/25)
       
        for(i in 1:kFolds) {
        
          df.train <- cvData[!(index == i),]
          df.test <- cvData[index == i,]
  
          results <- data.frame(k = k, 
                           Train = sum(knn(df.train[,2:dataCols], df.train[,2:dataCols], df.train[,1], k = k) == df.train[,1]) / NROW(df.train),
                           Test = sum(knn(df.train[,2:dataCols], df.test[,2:dataCols], df.train[,1], k = k) == df.test[,1]) / NROW(df.test))
        
          kk10FCV <- rbind(kk10FCV, results)  
         
       } 
         
    }
      
   }) 
        
       kk10FCV %>% 
         gather(sample, accuracy, Train:Test) %>%
         group_by(k, sample) %>%
         summarise(accuracy = mean(accuracy)) %>%
         
         ggplot(aes(x = k, y = accuracy*100, color = factor(sample, levels = c("Train", "Test")))) + 
         geom_line() +
         scale_color_manual(values=c("#BCBCBC", "#009EC2")) +
         labs(x = "Nearest Neighbours", y = "Accuracy (%)") +
         theme(panel.background = element_blank(),
               plot.background = element_blank(),
               legend.title=element_blank(),
               legend.key = element_blank(),
               panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
               panel.grid.major.x = element_line(colour = "grey", size = 0.1)) 
   
     })
     
     
     output$plotnaivecv <- renderPlot({
       
       if (is.null(trainData())) {
         return(NULL)
       }
       
       if (input$naive == FALSE) {
         return(NULL)
       } 
       
       withProgress({
         
        setProgress(message = "Cross-Validating Naive Bayes...")
       
       cvData <- trainData()
      
        kk10FCV <- data.frame()
       kFolds <- 10
       index <- kFoldIndex(kFolds, nrow(cvData))
       
       for(i in 1:kFolds) {
         
         incProgress(1/kFolds)
         
         df.train <- cvData[!(index == i),]
         df.test <- cvData[index == i,]
         
         fit <- naiveBayes(df.train[,-1], df.train[,1])
         
         probs_in <- predict(fit, df.train)
         probs_out <- predict(fit, df.test)
         
         Train <- sum(df.train[,1] == probs_in) / nrow(df.train)
         Test <- sum(df.test[,1] == probs_out) / nrow(df.test)
         
         results <- data.frame(fold = i, Train, Test)
         
         kk10FCV <- rbind(kk10FCV, results) 
         
       } 
       
       
    })
       
       
       kk10FCV %>% 
         gather(sample, accuracy, Train:Test) %>%
         ggplot(aes(x = factor(sample, levels = c("Train", "Test")), y = accuracy*100, fill = sample)) + 
         geom_boxplot(fill =c("#BCBCBC", "#009EC2")) +
         labs(x = "", y = "Accuracy (%)") +
         theme(panel.background = element_blank(),
               plot.background = element_blank(),
               panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
               panel.grid.major.x = element_line(colour = "grey", size = 0.1)) +
         scale_y_continuous(breaks = seq(0, 100, 1))
       
     })
     
     
      svmCvData  <- reactive({
       
       if (is.null(trainData())) {
         return(NULL)
       }
       
       if (input$svm == FALSE) {
         return(NULL)
       } 
       
       withProgress({
         
         setProgress(message = "Cross-Validating Support Vector Machines...")
        
         ###from global.R
         cvParameters <- list(linearParameters, polynomialParameters, radialParameters)
         
         cvData <- trainData()
    
         kk10FCV <- data.frame()
         kFolds <- 10
         index <- kFoldIndex(kFolds, nrow(cvData))
         
         for (k in 1:length(cvParameters)) {
           
           for (p in 1:nrow(cvParameters[[k]])) {
         
              for(i in 1:kFolds) {
           
              incProgress(1/(kFolds*sum(unlist(lapply(cvParameters, nrow)))))
           
              df.train <- cvData[!(index == i),]
              df.test <- cvData[index == i,]
           
              fit <- svm(df.train[,2:ncol(df.train)], df.train[,1], 
                         kernel = cvParameters[[k]]$kernel[p],
                         degree = cvParameters[[k]]$degree[p],
                         gamma = cvParameters[[k]]$gamma[p],
                         cost = cvParameters[[k]]$cost[p])
           
              probs_in <- predict(fit, df.train[,-1])
              probs_out <- predict(fit, df.test[,-1])
           
              Train <- sum(df.train[,1] == probs_in) / nrow(df.train)
              Test <- sum(df.test[,1] == probs_out) / nrow(df.test)
           
              results <- data.frame(iteration = i, 
                                    kernel = cvParameters[[k]]$kernel[p],
                                    degree = cvParameters[[k]]$degree[p],
                                    gamma = cvParameters[[k]]$gamma[p],
                                    cost = cvParameters[[k]]$cost[p],
                                    Train,
                                    Test)
           
           kk10FCV <- rbind(kk10FCV, results) 
           
         } 
         
       }
           
     }
         
   })
       
    kk10FCV 
       
  })
       
    
    output$plotsvmcvtype <- renderPlot({
      
      req(svmCvData())
   
      svmCvData() %>% 
         gather(sample, accuracy, Train:Test) %>%
         ggplot(aes(x = factor(sample, levels = c("Train", "Test")), 
                    y = accuracy*100, 
                    fill = (factor(sample, levels = c("Train", "Test"))))) + 
         geom_boxplot() +
         facet_grid(.~kernel) +
         labs(x = "", y = "Accuracy (%)") +
         scale_fill_manual(values = c("#BCBCBC", "#009EC2")) +
         theme(panel.background = element_blank(),
               plot.background = element_blank(),
               legend.title=element_blank(),
               legend.key = element_blank(),
               panel.grid.major.y = element_line(colour = "grey", linetype="dashed", size = 0.1),
               panel.grid.major.x = element_line(colour = "grey", size = 0.1)) 
    
    }) 
         
    
    output$plotsvmcvbest <- renderPlot({
      
      req(svmCvData())
    
      svmCvData() %>%
          group_by(kernel, cost, gamma, degree) %>%
          summarise(Train = mean(Train), 
                    Test = median(Test)) %>%
          arrange(-Test) %>% 
          head(5) %>% 
          left_join(svmCvData() , 
                    by = c("kernel", 
                           "cost", 
                           "gamma", 
                           "degree")) %>% 
          rename(Train = Train.y,
                 Test = Test.y) %>%
          mutate(model = paste0("kernel=", kernel,
                                ", cost=", cost,
                                ", gamma=", gamma,
                                ", degree=", degree)) %>%
            gather(sample, accuracy, Train:Test) %>%
            
            ggplot(aes(x = reorder(model, -Test.x), 
                       y = accuracy*100, 
                       fill = (factor(sample, levels = c("Train", "Test"))))) + 
            geom_boxplot() +
            labs(x = "", y = "Accuracy (%)") +
            scale_fill_manual(values = c("#BCBCBC", "#009EC2")) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) + 
            theme(panel.background = element_blank(),
                  plot.background = element_blank(),
                  legend.title=element_blank(),
                  legend.key = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey", size = 0.1),
                  panel.grid.major.x = element_line(colour = "grey", size = 0.1)) 
          
    }) 
         
         
     
    testedModels <- reactive({
       
       if (is.null(readData())) {
         return(NULL)
       }
       
       input$testmodels
      
       comparisonPlot <- isolate({
       
       chosenModels <-   c(input$logistic_2,
                           input$lda_2,
                           input$qda_2,
                           input$tree_2,
                           input$knn_2,
                           input$naive_2,
                           input$svm_2)
       
       potentialModels <- c("logistic",
                            "lda",
                            "qda",
                            "tree",
                            "knn",
                            "naive",
                            "svm")
       
      selectedModels <-  potentialModels[chosenModels]
       
      testResults  <- data.frame()
       
        for (i in selectedModels)  {
         
          testResults <- rbind(testResults, 
                               data.frame(Model = getModelLongName(i), 
                                          Short = i,
                                          Accuracy = getTestAccuracy(i, 
                                                                     trainData(), 
                                                                     testData(), 
                                                                     k = input$knnNumberComp,
                                                                     kernel = input$svmkernel,
                                                                     degree = ifelse(input$svmkernel == "polynomial", input$svmdegree, FALSE),
                                                                     gamma = ifelse(input$svmkernel %in% c("polynomial", "radial"), input$svmgamma, FALSE),
                                                                     cost = input$svmcost)))
         
     }
       
      testResults
      
       }) 
    
       comparisonPlot
       
          
    })   
       
    
    output$comparisonplot <- renderPlot({
       
      if (nrow(testedModels()) == 0) {
        return(NULL)
      }
       
      ggplot(testedModels(), aes(x = reorder(Model, -Accuracy), y = Accuracy*100, fill = reorder(Model, -Accuracy))) + 
                            geom_bar(stat = "identity", width = 0.5) + labs(x = "", y = "Accuracy (%)") +
                            geom_text(aes(label = paste(format(round(Accuracy*100, 2), nsmall = 2), "%")), vjust = -1) +
                            scale_fill_manual(values = colors[1:nrow(testedModels())]) +
                            scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) +
                            theme(panel.background = element_blank(),
                                  plot.background = element_blank(),
                                  axis.text.x = element_text(size = 12),
                                  legend.position = "none",
                                  panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = 20))
       
    })   
       
    
      predictionInput <- reactive({
         
      input$finalprediction 
         
      predictionTable  <- isolate({
         
        varNames <- colnames(readData())
        xVarCount <- as.integer(length(varNames))-1
         
         predictInputTable <- data.frame(lapply(1:xVarCount, function(i) {
           as.numeric(input[[paste0("var", i)]])
         
           }))
       
         req(sum(is.na(predictInputTable[1,]) == TRUE) == 0)
        
         predictInputTable <- data.frame(tempYCol = 0, predictInputTable)
         
         colnames(predictInputTable) <- varNames
     
         predictInputTable
 
         })
          
      predictionTable  
          
      })
      
    
      predictedClass <- reactive({
        
       req(predictionInput())
          
          getPrediction(getModelShortName(input$predictionmodel), 
                        readData(), 
                        predictionInput(), 
                        k = input$knnNumberComp,
                        kernel = input$svmkernel,
                        degree = ifelse(input$svmkernel == "polynomial", input$svmdegree, FALSE),
                        gamma = ifelse(input$svmkernel %in% c("polynomial", "radial"), input$svmgamma, FALSE),
                        cost = input$svmcost) 
          
        
      })
      
      
   
      
      output$plotCloud <- renderPlot({
        
       req(predictedClass())
        
       wordcloud(paste(predictedClass()), 3,  colors = "#009EC2", rot.per = 0, c(5,1))
          
      })
      
      
      
      
      
       
 
      
      ############
      ##########
      
      testData22 <- reactive({
        
        
        
        data.frame(model = c("Tree", "KNN", "LDA"), 
                   accuracy = c(88, 78, 95))
      })
      
      
      predData <- reactive({
        
        data.frame(variable1 = 3, 
                   variable2 = 5,
                   variable3 = 3.3)
      })
      ###########
      ########
      
      
      
      
      
           output$downloadReport <- downloadHandler(
      
              filename = "summary_report.pdf",
        
              content = function(file) {
          
                src <- normalizePath('report.Rmd')
          
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
          
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                file.copy(src, 'report.Rmd', overwrite = TRUE)
          
                modelStats <- testedModels() %>% arrange(-Accuracy)
                
                
                out <- render('report.Rmd', 
                              pdf_document(),
                              params = list(m = nrow(readData()), 
                                            ncol = ncol(readData()), 
                                            trainset = (100-input$testsize), 
                                            bestmodel = paste(modelStats$Model[1]),
                                            bestaccuracy = paste(round(modelStats$Accuracy[1]*100, 2)),
                                            prediction = predictedClass(),
                                            usedModel = getModelShortName(input$predictionmodel),
                                            data = modelStats,
                                            dataPred = createInputString(predictionInput()),
                                            colors = colors))
                
          file.rename(out, file)
        }
              
        )     
      
       
})
   

