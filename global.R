###This script contain some help functions on global input values
#################################################################
#################################################################

###Number of folds for the cross validations 
kFolds <- 10


###Cross validation parameters for SVM

costs <- c(0.1, 1, 10, 100)      #for all
degrees <- 2:5                   #for polynomial only
gammas <- c(0.01, 0.005, 0.001)  #for polynomial and radial only 

linearParameters <- data.frame(kernel = "linear", 
                               degree = FALSE,
                               gamma = FALSE,
                               cost = costs)

polynomialParameters <- data.frame(expand.grid(kernel = "polynomial", 
                                               degree = degrees, 
                                               gamma = gammas, 
                                               cost = costs))

radialParameters <- data.frame(expand.grid(kernel= "radial", 
                                           degree = FALSE, 
                                           gamma = gammas, 
                                           cost = costs))


###Colors for comparison graph
colors <- c("#009EC2", 
            "#000000", 
            "#424242",
            "#595959",
            "#6E6E6E",
            "#818181",
            "#949494",
            "#A8A8A8",
            "#BCBCBC",
            "#D1D1D1")

###Function for splitting data during cross validation
kFoldIndex <- function(k, length, random = TRUE) {
  idx <- cut(seq(1, length), breaks = k, labels = FALSE)
  if(random == TRUE) idx <- idx[order(rnorm(length))]
  return(idx)
}


###Function to get accuracy on test for all models 
getTestAccuracy <- function(model, 
                            df.train, 
                            df.test, 
                            k = FALSE, 
                            kernel = FALSE, 
                            degree = FALSE, 
                            gamma = FALSE, 
                            cost = FALSE) {
  
  if (model == "logistic") {
    
    positive <- paste(levels(df.train[,1])[2])
    negative <- paste(levels(df.train[,1])[1])
    
    fit <- glm(formula = paste0(colnames(df.train[1]), " ~."), data = df.train, family = "binomial")
    predictions <- predict(fit, df.test, type = "response")
    accuracy <- sum(df.test[,1] == ifelse(predictions > 0.5, positive, negative)) / nrow(df.test)
    
    return(accuracy)
    
  } else  if (model == "lda") {
    
    nVariables <- ncol(df.train)
    fit <- lda(df.train[,2:nVariables], df.train[,1])
    predictions <- predict(fit, df.test[,2:nVariables])
    accuracy <- sum(df.test[,1] == predictions$class) / nrow(df.test)
    
    return(accuracy)
    
  } else if (model == "qda") {
    
    nVariables <- ncol(df.train)
    fit <- qda(df.train[,2:nVariables], df.train[,1])
    predictions <- predict(fit, df.test[,2:nVariables])
    accuracy <- sum(df.test[,1] == predictions$class) / nrow(df.test)
    
    return(accuracy)
    
  } 
  
  else if (model == "tree") {
    
    nVariables <- ncol(df.train)
    fit <- tree(formula = paste0(colnames(df.train[1]), " ~."), data = df.train)
    predictions <- predict(fit, df.test[,2:nVariables], type = "class")
    accuracy <- sum(df.test[,1] == predictions) / nrow(df.test)
    
    return(accuracy)
    
  } 
  
  
  else if (model == "knn") {
    
    nVariables <- ncol(df.train)
    accuracy <- sum(knn(df.train[,2:nVariables], 
                        df.test[,2:nVariables], 
                        df.train[,1], 
                        k = k) == df.test[,1]) / NROW(df.test)
    
    return(accuracy)
    
  } 
  
  
  else if (model == "naive") {
    
    fit <- naiveBayes(df.train[,-1], df.train[,1])
    predictions <- predict(fit, df.test)
    accuracy <- sum(df.test[,1] == predictions) / nrow(df.test)
    
    return(accuracy)
    
  } 
  
  
  else if (model == "svm") {
    
    nVariables <- ncol(df.train)
    fit <- svm(df.train[,2:nVariables], df.train[,1], 
               kernel = kernel,
               degree = as.numeric(degree),
               gamma = as.numeric(gamma),
               cost = as.numeric(cost))
    
    predictions <- predict(fit, df.test[,-1])
    accuracy <- sum(df.test[,1] == predictions) / nrow(df.test)
    
    return(accuracy)
    
  } 
  
  
} 



###Function to get prediction based on chosen model
getPrediction <- function(model, 
                          df.train, 
                          df.test, 
                          k = FALSE, 
                          kernel = FALSE, 
                          degree = FALSE, 
                          gamma = FALSE, 
                          cost = FALSE) 
  {
  
  if (model == "logistic") {
    
    positive <- paste(levels(df.train[,1])[2])
    negative <- paste(levels(df.train[,1])[1])
    
    fit <- glm(formula = paste0(colnames(df.train[1]), " ~."), data = df.train, family = "binomial")
    
    predictions <- predict(fit, df.test, type = "response")
    predictedValue <- ifelse(predictions > 0.5, positive, negative)
    
    return(paste(predictedValue))
    
  } else  if (model == "lda") {
    
    nVariables <- ncol(df.train)
    fit <- lda(df.train[,2:nVariables], df.train[,1])
    predictions <- predict(fit, df.test[,2:nVariables])
    predictedValue <- predictions$class
    
    return(paste(predictedValue))
    
  } else if (model == "qda") {
    
    nVariables <- ncol(df.train)
    fit <- qda(df.train[,2:nVariables], df.train[,1])
    predictions <- predict(fit, df.test[,2:nVariables])
    predictedValue <- predictions$class
    
    return(paste(predictedValue))
    
  } else if (model == "tree") {
    
    nVariables <- ncol(df.train)
    
    fit <- tree(formula = paste0(colnames(df.train[1]), " ~."), data = df.train)
    predictedValue <- predict(fit, df.test[,2:nVariables], type = "class")
    
    
    return(paste(predictedValue))
    
  } else if (model == "knn") {
    
    nVariables <- ncol(df.train)
    
    fit <- knn(df.train[,2:nVariables], 
                        df.test[,2:nVariables], 
                        df.train[,1], 
                        k = k) 
    
    predictedValue <- fit
    
    return(paste(predictedValue))
    
  }  else if (model == "naive") {
    
    fit <- naiveBayes(df.train[,-1], df.train[,1])
    predictedValue <- predict(fit, df.test)
    
    return(paste(predictedValue))
    
  }  else if (model == "svm") {
    
    nVariables <- ncol(df.train)
   
     fit <- svm(df.train[,2:nVariables], df.train[,1], 
               kernel = kernel,
               degree = as.numeric(degree),
               gamma = as.numeric(gamma),
               cost = as.numeric(cost))
    
     predictedValue <- predict(fit, df.test[,-1])
   
    
    return(paste(predictedValue))
    
  } 
  
} 


###Function to convert model short name to long name
getModelLongName <- function(modelShort) {
  
  switch(modelShort, 
         "logistic" = "Logistic Regression",
         "lda" = "Linear Discriminant Analysis",
         "qda" = "Quadratic Discriminamt Analysis",
         "tree" = "Decision Tree",
         "knn" = "K-Nearest Neighbours",
         "naive" = "Naive Bayes",
         "svm" = "Support Vector Machine")
}


###Function to convest model long name to short name
getModelShortName <- function(modelLong) {
  
  switch(modelLong, 
         "Logistic Regression" = "logistic",
         "Linear Discriminant Analysis" = "lda",
         "Quadratic Discriminamt Analysis" = "qda",
         "Decision Tree" = "tree",
         "K-Nearest Neighbours" = "knn",
         "Naive Bayes" = "naive",
         "Support Vector Machine" = "svm")
}


###Function for creating a string with all the input values
createInputString <- function(df) {
  
  temp <- lapply(colnames(df[-1]), function(name) { paste(name, df[,name], sep=": ")})
  fullString <- paste(paste(temp[-length(temp)], collapse = ", "), "and", temp[length(temp)])
  
  return(fullString)
  
}


