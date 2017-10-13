library(plotly)

shinyUI(fluidPage(
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #009EC2}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #009EC2}")),
  
  titlePanel(title=div(img(src="ra_logo.png", width="80"), "")),
  
  hr(),
  
  navbarPage("",
   tabPanel("Select Data",
      sidebarLayout(
        sidebarPanel(helpText("Upload a data set to be analyzed. In order to be able to train classification 
                              models and make predictions, a categorical target variable with two classes needs 
                              to be placed in the first column followed by numeric feature variables.
                              In order to read and analyse own data you need 
                              to use an access code. Cross-validating the machine learning 
                              algorithms smoothly for large data sets requires a 
                              significant amount of memory and CPU and will slow
                              down the whole server. On order to test the functionality
                              of the software you can tick 'Use sample Data' which currently
                              consists of the iris data set where one of the classes will
                              be removed randomly"),
                     hr(),
                     fileInput('file1', 'Upload File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                     checkboxInput('header', 'Header', TRUE),
                     radioButtons('sep', 'Separator', c(Comma = ',', Semicolon = ';', Tab = '\t'),','),
                     radioButtons('quote', 'Quote', c(None= '', 'Double Quote' = '"', 'Single Quote' = "'"),'"'),
                     
                     textInput("acode", "", placeholder = "Enter Access Code:"),
                     hr(),
                     checkboxInput('sample', 'Use Sample Data', FALSE),
        
                     
                     actionButton("uploaddata", "Read Data"),
                     hr(),
                   
                 
                     helpText("In order to evaluate and compare the prediction models, the final test for the models needs 
                              to be performed on unseen data. Please select the share of your initial data set that shall be used 
                              for testing."),
                     sliderInput("testsize", "", 
                                 min = 5, max = 95, step = 5, post = " %", value =  30),
                     actionButton("splitdata", "Update Split"),
                     hr(),
                     textOutput("splittext")
                     ),
        
          mainPanel(tableOutput('tablehead')
                 ))), 
   
   tabPanel("Exploratory Data Analysis",
            sidebarLayout(
              sidebarPanel(helpText("Here you can explore the training data visually in 
                                    order to understand the dynamic."),
                           hr(),
                           checkboxInput('pca', 'Principal Component Analysis', FALSE),
                           helpText("Use orthogonal transformation to convert all 
                                    the feature variables into values of linearly 
                                    uncorrelated variables. Finally, the three first 
                                    principal components are plotted in a dynamic 3-D scatterplot."),
                           hr(),
                           checkboxInput('scatterplot', 'Exploratory Plot', FALSE),
                           helpText("Create own 2-D scatterplots based on the available feature variables"),
                           uiOutput("plotxUI"),
                           uiOutput("plotyUI")),
              mainPanel(
                tabsetPanel(
                  tabPanel("Principal Componets Analysis", plotlyOutput("pcaplot", height = "700px")),
                  tabPanel("Exploratory Plot", plotOutput("scatterplot", height = "700px")))      
                        ))),
   
    tabPanel("Cross Validation",
             sidebarLayout(
              sidebarPanel(helpText("10-fold cross validation will be performed on the training data for the selected models. 
                                    Based on the cross validation process you will be able to get an understanding 
                                    of how the different models overfit the data. In addition,  the cross validation for 
                                    the K-Nearest Neighbours and the Support Vector Machine will also help to choose the 
                                    optimal main parameters."),
                            hr(),
                            checkboxInput('logistic', 'Logistic Regression', FALSE),
                            checkboxInput('lda', 'Linear Discriminant Analysis', FALSE),
                            checkboxInput('qda', 'Quadratic Discriminant Analysis', FALSE),
                            checkboxInput('tree', 'Decision Tree', FALSE),
                            checkboxInput('knn', 'K-Nearest Neighbors', FALSE),
                            checkboxInput('naive', 'Naive Bayes', FALSE),
                            checkboxInput('svm', 'Support Vector Machines', FALSE),
                           hr()
                           ),
              
              mainPanel(
                tabsetPanel(
                  tabPanel("Logistic Regression", plotOutput("plotlogisticcv")),
                  tabPanel("Linear Discriminant Analysis", plotOutput("plotldacv")),
                  tabPanel("Quadratic Discriminant Analysis", plotOutput("plotqdacv")),
                  tabPanel("Decision Tree", plotOutput("plottreecv")),
                  tabPanel("K-Nearest Neighbours", plotOutput("plotknncv")),
                  tabPanel("Naive Bayes", plotOutput("plotnaivecv")),
                  tabPanel("Support Vector Machine", plotOutput("plotsvmcvtype"), plotOutput("plotsvmcvbest"))
                  
                )
               )
              )
             ),
  
  tabPanel("Testing",
           sidebarLayout(
             sidebarPanel(helpText("Here the selected models can be compared and 
                                   ranked based on the test set."),
                          hr(),
                          checkboxInput('logistic_2', 'Logistic Regression', FALSE),
                          checkboxInput('lda_2', 'Linear Discriminant Analysis', FALSE),
                          checkboxInput('qda_2', 'Quadratic Discriminant Analysis', FALSE),
                          checkboxInput('tree_2', 'Decision Tree', FALSE),
                          checkboxInput('knn_2', 'K-Nearest Neighbors', FALSE),
                          uiOutput("knncompare"),
                          checkboxInput('naive_2', 'Naive Bayes', FALSE),
                          checkboxInput('svm_2', 'Support Vector Machines', FALSE),
                          uiOutput("svmcompare"),
                          uiOutput("svmcomparedegree"),
                          uiOutput("svmcomparegamma"),
                          uiOutput("svmcomparecost"),
                          hr(),
                          actionButton("testmodels", "Compare")
             ),
             mainPanel(plotOutput("comparisonplot")       
             ))),
    
    tabPanel("Prediction",
             sidebarLayout(
               sidebarPanel(helpText("Here the models can be used to make predictions. Insert numeric values
                                     for all the features and selected the model"),
                            hr(),
                            uiOutput("varUI"), 
                            uiOutput("modelUI"),
                            hr(),
                            actionButton("finalprediction", "Predict")),
         
               mainPanel(plotOutput("plotCloud")
                      
               ))),
  
    tabPanel("Output",
           sidebarLayout(
             sidebarPanel(helpText("Here you can download a pdf docuemnt describing the process and model
                                   evaluation. In addition there will be information on the predictions."),
                          hr(),
                          downloadButton('downloadReport')),
             
             mainPanel())))))


