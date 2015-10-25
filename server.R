library(shiny)
library(datasets)
library(stats)

data <- NULL
datasets <- c("mtcars","airquality","swiss","attitude","USJudgeRatings")
currdataset <- "" #mtcars"

# Create a string of the dependent value versus selected features.
# Returned value is like mpg ~ cyl+disp
  getModelString <- function(dset,desiredFeatures) {
    y <- getDependentValue(dset)
    x <- getSelectedFeatures(dset,desiredFeatures)
    unlist(paste(paste(y,"~"),paste(x,collapse="+")))
  }

# Return the currently selected data (if dsetName matches current)
# or get a fresh dataset as necessary
  getDataset <- function(dsetName) {
    if ( length(dsetName) )
    {
      if ( dsetName == "airquality") {
        data <- airquality;
      }
      else if ( dsetName == "attitude" ) {
        data <- attitude
      }
      else if ( dsetName == "swiss") {
        data <- swiss
      }
      else if ( dsetName == "USJudgeRatings") {
        data <- USJudgeRatings
      }
      else {
        data <- mtcars
        data$am <- factor(data$am, labels=c("Automatic","Manual"))
      }
      currdataset <- dsetName
    }
    return(data)
  }

# Return the dependent value for the model.  This is currently
# the first feature in the data set
  getDependentValue <- function(dataset) {
    return(names(dataset)[1])
  }

# Return all available features except the dependent value
  getFeatures <- function(dataset) {
    modelTarget <- names(dataset)[1]
    modelFeatures <- names(dataset)[!(names(dataset) %in% modelTarget)]
    return(modelFeatures)
  }

# Return a fitted model of modelType
  getModel <- function(dataset,selFeatures) {
    mdl = NULL
    if ( length(selFeatures) ) {
      mdl <- glm(getModelString(dataset,selFeatures),family="gaussian",dataset)
    }
    return(mdl)
  }

# Return the subset of available features (from getFeatures) that
# are specified in the currSelected list.  If no matches, return all
# features
  getSelectedFeatures <- function(dataset, currSelected) {
    modelTarget <- names(dataset)[1]
    modelFeatures <- names(dataset)[!(names(dataset) %in% getDependentValue(dataset))]
    selFeatures <- modelFeatures[modelFeatures %in% currSelected]
    if ( length(selFeatures) < 1 )
      selFeatures <- modelFeatures
    return(selFeatures)
  }

  #########################################################################
  ### Shiny Server Code
  #########################################################################
  shinyServer(
    function(input,output,session) {
      # Input selector to choose from supported databases
        output$dataset <- renderUI({
          selectInput("selectedDataset","Data Source",datasets,selected = "attitude")
        })
    
      # cbFeatures - Generate a group of check boxes for each of the features.
      # The user can select or deselect each feature to be included
      # (or not) in the model
        output$cbFeatures <- renderUI({
        dset <- getDataset(input$selectedDataset)
        features <- getFeatures(dset)
        checkboxGroupInput("Features","Features",features,selected=unlist(features))
      })
    
      # rbModelInfo - Generate a group of radio buttons to choose the model to use for fitting
        output$rbModelInfo <- renderUI({
          choices = c("diagnostics","summary","anova","correlation")
          radioButtons("ModelInfoType","Fitted Model Info",choices,selected="diagnostics")
        })
    
      # pairplot - Create a pairs plot of the dependent variable and all of the selected features
        output$pairplot <- renderPlot({
          if ( length(input$selectedDataset) && length(input$Features) ) {
            dsetName = input$selectedDataset
            dset = getDataset(dsetName)
            pairs(dset[,unlist(c(getDependentValue(dset),getSelectedFeatures(dset,input$Features)))])
          }
       })
    
      # currModel - Make the current model formula (i.e. mgp ~ cyl+disp+am) available
        output$currModel <- renderText({
          dset <- getDataset(input$selectedDataset)
          paste("Current Model: ",getModelString(dset,input$Features))
        })
    
      # modelinfo - Generate the Fitted Model Info contents below the pairs plot.  This changes
      # depending on the item selected in the rbModelInfo radio buttons.  If diagnostics is
      # chosen, this section will be blank.  Otherwise, the text for some of the R commands
      # is captured and displayed.
        output$modelinfo <- renderUI({
          if ( length(input$ModelInfoType) ) {
            dset <- getDataset(input$selectedDataset)
            selFeatures <- getSelectedFeatures(dset,input$Features)
            if ( length(selFeatures)) {
              model <- getModel(dset,input$Features)
          
              if ( input$ModelInfoType == "anova") {
                info <- capture.output(anova(model))
                lines = ""
                for ( line in info ) {
                  if ( nchar(lines) > 0 || nchar(line) > 2 )
                    lines <- paste(lines,"<br/>",gsub(" ", "&nbsp;", line))
                }
                lines <- paste("<br/><h5>Analysis of Variance - anova(model)</h5><p style='margin-left: 20px; font-family: monospace;'>",lines,"</p>")
                return(HTML(lines)) # As HTML
              }  
              else if ( input$ModelInfoType == "correlation") {
                df <- data.frame(as.numeric(dset[,getDependentValue(dset)]))
                for ( i in selFeatures)
                  df <- cbind(df,as.numeric(dset[,i]))
                names(df) <- c(getDependentValue(dset),selFeatures)
                info <- capture.output(cor(df, use="complete.obs", method="pearson") )
                lines = ""
                for ( line in info ) {
                  if ( nchar(lines) > 0 || nchar(line) > 2 )
                    lines <- paste(lines,"<br/>",gsub(" ", "&nbsp;", line))
                }
                lines <- paste("<br/><h5>Correlation - cor(model)</h5><p style='margin-left: 20px; font-family: monospace;'>",lines,"</p>")
                return(HTML(lines))
              }
              else if ( input$ModelInfoType == "summary")  {
                info <- capture.output(summary(model))
                lines = ""
                for ( line in info )  {
                  if ( nchar(lines) > 0 || nchar(line) > 2 )
                    lines <- paste(lines,"<br/>",gsub(" ", "&nbsp;", line))
                }
                lines <- paste("<br/><h5>Model Summary - summary(model)</h5><p style='margin-left: 20px; font-family: monospace;'>",lines,"</p>")
                return(HTML(lines))
              }
            }
          }
        })
    
        # Fit a model and plot the diagnostics for the model
          output$modelplot <- renderPlot({
            if ( length(input$ModelInfoType) && input$ModelInfoType == "diagnostics" )  {
              dset <- getDataset(input$selectedDataset)
              selFeatures <- getSelectedFeatures(dset,input$Features)
                mdl <- getModel(dset,selFeatures)
                par(mfrow=c(2,2))
                plot(mdl)
                par(mfrow=c(1,1))
              }
          })
  })