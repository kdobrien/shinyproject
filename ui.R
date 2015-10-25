#setwd("/Users/kobrien/DataScientist/DataProducts/shinyproject")
library(shiny)
shinyUI(fluidPage(
  fluidRow(
    tags$style(type="text/css","h2 { color: darkblue; }"),
    tags$style(type="text/css",".divspace { margin-top: 80px; }"),
    tags$style(type="text/css",".col1 { width: 150px; font-size: 90%;}"),
    tags$style(type="text/css",".smalltext { font-size: 90%; }"),
    column(2,"",class="col1",
      div(class="divspace"),
      uiOutput("dataset"),
      uiOutput("cbFeatures")

    ),
    column(10,
      h2("Linear Model Explorer"),

      tags$div(class="smalltext",
        HTML("This application makes it easy to choose features to use for a linear regression and provides"),
        HTML("information on the effect of the choices.  Interactively change which features are used for"),
        HTML("the model fit and immediately view the effects on the Fitted Model Information.")
      ),
      tags$div(class="smalltext", style="padding-left: 20px;",
        HTML("1) Select a <em style='color:blue'>Data Source</em>"),
        HTML("<br/>2) Choose which <em style='color:blue'>Features</em> to include in the model"),
        HTML("<br/>3) View <em style='color:blue'>Fitted Model Information</em> to see the effects of the features chosen.  "),
        HTML("Repeat steps 2 and 3 as desired.")
      ),
      plotOutput("pairplot")
      
    )
  ),
  fluidRow(
    column(2,"",class="col1",
      uiOutput("rbModelInfo")
    ),
    column(10,
           h6(textOutput("currModel")),
           uiOutput("modelinfo"),
           plotOutput("modelplot")
    )
  )
))