
library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Play with mtcars data"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     h3("Exploratory Plots"),
                     selectInput("var1", "Select variable for x axis", 
                                 choices = c("mpg", "disp", "hp", "drat", "wt", "qsec"), 
                                 selected = "mpg"),
                     selectInput("var2", "Select variable for y axis", 
                                 choices = c("mpg", "disp", "hp", "drat", "wt", "qsec"), 
                                 selected = "hp"),
                     selectInput("var3", "Select a factor variable for color", 
                                 choices = c("cyl", "vs", "am", "gear"), 
                                 selected = "cyl")
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     h3("Compare between two models"),
                     selectInput("var4", "Select variable", 
                                 choices = c("cyl", "vs", "am", "gear"), 
                                 selected = "cyl"),
                     selectInput("var5", "Select predictor variables (minimum 2)", 
                                 choices = c("mpg", "disp", "hp", "drat", "wt", "qsec",
                                             "cyl", "vs", "am", "gear"), 
                                 selected = c("mpg", "disp"), multiple = TRUE),
                     checkboxInput("showModel1", "Show/Hide rpart model", 
                                 value = FALSE),
                     checkboxInput("showModel2", "Show/Hide Random Forest model", 
                                   value = FALSE)
    ) 
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Panel 1", plotOutput("plot1"), value = 1), 
      tabPanel("Panel 2", plotOutput("plot2"), tableOutput("table"), value = 2)
      , id = "conditionedPanels"
    )
  )
))
