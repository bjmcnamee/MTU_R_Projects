""" STAT8010 Assignment 2
1. You should build a Shiny app or dashboard allowing a scatterplot for any combination of variables to be displayed. Additionally, you should be able to generate histograms, boxplots
etc. of your data in this app.
2. You should include the ability to fit a linear regression model to the scatterplots generated. The chart should include the fitted line and a table with the slope and intercept should
be present within the Shiny App or dashboard"""

setwd("/home/barnyard/rstudio/scripts/college/Intro_R/Ass2")
library(shiny)
library(ggplot2)
library(gridExtra)
setwd("/home/barnyard/rstudio/scripts/college/Intro_R/Ass2")
df <- read.csv("process_sim.csv", header=TRUE) # read data file into dataframe 'df'
Variables = c("year"="year", "month"="month", "day"="day", "hour"="hour", "concentration of target product"="tconc", "ph reading"="Ph", 
              "Temperature (Celsius)"="TEMP", "Pressure (hPa)"="PRES", "label of feed used"="feed", "Undesired proteins"="undesP", 
              "Cumulated hours of unplanned down time"="udt", "Cumulated hours of planned down time"="pdt") # read dataframe columns as variables
df <- na.omit(df) # data cleaning - remove any rows with NA values (which may otherwise cause plotting issues)
df.feed <- factor(df$feed, labels = c("NW","cv","NE","SE")) # convert the one non-numeric variable to factor
# Define UI for app ----
ui <- fluidPage(
  # App title ----
  titlePanel("Shiny Plot Demo"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Selector for plot/chart type ----
      # create input variable 'chart' as drop down list of selectable charts
      selectInput("chart", "Chart Type:", c("Jitterplot"="j","Scatterplot" = "s", "Line plot" = "l", "Histogram" = "h","Boxplot" = "b")),
      # create input variable 'var1' as drop down list of selectable variables from df
      selectInput("var1", "Variable 1:", Variables),
      # create input variable 'var2' as drop down list of selectable variables from df
      selectInput("var2", "Variable 2:", Variables),
      # create checkbox for linear regression option
      checkboxInput("LR", "Show linear regression", FALSE) 
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Formatted text for caption ----
      h3(textOutput("plotcaption")),
      # Output: Plot of the requested variables ----
      plotOutput("Plot"),
      # Output: Table with slope and intercept ----
      h3(textOutput("tablecaption")),
      tableOutput("values"),
      )))
# Define server logic to plot various variables ----
server <- function(input, output) {
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$Plot functions
  formulaText1 <- reactive(paste(input$var1, " v ", input$var2))
  formulaText2 <- reactive(if (input$chart!="h") {paste(input$var1, " v ", input$var2)})
  
  
  # Return the formula text for printing as a caption ----
  output$tablecaption <- renderText({formulaText1()})
  output$plotcaption <- renderText({formulaText2()})
  
  # Generate a plot of the requested variables ----
  output$Plot <- renderPlot ({ 
    x <- df[input$var1][,] # create vector of var1 values
    y <- df[input$var2][,] # create vector of var2 values
    
    if (input$var2 == 'feed' || input$var1 == 'feed') {filling = df.feed} else {filling=""} # create filling for boxplots using the factor var 'feed'
     
    # if input$chart = a,b,c then plot a,b,c with labels, titles and linear regression
    if (input$chart == "j")
        {ggplot(df,aes(x,y)) + geom_jitter() + labs(x=input$var1,y=input$var2) + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + if (input$LR == TRUE) {geom_smooth(method='lm',se=FALSE)}}
    else if (input$chart == "s") 
        {ggplot(df,aes(x,y)) + geom_point() + labs(x=input$var1,y=input$var2) + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + if (input$LR == TRUE) {geom_smooth(method='lm',se=FALSE)}}
    else if (input$chart == "l")
        {ggplot(df,aes(x,y)) + geom_line() + labs(x=input$var1,y=input$var2) + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + if (input$LR == TRUE) {geom_smooth(method='lm',se=FALSE)}}
    else if (input$chart == "b") 
        {ggplot(df,aes(x,y, fill = filling))  + geom_boxplot() + labs(x=input$var1,y=input$var2)  + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))}
    # plot two separate histograms if h selected
    else if (input$chart == "h") 
        {require(gridExtra)
        plot1 <- ggplot(df,aes(x)) + geom_bar(width = 0.5) + labs(x = input$var1)  + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
        plot2 <- ggplot(df,aes(y)) + geom_bar(width = 0.5) + labs(x = input$var2)  + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
        grid.arrange(plot1, plot2, ncol=2)}
  })
  tableValues <- reactive({
    x = df[c(input$var1)][,]; y = df[c(input$var2)][,] # recalculate x,y
    # if not factor variable feed then...
    if (input$var2 != 'feed' && input$var1 != 'feed') {
        # create mini dataframe with slope and intercept from coef(lm(y~x,df)) function
        data.frame(
        Name =  c("Slope", "Intercept"),
        Value = c(
          round(coef(lm(y~x, data = df))[2],4), # get slope from coef() fn
          round(coef(lm(y~x, data = df))[1],4) # get intercept from coef() fn
        ))} 
  })
  # Show the values in an HTML table ----
  output$values <- renderTable({tableValues()})
}
# Create Shiny app ----
shinyApp(ui, server)



