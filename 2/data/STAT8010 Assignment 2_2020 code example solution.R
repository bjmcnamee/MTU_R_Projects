#set directory and load in data
setwd("/home/barnyard/rstudio/scripts/college/R/Ass2") # to set the working dirctory to the folder that contains the data for the assignment
process.data<-read.csv("process_sim.csv")  


#examine data and NAs
head(process.data) 
str(process.data) 
sum(is.na(process.data$tconc))
    
sapply(lapply(process.data, is.na),sum)#note NAs in tconc    

#convert appropriate varaibles to factor
process.data$feed<-as.factor(process.data$feed)

##can convert time variables also if desired - should do this if using these for modelling
#process.data$month<-as.factor(process.data$month)
#process.data$day<-as.numeric(process.data$day)
#process.data$hour<-as.numeric(process.data$hour)





##################
##################
#Q1&2 - Shiny app
##################
##################

library(shiny)
#optional themes to change appearace
#install.packages("shinythemes") 
#library(shinythemes) 

###################################################################################################
#The app below is a basic example of what was expected ; you could go further and make the app more
# aestetic, robust and intuitive if desired.  This is given as a quick indication of the minimum of 
# what was expected.
###################################################################################################

# Define UI for the application
ui <- fluidPage(#theme = shinytheme("Enter specified theme"),  #optional theme change
                titlePanel("Process Dataset"), 
                sidebarLayout(  
                  sidebarPanel( 
                   #set up radiobutton to select plot type
                     radioButtons("type", "Plot Type", 
                                 c("Scatterplot"="scat", 
                                   "Boxplot"="box")), 
                   #create dropdown list for x varaible, tconc is default
                    selectInput("x", "Select x-axis", 
                                c("year", "month", "day", "hour", 
                                  "Concentration of target product"="tconc", 
                                  "Ph reading"="Ph", 
                                  "Temperature(Celsius)"="TEMP", 
                                  "Pressure(hPa)"="PRES", 
                                  "label of feed used"="feed", 
                                  "Undesired proteins"="undesP", 
                                  "Cumulated hours of unplanned down time"="udt",
                                  "Cumulated hours of planned down time"="pdt"),"tconc"), 
                   #create dropdown list for y varaible, Ph is default
                    selectInput("y", "Select y-axis", 
                                c("year", "month", "day", "hour", 
                                  "Concentration of target product"="tconc",
                                  "Ph reading"="Ph", "Temperature(Celsius)"="TEMP", 
                                  "Pressure(hPa)"="PRES", 
                                  "label of feed used"="feed", 
                                  "Undesired proteins"="undesP", 
                                  "Cumulated hours of unplanned down time"="udt", 
                                  "Cumulated hours of planned down time"="pdt"),"Ph"), 
                    #checkbox to decide if regression line is included or not, off by default
                    checkboxInput("regline", "Show regression line", FALSE), 
                   #slider input to select number  of bins in histogram - can hide this if desired when hist is not active
                    sliderInput("bins",  
                                "Number of bins:",
                                min = 0, 
                                max = 50,
                                value = 30) 
                  ),
                  mainPanel(   
                    #tab panel for each output
                    tabsetPanel(#type = "pills",  # optional styling
                                #main plot output including table of coefficients if active
                                tabPanel("Plot", plotOutput("plotxy"), tableOutput("Coeff_Table")), 
                                #outpit of data summary
                                tabPanel("Summary", verbatimTextOutput("summary")), #to create a tab panel that includes a summary output
                                # histogram tab
                                tabPanel("Histogram", plotOutput("histx")) ##to create a tab panel that includes a histogram
                                
                                
                    )
                  )
                )
)

#define the server for the app used to show all the outputs in the ui
server <- function(input, output){ 
  #to render the main plot 
  output$plotxy <-renderPlot({  
    if(input$type=="scat"){  #if the input is a selected as a scatterplot, 
      if (input$regline==TRUE){ # regression checkbox is on, create model and include abline on plot:
        model <- lm(process.data[,input$y] ~ process.data[,input$x]) # the model 
        plot(process.data[input$x][[1]],process.data[input$y][[1]], # basic scatterplot 
             xlab=input$x, ylab=input$y, # labels
             main = paste("Scatterplot of", input$y, "vs", input$x)) # plot title
        abline(model$coeff) # add fitted line to the plot from the model
        
      }else{  #if checkbox is off, just do basic plot with no model or fitted line
        plot(process.data[input$x][[1]],process.data[input$y][[1]], 
             xlab=input$x, ylab=input$y, 
             main = paste("Scatterplot of", input$y, "vs", input$x)) 
        
      }
    }
    #to render boxplot 
    else if(input$type=="box"){# if input is selected as boxplot
      boxplot(
        as.formula(paste(input$y, "~", input$x)), #paste formula using inputs
        data=process.data, #dataset
        xlab=input$x, ylab=input$y,  #labels
        main = paste("Boxplot of", input$y, "vs", input$x)) #plot title
    }
      })
  
  #to render a summary of the dataset
  output$summary <-renderPrint({ 
    summary(process.data) 
  }
  
  )
  #to render coefficient table for model
  output$Coeff_Table <-renderTable({ 
    if (input$regline==TRUE){  # if the checkbox is on
      model <- lm(process.data[,input$y] ~ process.data[,input$x]) # fit model to specified data
      data.frame(Intercept=model$coeff[1], Slope=model$coeff[2]) #dataframe containing intercept and slope of model
    }
    
  }
  )
  #render histogram
  output$histx <- renderPlot({ 
    hist(process.data[input$x][[1]], 
         xlab = input$x, 
         main = paste("Histogram of", input$x),
         breaks=input$bins)  #user specified number of bins
    })
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server) 

##note you should add options and messages to stop the user doing silly things, like trying to do 
# a scatterplot with feed, or anything else the causes an error.  You could also have panels that 
# change depending on what was selected by the user.



###########################
###########################
#Q3 - process simulation
###########################
###########################

#check correlations of numeric varaibles
round(cor(process.data[-10], use = "c"),2)
#Ph, UndesP, Temp, are highest correlated (apart from day), 
# though non of these are particulary strong

#Linear model created using year as the time variable.
#Can use other time variables but this tricker as they need to be dealt with as factors
summary(lm(tconc~Ph+TEMP+PRES+undesP+udt+pdt+year, data = process.data))
#model pval<<0.05 => significant.  Note that year and intercept are not significant.
#need to keep year as time varaible, but could remove intercept

#storing model coefficients
model1<-summary(lm(tconc~Ph+TEMP+PRES+undesP+udt+pdt+year, data = process.data))$coeff 

#measure of spread
disp1 <- mad(summary(lm(tconc~Ph+TEMP+PRES+undesP+udt+pdt+year, data = process.data))$resid) 
# Median Absolute Deviation value is a better measure as it is less affected by large outliers.
#note that st.dev would be a bit larger here!

last <- nrow(process.data) # number of rows
 
#examine what years are in data, (2010 - 2014) 
unique(process.data$year) 


nyear<-sum(process.data$year==2014) # number of obs in last year

sim1<-NULL

#simulating from last data point in each variable
for(i in 1:nyear){
  sim1 = cbind(sim1, model1[1]+model1[2]*process.data$Ph[last]+
                      model1[3]*process.data$TEMP[last]+
                      model1[4]*process.data$PRES[last]+
                      model1[5]*process.data$undesP[last]+
                      model1[6]*process.data$udt[last]+
                      model1[7]*process.data$pdt[last]+
                      model1[8]*2015+ # index to the year 2015 for prediction
                      abs(rnorm(1 , 0, disp1))) # simulating residuals
} # absolute values returned as concentration can't be less than 0, 
# this will better mirror the real data, can use other methods also, 
# but you shouldn't be predicting negative numbers if they don't exist in the previous data 
# and are technically not possible.


par(mfrow=c(1,2)) # side by side view of plots

process2014 <- process.data %>% filter(year==2014)

hist(process2014$tconc, xlim = c(0,50) ) # last years distribution
hist(sim1[1,], xlim = c(0,50)) # simulated distribution
# not perfect, but not too bad


plot(process2014$tconc, ylim = c(0,60))
plot(sim1[1,], ylim = c(0,60))
#seems to capture the varition of the main chunk of the data, but not great - doesn't capture peaks
#########

#Simulate using all of last years data at each point in time.

sim2<-NULL
for (i in 1:nyear){
  sim2 = cbind(sim2, model1[1]+model1[2]*process2014$Ph[i]+
                      model1[3]*process2014$TEMP[i]+
                      model1[4]*process2014$PRES[i]+
                      model1[5]*process2014$undesP[i]+
                      model1[6]*process2014$udt[i]+
                      model1[7]*process2014$pdt[i]+
                      model1[8]*2015+ 
                      abs(rnorm(1 , 0, disp1))
  )
}


#compate distrbutions
hist(process2014$tconc, xlim = c(0,50) ) # last years distribution
hist(sim2[1,], xlim = c(0,50)) # simulated distribution



plot(process2014$tconc, ylim = c(0,60))
plot(sim2[1,], ylim = c(0,60))

#this seems to give a better representation of the spread of the data, but the distribtution of 
# points is off

####################################
#All you had to do was compare two models, so above would get you most of the marks if disussed 
# appropriatey.  But to get full marks, you would need to think about how best to model the situation.
# any reasonable attempt in this regard that showed some initative was marked generously  
######################################


#An advanced idea is to try a log model, where you try to predict the log of tconc, as this is more
# normal
hist(log(process.data$tconc))

summary(process.data$tconc)

#if taking the log, need to avoid log(0)
sum(process.data$tconc==0, na.rm = T) #this happens 2 times

#avoid this by simply adding 1 to these values
process.data$tconc[process.data$tconc==0] <- 1

summary(log(process.data$tconc)) #no issues

#log model
logmodel<-lm(log(tconc)~Ph+TEMP+PRES+undesP+udt+pdt+year, data = process.data)
summary(logmodel)
#note that udt is insignificant, so remove
logmodel<-lm(log(tconc)~Ph+TEMP+PRES+undesP+pdt+year, data = process.data)
summary(logmodel) #all significant

#model coeffs
model3<-summary(logmodel)$coeff

#dispersion
disp2 <- mad(summary(logmodel)$resid)  

#simulate using log model at each point from last year
sim3<-NULL
for (i in 1:nyear){
  sim3 = cbind(sim3, exp(model3[1]+ # take exponential to get actual predictions of tconc
                         model3[2]*process2014$Ph[i]+
                         model3[3]*process2014$TEMP[i]+
                         model3[4]*process2014$PRES[i]+
                         model3[5]*process2014$undesP[i]+
                         model3[6]*process2014$pdt[i]+
                         model3[7]*2015+ 
                         rnorm(1 ,mean = 0, disp2))
  )
}

hist(process2014$tconc, xlim = c(0,50), breaks = 20)
hist(sim3[1,],xlim = c(0,50), breaks = 20)
#better reflection of distribution

plot(process2014$tconc, ylim = c(0,70))
plot(sim3[1,], ylim = c(0,70))
#gives a better picture of overall variation of data, but does contain some outliers at 
# high values >80 (likely due to taking log of small values in model, could deal with this further)



#########################
#Q4 - machine simulation
#########################

End.day.dev <- rep(NA,1000)
for(j in 1:1000){# run the random walk 1000 times
  
  align.x <- 0 #aligned at start of day
  align.y <- 0
  for(i in 1:200){
    dev.x <- rnorm(1,0.1,0.1) # random deviation in x direction
    dev.y <- rnorm(1,-0.05,0.05)
    align.x <- align.x+dev.x # add this to previous steps - current alignment
    align.y <- align.y+dev.y
  }
  End.day.dev[j] <- sqrt(align.x^2+align.y^2) # how far misaligned at end of day
}  

hist(End.day.dev)
abline(v=20,col="red")
sum(End.day.dev>20)/1000
#significant chance of misalignment ~97%


#now to simulate visuilisation of outcomes
End.day.dev.x <- rep(NA,1000)
End.day.dev.y <- rep(NA,1000)

for(j in 1:1000){# run the random walk 1000 times
  
  align.x <- 0 #aligned at start of day
  align.y <- 0
  for(i in 1:200){
    dev.x <- rnorm(1,0.1,0.1) # random deviation in x direction
    dev.y <- rnorm(1,-0.05,0.05)
    align.x <- align.x+dev.x # add this to previous steps - current alignment
    align.y <- align.y+dev.y
  }
  End.day.dev.x[j] <- align.x
  End.day.dev.y[j] <- align.y
} 

#create data frame for ggplot
dev.data <- data.frame(Dev.x = End.day.dev.x, Dev.y = End.day.dev.y) 
library(ggplot2)

#create scatterplot of final allignments and show limit spec
ggplot(dev.data, aes(x=Dev.x, y=Dev.y))+
  geom_point()+
  annotate("path",
           x=20*cos(seq(0,2*pi,length.out=1000)),
           y=20*sin(seq(0,2*pi,length.out=1000)), colour="red")+
  xlim(15,30)+
  ylim(-15,0)


#function to just calculate how many runs it takes before allignment is out of spec
# if it remains within spec, function return the value n+1
when.outside <- function(n){#simulate if/when misalignment occurs
  dev.x <- rnorm(n,0.1,0.1)
  dev.y <- rnorm(n,-0.05,0.05)
  cum.dev.x <- cumsum(dev.x)
  cum.dev.y<- cumsum(dev.y)
  cum.dev <- sqrt(cum.dev.x^2+cum.dev.y^2)
  index <- 1:n
  ifelse(is.na(index[cum.dev>=20][1]),n+1,index[cum.dev>=20][1])
  #this will store the number if runs when it deviates outside 2cm, 
  # or will return n+1 if it remains inside this limit
}

sim <- replicate(1000, when.outside(200))
# when misaligned (n+1 [201] implies still within bounds at end of day)
hist(sim)
sum(sim<201)/1000
#~96.8% will become misaligned at some point in the day




###########################
#Q5-cost/profit simulation
############################

profit <- function(n){#function to simulate profit/loss
  ofline.cost <- 50000
  run.profit <- 400
  run.loss <- 500
  sim <- replicate(1000, when.outside(n))#simulate 1000 times
  costvec <- ifelse(sim<=n, run.profit*sim-ofline.cost-run.loss*(n-sim), n*run.profit)
  mean(costvec)#calculate mean profit over all simulations
}

#example profit for 220 runs per day
profit(220)

#run this expected mean profit simulation for various values if target n
N=100:200
ECOST = sapply(N, profit) # this may take a few seconds to run

#visualise results
plot(N, ECOST, xlab ="Number of runs per day", ylab ="Expected profit in Euro")

max(ECOST)
N[ECOST==max(ECOST)]
#clear peak around 160, which is the optimal runs per day at this cost and set up.
