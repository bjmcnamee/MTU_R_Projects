getwd(); setwd("/home/barnyard/rstudio/scripts/college/Intro_R/Ass1"); dir()    # setup project, check files, wd, etc
library(); library(xlsx)                                                        # check for library, load xlsx
library(ggplot2)

# 1. Using the xlsx or readxl package or otherwise, read each sheet in the ”assignment1.xlsx” file into R.

read.xlsx("assignment1.xlsx","Sheet1")                                          # read xls file 1st sheet into dataframe 'Sheet1'
read.xlsx("assignment1.xlsx","Sheet2")                                          # read xls file 2nd sheet into dataframe 'Sheet2'


# 2. Generate a data frame for each sheet in the file.

Sheet1 <- read.xlsx("assignment1.xlsx","Sheet1", stringsAsFactors=FALSE)        # read xls file 1st sheet into dataframe 'Sheet1' and read factors as characters/strings
Sheet2 <- read.xlsx("assignment1.xlsx","Sheet2", stringsAsFactors=FALSE)        # read xls file 2nd sheet into dataframe 'Sheet2' and read factors as characters/strings
head(Sheet1); str(Sheet1)                                                       # take a peek at data
head(Sheet2); str(Sheet2)                                                       # take a peek at data

# 3. The dataset in the first sheet is a random selection from a larger dataset. You will never get access to the full dataset so you should regenerate a new identification number for each subject in the dataset. This should be the row number of each entry in Sheet 1. You do not need to do this for Sheet 2.

for (row in 1:nrow(Sheet1)) {                                                   # loop through each row of df 'Sheet1'
  Sheet1$newID[row] <- row                                                      # assign value of row number to new var 'newID'
}
head(Sheet1)                                                                    # confirm all good


# 4. It is also required to have an additional identifier which is the number you have generated in (3) followed by the first letter of each subjects first name and then followed by the first letter of each subject’s surname. You do not need to do this for Sheet 2.

for (row in 1:nrow(Sheet1)) {                                                   # loop through each row of df 'Sheet1'
  # assign new value to new var 'otherID' - new value is a concatenation of row number, 1st char of 'FirstName' and 1st char of 'Surname'
  Sheet1$otherID[row] <- paste(row,substr(Sheet1$FirstName[row],1,1),substr(Sheet1$Surname[row],2,2), sep='')
}
head(Sheet1)                                                                    # confirm all good


# 5. Although the data is not available for most subjects, some data highlighting subjects state of health is available in Sheet 2. You should use the subjects ID number to match it and merge it with the data in Sheet 1.

for (row1 in 1:nrow(Sheet1)) {                                                  # loop through each row of df 'Sheet1' 
  for (row2 in 1:nrow(Sheet2)) {                                                # loop through each row of df 'Sheet2'
    # evaluate True or False: if Sheet1 IDNumber matches Sheet2 IDNumber AND also if Sheet2 IDNumber value is available, ie not 'NA'
    if ((Sheet1$IDNumber[row1] == Sheet2$IDNumber[row2]) && !is.na(Sheet2$IDNumber[row2])) {
      # if previous statement True then this statement is executed - assign Sheet2 Health value to Sheet2 Health value
      Sheet1$Health[row1] <- Sheet2$Health[row2]                                
      break                                                                     # row found so no need to continue searching Sheet2 --> break out of loop
    } else {                                                                    # if the if statement not True then execute the following line
      Sheet1$Health[row1] <- 'NA'                                                # otherwise assign 0 to represent no health status
  }
}}
head(Sheet1)                                                                    # confirm all good


# 6. Not every subject has its ID number included in Sheet 2. You should attempt to match the remaining subjects using their first and surnames. This must be done using tidyverse in a robust manner. Your code for doing this should work again in the case of a new sample of data being provided.
# install.packages("kableExtra")
library(tidyverse)                                                              # load the tidyverse library
x <- Sheet2 %>% filter(is.na(IDNumber))                                         # create a new dataframe 'x' with only rows (filter) from Sheet2 that have IDNumber = 'NA'
x                                                                               # check x as expected
# create a new dataframe 'y' with ;
# 1. rows (filter) from Sheet1 that have Health = 'NA'; 
# 2. rows (filter) that have Sheet1 FirstName = Sheet2 FirstName and Sheet1 Surname = Sheet2 Surname - and make vectors in each dataframe equal length
# 3. change (mutate) Sheet1 Health value to match Sheet2 Health value - if FirstName and Surname values also match
y <- Sheet1 %>% filter(Sheet1$Health == 'NA') %>% filter(FirstName %in% x$FirstName & Surname %in% x$Surname) %>% mutate(Health = ifelse(((FirstName==x$FirstName)&(Surname==x$Surname)), x$Health, Health))
Sheet1[match(y$IDNumber, Sheet1$IDNumber), ] <- y                               # overwrite rows in Sheet1 with all rows in dataframe y (with updated Health)
Sheet1                                                                          # check x as expected

# 7. You should add a column for age range. This should be 
"""
Age Range Category Name
0-17      1
18-35     2
35-54     3
54-74     4
74-       5
"""

Sheet1$AgeRange <- 'NA'                                                         # add new column titled 'Age Range' with no (NA) values
Sheet1$AgeRange[Sheet1$Age < 18] <- "1"                                         # assign all records with Age < 18 a label '1'
Sheet1$AgeRange[Sheet1$Age >= 18 & Sheet1$Age < 35] <- "2"                      # assign all records with Age 18 - 35  a label '2'
Sheet1$AgeRange[Sheet1$Age >= 35 & Sheet1$Age < 54] <- "3"                      # assign all records with Age 35 - 54 a label '3'
Sheet1$AgeRange[Sheet1$Age >= 54 & Sheet1$Age < 74] <- "4"                      # assign all records with Age 54 - 74 a label '4'
Sheet1$AgeRange[Sheet1$Age >= 74] <- "5"                                        # assign all records with Age > 74 a label '5'
head(Sheet1); str(Sheet1)                                                       # confirm all good


# 8. You should filter the data by each age category. Generate a bar plot using ggplot2 for the criminal record variable.
label = 'Age Range\tCategory Name\n\t\t0-17\t\t\t1\n\t\t18-35\t\t2\n\t\t35-54\t\t3\n\t\t54-74\t\t4\n\t\t74-\t\t\t5'
df <- Sheet1 %>% select(AgeRange, CriminalRecord)                               # select AgeRange and CriminalRecord from Sheet1 and then group by AgeRange
df <- data.frame(table(df))                                                     # create frequency table of values
ggplot(df, aes(AgeRange,Freq,fill=CriminalRecord)) + geom_histogram(stat='identity') + geom_text(aes(label = label, x = -Inf, y = Inf),size=4, hjust = -1, vjust = 1.1) # plot AgeRange v Frequency of AgeRange and fill bars with the CriminalRecord variable


# 9. You should generate an appropriate visualisation examining the relationships between height, weight, age and criminal records. Comment on this.
df <- Sheet1 %>% select(Height.m., Weight.kg., Age, CriminalRecord)
# shows the 6 charts for the reltionship between each variable and the other - comments no relationship between any of the variables for this dataset
plot(df)                                                                        
# although not requested, frequency distribution shows some interesting distributions for these variables
df %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()

# plot Height and Weight variables for each category of CriminalRecord (CR)
CR=as.factor(Sheet1$CriminalRecord)                                             # change CriminalRecord to factor variable
# scatterplot Height and Weight variables on xy axis and label each point with a colour corresponding to CrimoinalRecord category + display in two charts, one for each CR category
ggplot(Sheet1, aes(Height.m.,Weight.kg., col=CR)) + geom_point() + facet_grid(~CriminalRecord)

# boxplot for a different view of variable Weight --> the overall range, IQR range median is greater for CR=1 than CR=2
ggplot(Sheet1, aes(Height.m.,Weight.kg., col=CR)) + geom_boxplot() + facet_grid(~CriminalRecord)

# boxplot for a different view of variable Height --> the 3rd quantile is smaller for CR=1 than CR=2 but overall range and median the same
ggplot(Sheet1, aes(Weight.kg.,Height.m., col=CR)) + geom_boxplot() + facet_grid(~CriminalRecord)



# 10. Using filters, you should analyse if there are any interesting results in the dataset regarding the relationships between height, weight and criminal record. Use appropriate visualisations.

head(Sheet1); str(Sheet1)                                                       # take a peek at data
df1 <- Sheet1 %>% filter(CriminalRecord=='1') %>% select(Height.m.,Weight.kg.,CriminalRecord) # filter Height + Weight vars by CR = 1
df2 <- Sheet1 %>% filter(CriminalRecord=='2') %>% select(Height.m.,Weight.kg.,CriminalRecord) # filter Height + Weight vars by CR = 2

# although not requested, frequency distribution shows some interesting distributions for these variables
p1 <- ggplot(Sheet1) + geom_bar(mapping = aes(Height.m.))
p2 <- ggplot(Sheet1) + geom_bar(mapping = aes(Weight.kg.))
p3 <- ggplot(Sheet1) + geom_bar(mapping = aes(CR))
grid.arrange(p1, p2, p3, nrow = 1)

# plot both dataframes for Height and Weight vars as scatterplot adding linear regression, labels, legend
ggplot(df2,aes(Height.m.,Weight.kg.)) + geom_point(aes(color = factor(CriminalRecord))) + geom_smooth(data=df1,method=lm,aes(color = factor(CriminalRecord))) + geom_point(data=df1,aes(Height.m.,Weight.kg.,color = factor(CriminalRecord))) + geom_smooth(data=df2,method=lm,aes(color = factor(CriminalRecord))) + labs(x="Height (m)", y="Weight (kg)", color = "Criminal Record") + theme(legend.position = c(0.95, 0.95),legend.justification = c("right", "top"))


#11. Generate a smaller data frame for the subjects where health related data is available. 

# Examine if there is a relationship between the different states of health and height, weight or age. Use appropriate visualisations. Note this should include a modelling type analysis such as regression. (S.Weisberg. Applied Linear Regression. Wiley Series in Probability and Statistics, 2005. may be useful)'''

# create new smaller dataframe with only Health, Height and Weight vars and only (filter) Health var out where Health unknown ('NA') 
df <- Sheet1 %>% select(Health, Height.m., Weight.kg., Age) %>% filter(Health != 'NA') 
# plot Health v Height vars as scatterplot
ggplot(df, aes(Health,Height.m., col=Health)) + geom_jitter() + geom_smooth(method=lm) + stat_summary(fun.data = mean_se, col="black")
# plot Health against Weight
ggplot(df, aes(Health,Weight.kg., col=Health)) + geom_jitter() + geom_smooth(method=lm) + stat_summary(fun.data = mean_se, col="black")
# plot Health against Age
ggplot(df, aes(Health,Age, col=Health)) + geom_jitter() + geom_smooth(method=lm) + stat_summary(fun.data = mean_se, col="black")
# plot Height against Weight
ggplot(df, aes(Height.m.,Weight.kg.)) + geom_point() + geom_smooth(method='lm')
# plot Height against Age
ggplot(df, aes(Height.m.,Age)) + geom_point() + geom_smooth(method='lm')
# plot Weight against Age
ggplot(df, aes(Weight.kg.,Age)) + geom_point() + geom_smooth(method='lm')


# ADDITIONAL FINDINGS

# plot Age and Education variables on xy axis and label each point with a colour corresponding to CriminalRecord category + display in two charts, one for each CR category
# null or no relationship between Age and CR and between Education and CR

Sheet1Ed <- subset(Sheet1,(Sheet1$Education.Level!='FALSE'))                    # remove 'false' values, probably NAs
CR=as.factor(Sheet1Ed$CriminalRecord)                                           # change CriminalRecord to factor variable again

# reorder the labels according to education ranking and rename so more readable on chart
Sheet1Ed$Education.Level <- factor(Sheet1Ed$Education.Level,levels=c('Primary','secondary','graduate','masters','doctorate'),labels=c(A='1st',B='2nd',C='3rd',D='3M',E='PhD')) 
# boxplot view of variable Age and Education --> null/no meaningful reltionships/patterns
ggplot(Sheet1Ed, aes(Education.Level, fill=CR)) + geom_histogram(stat='count',bins = 30) + facet_grid(~CriminalRecord)

# I know I was not asked to look at Education v CR butI was curious to test my assumption that men (dataset has only males) with less education (reflecting  lower social demographics) would be more likely to have a criminal record - even though we're not told what CR = 1 or 2 actually mean, 1 = petty crime? 2 = serious crime?

