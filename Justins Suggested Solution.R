

##############################################################################
# Install and Load xlsx package
#install.packages("xlsx")
#install.packages("tidyverse")
#install.packages("gridExtra")

library(xlsx)
library(tidyverse)
library(gridExtra)
library(readxl)
getwd()


setwd("/home/barnyard/rstudio/scripts/college/R/Ass1")



#?read.xlsx()
#read in file
data.sheet.1 <- read_xlsx("assignment1.xlsx",1,col_names = TRUE)

str(data.sheet.1)
#note that this is a 160x10 tibble, can leave as a tibble or convert to data.frame if desired
data.sheet.1 <- as.data.frame(data.sheet.1)
View(data.sheet.1)


head(data.sheet.1)
# IDNumber FirstName     Surname  Age        DoB Height(m) Weight(kg) Education Level  Salary CriminalRecord
# 1        4    Marvin       Kelly 32.3 1986-07-08       1.6        205        graduate 26424.7              2
# 2        8      Glen  O'Sullivan 27.8 1991-01-10       1.5        189       doctorate 28300.7              2
# 3       12     Milan       Walsh 11.3 2007-07-10       1.9         93       secondary 14745.3              2
# 4       15     Ollie       Smith  5.1 2013-08-24       1.6        131       secondary 28268.0              2
# 5       23    Harlan     O'Brien 15.5 2003-04-12       2.1        233           FALSE  6904.7              1
# 6       26    Hollis       Byrne 76.0 1942-11-02       2.1        178       secondary 13856.7              2

names(data.sheet.1)
# [1] "IDNumber"        "FirstName"       "Surname"         "Age"             "DoB"             "Height(m)"      
# [7] "Weight(kg)"      "Education Level" "Salary"          "CriminalRecord" 

str(data.sheet.1)
# 'data.frame':	160 obs. of  10 variables:
# $ IDNumber       : num  4 8 12 15 23 26 32 36 41 48 ...
# $ FirstName      : chr  "Marvin" "Glen" "Milan" "Ollie" ...
# $ Surname        : chr  " Kelly" " O'Sullivan" " Walsh" " Smith" ...
# $ Age            : num  32.3 27.8 11.3 5.1 15.5 76 9.1 75.5 53.4 50.4 ...
# $ DoB            : POSIXct, format: "1986-07-08" "1991-01-10" "2007-07-10" "2013-08-24" ...
# $ Height(m)      : num  1.6 1.5 1.9 1.6 2.1 2.1 1.9 1.9 1.6 1.5 ...
# $ Weight(kg)     : num  205 189 93 131 233 178 67 96 68 131 ...
# $ Education Level: chr  "graduate" "doctorate" "secondary" "secondary" ...
# $ Salary         : num  26425 28301 14745 28268 6905 ...
# $ CriminalRecord : num  2 2 2 2 1 2 1 2 2 2 ...

#convert DOB to date format - optional
data.sheet.1$DoB <- as.Date(data.sheet.1$DoB)

str(data.sheet.1)

#read in 2nd sheet
data.sheet.2 <- read_xlsx("assignment1.xlsx",2,col_names = TRUE)

#convert to data frame - optional (can leave as tibble if desired)
data.sheet.2 <- as.data.frame(data.sheet.2)

head(data.sheet.2)
# IDNumber FirstName   Surname Health
# 1       12     Milan     Walsh      1
# 2       15     Ollie     Smith      2
# 3       23    Harlan   O'Brien      1
# 4       NA    Hollis     Byrne      2
# 5       32      Chet      Ryan      2
# 6       36   Refugio  O'Connor      2


str(data.sheet.2)
# 'data.frame':	140 obs. of  4 variables:
# $ IDNumber : num  12 15 23 NA 32 36 41 48 NA 55 ...
# $ FirstName: chr  "Milan" "Ollie" "Harlan" "Hollis" ...
# $ Surname  : chr  " Walsh" " Smith" " O'Brien" " Byrne" ...
# $ Health   : num  1 2 1 2 2 2 2 1 1 1 ...




##############################################################################
#
#   Regenerate a new identification number based on the row number of each entry in Sheet 1
#   Use dplyr library from tidyverse
#

data.sheet.1 <- data.sheet.1 %>% mutate(NewID = row_number())
# Verify the number was changed
head(data.sheet.1)
head(data.sheet.1[c("IDNumber", "NewID", "FirstName", "Surname", "DoB")])
#   IDNumber NewID FirstName     Surname        DoB
# 1        4     1    Marvin       Kelly 1986-07-08
# 2        8     2      Glen  O’Sullivan 1991-01-10
# 3       12     3     Milan       Walsh 2007-07-10
# 4       15     4     Ollie       Smith 2013-08-24
# 5       23     5    Harlan     O’Brien 2003-04-12
# 6       26     6    Hollis       Byrne 1942-11-02

#note the ' (dash) in some of the names might not save correctly depending on the file encoding, so ignore errors 
# like ’ in above comment if this occurs.


##############################################################################
#
#   Add additional identifier using the NewID followed subjects initials
#


data.sheet.1$Surname
##note whitespaces are  present in SOME of the surnames, which can casue an issue
# remove these.  Various methods can be used

data.sheet.1$Surname <- trimws(data.sheet.1$Surname, whitespace = "[\\h\\v]")
#note that the space present is a non-breaking white space, so the above code recognizes this

data.sheet.1$Surname
#spaces have now been cleaned

#do same for sheet 2
data.sheet.2$Surname
data.sheet.2$Surname <- trimws(data.sheet.2$Surname, whitespace = "[\\h\\v]")
data.sheet.2$Surname

#create new identifier with IDnum and initials 
data.sheet.1 <- data.sheet.1 %>% 
  mutate(AdditionalID = paste0(NewID,
                               substr(FirstName,1,1),
                               substr(Surname,1,1)))


# Check
head(data.sheet.1[c("IDNumber", "FirstName", "Surname", "DoB", "NewID", "AdditionalID")])
#   IDNumber FirstName    Surname        DoB NewID AdditionalID
# 1        4    Marvin      Kelly 1986-07-08     1          1MK
# 2        8      Glen O'Sullivan 1991-01-10     2          2GO
# 3       12     Milan      Walsh 2007-07-10     3          3MW
# 4       15     Ollie      Smith 2013-08-24     4          4OS
# 5       23    Harlan    O'Brien 2003-04-12     5          5HO
# 6       26    Hollis      Byrne 1942-11-02     6          6HB



##############################################################################
#
#   Merge sheet 1 and 2 based on IDnumber

head(data.sheet.1)
head(data.sheet.2)


data.merged.1 <- data.sheet.1 %>% left_join(data.sheet.2, by = "IDNumber")

head(data.merged.1)
#   IDNumber FirstName.x  Surname.x  Age        DoB Height(m) Weight(kg) Education Level  Salary CriminalRecord NewID AdditionalID FirstName.y Surname.y Health
# 1        4      Marvin      Kelly 32.3 1986-07-08       1.6        205        graduate 26424.7              2     1          1MK        <NA>      <NA>     NA
# 2        8        Glen O'Sullivan 27.8 1991-01-10       1.5        189       doctorate 28300.7              2     2          2GO        <NA>      <NA>     NA
# 3       12       Milan      Walsh 11.3 2007-07-10       1.9         93       secondary 14745.3              2     3          3MW       Milan     Walsh      1
# 4       15       Ollie      Smith  5.1 2013-08-24       1.6        131       secondary 28268.0              2     4          4OS       Ollie     Smith      2
# 5       23      Harlan    O'Brien 15.5 2003-04-12       2.1        233           FALSE  6904.7              1     5          5HO      Harlan   O'Brien      1
# 6       26      Hollis      Byrne 76.0 1942-11-02       2.1        178       secondary 13856.7              2     6          6HB        <NA>      <NA>     NA

##remove unnessesary columns and rename as appropriate
data.merged.1[c("FirstName.y", "Surname.y")] <- NULL
head(data.merged.1)
names(data.merged.1)
##note that we can remove the brackets from height and weight also to avoid issues
names(data.merged.1) <- c(names(data.merged.1)[1],"FirstName","Surname", names(data.merged.1)[4:5], 
                          "Height", "Weight", names(data.merged.1)[8:13])

str(data.merged.1)


sum(is.na(data.merged.1$Health))
#note there are 28 missing values in Health at this stage
# there are 8 values not merged due to missing IDnum in sheet 2.
##############################################################################
# 
# 6. Not every subject has its id number included in Sheet 2. 
# You should attempt to match the remaining subjects using their first and surnames.
# 

data.merged.2 <- data.merged.1 %>% 
  left_join(data.sheet.2, by = c("FirstName", "Surname"))

head(data.merged.2)

#remove extra columns and rename
data.merged.2[c("Health.x", "IDNumber.y")] <- NULL

names(data.merged.2) <- c("IDNumber", names(data.merged.2)[c(-1,-13)],"Health")
head(data.merged.2)

nrow(data.sheet.2)
#we should have 140 health values in total if merged correctly
sum(is.na(data.merged.2$Health))
#note now have 20 missing values from 160 rows => 140 health values as expected

##There are various other methods to join the data sets and clean, this is just an example solution!!

#rename dataframe for convenience
data.merged <- data.merged.2
##############################################################################
#
# 7. Add a column for age range. This should be
#  0-17     1
# 18-35     2
# 35-54     3
# 54-74     4
# 74-       5
#

data.merged <- data.merged %>%
  mutate(AgeRange=cut(Age,
                      breaks = c(-Inf, 17, 35, 54, 74, Inf),
                      labels = c(1, 2, 3, 4, 5)))

names(data.merged)
# [1] "IDNumber"        "FirstName"       "Surname"         "Age"             "DoB"             "Height"         
# [7] "Weight"          "Education Level" "Salary"          "CriminalRecord"  "NewID"           "AdditionalID"   
# [13] "Health"          "AgeRange"   

str(data.merged)
#note agerange is a factor, as it should be in this case!!

#Education, Criminal record & health should also be factor

data.merged[c("Education Level", "CriminalRecord", "Health")] <- 
  lapply(data.merged[c("Education Level", "CriminalRecord", "Health")], factor)

#check
str(data.merged)


##define dataset with just health data avaialble - may be used later
health.data <- na.omit(data.merged)

str(health.data) #140 obs. of  14 variables

##############################################################################
#
# Generate a bar plot using ggplot2 for the criminal record variable.
# Filter the data by each age category. 
#

# Show CriminalRecord and AgeRange data in a table
table(data.merged[c("CriminalRecord", "AgeRange")])
#               AgeRange
# CriminalRecord  1  2  3  4  5
#              1 19 15 15 18 11
#              2 21 15 16 12 18

data.merged[c("CriminalRecord", "AgeRange")] %>%
  group_by(AgeRange, CriminalRecord) %>%
  summarise(Number = n())
# # A tibble: 10 x 3
# # Groups:   AgeRange [?]
#   AgeRange CriminalRecord Number
#   <fct>    <fct>           <int>
# 1 1        1                  19
# 2 1        2                  21
# 3 2        1                  15
# 4 2        2                  15
# 5 3        1                  15
# 6 3        2                  16
# 7 4        1                  18
# 8 4        2                  12
# 9 5        1                  11
#10 5        2                  18



#Display data in bar chart
CriminalRecordAndAge <- data.merged[c("AgeRange", "CriminalRecord")] 
ggplot(CriminalRecordAndAge, aes(x = AgeRange, y = ..count..)) + 
  geom_bar(aes(fill = CriminalRecord), position="dodge")

ggplot(CriminalRecordAndAge, aes(x = AgeRange, y = ..count..)) + 
  geom_bar(aes(fill = CriminalRecord), position="dodge2")


# Adding more information to bar chart
ggplot(CriminalRecordAndAge, aes(x = AgeRange, y = ..count..)) + 
  geom_bar(aes(fill = CriminalRecord), position="dodge") +
  labs(title="Criminal Record by Age Range") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = rel(2.0)),
        legend.position="bottom", 
        legend.direction="horizontal")


# Basic stacked bar chart
ggplot(CriminalRecordAndAge, aes(x = AgeRange, y = ..count..)) + 
  geom_bar(aes(fill = CriminalRecord), position="stack")


# Stacked bar charts with more information
ggplot(CriminalRecordAndAge, aes(x = AgeRange, y = ..count..)) + 
  geom_bar(aes(fill = CriminalRecord), position="stack") +
  labs(title="Criminal Record by Age Range") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = rel(2.0)),
        legend.position="bottom", 
        legend.direction="horizontal")




#######################################################################################
##Below is an example of some plot explorations in no particular order
# you were not expected to go as far as I have here but I've given the code as an example 
# and to give you and idea of how in-depth you can go 

#It was also important to discuss and interpret the plots in the report and relay the 
# finding to the readers

#Make sure that any plots you create are sensible and interpretable.

#You need to use appropriate plots for factors (boxplots, as colours on scatterplots, etc.)!!!

#There were many cases where I saw scatterplots of categorical variables, which just looked like 2
# vertical lines and showed nothing useful.
####################################################################################################
#
##################################################
#
# CriminalRecord plots
#

ggplot(data.merged, aes(x = CriminalRecord, y = ..count..)) + 
  geom_bar()


summary(data.merged$CriminalRecord)
# 1  2 
# 78 82 

##or##
data.merged[c("CriminalRecord")] %>%
  group_by(CriminalRecord) %>%
  summarise(Number = n())
# # A tibble: 2 x 2
# CriminalRecord Number
# <fct>           <int>
# 1 1                  78
# 2 2                  82


Criminaldata <- data.merged["CriminalRecord"] %>% 
  group_by(CriminalRecord) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(proportion = n/sum(n)) %>% 
  arrange(CriminalRecord)

Criminaldata

Criminaldata$label <- scales::percent(Criminaldata$proportion)
Criminaldata

pie <- ggplot(Criminaldata, aes(x="", y=proportion, fill=CriminalRecord)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  geom_text(aes(x=1, y = cumsum(proportion) - proportion/2, label=label)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom", 
        legend.direction="horizontal") +
  labs(title="Criminal Record")

bar <- ggplot(Criminaldata, aes(x="", y=proportion, fill=CriminalRecord)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = label), 
            position = position_dodge(width=0.9), vjust=20.25) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom", 
        legend.direction="horizontal") +
  labs(title="Criminal Record")

# Display bar chart and pie chart side by side
grid.arrange(bar, pie, ncol = 2) 




##############################################################################
#
# 9. Generate an appropriate visualisation examining the relationships 
#    between height, weight, age and criminal records.

str(data.merged)

h1 <- ggplot(data.merged, aes(y = Height)) +
  geom_boxplot() + theme_classic() +
  labs(subtitle="Height distribution", 
       y="Height (m)", 
       title="Height Boxplot", 
       caption = "Source: data.merged")

w1 <- ggplot(data.merged, aes(y = Weight)) +
  geom_boxplot() + theme_classic() +
  labs(subtitle="Weight distribution", 
       y="Weight (kg)", 
       title="Weight Boxplot", 
       caption = "Source: data.merged")

a1 <- ggplot(data.merged, aes(y = Age)) +
  geom_boxplot() + theme_classic() +
  labs(subtitle="Age distribution", 
       y="Age (years)", 
       title="Age Boxplot", 
       caption = "Source: data.merged")

# Arrange the plots in a row
grid.arrange(h1, w1, a1, ncol = 3) 


###############
#
# Split the boxplots by CriminalRecord
#

h2 <- ggplot(data.merged, aes(x = CriminalRecord, 
                               y = Height, 
                               fill = CriminalRecord)) +
  geom_boxplot(alpha = 0.5) + theme_classic() +
  labs(subtitle="Height distribution by CriminalRecord", 
       y="Height (m)", 
       caption = "Source: data.merged") +
  scale_fill_manual(values=c("steelblue", "tomato")) +
  theme(legend.position="bottom")

w2 <- ggplot(data.merged, aes(x = CriminalRecord, 
                               y = Weight,
                               fill = CriminalRecord)) +
  geom_boxplot(alpha = 0.5) + theme_classic() +
  labs(subtitle="Weight distribution by CriminalRecord", 
       y="Weight (kg)", 
       caption = "Source: data.merged") +
  scale_fill_manual(values=c("steelblue", "tomato")) +
  theme(legend.position="bottom")

a2 <- ggplot(data.merged, aes(x = CriminalRecord, 
                               y = Age,
                               fill = CriminalRecord)) +
  geom_boxplot(alpha = 0.5) + theme_classic() +
  labs(subtitle="Age distribution by CriminalRecord", 
       y="Age (years)", 
       caption = "Source: data.merged") +
  scale_fill_manual(values=c("steelblue", "tomato")) +
  theme(legend.position="bottom")

# Arrange the plots in a row
grid.arrange(h2, w2, a2, ncol = 3) 
########################
#nothing significant noted here
#can use statistical tests to analyse differences between groups also
###############
#


###############
#
# Histograms
#

# Histogram of Height
HH1 <- ggplot(data.merged, aes(Height)) +
  geom_histogram(binwidth = 0.1, 
                 color = "darkblue", 
                 fill = "steelblue") + 
  theme_classic() +
  labs(x="Height", 
       title="Height Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Histogram of Height showing CriminalRecord in different colors
HH2 <- ggplot(data.merged, 
              aes(Height, fill = CriminalRecord)) +
  geom_histogram(binwidth = 0.1) + 
  theme_classic() +
  scale_fill_manual(values=c("lightblue", "steelblue")) +
  labs(x="Height", 
       title="Height Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Histogram of Height splitting on CriminalRecord in different charts
HH3 <- ggplot(data.merged, aes(Height, fill = CriminalRecord)) +
  geom_histogram(binwidth = 0.1) + 
  theme_classic() +
  scale_fill_manual(values=c("lightblue", "steelblue")) +
  labs(x="Height", 
       title="Height Distribution split by CriminalRecord", 
       caption = "Source: data.merged") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "") +
  facet_wrap(~CriminalRecord)

grid.arrange(HH1,HH2,HH3, layout_matrix = rbind(c(1,2),c(3,3)))



# Histogram of Age
AH1 <- ggplot(data.merged, aes(Age)) +
  geom_histogram(binwidth = 5, 
                 color = "aquamarine4", 
                 fill = "aquamarine") + 
  theme_classic() +
  labs(x="Age", 
       title="Age Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Histogram of Age showing CriminalRecord in different colors
AH2 <- ggplot(data.merged, 
              aes(Age, fill = CriminalRecord)) +
  geom_histogram(binwidth = 5) + 
  theme_classic() +
  scale_fill_manual(values=c("aquamarine", "aquamarine4")) +
  labs(x="Age", 
       title="Age Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Histogram of Age splitting on CriminalRecord in different charts
AH3 <- ggplot(data.merged, aes(Age, fill = CriminalRecord)) +
  geom_histogram(binwidth = 5) + 
  theme_classic() +
  scale_fill_manual(values=c("aquamarine", "aquamarine4")) +
  labs(x="Age", 
       title="Age Distribution split by CriminalRecord", 
       caption = "Source: data.merged") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "") +
  facet_wrap(~CriminalRecord)

grid.arrange(AH1,AH2,AH3, layout_matrix = rbind(c(1,2),c(3,3)))



# Histogram of Weight
WH1 <- ggplot(data.merged, aes(Weight)) +
  geom_histogram(binwidth = 10, 
                 color = "firebrick4", 
                 fill = "tomato2") + 
  theme_classic() +
  labs(x="Weight", 
       title="Weight Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Histogram of Weight showing CriminalRecord in different colors
WH2 <- ggplot(data.merged, 
              aes(Weight, fill = CriminalRecord)) +
  geom_histogram(binwidth = 10) + 
  theme_classic() +
  scale_fill_manual(values=c("tomato2", "firebrick4")) +
  labs(x="Weight", 
       title="Weight Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Histogram of Weight splitting on CriminalRecord in different charts
WH3 <- ggplot(data.merged, aes(Weight, fill = CriminalRecord)) +
  geom_histogram(binwidth = 10) + 
  theme_classic() +
  scale_fill_manual(values=c("tomato2", "firebrick4")) +
  labs(x="Weight", 
       title="Weight Distribution split by CriminalRecord", 
       caption = "Source: data.merged") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "") +
  facet_wrap(~CriminalRecord)

grid.arrange(WH1,WH2,WH3, layout_matrix = rbind(c(1,2),c(3,3)))



########################################################################
#############################################
#
#review graphs and discuss.....

###########################################################################
#some other examples of showing distributions


ggplot(data.merged, aes(Height, colour = CriminalRecord)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data.merged, aes(Weight, colour = CriminalRecord)) +
  geom_freqpoly(binwidth = 10)

ggplot(data.merged, aes(Age, colour = CriminalRecord)) +
  geom_freqpoly(binwidth = 5)



###########################################################################


ah1 <- ggplot(data.merged, 
       aes(x = Age, y = Height, color = CriminalRecord)) + 
  geom_point() +  theme_classic() +
  labs(x="Age (years)", y = "Height (m)",
       title="Height vs Age showing CriminalRecord") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

ah2 <- ggplot(data.merged, 
       aes(x = Age, y = Height, color = CriminalRecord)) + 
  geom_point() +  theme_classic() +
  labs(x="Age (years)", y = "Height (m)",
       title="Height vs Age split by CriminalRecord") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(~CriminalRecord)


aw1 <- ggplot(data.merged, 
              aes(x = Age, y = Weight, color = CriminalRecord)) + 
  geom_point() + theme_classic() +
  labs(x="Age (years)", y = "Weight (kg)",
       title="Weight vs Age showing CriminalRecord") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

aw2 <- ggplot(data.merged, 
              aes(x = Age, y = Weight, color = CriminalRecord)) + 
  geom_point() + theme_classic() +
  labs(x="Age (years)", y = "Weight (Kg)",
       title="Weight vs Age split by CriminalRecord") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(~CriminalRecord)

grid.arrange(ah1, ah2, aw1, aw2, ncol=2, nrow=2)





data.merged[c("NewID", "Age", "Height", "Weight")] %>% 
  filter(Age < 2) %>%
  arrange(Age)
#   NewID Age Height.m. Weight.kg.
# 1    64 0.7       1.8         82
# 2    36 1.2       1.8         62
# 3    17 1.3       1.5         73
# 4   120 1.7       1.9         72
# 5    23 1.9       1.7         97

#maybe something suspicious with data - many of you noticed this and correctly 
# called it out in the report

###########################################################################
# 
# Split by Age Range

cr1 <- data.merged %>% filter(CriminalRecord == 1)
cr2 <- data.merged %>% filter(CriminalRecord == 2)
cra1 <- ggplot(cr1, 
              aes(x = Weight, y = Height, color = AgeRange)) + 
  geom_point() +  theme_classic() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x="Weight (kg)", y = "Height (m)",
       title="Height vs Weight split by AgeRange") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(~AgeRange)

cra2 <- ggplot(cr2, 
               aes(x = Weight, y = Height, color = AgeRange)) + 
  geom_point() +  theme_classic() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x="Weight (kg)", y = "Height (m)",
       title="Height vs Weight split by AgeRange") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(~AgeRange)

grid.arrange(cra1, cra2, nrow=2)


###########################################################################
# 
# Filter Age less than 17 or 20
#

lessThanTwenty <- data.merged %>% filter(Age < 20)
# teens = data.merged %>% filter(Age > 10, Age < 20)
greaterThanTwenty <- data.merged %>% filter(Age > 20)

summary(lessThanTwenty)
summary(greaterThanTwenty)

ahUnder1 <- ggplot(lessThanTwenty, 
                   aes(x = Age, y = Height, color = CriminalRecord)) + 
  geom_point() +  theme_classic() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x="Age (years)", y = "Height (m)",
       title="Height for Under Twenties") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "") +
  facet_wrap(~CriminalRecord)

ahUnder2 <- ggplot(lessThanTwenty, 
                   aes(x = Age, y = Weight, color = CriminalRecord)) + 
  geom_point() + theme_classic() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x="Age (years)", y = "Weight (Kg)",
       title="Weight for Under Twenties") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "") +
  facet_wrap(~CriminalRecord)

ahOver1 <- ggplot(greaterThanTwenty, 
                aes(x = Age, y = Height, color = CriminalRecord)) + 
  geom_point() +  theme_classic() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x="Age (years)", y = "Height (m)",
       title="Height for Over Twenties") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "") +
  facet_wrap(~CriminalRecord)


ahOver2 <- ggplot(greaterThanTwenty, 
                aes(x = Age, y = Weight, color = CriminalRecord)) + 
  geom_point() + theme_classic() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x="Age (years)", y = "Weight (Kg)",
       title="Weight for Over Twenties") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "") +
  facet_wrap(~CriminalRecord)

grid.arrange(ahUnder1, ahUnder2, ahOver1, ahOver2, ncol=2, nrow=2)



##############################################################################
# 
# 10. Using filters you should analyse 
# if there are any interesting results in the dataset regarding the relationships 
# between height weight and criminal record. Use appropriate visualisations.
# 

# HeightByCriminalRecord <- 
data.merged[c("CriminalRecord", "Height", "Weight", "Age")] %>%
  group_by(CriminalRecord) %>%
  summarise(N = n(),
            AverageHeight = mean(Height),
            MedianHeight=median(Height),
            SdHeight = sd(Height),
            AverageWeight = mean(Weight),
            MedianWeight=median(Weight),
            SdWeight = sd(Weight),
            AverageAge = mean(Age),
            MedianAge = median(Age),
            SdAge = sd(Age))
# 

# # A tibble: 2 x 11
# CriminalRecord       N AverageHeight MedianHeight SdHeight AverageWeight MedianWeight SdWeight AverageAge MedianAge SdAge
# <fct>            <int>         <dbl>        <dbl>    <dbl>         <dbl>        <dbl>    <dbl>      <dbl>     <dbl> <dbl>
# 1 1                 78          1.85          1.9    0.222          145.          144     64.4       42.2      44.4  28.2
# 2 2                 82          1.87          1.9    0.236          144.          133     52.4       43.3      43.3  28.8

# no obvious difference in Criminal Record, perhaps a slight difference in Weight??


hw1 <- ggplot(data.merged, 
              aes(x = Height, y = Weight, 
                  color = CriminalRecord)) + 
  geom_point() +  theme_classic() +
  labs(x="Height (m)", y = "Weight (Kg)",
       title="Weight vs Height showing CriminalRecord") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

hw2 <- ggplot(data.merged, 
              aes(x = Height, y = Weight, 
                  color = CriminalRecord)) + 
  geom_point() +  theme_classic() +
  labs(x="Height (m)", y = "Weight (Kg)",
       title="Weight vs Height split by CriminalRecord") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(~CriminalRecord)

grid.arrange(hw1, hw2, ncol = 2)

#doesn't look there is any realtionship between height and weight, 
# even though one might be expected!

#Check this via a model:
# Examine linear regression between Height and Weight
m1 <- lm(data.merged$Weight ~ data.merged$Height )

summary(m1)
# Call:
#   lm(formula = data.merged$Weight ~ data.merged$Height)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -97.078 -47.381  -2.362  45.424 122.069 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)           63.57      37.47   1.697   0.0917 .
# data.merged$Height    43.58      20.01   2.177   0.0309 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 57.69 on 158 degrees of freedom
# Multiple R-squared:  0.02913,	Adjusted R-squared:  0.02299 
# F-statistic: 4.741 on 1 and 158 DF,  p-value: 0.03094

#model is significant, but very small relationship between varaibles

confint(m1)
# > confint(m1)
#                             2.5 %    97.5 %
# (Intercept)            -10.433937 137.56551
# data.merged$Height.m.   4.048347  83.10587

plot(data.merged$Height, 
     data.merged$Weight, 
     pch = 16, 
     cex = 1.2, 
     col = "blue", 
     main = "Weight vs Height", 
     xlab = "Height (m)", 
     ylab = "Weight (Kg)")
abline(m1)


###Investigate other relationships also

##############################################################################
# 
# Add lines to the correlation graphs
hw3 <- hw1 + geom_smooth(method = "lm", se = FALSE)
hw4 <- hw2 + geom_smooth(method = "lm", se = FALSE)

grid.arrange(hw3, hw4, nrow = 2)


##############################################################################
# 
# 11. Generate a smaller data frame for the subjects where health related data is available. 
#     Examine if there is a relationship between the different states of health and height, 
#     weight or age. Use appropriate visualisations.
# 

str(health.data)
# 'data.frame':	140 obs. of  14 variables:
# $ IDNumber       : num  12 15 23 26 32 36 41 48 53 55 ...
# $ FirstName      : chr  "Milan" "Ollie" "Harlan" "Hollis" ...
# $ Surname        : chr  "Walsh" "Smith" "O'Brien" "Byrne" ...
# $ Age            : num  11.3 5.1 15.5 76 9.1 75.5 53.4 50.4 57.9 29.7 ...
# $ DoB            : Date, format: "2007-07-10" "2013-08-24" ...
# $ Height         : num  1.9 1.6 2.1 2.1 1.9 1.9 1.6 1.5 2.1 1.9 ...
# $ Weight         : num  93 131 233 178 67 96 68 131 257 226 ...
# $ Education Level: Factor w/ 6 levels "doctorate","FALSE",..: 6 6 2 6 5 1 3 1 5 5 ...
# $ Salary         : num  14745 28268 6905 13857 9982 ...
# $ CriminalRecord : Factor w/ 2 levels "1","2": 2 2 1 2 1 2 2 2 1 1 ...
# $ NewID          : int  3 4 5 6 7 8 9 10 11 12 ...
# $ AdditionalID   : chr  "3MW" "4OS" "5HO" "6HB" ...
# $ Health         : Factor w/ 2 levels "1","2": 1 2 1 2 2 2 2 1 1 1 ...
# $ AgeRange       : Factor w/ 5 levels "1","2","3","4",..: 1 1 1 5 1 5 3 3 4 2 ...
# - attr(*, "na.action")= 'omit' Named int [1:20] 1 2 143 144 145 146 147 148 149 150 ...
# ..- attr(*, "names")= chr [1:20] "1" "2" "143" "144" ...


###############################################################
#
# Barchart and piechart of Health data
#
HealthData <-  health.data["Health"] %>% 
  group_by(Health) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(proportion = n/sum(n)) %>% 
  arrange(Health)

HealthData$label <- scales::percent(HealthData$proportion)
HealthData
# # A tibble: 2 x 4
# Health     n proportion label 
#   <fct>  <int>      <dbl> <chr> 
# 1 1         66      0.471 47.14%
# 2 2         74      0.529 52.86%

hpie <- ggplot(HealthData, aes(x="", y=proportion, fill=Health)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  geom_text(aes(x=1, y = cumsum(proportion) - proportion/2, label=label)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom", 
        legend.direction="horizontal") +
  scale_fill_manual(values=c("purple4", "thistle")) +
  labs(title="Health")

hbar <- ggplot(HealthData, aes(x="", y=proportion, fill=Health)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = label), 
            position = position_dodge(width=0.9), vjust=20.25) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom", 
        legend.direction="horizontal") +
  scale_fill_manual(values=c("purple4", "thistle")) +
  labs(title="Health")

# Display bar chart and pie chart side by side
grid.arrange(hbar, hpie, ncol = 2) 



###############################################################
#
# Boxplots of Height, Weight and Age using colour for Health data
#

hh2 <- ggplot(health.data, aes(x = Health, 
                               y = Height, 
                               fill = Health)) +
  geom_boxplot(alpha = 0.8) + theme_classic() +
  labs(subtitle="Height distribution by Health", 
       y="Height (m)") +
  scale_fill_manual(values=c("purple4", "thistle")) +
  theme(legend.position="bottom")

hw2 <- ggplot(health.data, aes(x = Health, 
                               y = Weight,
                               fill = Health)) +
  geom_boxplot(alpha = 0.8) + theme_classic() +
  labs(subtitle="Weight distribution by Health", 
       y="Weight (kg)") +
  scale_fill_manual(values=c("purple4", "thistle")) +
  theme(legend.position="bottom")

ha2 <- ggplot(health.data, aes(x = Health, 
                               y = Age,
                               fill = Health)) +
  geom_boxplot(alpha = 0.8) + theme_classic() +
  labs(subtitle="Age distribution by Health", 
       y="Age (years)") +
  scale_fill_manual(values=c("purple4", "thistle")) +
  theme(legend.position="bottom")

# Arrange the plots in a row
grid.arrange(hh2, hw2, ha2, ncol = 3) 

#possible difference in age seen

###############################################################
#
# Histograms of Height, Weight and Age using colour for Health data
#

# Histogram of Weight splitting on Health in different charts
hhh <- ggplot(health.data, aes(Height, fill = Health)) +
  geom_histogram(binwidth = .1) + 
  theme_classic() + 
  scale_fill_manual(values=c("purple4", "thistle")) +
  labs(x="Height", title="Height Distribution split by Health") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "") +
  facet_wrap(~Health)

# Histogram of Weight splitting on Health in different charts
hwh <- ggplot(health.data, aes(Weight, fill = Health)) +
  geom_histogram(binwidth = 15) + 
  theme_classic() + 
  scale_fill_manual(values=c("purple4", "thistle")) +
  labs(x="Weight", title="Weight Distribution split by Health") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "") +
  facet_wrap(~Health)

# Histogram of Age splitting on Health in different charts
hah <- ggplot(health.data, aes(Age, fill = Health)) +
  geom_histogram(binwidth = 10) + 
  theme_classic() + 
  scale_fill_manual(values=c("purple4", "thistle")) +
  labs(x="Age", title="Age Distribution split by Health") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "") +
  facet_wrap(~Health)

grid.arrange(hhh, hwh, hah, ncol = 3) 


###############################################################
#
# Histograms of Height, Weight and Age using colour for Health data
#

dhh <- ggplot(health.data, aes(x = Height, fill = Health)) +
  geom_density(alpha = 0.7) + theme_bw() +
  scale_fill_manual(values=c("purple4", "thistle")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.15, 0.85),
        legend.direction = "horizontal") +
  labs(title="Health Density Estimates for Height") +
  guides(fill=guide_legend(title=""))

dwh <- ggplot(health.data, aes(x = Weight, fill = Health)) +
  geom_density(alpha = 0.7) + theme_bw() +
  scale_fill_manual(values=c("purple4", "thistle")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8, 0.9),
        legend.direction = "horizontal") +
  labs(title="Health Density Estimates for Weight") +
  guides(fill=guide_legend(title=""))

dah <- ggplot(health.data, aes(x = Age, fill = Health)) +
  geom_density(alpha = 0.7) + theme_bw() +
  scale_fill_manual(values=c("purple4", "thistle")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.5, 0.95),
        legend.direction = "horizontal") +
  labs(title="Health Density Estimates for Age") +
  guides(fill=guide_legend(title=""))

grid.arrange(dhh, dwh, dah, layout_matrix = rbind(c(1,2),c(3,3)))



###############################################################
#
# Scatterplots of Height, Weight and Age using colour for Health data
#

# Height vs Age
hah1 <- ggplot(health.data, aes(x = Age, y = Height, color = Health)) + 
  geom_point() +  theme_classic() +
  labs(x="Age (years)", y = "Height (m)",
       title="Height vs Age showing Health") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") + 
  scale_colour_manual(values=c("purple4", "thistle")) +
  geom_smooth(method = "lm", se = FALSE)

hah2 <- ggplot(health.data, aes(x = Age, y = Height, color = Health)) + 
  geom_point() +  theme_classic() +
  labs(x="Age (years)", y = "Height (m)",
       title="Height vs Age split by Health") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_colour_manual(values=c("purple4", "thistle")) +
  facet_wrap(~Health) +
  geom_smooth(method = "lm", se = FALSE)

# Weight vs Age
haw1 <- ggplot(health.data, aes(x = Age, y = Weight, color = Health)) + 
  geom_point() + theme_classic() +
  labs(x="Age (years)", y = "Weight (kg)",
       title="Weight vs Age showing Health") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_colour_manual(values=c("purple4", "thistle")) +
  geom_smooth(method = "lm", se = FALSE)

haw2 <- ggplot(health.data, aes(x = Age, y = Weight, color = Health)) + 
  geom_point() + theme_classic() +
  labs(x="Age (years)", y = "Weight (Kg)",
       title="Weight vs Age split by Health") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_colour_manual(values=c("purple4", "thistle")) +
  facet_wrap(~Health) +
  geom_smooth(method = "lm", se = FALSE)

# Weight vs Height
hhw1 <- ggplot(health.data, aes(x = Height, y = Weight, color = Health)) + 
  geom_point() +  theme_classic() +
  labs(x="Height (m)", y = "Weight (Kg)",
       title="Weight vs Height showing Health") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_colour_manual(values=c("purple4", "thistle")) +
  geom_smooth(method = "lm", se = FALSE)

hhw2 <- ggplot(health.data, aes(x = Height, y = Weight, color = Health)) + 
  geom_point() +  theme_classic() +
  labs(x="Height (m)", y = "Weight (Kg)",
       title="Weight vs Height split by Health") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_colour_manual(values=c("purple4", "thistle")) +
  facet_wrap(~Health) +
  geom_smooth(method = "lm", se = FALSE)

grid.arrange(hah1, hah2, haw1, haw2, hhw1, hhw2, ncol=2, nrow=3)


###############################################################
#
# Barchart of Age range showing Health data
#

# Create barchart of the subjects showing Health categories in different colours
ggplot(health.data, aes(x = AgeRange, y = ..count..)) + 
  geom_bar(aes(fill = Health), position="stack") +
  labs(title="Health by Age Range") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom", 
        legend.direction="horizontal") +
  scale_fill_manual(values=c("purple4", "thistle"))


###############################################################
#
# Scatter plot of Height vs Weight using Age as the size and Health for colour
#


summary(health.data[c("Age","Height","Weight","CriminalRecord", "Health", "AgeRange")])
# Age            Height          Weight       CriminalRecord Health AgeRange
# Min.   : 0.70   Min.   :1.500   Min.   : 51.00   1:68           1:66   1:38    
# 1st Qu.:15.28   1st Qu.:1.700   1st Qu.: 96.75   2:72           2:74   2:22    
# Median :43.75   Median :1.900   Median :140.50                         3:28    
# Mean   :42.58   Mean   :1.866   Mean   :144.51                         4:25    
# 3rd Qu.:69.22   3rd Qu.:2.100   3rd Qu.:191.00                         5:27    
# Max.   :91.30   Max.   :2.200   Max.   :260.00    


ggplot(health.data, aes(x=Height, y=Weight)) + 
  geom_point(aes(col=Health, size=Age)) + 
  xlim(c(1.4, 2.3)) + 
  ylim(c(50, 270)) + 
  labs(y="Weight (Kg)", 
       x="Height (m)", 
       title="Weight vs Height by Health category") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("purple4", "thistle"))

ggplot(health.data, aes(x=Height, y=Weight)) + 
  geom_point(aes(col=Health, size=Age, shape = CriminalRecord)) + 
  xlim(c(1.4, 2.4)) + 
  ylim(c(40, 280)) + 
  labs(y="Weight (Kg)", 
       x="Height (m)", 
       title="Weight vs Height by Health category") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("red", "green"))
#could also do faced versions of these plots or grouped datasets


####Could try to incorporate Health into modeling, but as it's a factor, can only have it as an 
# x variable in linear regression (cannot be a y variable unless using logistic regression - glm())


##I left the modelling part relativly free and up to you, as long as you had some justification and
# understanding of the model(s) you choose.  

#For example, lets try predict salary from all other reasonable variables:
str(data.merged)
sal.model <- lm(Salary~Age+`Education Level`+CriminalRecord+Weight+Height+Health+AgeRange, data.merged)
summary(sal.model)

#note numerous variables are insignificant - should try refine this by removing insignificant 
# variables or only including variables with reasonable correlations.

#check numeric varaibles
cor(data.merged[c(4,6,7,9)])
#All seem to have a small effect on salary

##lets try a simpler model wiht just numeric variables (no factor variables)
sal.model2 <- lm(Salary~Age+Weight+Height, data.merged)
summary(sal.model2)#this is not significant

ggplot(data.merged, aes(`Education Level`, Salary))+
  geom_boxplot()
#some slight differences but all boxes overlap, can't prove any obvious difference
#larger spread for masters

ggplot(data.merged, aes(Health, Salary))+
  geom_boxplot()
#no obvious difference

ggplot(data.merged, aes(CriminalRecord, Salary))+
  geom_boxplot()
#seems to be an obvious difference here

ggplot(data.merged, aes(AgeRange, Salary))+
  geom_boxplot()
#no obvious difference

#can confirm with statistical tests if desired
#eg
t.test(data.merged$Salary~data.merged$Health)
#not significant => no difference

t.test(data.merged$Salary~data.merged$CriminalRecord)
#significant differnce

