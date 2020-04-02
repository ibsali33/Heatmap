#The objective for this code is to plot the number of cases of COVID19 recorded per million people. This will help visualize rates of infection in different states. First we load the packages required for the project.

library(dplyr)
library(ggplot2)
library(gplots)
library(tidyr)
library(tibble)

#COVID19 data is a bit messy so we are going to need to do some data manipulation. Load two csv files the first containing the COVID19 data downloaded from "kaggle.com/sudalairajkumar/covid19-in-usa"

alldata <- read.csv("us_states_covid19_daily.csv", sep = ",")
statepop <- read.csv("States.csv", sep = ",")

#We are interested in the number of positive cases organized by state over time and are interested in state population (including DC and Puerto Rico). We need to reorganize the data in a way that specificially selects for the states/regions we are interested in (indicated by the States.csv file) and organizes the positive cases by date. 

allpositivecases <- data.frame(alldata$date, alldata$state, alldata$positive)
selectedpops <- data.frame(statepop$StateAbbreviation, statepop$Pop.millions)

#Now we can merge the two tables based on their state abbrevations.

tablemerge <- full_join(allpositivecases, selectedpops, by = c("alldata.state" = "statepop.StateAbbreviation"))

#Now that the tables are merged lets divide the number of cases by the population to get the number of cases per million (CPM) individuals, in the same step, lets remove all states that are not on our selected populations list. 

casespermillion <- tablemerge %>%
  mutate(CPM=alldata.positive/statepop.Pop.millions) %>%
  filter(statepop.Pop.millions != "NA")

#Subset the full table into two data frames, one containing the cases per million values, and two contianing the raw positive numbers.

forheatmap1 <- casespermillion %>%
  select(alldata.date, alldata.state, CPM)

#Note that the number of observations reduces here, this shows that the removal of NAs worked.

#Now its time to build our heatmap for cases per million residents. In order to do that we have to spread out the values such that state names are the rows and dates are the columns. So we group by state and date and spread the CPM value along the date variable.

forheatmapcpm <- forheatmap1 %>%
  group_by(alldata.state, alldata.date) %>%
  spread(key = alldata.date, value = CPM)

#Since some states do not have case number date for certain dates - here we convert all NA values into 0

forheatmapcpm[is.na(forheatmapcpm)] <- 0

#The heatmap function only accepts numeric matricies. Currently we have a data frame with a column for state name and a series of columns for dates. First we force the State name column into the rownames category.

forheatmapcpm <- column_to_rownames(forheatmapcpm, var = "alldata.state")

#Then we convert the data frame into a numeric matrix.

cpmmatrix <- as.matrix(forheatmapcpm)

#Finally we write the code for the heatmap. We use the heatmap.2 function because it gives us some more customizability relative to the base R heatmap function. We set the scale to "none" because we want the graph to scale the plot based on all the values (not the columns or rows). The heatmap function automatically creates dendograms for the x and the y axis which reorganizes the data. We want to organize the data based on the states with the highest to lowest CPM, but we don't want to reorganize the columns because we want to look at the data in chronological order. Colv or Rowv are the designations that determine whether or not the rows are reorganized. Finally, I want to set the color key to a log2 scale so I set the color breaks based on caseload doubling time with a threshold of .5 cases per million to the max caseload (see NY). Heatmap2 does density tracing (I have no idea what that means, but it looks bad on the graph, so I remove the trace and the density.info)

cpmplot <- heatmap.2(cpmmatrix, scale="none", Colv = NA, breaks = c(0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192), trace = "none", density.info = "none")

