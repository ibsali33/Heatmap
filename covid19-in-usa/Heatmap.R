#The objective for this code is to plot the number of cases of COVID19 recorded per million people. This will help visualize rates of infection in different states.

library(dplyr)
library(ggplot2)
library(tidyr)

#COVID19 data is a bit messy so we are going to need to do some data manipulation. Load two csv files the first containing the COVID19 data downloaded from "kaggle.com/sudalairajkumar/covid19-in-usa"

alldata <- read.csv("us_states_covid19_daily.csv", sep = ",")
statepop <- read.csv("States.csv", sep = ",")

#We are interested in the number of positive cases organized by state over time and are interested in state population (including DC and Puerto Rico). We need to reorganize the data in a way that specificially selects for the states/regions we are interested in (indicated by the States.csv file) and organizes the positive cases by date. 

allpositivecases <- data.frame(alldata$date, alldata$state, alldata$positive)
selectedpops <- data.frame(statepop$StateAbbreviation, statepop$Pop.millions)

#Now we can merge the two tables based on their state abbrevations.

tablemerge <- full_join(allpositivecases, selectedpops, by = c("alldata.state" = "statepop.StateAbbreviation"))

#Now that the tables are merged lets divide the number of cases by the population to get the number of cases/million individuals, in the same step, lets remove all states that are not on our selected populations list. 

casespermillion <- tablemerge %>%
  mutate(CPM=alldata.positive/statepop.Pop.millions) %>%
  filter(statepop.Pop.millions != "NA")

#Subset the full table into two data frames, one containing the cases per million values, and two contianing the raw positive numbers.

forheatmap1 <- casespermillion %>%
  select(alldata.date, alldata.state, CPM)

forheatmap3 <- forheatmap1 %>%
  filter(alldata.state != "NY", alldata.state != "NJ")

forheatmap2 <- casespermillion %>%
  select(alldata.date, alldata.state, alldata.positive)

#Note that the number of observations reduces here, this shows that the removal of NAs worked.

#Now its time to build our heatmap for cases per million residents

forheatmapcpm <- forheatmap1 %>%
  group_by(alldata.state, alldata.date) %>%
  spread(key = alldata.date, value = CPM)

forheatmapcpm[is.na(forheatmapcpm)] <- 0
forheatmapcpm <- column_to_rownames(forheatmapcpm, var = "alldata.state")
cpmmatrix <- as.matrix(forheatmapcpm)
plot <- heatmap(cpmmatrix, scale="none", Colv = NA)



forheatmapcpmnony <- forheatmap3 %>%
  group_by(alldata.state, alldata.date) %>%
  spread(key = alldata.date, value = CPM)

forheatmapcpmnony[is.na(forheatmapcpmnony)] <- 0
forheatmapcpmnony <- column_to_rownames(forheatmapcpmnony, var = "alldata.state")
cpmnonymatrix <- as.matrix(forheatmapcpmnony)
plot <- heatmap(cpmnonymatrix, scale="none", Colv = NA)

