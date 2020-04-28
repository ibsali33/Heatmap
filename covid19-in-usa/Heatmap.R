#The objective for this lesson is to use some data manipulation, cleaning and visualization tools in order to plot the number of cases of COVID19 recorded per million people for every US state and major territories. First we must load the packages required to run the data analysis here. 

#Note: if the library(packagename) command does not work, try install.packages('packagename') first and then rerun library(packagename)

library(dplyr) #data manipulation and wrangling library
library(gplots) #data plottling library
library(tidyr) #data manipulation and wrangling library
library(tibble) #table manipulation library

#Within ths Rproj directory we will find two tables - one containing publicly available COVID data downloaded from "kaggle.com/sudalairajkumar/covid19-in-usa" the second is a .csv file which I created contianing population information for the 50 states plus DC and Puerto Rico.

#I use the read.csv command to take the csv files and create data frames with the information.

alldata <- read.csv("us_states_covid19_daily.csv", sep = ",")
statepop <- read.csv("States.csv", sep = ",")

#PC users might have issues with the statepop name. Check to see what the column name is. If it has any strange symbol run: names(statepop)[1]<-"State"

#The first thing we need to consider is: What exactly is the data that we need from these tables in order to create a table of cases/million (CPM) data organized by state and date? The COIVD19 data is in a large table containing a lot of information we do not need for this analysis. 

#For this analysis we are only interested in looking at the following items from the full COVID dataset (1) the number of positive cases, (2) the state information and (3) the date the data was collected. We also need (4) information about in state population (including DC and Puerto Rico). 

#With the next set of code we will use the data.frame command in order to create a new data frame with the selected columns from the data we have loaded. Note the syntax here.

allpositivecases <- data.frame(alldata$date, alldata$state, alldata$positive)
selectedpops <- data.frame(statepop$StateAbbreviation, statepop$Pop.millions)

#We need to reorganize the data in a way that aligns the state population information with the number of positive cases. In order for R to merge tables - they need to have some level of overlapping information so that it knows how to join the tables. In this case the alldata.state column in allpositivecases has information that is identical to the statepop.StateAbbreviation information. We can use the dplyr join() command in order to bring the tables together and use the "by =" fucntion to tell it what columns should be treated as equivalent.

tablemerge <- full_join(allpositivecases, selectedpops, by = c("alldata.state" = "statepop.StateAbbreviation"))

#Now that the tables are merged lets divide the number of cases by the population to get the number of cases per million (CPM) individuals, in the same step, lets remove all states that are not on our selected populations list. We can do these two steps at once by using the dplyr "pipe" %>%. A pipe allows you to run a designated set of data through multiple commands at once.  

casespermillion <- tablemerge %>%
  mutate(CPM=alldata.positive/statepop.Pop.millions) %>% #calculates CPM
  filter(statepop.Pop.millions != "NA") #removes NA values - ie:states that have no info

#Note that the number of observations reduces here, this shows that the removal of NAs worked.

#For our heatmap we will need a numeric matrix that only has information about state, date and CPM. We can use dplyr to select the columns we want to use to create our data matrix.

forheatmap1 <- casespermillion %>%
  select(alldata.date, alldata.state, CPM)

#This data is still in the form of data frame, which does not work for the heatmap(), heatmap.2() or pheatmap() commands. These commands require a numeric matrix, so we need to reformat the data into this form. 

#In order to do that we have to spread out the values such that state names are the rows and dates are the columns. So we use the group_by() command on state and date information, and use the spread() function to spread the CPM value along the key alldata.date variable.

forheatmapcpm <- forheatmap1 %>%
  group_by(alldata.state, alldata.date) %>%
  spread(key = alldata.date, value = CPM)

#Notice how this reshapes the data. This is an essential first step to create the numeric matrix. Since some states do not have any recorded information on positive cases on certain dates - here we convert all NA values into 0. 

forheatmapcpm[is.na(forheatmapcpm)] <- 0

#Now we have a data frame called forheatmapcpm with a column for state name and a series of columns for dates. The next step is that we want to force the State name column into the rownames category. This can be done with column_to_rownames() command in the tibble() library.

forheatmapcpm <- column_to_rownames(forheatmapcpm, var = "alldata.state")

#We also happen to know that exporting massive heatmaps can take a lot of ram and time and can easily overload a machine, so we limit the data that we export to the most recent 30 days of data.

forheatmap30 <- forheatmapcpm[(ncol(forheatmapcpm)-29):(ncol(forheatmapcpm))]

#Now that all non-numeric characters have been removed from the data frame we can convert the data frame into a numeric matrix using the as.matrix() command.

cpmmatrix <- as.matrix(forheatmap30)

#Finally we write the code for the heatmap. Here we use the heatmap.2 function because it gives us some more customizability relative to the base R heatmap function. 

#We set the scale to "none" because we want the graph to scale the plot based on all the values (not the columns or rows). 
#The heatmap function automatically creates dendograms for the x and the y axis which reorganizes the data. We want to organize the data based on the states with the highest to lowest CPM, but we don't want to reorganize the columns because we want to look at the data in chronological order. Colv or Rowv are the designations that determine whether or not the rows are reorganized. 
#Finally, I want to set the color key to a log2 scale so I set the color breaks based on caseload doubling time with a lower threshold of .5 cases per million to the max caseload (see NY). 
#Heatmap2 does density tracing (I have no idea what that means, but it looks bad on the graph, so I remove the trace and the density.info as well)

cpmplot <- heatmap.2(cpmmatrix, scale="none", Colv = NA, breaks = c(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16314), trace = "none", density.info = "none")
