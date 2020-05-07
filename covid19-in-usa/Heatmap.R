#The objective for this lesson is to use some data manipulation, cleaning and visualization tools in order to plot the number of cases of COVID19 recorded per million people for every US state and major territories. First we must load the packages required to run the data analysis here. 

#Note: if the library(packagename) command does not work, try install.packages('packagename') first and then rerun library(packagename)

library(dplyr) #data manipulation and wrangling library
library(pheatmap) #heatmap plottling library
library(RColorBrewer) #color palette library
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

casespermillion <- tablemerge %>%
  mutate(Log2_CPM=log2(alldata.positive/statepop.Pop.millions)) %>% #calculates CPM
  filter(statepop.Pop.millions != "NA") %>%
  filter(Log2_CPM != -Inf)

forheatmapcpm <- casespermillion %>%
  select(alldata.date, alldata.state, Log2_CPM)

#This data is still in the form of data frame, which does not work for the heatmap(), heatmap.2() or pheatmap() commands. These commands require a numeric matrix, so we need to reformat the data into this form. 

#In order to do that we have to spread out the values such that state names are the rows and dates are the columns. So we use the group_by() command on state and date information, and use the spread() function to spread the CPM value along the key alldata.date variable.

forheatmapcpm <- forheatmap1 %>%
  group_by(alldata.state, alldata.date) %>%
  spread(key = alldata.date, value = CPM) 

#note that the spread() function is depreciated/retired. This means that there are no longer updates to the particular package. For an open source software like R why is it a problem to use a retired function since it still works right now...? Within the help area look up to see what function serves to replace it. What do they recommend? Why is it better to use the replacement function?

#Notice how this reshapes the data. This is an essential first step to create the numeric matrix. Since some states do not have any recorded information on positive cases on certain dates - here we convert all NA values into 0. 

forheatmapcpm[is.na(forheatmapcpm)] <- min(forheatmapcpm, na.rm=TRUE)

#Now we have a data frame called forheatmapcpm with a column for state name and a series of columns for dates. The next step is that we want to force the State name column into the rownames category. This can be done with column_to_rownames() command in the tibble() library.

forheatmapcpm <- column_to_rownames(forheatmapcpm, var = "alldata.state")

#We also happen to know that only a few states were reporting data before March 1, but other states have data from as early as january, so we trim the data table to March 1, 2020.

forheatmaptrim <- forheatmapcpm[as.integer(which(colnames(forheatmapcpm)=="20200301")):(ncol(forheatmapcpm))]


#Now that all non-numeric characters have been removed from the data frame we can convert the data frame into a numeric matrix using the as.matrix() command.

cpmmatrix <- as.matrix(forheatmaptrim)

#Finally we write the code for the heatmap. Here we use the pheatmap function because it gives us some more customizability relative to the base R, and gplots heatmap functions. 
#We set the scale to "none" because we want the graph to scale the plot based on all the values (otherwise, it will normalize the data based on column or row values in isolation). 

#The pheatmap function automatically creates dendograms for the x and the y axis which reorganizes the data. We want to organize the data based on the states with the ~highest to lowest CPM, but we don't want to reorganize the columns because we want to look at the data in chronological order. culster_cols or cluster_rows are the designations that determine whether or not the rows are reorganized. 

#Next we set some parameters for colors. In order to visualize doubling time, we log2 transformed the data. In order to see doubling time clearly on the map - it would be best to set one shade as a whole-number integer bin in log2 space. The 'breaks =' parameter allows us to designate the range bins to create from the heatmap, so we use the as.integer() function to caculate the min and max vaules for the matrix of data. Below we set the range of values as a list called cpmbreaks.

cpmbreaks <- as.integer(min(forheatmapcpm-1)):as.integer(max(forheatmapcpm))

#Finally we compose the code for the heatmap setting each parameter on a separate line to encourage readability. For color, we loaded the RColorBrewer package at the beginning of the document. Using the colorRampPalette function we set a color "Ramp", or gradient, using the full range (9) of the "Blues" brewer paletee designated one color for the number of values in cpmbreaks we calculated above. We remove the border_color for ease of reading the heatmap.

cpmplot <- pheatmap(
  cpmmatrix, 
  scale="none", 
  cluster_cols = FALSE, 
  breaks = cpmbreaks, 
  color = colorRampPalette(brewer.pal(9, "Blues"))(length(cpmbreaks)),
  border_color = NA
)
