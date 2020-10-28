# The objective for this lesson is to use some data manipulation, cleaning and visualization 
# tools in order to plot the number of cases of COVID19 recorded per million people for every 
# US state and major territories. First we must load the packages required to run the data analysis here. 

# Note: if the library(packagename) command does not work, try install.packages('packagename') first and 
# then rerun library(packagename)

library(dplyr) #data manipulation and wrangling library
library(pheatmap) #heatmap plottling library
library(RColorBrewer) #color palette library
library(tidyr) #data manipulation and wrangling library
library(tibble) #table manipulation library

# Within ths Rproj directory we will find two tables - one containing publicly available 
# COVID data downloaded from "kaggle.com/sudalairajkumar/covid19-in-usa" the second is 
# a .csv file which I created contianing population information for the 50 states plus DC 
# and Puerto Rico.

# We can use the read.csv command to take the csv files and create data frames with the information.

alldata <- read.csv("us_states_covid19_daily.csv", sep = ",")
statepop <- read.csv("States.csv", sep = ",")

# PC users might have issues with the statepop name. Check to see what the column name is. 
# If it has any strange symbol run: names(statepop)[1]<-"State"

# The first thing we need to consider is: What exactly is the data that we need from 
# these tables in order to create a table of cases/million (CPM) data organized by state 
# and date? The COIVD19 data is in a large table containing a lot of information we do not 
# need for this analysis. 

# For this analysis we are only interested in looking at the following items from the full 
# COVID dataset (1) the number of positive cases, (2) the state information and (3) the date 
# the data was collected. We also need (4) information about in state population (including 
# DC and Puerto Rico). 

# With the next set of code we will use the data.frame command in order to create a new data 
# frame with the selected columns from the data we have loaded. Note the syntax here.

allpositivecases <- data.frame(alldata$date, alldata$state, alldata$positive)
selectedpops <- data.frame(statepop$StateAbbreviation, statepop$Pop.millions)

# We need to reorganize the data in a way that aligns the state population 
# information with the number of positive cases. In order for R to merge tables
# they need to have some level of overlapping information so that it knows how to 
# join the tables. In this case the alldata.state column in allpositivecases has 
# information that is identical to the statepop.StateAbbreviation information. 
# We can use the dplyr join() command in order to bring the tables together and 
# use the "by =" fucntion to tell it what columns should be treated as equivalent.

tablemerge <- full_join(allpositivecases, selectedpops, by = c("alldata.state" = "statepop.StateAbbreviation"))

# Now that the tables are merged lets calculate the number of cases per million 
# (CPM) individuals normalized in a log2 scale to better visualize doubling time. 
# In the same step, lets remove all the data that is not properly calculated or is 
# results in an NA value. We can group these steps all together these two steps at 
# once by using the dplyr "pipe" %>%. A pipe allows you to run a designated set of
# data through multiple commands at once.  

casespermillion <- tablemerge %>%
  mutate(Log2_CPM=log2(alldata.positive/statepop.Pop.millions)) %>% #calculates CPM
  filter(statepop.Pop.millions != "NA") %>%
  filter(Log2_CPM != -Inf)

# Note that the number of observations reduces here, this shows that the removal of NAs,
# and removal of Log2(0) errors, worked. What would happen if we don't filter the data?
# How would that influence the later steps?

# For our heatmap we will need a numeric matrix that contains only CPM numbers, but keeps 
# some information about state and date (so we can interpret the CPM information). 
# We can use dplyr to select the columns we want to use to create our data matrix.

forheatmapcpm <- casespermillion %>%
  select(alldata.date, alldata.state, Log2_CPM)

# This data is still in the form of data frame, which does not work for any of the plotting
# commands that are useful for creating heatmaps. These commands include: heatmap(), 
# heatmap.2() or pheatmap() commands. These commands require a numeric matrix, so we 
# need to reformat the data into the numeric matrix class. Use class() to check the
# class of your data.

# In order to create the numeric matrix that we have to spread out the values so that state 
# names are the rows and dates are the columns. So we use the group_by() command on state 
# and date information, and use the spread() function to spread the CPM value along the key 
# alldata.date variable.

forheatmapcpm <- forheatmapcpm %>%
  group_by(alldata.state, alldata.date) %>%
  spread(key = alldata.date, value = Log2_CPM) 

# Check the shape of your data to see if we have a grid where data is the column name and state
# is in the row. Some people might receive an error that the spread() command is deprecated
# that means that it is no longer maintained or updated. This might be a problem if a new 
# version of R is no longer compatible with the spread() function. For now it should still
# work.

# Once we have the numbers spread out, we can then push the first column of information into rownames.
# This is important because in a numeric matrix you can only have numbers. So text can be moved to 
# rownames to retain that information, why only keeping the numbers in your matrix.

forheatmapcpm <- column_to_rownames(forheatmapcpm, var = "alldata.state")

# Notice how this reshapes the data. This is an essential first step to create the numeric matrix. 
# Since some states do not have any recorded information on positive cases on certain dates it 
# reintroduces a bunch of NAs - we can convert all NA values into the lowest value on our table. 
# The is.na() command returns a TRUE/FALSE statement of whether or not a value is NA. So when we
# include our table within that command is.na(forheatmapcpm) we are identifying all values that
# are NA. forheatmapcpm[is.na(forheatmapcpm)] putting this in brackets allows us to search for every
# location in the data frame where NA is TRUE and with <- re assign that value to something we define. 
# In this case min() searchers for the lowest value in the table. Sometimes min() will return NA, so
# in order to ignore NA values in the min() command we add the argument na.rm=TRUE to ignore NA values
# when we search for the minimum value in the table. 

forheatmapcpm[is.na(forheatmapcpm)] <- min(forheatmapcpm, na.rm=TRUE)

# This step is a workaround to prevent NA values in heatmaps from returning as empty boxes
# and might not be appropriate for all analyses. Look into the pheatmap documentation for
# additional customization features.

# After following this step we have a data frame called forheatmapcpm with a column 
# for state name and a series of columns for dates. 

# We also happen to know that only a few states were reporting data before March 1, but 
# other states have data from as early as january, so we trim the data table to March 1, 2020.
# The step below trims the data table. It is a complex command with multiple layers.
# Try running each component of the command separately to see what each of the parts do:

# colnames(forheatmapcpm)=="20200301"
# which(colnames(forheatmapcpm)=="20200301")
# ncol(forheatmapcpm)
# as.integer(which(colnames(forheatmapcpm)=="20200301"))
# as.integer(which(colnames(forheatmapcpm)=="20200301")):(ncol(forheatmapcpm))
# forheatmapcpm[as.integer(which(colnames(forheatmapcpm)=="20200301")):(ncol(forheatmapcpm))]

forheatmaptrim <- forheatmapcpm[as.integer(which(colnames(forheatmapcpm)=="20200301")):(ncol(forheatmapcpm))]


# Now that all non-numeric characters have been removed from the data frame we 
# can convert the data frame into a numeric matrix using the as.matrix() command.

cpmmatrix <- as.matrix(forheatmaptrim)

# Finally we write the code for the heatmap. Here we use the pheatmap function 
# because it gives us some more customizability relative to the base R, and gplots 
# heatmap functions. 

# We set the scale to "none" because we want the graph to scale the plot based on 
# all the values (otherwise, it will normalize the data based on column or row values 
# in isolation). 

# The pheatmap function automatically creates dendograms for the x and the y axis which 
# reorganizes the data. We want to organize the data based on the states with the ~highest
# to lowest CPM, but we don't want to reorganize the columns because we want to look at 
# the data in chronological order. culster_cols or cluster_rows are the designations that 
# determine whether or not the rows are reorganized. 

# Next we set some parameters for colors. In order to visualize doubling time, we log2 
# transformed the data. In order to see doubling time clearly on the map - it would be 
# best to set one shade as a whole-number integer bin in log2 space. The 'breaks =' parameter 
# allows us to designate the range bins to create from the heatmap, so we use the as.integer() 
# function to caculate the min and max vaules for the matrix of data. Below we set the range 
# of values as a list called cpmbreaks.

cpmbreaks <- as.integer(min(forheatmapcpm-1)):as.integer(max(forheatmapcpm))

# Finally we compose the code for the heatmap setting each parameter on a separate 
# line to encourage readability. For color, we loaded the RColorBrewer package at 
# the beginning of the document. Using the colorRampPalette function we set a color 
# "Ramp", or gradient, using the full range (9) of the "Blues" brewer palette designated 
# one color for the number of values in cpmbreaks we calculated above. We remove the 
# border_color for ease of reading the heatmap. fontsize_col and fontsize_row customizes 
# the fontsizes of the columns, or rows, respectively. main = Title

cpmplot <- pheatmap(
  mymatrix, 
  scale="none", 
  cluster_cols = FALSE, 
  breaks = mybreaks, 
  color = colorRampPalette(brewer.pal(9, "Blues"))(length(mybreaks)),
  border_color = NA,
  fontsize_col = 4,
  fontsize_row = 5,
  main = "Log2(COVID Cases per Million) across the states"
  )

# That's my overview for creating a heatmap using this particular Covid data sets. 
# Good luck with your data analysis!
