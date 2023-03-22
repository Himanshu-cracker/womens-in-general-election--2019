# Loading all the necessary packages.
library(tidyverse)
library(janitor)
library(readr)
library(treemapify)
library(ggplot2)

# Importing dataset

women_participation <- read_csv("C:\\Users\\HIMANSHU KUMAR\\OneDrive\\Desktop\\womens in indian election -2019\\participation_of_women_electors_2019.csv")
glimpse(women_participation)


## conducting EDA and Data Cleaning process on the data set ##




# Changing the names to lowercase because R is case sensistive language 

women_participation <- women_participation %>% 
  rename_with(tolower)

# Checking if the columns' names have changed or not.

colnames(women_participation)





# Removing the spaces between the names.

colnames(women_participation) <- gsub(" ","_",colnames(women_participation))

# Rechecking the columns' names.

colnames(women_participation)

women_participation <- women_participation %>% 
  clean_names()





## head  of women participation
head(women_participation)

## tail of women participation
tail(women_participation)



## finding total number of electors in 2019 LOK SABHA Elections.

total_electors_india <- sum(women_participation$total_electors)
total_electors_india



# Total number of women electors.

total_women_electors <- sum(women_participation$women_electors)
total_women_electors



# total number of male electors.

total_nonwomen_electors <- total_electors_india - total_women_electors
total_nonwomen_electors



# Percentage of women votersof total voters.

percentage_women_electors <- (total_women_electors*100)/total_electors_india
percentage_women_electors


# Voters are those who casted their vote.

total_voters_india <- sum(women_participation$total_voters)
total_voters_india

# Total number of women electors.

total_women_voters <- sum(women_participation$women_voters)
total_women_voters



# finding the percentage of women turnout.

percentage_women_turnout <- (total_women_voters*100)/total_women_electors
percentage_women_turnout




# Total number of non women voters.

total_nonwomen_voters <- total_voters_india - total_women_voters
total_nonwomen_voters



# Calculating the non women turnout.

percentage_nonwomen_turnout <- (total_nonwomen_voters*100)/total_nonwomen_electors
percentage_nonwomen_turnout



# Total turnout.

percentage_total_turnout <- (total_voters_india*100)/total_electors_india
percentage_total_turnout

summary(women_participation)



#  finding the state with the maximum women turnout

max_turnout <- women_participation[which.max(women_participation$percent_of_women_voters_over_women_electors),]
max_turnout


# State with the lowest women turnout.

min_turnout <- women_participation[which.min(women_participation$percent_of_women_voters_over_women_electors),]
min_turnout

# State with highest women electors
state_max_women_electors <- women_participation[which.max(women_participation$women_electors),]
state_max_women_electors


# State with the lowest women electors
state_min_women_electors <- women_participation[which.min(women_participation$women_electors),]
state_min_women_electors




# State with the highest women voters.

state_max_women_voters <- women_participation[which.max(women_participation$women_voters),]
state_max_women_voters


# State with the lowest women voters
state_min_women_voters <- women_participation[which.min(women_participation$women_voters),]
state_min_women_voters





##  visualisation of women voters statewise via treemap
### (A treemap is a data visualization technique that displays hierarchical data as a set of nested rectangles. Each rectangle represents a specific portion of the data, and its size and color can be used to convey additional information about the data.)
ggplot(women_participation, aes(area = women_voters, label = state, fill = women_voters)) +
  geom_treemap()+
  geom_treemap_text(place = "centre") +
  labs(title = "Women voters statewise")  + theme(legend.position = "none")




 ##treemap of women voters statewise.
ggplot(women_participation, aes(area = percent_of_women_voters_over_women_electors, label = state, fill = percent_of_women_voters_over_women_electors)) +
  geom_treemap()+
  geom_treemap_text(place = "centre") +
  labs(title = "Women turnout statewise") + scale_fill_viridis_c() + theme(legend.position = "none")





## Finding the total number of LOK SABHA seats in India.
total_seats <- sum(women_participation$no_of_seats)
total_seats

##  statewise distribution of parlaiment seats
ggplot(women_participation, aes(x = reorder(state, -no_of_seats), y = no_of_seats)) +
  geom_text(aes(label = no_of_seats), vjust = -0.5) +
  geom_col(stat = "identity", fill = "darkorange", width = 0.5) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
  labs(title = "Parliament Seats statewise", x = "States", y = "No. of seats")






## scatter plot for Women electors vs Women voters--( scatter plot is used to know relation between variables)
ggplot(women_participation,aes(x=women_electors, y=women_voters)) + 
  geom_point() + geom_smooth(se = FALSE, color = "purple") + 
  labs(title = "Women electors vs Women voters", x = "Women Electors", y = "Women Voters") + theme_minimal()

## calculating correlation coefficient 
women_participation %>%
  summarise(cr_womenelectors_womenvoters = cor(women_electors,women_voters))







## scatter plot for Women electors vs Women Turnout--( scatter plot is used to know relation between variables)
ggplot(women_participation,aes(x=women_electors,y=percent_of_women_voters_over_women_electors)) + geom_point() + geom_smooth(se = FALSE, color = "purple") + 
  labs(title = "Women electors vs Women Turnout", x = "Women Electors", y = "Women Turnout in %") +
  theme_minimal()

## calculating correlation coefficient 
women_participation %>%
  summarise(cr_womenelectors_womenturnout = cor(women_electors,percent_of_women_voters_over_women_electors))







