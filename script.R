library(dplyr)
library(tidyverse)
library(ggplot2)
#reordering the factor levels first according to their frequency `forcats package`
library(forcats)
#Run in console `options("scipen"=100, "digits"=4)` for getting more user-friendly information
library(arules)
#Interective vizualization
library(arulesViz)
library(htmlwidgets)
#Scaterplot/ for sequential_hcl
library(colorspace)
library(Rgraphviz)
#Shiny App pack
library(shinythemes)
#Manipulating with date and time pack
library(lubridate)
library(grid)
library(gridExtra)



#1 importing data
dataset_movie <- read.csv(file = 'movie.csv')
dataset_rating <- read.csv(file = 'rating.csv')

str(dataset_movie)
str(dataset_rating)

head(dataset_movie)
head(dataset_rating)


dataset_joined <- dataset_rating %>%
  inner_join(dataset_movie, by=c("movieId"="movieId"), suffix=c("_rating","_movie"))

head(dataset_joined)
str(dataset_joined)

#selectiong subset of 2015 year
dataset_joined_sub2014 <- dataset_joined %>%
  mutate(Year=year(ymd_hms(timestamp))) %>%
  filter(Year == 2015) %>%
  mutate(as.factor(Year))

#2 Inspecting
dataset_joined_sub2014 %>%
  filter(userId==25) %>%
  summarise(
    total = n(),
    unique_items = n_distinct(title),
    number_of_users = n_distinct(userId)
  )

dataset_joined_sub2014 %>%
  group_by(userId) %>%
  summarise(
    total = n(), #total viewed movies
    unique_items = n_distinct(title) #distinct viewed movies
  )

dataset_joined_sub2014 %>%
  summarise(
    total = n(),
    unique_items = n_distinct(title),
    number_of_users = n_distinct(userId)
  )

#unique values in Item column
unique(dataset_joined_sub2014$title)
length(unique(dataset_joined_sub2014$title))

#order by frequency
dataset_joined_sub2014$title <- fct_infreq(dataset_joined_sub2014$title)

#Movie website contains 20367 movies
#Movie website has 9148 number of users

n_movies <- 14634
n_users <- 3903

#Calculating the all possible number of watched movies by users with Newton's Binom formula
2^14634 
# RESULT: Inf

#Looping for the all possible values of user session fillings with different movies in it
combinations = matrix(NA, nrow = 14635, ncol = 2)
for (i in 0:n_movies){
  combinations[i+1, ] = c(i, choose(n_movies, i))
}
colnames(combinations)=c("size", "number_of_combination")
combinations

#remove(combinations)

fun_nk = function(x) choose(n_movies, x)
# Plotting
ggplot(data = data.frame(x = 0),    
       mapping = aes(x=x))+  
       stat_function(fun = fun_nk)+  
       xlim(0, n_movies)+  
       xlab("Subset size")+  
       ylab("Number of subsets")

str(dataset_joined_sub2014)

# Sessions per month
dataset_joined_sub2014 %>%
  mutate(Month=as.factor(month(timestamp))) %>%
  group_by(Month) %>%
  summarise(Sessions=n_distinct(userId)) %>%
  ggplot(aes(x=Month, y=Sessions)) +
  geom_bar(stat="identity", fill="#ffe393", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Sessions)) +
  labs(title="Sessions per month") +
  theme_bw() 

# Sessions per weekday
dataset_joined_sub2014 %>%
  mutate(WeekDay=as.factor(weekdays(as.Date(timestamp)))) %>%
  group_by(WeekDay) %>%
  summarise(Sessions=n_distinct(userId)) %>%
  ggplot(aes(x=WeekDay, y=Sessions)) +
  geom_bar(stat="identity", fill="#a8ff93", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Sessions)) +
  labs(title="Sessions per weekday") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw() 


# Sessions per hour
dataset_joined_sub2014 %>%
  mutate(Hour=as.factor(hour(ymd_hms(timestamp)))) %>%
  group_by(Hour) %>%
  summarise(Sessions=n_distinct(userId)) %>%
  ggplot(aes(x=Hour, y=Sessions)) +
  geom_bar(stat="identity", fill="#ffbf46", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Sessions)) +
  labs(title="Sessions per hour") +
  theme_bw()

#3 Converting to transactional class
#Create lists with split function
#1) Tranform transaction ID into factor
dataset_joined_sub2014$userId <- factor(dataset_joined_sub2014$userId)

#2) Split into groups
dataset_list <- split(dataset_joined_sub2014$title, dataset_joined_sub2014$userId)
head(dataset_list)



#Transform to transaction class (transactional dataset)
data_transactional <- as(dataset_list, "transactions")

#Check the class og data_transactional
class(data_transactional)

str(data_transactional)

inspect(head(data_transactional))


#Item frequency plot (absolute)
itemFrequencyPlot(
  data_transactional,
  topN = 5,
  main = 'Absolute Item Frequency Plot', 
  type = "absolute",
  col = "khaki"
)

#Item frequency plot (relative)
itemFrequencyPlot(
  data_transactional,
  topN = 5,
  main = 'Relative Item Frequency Plot', 
  type = "relative",
  col = rainbow(15),
  horiz = TRUE
)

#We should notice that the most popular item has frequency ~0.39 and remember when we will try to detect support level


# Set of confidence levels
confidenceLevels = seq(from=0.1, to=0.9, by =0.1)

# Create vector with possible support levels
supportLevels <- c(0.05, 0.1, 0.15, 0.2, 0.3)


# Empty integers 
rules_sup_5perc <- integer(length=9)  #[OUT]: 0 0 0 0 0 0 0 0 0
rules_sup_10perc <- integer(length=9) #[OUT]: 0 0 0 0 0 0 0 0 0
rules_sup_20perc <- integer(length=9) #[OUT]: 0 0 0 0 0 0 0 0 0
rules_sup_30perc <- integer(length=9)  #[OUT]: 0 0 0 0 0 0 0 0 0
rules_sup_15perc <- integer(length=9)  #[OUT]: 0 0 0 0 0 0 0 0 0


remove(rules_sup_1perc)

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup_5perc[i] <- length(apriori(data_transactional, 
                                       parameter=list(sup=supportLevels[1], 
                                                      conf=confidenceLevels[i], 
                                                      target="rules")))
  
}

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup_10perc[i] <- length(apriori(data_transactional, 
                                        parameter=list(sup=supportLevels[2], 
                                                       conf=confidenceLevels[i], 
                                                       target="rules")))
  
}


# Apriori algorithm with a support level of 20%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup_20perc[i] <- length(apriori(data_transactional, 
                                        parameter=list(sup=supportLevels[4], 
                                                       conf=confidenceLevels[i], 
                                                       target="rules")))
  
}


# Apriori algorithm with a support level of 30%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup_30perc[i] <- length(apriori(data_transactional, 
                                       parameter=list(sup=supportLevels[5], 
                                                      conf=confidenceLevels[i], 
                                                      target="rules")))
  
}

# Apriori algorithm with a support level of 15%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup_15perc[i] <- length(apriori(data_transactional, 
                                       parameter=list(sup=supportLevels[3], 
                                                      conf=confidenceLevels[i], 
                                                      target="rules")))
  
}



# Number of rules found with a support level of 5%
plot5 <- qplot(confidenceLevels, rules_sup_5perc, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  scale_y_continuous(breaks=seq(0, 130000, 10000)) +
  theme_bw()


# Number of rules found with a support level of 10%
plot10 <- qplot(confidenceLevels, rules_sup_10perc, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") + 
  scale_y_continuous(breaks=seq(0, 2000, 150)) +
  theme_bw()

# Number of rules found with a support level of 15%
plot15 <- qplot(confidenceLevels, rules_sup_15perc, geom=c("point", "line"), 
                xlab="Confidence level", ylab="Number of rules found", 
                main="Apriori with a support level of 15%") + 
  scale_y_continuous(breaks=seq(0, 150, 25)) +
  theme_bw()

# Number of rules found with a support level of 20%
plot20 <- qplot(confidenceLevels, rules_sup_20perc, geom=c("point", "line"), 
                xlab="Confidence level", ylab="Number of rules found", 
                main="Apriori with a support level of 20%") + 
  scale_y_continuous(breaks=seq(0, 35, 5)) +
  theme_bw()

# Number of rules found with a support level of 30%
plot30 <- qplot(confidenceLevels, rules_sup_30perc, geom=c("point", "line"), 
                xlab="Confidence level", ylab="Number of rules found", 
                main="Apriori with a support level of 30%") + 
  scale_y_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Subplot
grid.arrange(plot5, plot10, plot15, plot20, plot30, ncol=2)

# Data frame
num_rules <- data.frame(rules_sup_5perc, 
                        rules_sup_10perc,
                        rules_sup_15perc,
                        rules_sup_20perc,
                        rules_sup_30perc,
                        confidenceLevels)


# Number of rules found with a support level of 5%, 10%, 15%, 20% and 30%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 5%)
  #geom_line(aes(y=rules_sup_5perc, colour="Support level of 5%")) + 
  #geom_point(aes(y=rules_sup_5perc, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 10%)
  #geom_line(aes(y=rules_sup_10perc, colour="Support level of 10%")) +
  #geom_point(aes(y=rules_sup_10perc, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 15%)
  #geom_line(aes(y=rules_sup_15perc, colour="Support level of 15%")) + 
  #geom_point(aes(y=rules_sup_15perc, colour="Support level of 15%")) +
  
  # Plot line and points (support level of 20%)
  geom_line(aes(y=rules_sup_20perc, colour="Support level of 20%")) + 
  geom_point(aes(y=rules_sup_20perc, colour="Support level of 20%")) +
  
  # Plot line and points (support level of 30%)
  geom_line(aes(y=rules_sup_30perc, colour="Support level of 30%")) + 
  geom_point(aes(y=rules_sup_30perc, colour="Support level of 30%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())


#I will consider support level of 15% with confidence > 75% and the support level of 20% with the confidence > 65%


#Finding frequent items with support level of 15% and 20%
#Frequent items
freq_supp15 = apriori(data_transactional,
                      parameter = list(
                        supp = 0.15,
                        conf = 0.75,
                        minlen = 2,
                        target = "frequent items"
                      ))

inspect(freq_supp15)

freq_supp20 = apriori(data_transactional,
                      parameter = list(
                        supp = 0.2,
                        conf = 0.65,
                        minlen = 2,
                        target = "frequent items"
                      ))

inspect(freq_supp20)

#Rules
rules_supp15 = apriori(data_transactional,
                       parameter = list(
                         supp = 0.15,
                         conf = 0.75,
                         minlen = 2,
                         target = "rules"
                       ))
inspect(rules_supp15)

rules_supp20 = apriori(data_transactional,
                       parameter = list(
                         supp = 0.20,
                         conf = 0.65,
                         minlen = 2,
                         target = "rules"
                       ))
inspect(rules_supp20)

#Checking redundancy
#Checking if exists redundant rules
redundant_rules15 = is.redundant(rules_supp15)
redundant_rules20 = is.redundant(rules_supp20)
#looking for non-redundant rules
non_redundant_rules15 = redundant_rules15[!redundant_rules15]
non_redundant_rules20 = redundant_rules20[!redundant_rules20]

#No redundancy found

#Interective inspectation (Datatable inspectation)
inspectDT(rules_supp15)
inspectDT(rules_supp20)

# Plot rules as scatterplot
plot(rules_supp15, method = "scatterplot", col=sequential_hcl(80, palette="OrYel"), engine = "html")
plot(rules_supp20, method = "scatterplot", control = list(col=sequential_hcl(25, palette="Inferno")), engine = "html")

# Plot rules as graph
plot(rules_supp15, method = "graph", engine = "html")
plot(rules_supp20, method = "graph", engine = "html")

# Top 10 rules with highest confidence
top10_rules_supp15 = head(sort(rules_supp15, by = "confidence"), 10)
inspect(top10_rules_supp15)

top10_rules_supp20 = head(sort(rules_supp20, by = "confidence"), 10)
inspect(top10_rules_supp20)

# Plot the top 10 rules
plot(top10_rules_supp15, method = "graph", col=sequential_hcl(100, palette="OrYel"), engine = "html")
plot(top10_rules_supp20, method = "graph", col=sequential_hcl(100, palette="OrYel"), engine = "html")


#Rule Exploring
ruleExplorer(rules_supp15)
ruleExplorer(rules_supp20)

#What influenced Star Wars: Episode IV - A New Hope (1977)  | (as a consequent)
# Extract rules with Star Wars: Episode IV - A New Hope (1977) on the right side

StarWars4_rules_rhs = apriori(data_transactional, parameter = list(supp = 0.15,conf = 0.75), appearance = list(default = "lhs",rhs = "Star Wars: Episode IV - A New Hope (1977)"))

# Find first rules with highest lift
inspect(head(sort(StarWars4_rules_rhs, by="lift")))
inspect(StarWars4_rules_rhs)

#ANSWER:Star Wars: Episode V - The Empire Strikes Back (1980)

#What did Star Wars: Episode IV - A New Hope (1977) influence (as an antecedent)
StarWars4_rules_lhs = apriori(data_transactional, parameter = list(supp = 0.15,conf = 0.75, minlen = 2), appearance = list(default = "rhs",lhs = "Star Wars: Episode IV - A New Hope (1977)"))

inspect(head(sort(StarWars4_rules_lhs, by="lift")))
inspect(StarWars4_rules_lhs)

#ANSWER: Star Wars: Episode V - The Empire Strikes Back (1980) with the confidence ~80% and lift 4.405
#        Matrix, The (1999) with the confidence ~81% and lift 2.628


