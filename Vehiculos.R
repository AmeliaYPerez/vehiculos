library(plyr)
library(ggplot2)
library(reshape2)
setwd("~/Yayu/Python")

vehicles <- read.csv(unz("Data/vehicles.csv.zip","vehicles.csv"),stringsAsFactors = F)

#First let's find out how many observations are in our data 
nrow(vehicles)

#Now let's find out how many variables are in our data 
ncol(vehicles)

#Now lets get a sense of which columns of data are present in the data frame using the name fuction 
names(vehicles)

#Let's find out how many unique years of data are included in this dataset by computing a vector of the uniques values in the year
length(unique(vehicles[,"year"]))

#Now, we determine the first and the last years present in the dataset. 
first_year <- min(vehicles[,"year"])
last_year <- max(vehicles[,"year"])

length(unique(vehicles$year))

#lets find out what types of fuel are used as the automobiles primary fuel types
table(vehicles$fuelType1)

#lets explore the types of transmissions used by these automobiles 
vehicles$trany[vehicles$trany == ""] <- NA

# Now, the trany column is text, and we only care whether the car's transmission
# is automatic or manual. Thus, we use the substr function to extract the first four
# characters of each trany column value and determine whether it is equal to Auto.
# If so, we set a new variable, trany2, equal to Auto; otherwise, the value is set to
# Manual:

vehicles$trany2 <- ifelse(substr(vehicles$trany, 1, 4) ==
                            "Auto", "Auto", "Manual")

# Finally, we convert the new variable to a factor and then use the table function to see
# the distribution of values:

vehicles$trany <- as.factor(vehicles$trany)
table(vehicles$trany2)

#Analyzing automobile fuel efficiency over time 

#lets start  by looking at whether there is an overall trend og how MPG changes over time on an average 
mpgByYr <- ddply(vehicles,~year, summarise, avgMPG = mean(comb08), avgHghy = mean(highway08),avgCity = mean(city08))

ggplot(mpgByYr, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + 
  ggtitle("All Cars")

# Based on this visualization, one might conclude that there has been a tremendous
# increase in the fuel economy of cars sold in the last few years

# Let's look at just gasoline cars, even though there are not many non-gasoline
# powered cars, and redraw the preceding plot. 

gasCars <- subset(vehicles, fuelType1 %in% c("Regular Gasoline", "Premium Gasoline", "Midgrade Gasoline") &
                    fuelType2 == "" & atvType != "Hybrid")

mpgByYr_Gas <- ddply(gasCars, ~year, summarise, avgMPG = mean(comb08))
ggplot(mpgByYr_Gas, aes(year, avgMPG)) + geom_point() + geom_smooth() +xlab("Year") + ylab("Average MPG") +
  ggtitle("Gasoline cars")
#first, let's verify whether cars with larger engines have worse fuel efficiency

gasCars$displ <- as.numeric(gasCars$displ)
ggplot(gasCars, aes(displ, comb08)) + geom_point()+geom_smooth()

# This scatter plot of the data offers the convincing evidence that there is a negative,
# or even inverse correlation, between engine displacement and fuel efficiency; thus,
# smaller cars tend to be more fuel-efficient

# Now, let's see whether more small cars were made in later years, which can explain
# the drastic increase in fuel efficiency:

avgCarSize <- ddply(gasCars, ~year, summarise, avgDispl = mean(displ))
ggplot(avgCarSize, aes(year, avgDispl)) + geom_point() +
  geom_smooth() + xlab("Year") + ylab("Average engine displacement (l)")

# the average engine displacement has
# decreased substantially since 2008. To get a better sense of the impact
# this might have had on fuel efficiency, we can put both MPG and
# displacement by year on the same graph

byYear <- ddply(gasCars, ~year, summarise, avgMPG = mean(comb08),
                avgDispl = mean(displ))

byYear2 = melt(byYear, id = "year")

ggplot(byYear2, aes(year, value)) + geom_point() +
  geom_smooth() + facet_wrap(~variable, ncol = 1, scales =
                               "free_y") + xlab("Year") + ylab("")

# let's look at the change in proportion of manual cars available each
# year

gasCars4 <- subset(gasCars, cylinders == "4")
ggplot(gasCars4, aes(factor(year), comb08)) + geom_boxplot() 

ggplot(gasCars4, aes(factor(year), fill = factor(trany2))) +
  geom_bar(position = "fill") + labs(x = "Year", y = "Proportion
of cars", fill = "Transmission") + theme(axis.text.x =
                                           element_text(angle = 45)) + geom_hline(yintercept = 0.5,
                                                                                  linetype = 2)

ggplot(mpgByYr, aes(year, avgMPG)) + geom_point() + geom_smooth() +
  xlab("Year") + ylab("Average MPG") + ggtitle("All cars")
