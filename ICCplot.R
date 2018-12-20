# Author: Pedro Henrique Ribeiro Santiago
# Description: Creates Test Characteristic Curve and Item Characteristic Curve for one subgroup

###Loading libraries###

install.packages("ggplot2")  #Install ggplot2 package
library(ggplot2) #Load ggplot2 package

## FILE = ICC.txt ###

##############################################################
### Creating Item Characteristic Curves for A SINGLE GROUP ###
##############################################################

### Creating Test Characteristic Curves ###

dbicc = file.choose() #Choose the ICC.txt
iccdata <- read.table(dbicc, header = TRUE, sep = "", dec = ",") #Inputs the data into dataframe

#Execute the function below

TCCplot <- function(subgroup) {
ggplot(subset(iccdata, subgroup), aes(x=Theta, y=score)) + 
  ggtitle("Test Characteristic Curve") + #Choose title
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Total Score") +
  scale_x_continuous(breaks = round(seq(min(iccdata$Theta), max(iccdata$Theta), by = 1.0),1)) + #x-axis 1 logit
  scale_y_continuous(breaks = round(seq(min(0), max(iccdata$score+0.5), by = 1.0),1)) +
  scale_color_brewer(palette = 'Paired') +
  geom_point(colour="#F8766D", size=1.5)
}

#Create plot of TCC for a single group

TCCplot(iccdata$P==1&iccdata$R==1&iccdata$T==1) #Choose subgroup

#NOTE: If there are no subgroups, leave blank between parentheses (e.g. TCCplot())

### Creating Item Characteristic Curves ###

#Execute function below

ICCplot <- function(subgroup) {
ggplot(subset(iccdata, subgroup), aes(x=Theta, y=D, col=as.factor(type))) + #Choose item
  ggtitle("Item Characteristic Curve") + #Choose title
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  labs(y = "Item Score") +
  scale_x_continuous(breaks = round(seq(min(iccdata$Theta), max(iccdata$Theta), by = 1.0),1)) + 
  scale_y_continuous(breaks = round(seq(min(0), max(iccdata$D), by = 1.0),1)) +
  scale_color_brewer(palette = 'Paired', name="", labels=c("Expected Item Score", "Average Observed Item Score")) +
  geom_point()
}

#Create plot of ICC for a single group

ICCplot(iccdata$P==1&iccdata$R==1&iccdata$T==1) #Choose subgroup

#NOTE: If there are no subgroups, leave blank between parentheses (e.g. TCCplot())


