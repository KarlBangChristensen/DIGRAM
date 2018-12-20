# Author: Pedro Henrique Ribeiro Santiago
# Description: Information Plots

###Loading libraries###

install.packages("ggplot2")  #Install ggplot2 package
library(ggplot2) #Load ggplot2 package

## FILE = ItemInf.txt ###

##########################################################
### Creating Information plots for ALL GROUPS COMBINED ###
##########################################################

dbinf = file.choose() #Choose the ItemInf.txt
infdata <- read.table(dbinf, header = TRUE, sep = "", dec = ",") #Inputs the data into dataframe 

### Creating Test Information plots ###

#Execute the function below

TestInfPlotAll <- function(group1, group2, group3) {
infdata <- cbind(infdata, paste(group1, group2, group3))
colnames(infdata)[ncol(infdata)] <- "subgroup"
ggplot(subset(infdata, ), aes(x=Theta, y=TestInf, col=subgroup)) + 
  ggtitle("Test Information") + #Plot title
  theme_light() + #Choose background theme
  theme(plot.title = element_text(hjust = 0.5)) + #Align title
  labs(y = "Information") + #Label y-axis
  scale_x_continuous(breaks = round(seq(min(infdata$Theta), max(infdata$Theta), by = 1.0),1)) + #Set x-axis ticks
  scale_y_continuous(breaks = round(seq(min(0), max(infdata$TestInf), by = 0.5),1)) + #Set y-axis ticks
  scale_color_brewer(palette = 'Paired', name = "Subgroup") + #Choose colors 
  geom_point() #Plot test information for all subgroups
}

TestInfPlotAll(infdata$P, infdata$R, infdata$T) #Input the subgroups here
#NOTE: If you are using more or less than 3 groups, please change the 2 first lines of the function

### Creating Item Information plots ###

#Execute the function below

ItemInfPlotAll <- function(group1, group2, group3) {
  infdata <- cbind(infdata, paste(group1, group2, group3))
  colnames(infdata)[ncol(infdata)] <- "subgroup"
  ggplot(subset(infdata, ), aes(x=Theta, y=C, col=subgroup)) +  
    ggtitle("Item Information  ") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = "Information") +
    scale_x_continuous(breaks = round(seq(min(infdata$Theta), max(infdata$Theta), by = 1.0),1)) + #x-axis 1 logit
    scale_y_continuous(breaks = round(seq(min(0), max(infdata$TestInf), by = 0.1),1)) +
    scale_color_brewer(palette = 'Paired',  name = "Subgroup") +
    geom_point(size=1.0) #Plot item information for all subgroups
}

ItemInfPlotAll(infdata$P, infdata$R, infdata$T) #Input the subgroups here
#NOTE: If you are using more or less than 3 groups, please change the 2 first lines of the function

### Creating SEM plots ###

#Execute the function below

SEMPlotAll <- function(group1, group2, group3) {
  infdata <- cbind(infdata, paste(group1, group2, group3))
  colnames(infdata)[ncol(infdata)] <- "subgroup"
  ggplot(subset(infdata, ), aes(x=Theta, y=sem, col=subgroup)) +  
    ggtitle("Standard Error of Measurement") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = "SEM") +
    scale_x_continuous(breaks = round(seq(min(infdata$Theta), max(infdata$Theta), by = 1.0),1)) + 
    scale_y_continuous(breaks = round(seq(min(0), max(infdata$sem), by = 0.5),1)) +
    scale_color_brewer(palette = 'Paired', name = "Subgroup") +
    geom_point(size=1.0) #Plot SEM for all subgroups
}

SEMPlotAll(infdata$P, infdata$R, infdata$T) #Input the subgroups here
#NOTE: If you are using more or less than 3 groups, please change the 2 first lines of the function

#####################################################
### Creating Information plots for A SINGLE GROUP ###
#####################################################

### Creating Test Information plots ###

#Execute the function below

TestInfPlotSub <- function(sub) {
  ggplot(subset(infdata, sub), aes(x=Theta, y=TestInf)) + 
  ggtitle("Test Information") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Information") +
  scale_x_continuous(breaks = round(seq(min(infdata$Theta), max(infdata$Theta), by = 1.0),1)) + #x-axis 1 logit
  scale_y_continuous(breaks = round(seq(min(0), max(infdata$TestInf), by = 0.5),1)) +
  geom_area(colour="#F8766D", fill="#F8766D", alpha=0.3) + #This line includes the area under the curve
  geom_point(colour="#F8766D", size=1.5) #Plot test information for a single subgroup
}
 
TestInfPlotSub(infdata$P==1&infdata$R==1&infdata$T==1) #Input the subgroup here

### Creating Item Information plots ###

#Execute the function below

ItemInfPlotSub <- function(sub) {
ggplot(subset(infdata, sub), aes(x=Theta, y=C)) + #Change the item here
  ggtitle("Item Information") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Information") +
  scale_x_continuous(breaks = round(seq(min(infdata$Theta), max(infdata$Theta), by = 1.0),1)) + 
  scale_y_continuous(breaks = round(seq(min(0), max(infdata$TestInf), by = 0.1),1)) +
  geom_area(colour="#00BFC4", fill="#00BFC4", alpha=0.3) + 
  geom_point(colour="#00BFC4", size=1.5) #Plot item information for a single subgroup
}

ItemInfPlotSub(infdata$P==1&infdata$R==1&infdata$T==1) #Input the subgroups here

### Creating SEM plots ###

#Execute the function below

SEMPlotSub <- function(sub) {
ggplot(subset(infdata, sub), aes(x=Theta, y=sem, col=subgroup)) +  
  ggtitle("Standard Error of Measurement") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "SEM") +
  scale_x_continuous(breaks = round(seq(min(infdata$Theta), max(infdata$Theta), by = 1.0),1)) + 
  scale_y_continuous(breaks = round(seq(min(0), max(infdata$sem), by = 0.5),1)) +
  geom_area(colour="#00BFC4", fill="#00BFC4", alpha=0.3) +
  geom_point(colour="#00BFC4", size=1.5) #Plot SEM for a single subgroup
}

SEMPlotSub(infdata$P==1&infdata$R==1&infdata$T==1) #Input the subgroup

### Creating Test Information + SEM plots ###

#Execute the function below

TestInfSEMPlotSub <- function(sub) {
  ggplot() + 
  geom_point(data=subset(infdata, sub), aes(x=Theta, y=TestInf), colour="#00BFC4") +
  geom_point(data=subset(infdata, sub), aes(x=Theta, y=sem), colour="#F8766D") +
  geom_area(data=subset(infdata, sub), aes(x=Theta, y=TestInf), colour="#00BFC4", fill="#00BFC4", alpha=0.3) +
  ggtitle("Test Information + Standard Error of Measurement") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  labs(y = "Information") +
  scale_x_continuous(breaks = round(seq(min(infdata$Theta), max(infdata$Theta), by = 1.0),1)) + #x-axis 1 logit
  scale_y_continuous(sec.axis = sec_axis(~., name="SEM",
                                         breaks=c(seq(-10,1,0.5), seq(1.5,4,0.5))),
                     breaks = c(round(seq(min(0), max(infdata$sem), by = 0.5),1)), name="Information") +
  scale_color_brewer(palette = 'Paired') +
  geom_point(size=1.0) #Plot test information and SEM for a single subgroup
}

TestInfSEMPlotSub(infdata$P==1&infdata$R==1&infdata$T==1) #Input the subgroup
