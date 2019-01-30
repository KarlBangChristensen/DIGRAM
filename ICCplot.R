###########################################
### Test and Item Characteristic Curves ###
###########################################

# Author: Pedro Henrique Ribeiro Santiago
# Last update: 08/01/2019

###########################################
### STEP 0: Install required R packages ###
###########################################

install.packages("ggplot2")  #Install ggplot2 package

###########################################
### STEP 1: Entering DIGRAM data into R ###
###########################################

dbicc = file.choose() #Execute this command line and choose the ICC.txt
iccdata <- read.table(dbicc, header = TRUE, sep = "", dec = ",") #Execute this command to input DIGRAM data into R dataframe

####################################
### STEP 2: Execute the function ###
####################################

# To execute this command, single click on ICCplot and press Control+Enter (or Command+Enter for MAC users) #

ICCplot <- function(groupno="0", group1=c("P","2"), group2=c("R","2"), group3=c("S","2"), group4=c("T","2"), 
                    item, pallete='Paired', method="totalscore", color="#F8766D", show="no", title="Test Characteristic Curve",
                    ytitle="Total Score", xtitle="Theta", xticks=1.0, yticks=1.0, cinumber=8) {
  library(ggplot2)
  n1 <- which(colnames(iccdata)==group1[1])
  n2 <- group1[2]
  n3 <- which(colnames(iccdata)==group2[1])
  n4 <- group2[2]
  n5 <- which(colnames(iccdata)==group3[1])
  n6 <- group3[2]
  n7 <- which(colnames(iccdata)==group4[1])
  n8 <- group4[2]
  
  if(groupno==0) {
    subgroup=iccdata}
  else if(groupno==1) {
    subgroup=subset(iccdata, iccdata[,n1]==n2)}
  else if (groupno==2) {
    subgroup=subset(iccdata, iccdata[,n1]==n2&iccdata[,n3]==n4)}
  else if (groupno==3) {
    subgroup=subset(iccdata, iccdata[,n1]==n2&iccdata[,n3]==n4&iccdata[,n5]==n6)}
  else if (groupno==4) {
    subgroup=subset(iccdata, iccdata[,n1]==n2&iccdata[,n3]==n4&iccdata[,n5]==n6&iccdata[,n7]==n8)}
  else if(groupno=="all") {
    subgroup=iccdata
  }
  else {subgroup=NA}
  
  n <- which(colnames(subgroup)==item)
  
  if(method=="cut") {
    sub <- subset(subgroup, subgroup$type==1)
    sub$class <- cut2(sub$score, g=cinumber, oneval=TRUE, levels.mean = TRUE) #Creates Class Intervals
    obs <- aggregate(sub[,n], by=list(sub$class), FUN=mean) #Calculates Class Intervals Means
    obs[,1] <- as.numeric(levels(obs[,1]))
    type <- 1
    for (i in 1:cinumber) {
      x[i] = subgroup$Theta[which(abs(subgroup$score-obs[i,1])==min(abs(subgroup$score-obs[i,1])))]
    }
    x <- x[1:cinumber]
    obs <- cbind(x, obs[,2], type)
    obs <- as.data.frame(obs)
    names(obs) <- c("Theta", "score", "type")
    subgroup <- subgroup[subgroup$type!=1,]
    subgroup <- cbind(subgroup[,1], subgroup[,n], subgroup[,which(colnames(subgroup)=="type")])
    subgroup <- subgroup[,1:3]
    subgroup <- as.data.frame(subgroup)
    names(subgroup) <- c("Theta", "score", "type")
    subgroup <- rbind(subgroup, obs)
    rightcol <- which(colnames(subgroup)=="score")
    
    ggplot(data=subgroup, aes(x=Theta, y=subgroup[,2], col=as.factor(type))) + #Choose item
      ggtitle(title) + #Choose title
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
      labs(y = ytitle, x = xtitle) +
      scale_x_continuous(breaks = round(seq(min(subgroup$Theta), max(subgroup$Theta), by = xticks),1)) + 
      scale_y_continuous(breaks = round(seq(min(0), max(subgroup[,2]+0.5), by = yticks),1)) +
      scale_color_brewer(palette = pallete, name="", labels=c("Expected Item Score", "Average Observed Item Score")) +
      geom_point()
    }
  
  else {

  suppressWarnings(
  
  if (item=="score"&groupno!="all")
    
    ggplot(data=subgroup, aes(x=Theta, y=subgroup[,n])) + 
      ggtitle(title) + 
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = ytitle, x=xtitle) +
      scale_x_continuous(breaks = round(seq(min(subgroup$Theta), max(subgroup$Theta), by = xticks),1)) + #x-axis 1 logit
      scale_y_continuous(breaks = round(seq(min(0), max(subgroup[,n]+0.5), by = yticks),1)) +
      scale_color_brewer() +
      geom_point(colour=color, size=1.5) 
  
  else if (item=="score"&groupno=="all")
    
    ggplot(data=subgroup, aes(x=Theta, y=subgroup[,n])) + 
    ggtitle(title) + #Choose title
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y = ytitle, x=xtitle) +
    scale_x_continuous(breaks = round(seq(min(subgroup$Theta), max(subgroup$Theta), by = xticks),1)) + #x-axis 1 logit
    scale_y_continuous(breaks = round(seq(min(0), max(subgroup[,n]+0.5), by = yticks),1)) +
    scale_color_brewer(palette = pallete) +
    geom_point(colour=as.factor(subgroup[,n1]), size=0.1) 
    
  else if (item!="score"&groupno!="all")
  
  ggplot(data=subgroup, aes(x=Theta, y=subgroup[,n], col=as.factor(type))) + #Choose item
  ggtitle(title) + #Choose title
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  labs(y = ytitle, x = xtitle) +
  scale_x_continuous(breaks = round(seq(min(subgroup$Theta), max(subgroup$Theta), by = xticks),1)) + 
  scale_y_continuous(breaks = round(seq(min(0), max(subgroup[,n]+0.5), by = yticks),1)) +
  scale_color_brewer(palette = pallete, name="", labels=c("Expected Item Score", "Average Observed Item Score")) +
  geom_point()
  
  else
    
  ggplot(data=subgroup, aes(x=Theta, y=subgroup[,n])) + #Choose item
  ggtitle(title) + #Choose title
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  labs(y = ytitle, x = xtitle) +
  scale_x_continuous(breaks = round(seq(min(subgroup$Theta), max(subgroup$Theta), by = xticks),1)) + 
  scale_y_continuous(breaks = round(seq(min(0), max(subgroup[,n]+0.5), by = yticks),1)) +
  scale_color_brewer(palette = pallete, name="", labels=c("Expected Item Score", "Average Observed Item Score")) +
  geom_point(colour=as.factor(subgroup[,n1]), size=0.1)
  )
  }
  }

###############################
### STEP 3: Create the plot ###
###############################

ICCplot(groupno="0", group1 = c("P","2"), group2 = c("R","1"), group3=c("T","1"), 
        item="D", title="Pedro", ytitle="Henrique", xtitle="Stress", method="cut", cinumber=8)

#groupno: Number of subgroups. If there are no subgroups, choose zero. If you want the graph for all subgroups, choose groupno="all".
#group1: If there are more than 1 subgroup, define the subgroup name (for example, "P") and subgroup number (for example, "1").
#item: Define the name of the item (for example, item "C").
#pallete: Define the color pallete. The default is 'Paired'.
#color: Color of the Test Characteristic Curve. To change the colors write color="mycolor1". 
#title: Write title="Mytitle" to chose the plot title. Default is "Categories Probability Curves".
#ytitle: Write xtitle="MyYtitle" to chose the y-axis title. Default is "Probability".
#xtitle: Write xtitle="MyXtitle" to chose the x-axis title. Default is "Theta".
#xticks: Size of the x-axis ticks. The default is 1 score. To change write xticks=mynumber.
#yticks: Size of the y-axis ticks. The default is 1 logit. To change write yticks=mynumber.
#method: Observed scores are displayed according to total scores or class intervals. Default is method="totalscore".
#cinumber: The number of class intervals. Default is cinumber=8.


