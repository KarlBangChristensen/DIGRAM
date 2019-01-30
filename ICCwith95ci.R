##################################################
# DIGRAM graphs                                  #
# Pedro Henrique Ribeiro Santiago                #
# 12-10-2018                                     #
##################################################

###########################################
### STEP 0: Install required R packages ###
###########################################

install.packages("ggplot2")  #Install ggplot2 package
library(ggplot2) #Load ggplot2 package

###########################################
### STEP 1: Entering DIGRAM data into R ###
###########################################

dbci = file.choose() #Execute this command line and choose the Ari_dot.csv
confidence <- read.csv(dbci, header = TRUE, sep = ";") #Execute this command to input DIGRAM data into R dataframe
dbicc = file.choose() #Execute this command line and choose the ICC.txt
iccdata <- read.table(dbicc, header = TRUE, sep = "", dec = ",") #Execute this command to input DIGRAM data into R dataframe

# NOTE: The Ari_dot.csv and ICC.txt should display THE SAME ITEMS

####################################
### STEP 2: Execute the function ###
####################################

# To execute this command, single click on ICCwith95 and press Control+Enter (or Command+Enter for MAC users) #

  ICCwith95 <- function(item, itemname=name, title="Item Characteristic Curve",
                        ytitle="Item Score", xtitle="Theta", pointcolor="black", linecolor="black", fillcolor="gray") {
    step <- subset(confidence$Item, confidence$ItemNo==item)
    name <- paste(step[1])
    library(ggplot2)
    annotations <- data.frame(
      xpos = c(-Inf,-Inf,Inf,Inf),
      ypos =  c(-Inf, Inf,-Inf,Inf),
      annotateText = itemname,
      hjustvar = c(-0.5) ,
      vjustvar = c(3.0)) #<- adjust
  confidence$ExpVar <- as.numeric(confidence$ExpVar)
  ggplot(data=subset(confidence, confidence$ItemNo==item), aes(x=Score, y=ObsMean)) +
  theme_light() +
    ggtitle(title) + #Choose title
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
  labs(y = ytitle, x = xtitle) +
  scale_color_brewer(palette = 'Paired', name="", labels=c("Expected Item Score", "Average Observed Item Score")) +
  geom_point(data=subset(confidence, confidence$ItemNo==item), aes(x=Score, y=ObsMean), colour=pointcolor) +
  geom_line(data=subset(confidence, confidence$ItemNo==item), aes(x=Score, y=ExpMean), colour=linecolor) +
  geom_ribbon(data=subset(confidence, confidence$ItemNo==item), aes(x=Score, 
                        ymin=ExpMean - 1.96*sqrt(ExpVar/n), ymax=ExpMean + 1.96*sqrt(ExpVar/n)), fill=fillcolor, alpha=0.3) +
scale_x_continuous(breaks = confidence$Score[1:length(subset(iccdata$Theta, iccdata$type=="1"))], 
                   labels=format(round(subset(iccdata$Theta, iccdata$type=="1"), digits=2), nsmall=2)) +
    geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))
  }

###############################
### STEP 3: Create the plot ###
###############################
  
ICCwith95("4", title="Pedro")

#groupno: Number of subgroups. If there are no subgroups, choose zero. If you want the graph for all subgroups, choose groupno="all".
#group1: If there are more than 1 subgroup, define the subgroup name (for example, "P") and subgroup number (for example, "1").
#title: Write title="Mytitle" to chose the plot title. Default is "Categories Probability Curves".
#ytitle: Write xtitle="MyYtitle" to chose the y-axis title. Default is "Probability".
#xtitle: Write xtitle="MyXtitle" to chose the x-axis title. Default is "Theta".
#pointcolor: Color of the Observed Scores. To change the colors write pointcolor=mycolor1. Default is black.
#linecolor: Color of the Item Characteristic Curve. To change the colors write linecolor=mycolor1. Default is black.
#fillcolor: Color of the 95% confidence regions. To change the colors write fillcolor=mycolor1. Default is grey.

