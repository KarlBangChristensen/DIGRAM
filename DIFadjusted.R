# Author: Pedro Henrique Ribeiro Santiago
# Description: Creates a plot for Adjusted vs Observed Scores

###Loading required libraries###

install.packages("ggplot2")  #Install ggplot2 package

###########################################
### STEP 1: Entering DIGRAM data into R ###
###########################################

difobsadj = file.choose() # Execute this command line and choose the PersonLocations-comma.txt
difdata <- read.table(difobsadj, header = TRUE, sep = "", dec = ",") #Execute this command to input DIGRAM data into R dataframe

####################################
### STEP 2: Execute the function ###
####################################

ObsAdjPlot <- function(groupno="1", group1=c("P","2"), group2=c("R","2"), group3=c("T","2"), group4=c("S","2"),
                       design="dots", colorobs="orange", coloradj="blue", xtitle="DIF Adjusted Scores",
                      ytitle="Frequency of responses", title="Observed x Equated Scores") {
  library(ggplot2)
  difdata <- read.table(difobsadj, header = TRUE, sep = "", dec = ",")
  n1 <- which(colnames(difdata)==group1[1])
  n2 <- group1[2]
  n3 <- which(colnames(difdata)==group2[1])
  n4 <- group2[2]
  n5 <- which(colnames(difdata)==group3[1])
  n6 <- group3[2]
  n7 <- which(colnames(difdata)==group4[1])
  n8 <- group4[2]
  if(groupno==1) {
    subgroup=subset(difdata, difdata[,n1]==n2)}
  else if (groupno==2) {
    subgroup=subset(difdata, difdata[,n1]==n2&difdata[,n3]==n4) }
  else if (groupno==3) {
    subgroup=subset(difdata, difdata[,n1]==n2&difdata[,n3]==n4&difdata[,n5]==n6)}
  else if (groupno==4) {
    subgroup=subset(difdata, difdata[,n1]==n2&difdata[,n3]==n4&difdata[,n5]==n6&difdata[,n7]==n8)}
  else {subgroup=NA}

  if (design=="bar") {
  ggplot(subgroup, aes(x=score)) + #Manually select the subgroup
    geom_bar(data=subgroup, 
             aes(x=score, weight=count), colour=colorobs, fill=colorobs, width=0.1, alpha=0.5) + #Modify color/transparency/width of obs scores
    geom_bar(data=subgroup, 
             aes(x=Escore, weight=count), colour=coloradj, fill=coloradj, width=0.1, alpha=0.5) + #Modify color/transparency/width of equated scores
    scale_x_continuous(breaks = round(seq(min(0), max(difdata$score), by = 1.0),1)) +
    theme_light() +
    labs(x = xtitle, y= ytitle) + #Axis labels
    annotate("text", x = (max(difdata$score)-max((difdata$score/5))), 
             y = max(subgroup$count)-max((subgroup$count)/10), 
             label = print(paste0("Mean Observed Score = ", format(round(weighted.mean(subgroup$score, 
                                                                                       subgroup$count), digits=2),
                                                                   nsmall=2)))) + #Annotates the Mean Observed Score
    annotate("text", x = (max(difdata$score)-max((difdata$score/5))), 
             y = max(subgroup$count)-max((subgroup$count)/5), 
             label = print(paste0("Mean Equated Score = ", format(round(weighted.mean(subgroup$Escore, 
                                                                                      subgroup$count), digits=2), nsmall=2)))) +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank()) + #Annotates the Mean Equated Score
    ggtitle(title) }
  else {
    ggplot(data=subgroup, aes(x=score)) + #Manually select the subgroup
      geom_line(data=subgroup, 
                aes(x=score, y=count), colour=colorobs, show.legend=TRUE) + #Modify color of obs scores
      geom_line(data=subgroup, 
                aes(x=Escore, y=count), colour=coloradj,  alpha=0.5, show.legend=TRUE) + #Modify color/transparency of equated scores
      geom_point(data=subgroup, 
                 aes(x=score, y=count), colour=colorobs) + #Modify color of obs scores
      geom_point(data=subgroup, 
                 aes(x=Escore, y=count), colour=coloradj,  alpha=0.5) + #Modify color/transparency of equated scores
      scale_x_continuous(breaks = round(seq(min(0), max(difdata$score), by = 1.0),1)) +
      theme_light() +
      labs(x = "Scores", y="Frequency of responses") + #Axis labels
      annotate("text", x = (max(difdata$score)-max((difdata$score/5))), 
               y = max(subgroup$count)-max((subgroup$count)/10), 
               label = print(paste0("Mean Observed Score = ", format(round(weighted.mean(subgroup$score, 
                                                                                         subgroup$count),
                                                                           digits=2), nsmall=2)))) + #Annotates the Mean Observed Score
      annotate("text", x = (max(difdata$score)-max((difdata$score/5))), 
               y = max(subgroup$count)-max((subgroup$count)/5), 
               label = print(paste0("Mean Equated Score = ", format(round(weighted.mean(subgroup$Escore, 
                                                                                        subgroup$count),
                                                                          digits=2), nsmall=2)))) + #Annotates the Mean Equated Score
      theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank()) +
      ggtitle("Observed x Equated Scores") }
}

###############################
### STEP 3: Create the plot ###
###############################

ObsAdjPlot(groupno="3", group1=c("R","2"), group2=c("S","2"), group3=c("T","2"), design="dots")

#groupno: Number of subgroups. If there are no subgroups, choose zero. If you want the graph for all subgroups, choose groupno="all".
#group1: If there are more than 1 subgroup, define the subgroup name (for example, "P") and subgroup number (for example, "1").
#design: Choose between design="bars" or design="dots". Dots are the default.
#colorobs: Color of the observed scores. Default is orange. To change write colorobs="mycolor".
#coloradj: Color of the adjusted scores. Default is blue. To change write coloradj="mycolor".
#xtitle: Write xtitle="MyXtitle" to chose the x-axis title. Default is "Theta".
#ytitle: Write xtitle="MyYtitle" to chose the y-axis title. Default is "Probability".
#title: Write title="Mytitle" to chose the plot title. Default is "Categories Probability Curves".
