#####################################
### Categories Probability Curves ###
#####################################

###########################################
### STEP 0: Install required R packages ###
###########################################

install.packages("ggplot2")  # Execute this command to install ggplot2 package

###########################################
### STEP 1: Entering DIGRAM data into R ###
###########################################

dbcat = file.choose() # Execute this command line and choose the ItemProbs.txt
probdata <- read.table(dbcat, header = TRUE, sep = "", dec = ",")  #Execute this command to input DIGRAM data into R dataframe

####################################
### STEP 2: Execute the function ###
####################################

# To execute this command, single click on CatProbCurves and press Control+Enter (or Command+Enter for MAC users) #

CatProbCurves <- function(groupno="0", group1=c("P","2"), group2=c("R","2"), group3=c("S","2"), group4=c("T","2"), 
                          item, catnumber, shade=no, title="Categories Probability Curves", xtitle="Theta", ytitle="Probability",
                          color=c("blue","red","orange","green","yellow"), fill="#96CDFF", fillalpha=0.25) {
  probdata <- read.table(dbcat, header = TRUE, sep = "", dec = ",")
  library(ggplot2)
  n1 <- which(colnames(probdata)==group1[1])
  n2 <- group1[2]
  n3 <- which(colnames(probdata)==group2[1])
  n4 <- group2[2]
  n5 <- which(colnames(probdata)==group3[1])
  n6 <- group3[2]
  n7 <- which(colnames(probdata)==group4[1])
  n8 <- group4[2]
  
  if(groupno==0) {
    probdata=probdata}
  else if(groupno=="all") {
    probdata=probdata }
  else if(groupno==1) {
    probdata=subset(probdata, probdata[,n1]==n2)}
  else if (groupno==2) {
    probdata=subset(probdata, probdata[,n1]==n2&probdata[,n3]==n4)}
  else if (groupno==3) {
    probdata=subset(probdata, probdata[,n1]==n2&probdata[,n3]==n4&probdata[,n5]==n6)}
  else if (groupno==4) {
    probdata=subset(probdata, probdata[,n1]==n2&probdata[,n3]==n4&probdata[,n5]==n6&probdata[,n7]==n8)}
  else {probdata=NA}
  
  n <- which(colnames(probdata)==as.character(paste(item,"0",sep="")))
  
  if(shade=="no") {
  
  if (catnumber==1)
  {
  g <- ggplot(data=probdata) +
    geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour=color[1]) +
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
    ggtitle(title) +
    labs(x=xtitle, y=ytitle) + 
    scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
    scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1))
  } else if (catnumber==2)  {
    ggplot(data=probdata) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour=color[1]) +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
      ggtitle(title) +
      labs(x=xtitle, y=ytitle) + 
      scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
      scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1)) +
     geom_line(data=probdata, aes(x=Theta, y=probdata[,n+1]), colour=color[2]) }
  else if (catnumber==3) {
    ggplot(data=probdata) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour=color[1]) +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
      ggtitle(title) +
      labs(x=xtitle, y=ytitle) + 
      scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
      scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1)) +
     geom_line(data=probdata, aes(x=Theta, y=probdata[,n+1]), colour=color[2]) +
     geom_line(data=probdata, aes(x=Theta, y=probdata[,n+2]), colour=color[3]) }
  else if (catnumber==4) {
    ggplot(data=probdata) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour=color[1]) +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
      ggtitle(title) +
      labs(x=xtitle, y=ytitle) + 
      scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
      scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1)) +
     geom_line(data=probdata, aes(x=Theta, y=probdata[,n+1]), colour=color[2]) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+2]), colour=color[3]) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+3]), colour=color[4])}
  else {
    ggplot(data=probdata) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour=color[1]) +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
      ggtitle(title) +
      labs(x=xtitle, y=ytitle) + 
      scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
      scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1)) + 
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+1]), colour=color[2]) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+2]), colour=color[3]) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+3]), colour=color[4]) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+4]), colour=color[5])}
  } 
  
  else
    
    if (catnumber==1)
    {
      g <- ggplot(data=probdata) +
        geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour="grey") +
        geom_area(data=probdata, aes(x=Theta, y=probdata[,n]), fill=fill, alpha=fillalpha) +
        theme_light() + 
        theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
        ggtitle(title) +
        labs(x=xtitle, y=ytitle) + 
        scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
        scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1))
      g } else if (catnumber==2)  {
        ggplot(data=probdata) +
          geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour="grey") +
          geom_area(data=probdata, aes(x=Theta, y=probdata[,n]), fill=fill, alpha=fillalpha) +
          theme_light() + 
          theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
          ggtitle(title) +
          labs(x=xtitle, y=ytitle) + 
          scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
          scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1)) +
          geom_line(data=probdata, aes(x=Theta, y=probdata[,n+1]), colour="grey") +
          geom_area(data=probdata, aes(x=Theta, y=probdata[,n+1]), fill=fill, alpha=fillalpha)}
  else if (catnumber==3) {
    ggplot(data=probdata) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n]), fill=fill, alpha=fillalpha) +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
      ggtitle(title) +
      labs(x=xtitle, y=ytitle) + 
      scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
      scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1)) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+1]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n+1]), fill=fill, alpha=fillalpha) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+2]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n+2]), fill=fill, alpha=fillalpha)}
  else if (catnumber==4) {
    ggplot(data=probdata) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n]), fill=fill, alpha=fillalpha) +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
      ggtitle(title) +
      labs(x=xtitle, y=ytitle) + 
      scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
      scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1)) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+1]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n+1]), fill=fill, alpha=fillalpha) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+2]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n+2]), fill=fill, alpha=fillalpha) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+3]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n+3]), fill=fill, alpha=fillalpha)}
  else {
    ggplot(data=probdata) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n]), fill=fill, alpha=fillalpha) +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
      ggtitle(title) +
      labs(x=xtitle, y=ytitle) + 
      scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
      scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1)) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+1]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n+1]), fill=fill, alpha=fillalpha) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+2]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n+2]), fill=fill, alpha=fillalpha) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+3]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n+3]), fill=fill, alpha=fillalpha) +
      geom_line(data=probdata, aes(x=Theta, y=probdata[,n+4]), colour="grey") +
      geom_area(data=probdata, aes(x=Theta, y=probdata[,n+4]), fill=fill, alpha=fillalpha)}
}  

###############################
### STEP 3: Create the plot ###
###############################

CatProbCurves(groupno="0", group1 = c("P","1"), group2 = c("R","2"), group3=c("T","1"), item="C", catnumber=5, shade="yes")

#groupno: Number of subgroups. If there are no subgroups, choose zero. If you want the graph for all subgroups, choose groupno="all".
#group1: If there are more than 1 subgroup, define the subgroup name (for example, "P") and subgroup number (for example, "1").
#item: Define the name of the item (for example, item "C").
#catnumber: Define the number of item categories.
#shade: Choose between shade (shade="yes") and no shade (shade="no").
#title: Write title="Mytitle" to chose the plot title. Default is "Categories Probability Curves".
#xtitle: Write xtitle="MyXtitle" to chose the x-axis title. Default is "Theta".
#ytitle: Write xtitle="MyYtitle" to chose the y-axis title. Default is "Probability".
#color: Color of category probability curves. To change the colors write color=c("mycolor1","mycolor2","mycolor3","mygreen","myyellow"). 
          #Default is color=c("blue","red","orange","green","yellow").
#fill: Filling of the area below the category probability curves. To change write fill="mycolorhere". Default is fill="#96CDFF".
#fillalpha: Transparency of the area below the category probability curves. To change write fillalpha=mynumber. Default is fillalpha=0.25.
