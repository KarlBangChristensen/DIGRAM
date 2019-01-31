##################################################
# DIGRAM graphs                                  #
# Pedro Henrique Ribeiro Santiago                #
# 12-10-2018                                     #
##################################################

###########################################
### STEP 0: Install required R packages ###
###########################################

install.packages("ggplot2")  #Install ggplot2 package

###########################################
### STEP 1: Entering DIGRAM data into R ###
###########################################

dbmap = file.choose() #Execute this command line and choose the Imap.txt
imapdata <- read.table(dbmap, header = TRUE, sep = "", dec = ",") #Execute this command to input DIGRAM data into R dataframe
dbinf = file.choose() #Execute this command line and choose the ItemInf.txt
infdata <- read.table(dbinf, header = TRUE, sep = "", dec = ",") #Execute this command to input DIGRAM data into R dataframe

####################################
### STEP 2: Execute the function ###
####################################

# To execute this command, single click on Imap and press Control+Enter (or Command+Enter for MAC users) #

Imap <- function(groupno=0, group1=c("P","1"), group2=c("R","1"), group3=c("T","1"), group4=c("T","2"), 
                 inf="yes", color="colorful", grid="no", xtitle="Theta", ytitle="Item Map", title="", titlealign="center",
                 WMLheight=1.75) {
  
  if(color=="colorful") {
    color<- c("orange","#F8766D","#6290C3", "green", "black")}
  else {color <- c("black", "black", "black", "black", "black")}
  
  if(grid=="no") {
    background <- element_blank()
    gridy <- element_blank()
    gridx <- element_blank()
    panelgrid <- element_blank()}
  else {background <- element_rect(fill = "white", colour="black")
  gridy <- element_blank()
  gridx <- element_blank()
  panelgrid <- element_line(colour="grey87", size=0.25)}
  
  if(titlealign=="center") {
    titlealign <- 0.5}
  else if (titlealign=="left") {
    titlealign <- 0.0}
  else {titlealign <- 0.0}
  
  n1 <- which(colnames(imapdata)==group1[1])
  n2 <- group1[2]
  n3 <- which(colnames(imapdata)==group2[1])
  n4 <- group2[2]
  n5 <- which(colnames(imapdata)==group3[1])
  n6 <- group3[2]
  n7 <- which(colnames(imapdata)==group4[1])
  n8 <- group4[2]
  w1 <- which(colnames(infdata)==group1[1])
  w2 <- group1[2]
  w3 <- which(colnames(infdata)==group2[1])
  w4 <- group2[2]
  w5 <- which(colnames(infdata)==group3[1])
  w6 <- group3[2]
  w7 <- which(colnames(infdata)==group4[1])
  w8 <- group4[2]
  
  library(ggplot2)
  
  if(groupno==0) {
    groupmapdata=imapdata
    groupinfdata=infdata}
  else if(groupno==1) {
    groupmapdata=subset(imapdata, imapdata[,n1]==n2)
    groupinfdata=subset(infdata, infdata[,w1]==w2)}
  else if (groupno==2) {
    groupmapdata=subset(imapdata, imapdata[,n1]==n2&imapdata[,n3]==n4)
    groupinfdata=subset(infdata, infdata[,w1]==w2&infdata[,w3]==w4)}
  else if (groupno==3) {
    groupmapdata=subset(imapdata, imapdata[,n1]==n2&imapdata[,n3]==n4&imapdata[,n5]==n6)
    groupinfdata=subset(infdata, infdata[,w1]==w2&infdata[,w3]==w4&infdata[,w5]==w6)}
  else if (groupno==4) {
    groupmapdata=subset(imapdata, imapdata[,n1]==n2&imapdata[,n3]==n4&imapdata[,n5]==n6&imapdata[,n7]==n8)
    groupinfdata=subset(infdata, infdata[,w1]==w2&infdata[,w3]==w4&infdata[,w5]==w6&infdata[,w7]==w8)}
  else {subgroup=NA}
  
  group1 <- subset(groupmapdata, type==1) #Choose WML estimates 
  group1$theta <- as.numeric(as.character(group1$theta))
  group2 <- subset(groupmapdata, type==2) #Choose population   
  group2$theta <- as.numeric(as.character(group2$theta))
  group3 <- subset(groupmapdata, type==3) #Choose item thresholds  
  group3$theta <- as.numeric(as.character(group3$theta))
  group5 <- subset(groupmapdata, type==5) #Choose information
  group5$theta <- as.numeric(as.character(group5$theta))
  
  suppressWarnings(
  binwidth <- function(x){
    diffbin = group2[(1:nrow(group2)+1),which(colnames(group2)=="theta")]-group2[1:nrow(group2),which(colnames(group2)=="theta")]
    difvar <- as.numeric(paste(diffbin))
    meanbin <<- mean(difvar, na.rm=TRUE)
  }) #Adjusts the binwidth so population bars touch each other
  suppressWarnings(binwidth(as.numeric(rownames(group2))))

  suppressWarnings(
  if(inf=="no") {
  
  g <- suppressWarnings(print(ggplot(group2, aes(x=theta)) + #This is the IMAP where WLM estimates are half the size of population bars
    ggtitle(title) +
      theme(plot.title = element_text(hjust = titlealign), 
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid = panelgrid,
            panel.grid.major.y = gridy,
            panel.grid.major.x = gridx,
            panel.border = element_rect(colour="black", size=0.25, fill=NA),
            panel.background = background,
            legend.position="none") +
    labs(x = xtitle, y= ytitle) +
    scale_x_continuous(breaks = round(seq(min(pmin(subset(group2$theta, group2$weight!=0)), 
                                              subset(group3$theta, group2$weight!=0)), 
                                          max(pmax(group2$theta, group3$theta))+0.5, by = 1.0),1),
                       limits=c(min(pmin(subset(group2$theta, group2$weight!=0)), 
                                    subset(group3$theta, group2$weight!=0))-0.5,
                                max(pmax(group2$theta, group3$theta))+0.5)) + 
    geom_bar(data=group1, aes(x=theta, weight=weight/WMLheight), colour=color[1], fill=color[1], alpha=1.0) + #Notice weight is divided by 2
    geom_bar(data=subset(group3, weight!=0), aes(x=theta, weight=-weight/2), fill=color[2], width=1/2*meanbin) +
    geom_bar(aes(weight = weight), fill=color[3], width=meanbin, colour=color[5], size=0.3, alpha=0.2) +
    geom_segment(aes(x=min(pmin(subset(group2$theta, weight!=0)), subset(group3$theta, weight!=0)-0.3), y=0, 
                     xend=max(pmax(group2$theta, group3$theta)+0.5), yend=0), arrow = arrow(length = unit(0.25, "cm")))))
  g }
  
  else {
    suppressWarnings(print(ggplot(group2, aes(x=theta)) + #This is the IMAP where WLM estimates are half the size of population bars
      ggtitle(title) +
      theme(plot.title = element_text(hjust = titlealign), 
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid = panelgrid,
            panel.grid.major.y = gridy,
            panel.grid.major.x = gridx,
            panel.border = element_rect(colour="black", fill=NA, size=0.25),
            panel.background = background,
            legend.position="none") +
      labs(x = xtitle, y= ytitle) +
      scale_x_continuous(breaks = round(seq(min(pmin(subset(group2$theta, group2$weight!=0)), 
                                                subset(group3$theta, group2$weight!=0)), 
                                            max(pmax(group2$theta, group3$theta))+0.5, by = 1.0),1),
                         limits=c(min(pmin(subset(group2$theta, group2$weight!=0)), 
                                      subset(group3$theta, group2$weight!=0))-0.5,
                                  max(pmax(group2$theta, group3$theta))+0.5)) + 
      geom_bar(data=group1, aes(x=theta, weight=weight/WMLheight), colour=color[1], fill=color[1], alpha=1.0) + #Notice weight is divided by 2
      geom_bar(data=subset(group3, weight!=0), aes(x=theta, weight=-weight/2), fill=color[2], width=1/2*meanbin) +
      geom_bar(aes(weight = weight), fill=color[3], width=meanbin, colour=color[5], size=0.3, alpha=0.2) +
      geom_segment(aes(x=min(pmin(subset(group2$theta, weight!=0)), subset(group3$theta, weight!=0)-0.3), y=0, 
                       xend=max(pmax(group2$theta, group3$theta)+0.5), yend=0), arrow = arrow(length = unit(0.25, "cm"))) + 
      geom_line(data=groupinfdata[groupinfdata$Theta >= min(pmin(subset(group2$theta, group2$weight!=0)), 
  subset(group3$theta, group2$weight!=0)) & groupinfdata$Theta <= max(pmax(group2$theta, group3$theta)),], 
  aes(x=Theta, y=(max(group2$weight)/max(groupinfdata$TestInf))*0.75*TestInf), colour=color[4]) +
    geom_segment(aes(x=groupinfdata[which.max(groupinfdata$TestInf),1], y=0, 
  xend=groupinfdata[which.max(groupinfdata$TestInf),1], 
  yend=(max(group2$weight)/max(groupinfdata$TestInf))*0.75*max(groupinfdata$TestInf)), colour=color[4],
                 size=0.1)))
    })
  }

###############################
### STEP 3: Create the plot ###
###############################

Imap(groupno=3, group1=c("P","1"), group2=c("R","1"), group3=c("T","1"), inf="yes", color="black",
     ytitle="", title="Item Map", titlealign="center", grid="yes", WMLheight = 2.0)

#groupno: Number of subgroups. If there are no subgroups, choose zero.
#group1: If there are more than 1 subgroup, define the subgroup name (for example, "P") and subgroup number (for example, "1").
#inf: Shows information function, choose between "yes" and "no". Default is "yes".
#color: Choose color="black" for black-and-white and "colorful" for color. Default is "colorful".
