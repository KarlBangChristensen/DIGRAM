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

inf = file.choose() # Execute this command line and choose the ItemInf.txt
infdata <- read.table(inf, header = TRUE, sep = "", dec = ",")  #Execute this command to input DIGRAM data into R dataframe

####################################
### STEP 2: Execute the function ###
####################################

# To execute this command, single click on InfPlot and press Control+Enter (or Command+Enter for MAC users) #

InfPlot <- function(groupno="0", group1=c("P","2"), group2=c("R","2"), group3=c("S","2"), group4=c("T","2"), 
                    item="TestInf", fill="#F8766D", color="#F8766D", title="Test Information",
                           ytitle="Information", xtitle="Theta", shade="yes", sem="yes", dualaxis="yes", inf="yes",
                    semcolor="#619CFF", pointsize=1.5, semsize=0.5, ybreaks=0.5) {
  
  n1 <- which(colnames(infdata)==group1[1])
  n2 <- group1[2]
  n3 <- which(colnames(infdata)==group2[1])
  n4 <- group2[2]
  n5 <- which(colnames(infdata)==group3[1])
  n6 <- group3[2]
  n7 <- which(colnames(infdata)==group4[1])
  n8 <- group4[2]
  
  if(groupno==0) {
    infdata=infdata}
  else if(groupno=="all") {
    infdata=infdata }
  else if(groupno==1) {
    infdata=subset(infdata, infdata[,n1]==n2)}
  else if (groupno==2) {
    infdata=subset(infdata, infdata[,n1]==n2&infdata[,n3]==n4)}
  else if (groupno==3) {
    infdata=subset(infdata, infdata[,n1]==n2&infdata[,n3]==n4&infdata[,n5]==n6)}
  else if (groupno==4) {
    infdata=subset(infdata, infdata[,n1]==n2&infdata[,n3]==n4&infdata[,n5]==n6&infdata[,n7]==n8)}
  else {infdata=NA}
  
  n <- which(colnames(infdata)==item)
  
  if(inf=="yes") {color <- color
    }
  else {color <- NA
  fill <- NA}
  if(sem=="yes"&dualaxis=="yes") {
    s <- which(colnames(infdata)=="sem")
    semcol <- semcolor
    nameaxis <- "SEM"
    sembreaks <- round(seq(min(0), max(infdata[,n], infdata[,s]), by = ybreaks),1)}
  else if(sem=="yes"&dualaxis=="no") {
    s <- which(colnames(infdata)=="sem")
    semcol <- semcolor
    nameaxis <- NULL
    sembreaks <- NULL}
  else {s <- which(colnames(infdata)==item)
  semcol <- NA
  nameaxis <- NULL
  sembreaks <- NULL
  }
  if(shade=="yes") {
    area<- fill}
  else {area <- NA}

  suppressWarnings(print(ggplot() + 
  geom_point(data=infdata, aes(x=Theta, y=infdata[,n]), colour=color, size=pointsize) +
  geom_area(data=infdata, aes(x=Theta, y=infdata[,n]), colour=color, fill=area, alpha=0.3) +
  geom_point(data=infdata, aes(x=Theta, y=infdata[,s]), colour=semcol, size=semsize) +
  ggtitle(title) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = ytitle, x=xtitle) +
  scale_x_continuous(breaks = round(seq(min(infdata$Theta), max(infdata$Theta), by = 1.0),1)) + #x-axis 1 logit
  scale_y_continuous(sec.axis = sec_axis(~., name=nameaxis, breaks=sembreaks), 
                     breaks = round(seq(min(0), max(infdata[,n], infdata[,s]), by = ybreaks),1))))}

###############################
### STEP 3: Create the plot ###
###############################
  
InfPlot(groupno="3", group1=c("P","1"), group2=c("R","1"), group3=c("T","2"),
        item="C", shade="yes", sem="yes", inf="yes", ybreaks=0.5) #Input the subgroup

#groupno: Number of subgroups. If there are no subgroups, choose zero. If you want the graph for all subgroups, choose groupno="all".
#group1: If there are more than 1 subgroup, define the subgroup name (for example, "P") and subgroup number (for example, "1").
#item: Define the name of the item (for example, item "C"). If you want the test information, choose item="TestInf".
#fill: Filling of the area below the information function. To change write fill="mycolorhere". Default is fill="#F8766D".
#color: Color of the information function. To change the colors write color="mycolor1". Default is color="#F8766D".
#title: Write title="Mytitle" to chose the plot title. Default is "Test Information".
#ytitle: Write xtitle="MyYtitle" to chose the y-axis title. Default is "Information".
#xtitle: Write xtitle="MyXtitle" to chose the x-axis title. Default is "Theta".
#shade: Shade below the information function. Choose between shade="yes" and shade="no".Default is "yes".
#sem: Standard Error of Measurement curve. Choose between sem="yes" and sem="no". Default is "yes".
#dualaxis: Dual axis with SEM. Choose between sem="yes" and sem="no". Default is "yes".
#inf: Information curve. Choose between sem="yes" and sem="no". Default is "yes".
#semcolor: Color of the SEM curve. To change the colors write semcolor="mycolor1". Default is semcolor="#619CFF".
#pointsize: Size of the information curve. To change write pointsize=mynumber. Default is pointsize=0.5.
#semsize: Size of the SEM curve. To change write semsize=mynumber. Default is semsize=0.5.
#ybreaks: Change the ticks of the y-label. To change write yticks=mynumber. Default is ybreaks=0.5.
