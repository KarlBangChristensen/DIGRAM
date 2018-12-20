# Author: Pedro Henrique Ribeiro Santiago
# Description: Creates Item Maps

dbmap = file.choose() #Choose the Imap.txt
imapdata <- read.table(dbmap, header = TRUE, sep = "", dec = ",") #Inputs the data into dataframe
dbinf = file.choose() #Choose the ItemInf.txt
infdata <- read.table(dbinf, header = TRUE, sep = "", dec = ",") #Inputs the data into dataframe 

# NOTE: The Imap.txt and ItemInf.txt should display THE SAME SUBGROUPS

# Execute the function below

Imap <- function(sub, sub2) {
  groupmapdata <- subset(imapdata, sub) 
  groupinfdata <- subset(infdata, sub2) 
  group1 <- subset(groupmapdata, type==1) #Choose WML estimates 
  group1$theta <- as.numeric(as.character(group1$theta))
  group2 <- subset(groupmapdata, type==2) #Choose population   
  group2$theta <- as.numeric(as.character(group2$theta))
  group3 <- subset(groupmapdata, type==3) #Choose item thresholds  
  group3$theta <- as.numeric(as.character(group3$theta))
  group5 <- subset(groupmapdata, type==5) #Choose information
  group5$theta <- as.numeric(as.character(group5$theta))
  binwidth <- function(x){
    diffbin = group2[(1:nrow(group2)+1),which(colnames(group2)=="theta")]-group2[1:nrow(group2),which(colnames(group2)=="theta")]
    difvar <- as.numeric(paste(diffbin))
    meanbin <<- mean(difvar, na.rm=TRUE)
  } #Adjusts the binwidth so population bars touch each other
  binwidth(as.numeric(rownames(group2))) 
  ggplot(group2, aes(x=theta)) + 
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position="none") +
    labs(x = "Theta", y="Item Map") +
    scale_x_continuous(breaks = round(seq(min(pmin(subset(group2$theta, group2$weight!=0)), 
                                              subset(group3$theta, group2$weight!=0)), 
                                          max(pmax(group2$theta, group3$theta))+0.5, by = 1.0),1),
                       limits=c(min(pmin(subset(group2$theta, group2$weight!=0)), 
                                    subset(group3$theta, group2$weight!=0))-0.5,
                                max(pmax(group2$theta, group3$theta))+0.5)) + 
    geom_bar(data=group1, aes(x=theta, weight=weight/2), colour="orange", fill="orange", alpha=1.0) + #Notice weight is divided by 2
    geom_bar(data=subset(group3, weight!=0), aes(x=theta, weight=-weight/2, fill="red"), width=1/2*meanbin) +
    geom_bar(aes(weight = weight), fill="#6290C3", width=meanbin, colour="black", size=0.3, alpha=0.2) +
    geom_line(data=groupinfdata[groupinfdata$Theta >= min(pmin(subset(group2$theta, group2$weight!=0)), subset(group3$theta, group2$weight!=0)) & groupinfdata$Theta <= max(pmax(group2$theta, group3$theta)),], aes(x=Theta, y=(max(group2$weight)/max(groupinfdata$TestInf))*0.75*TestInf), colour="green") +
    geom_segment(aes(x=groupinfdata[which.max(groupinfdata$TestInf),1], y=0, 
                     xend=groupinfdata[which.max(groupinfdata$TestInf),1], yend=(max(group2$weight)/max(groupinfdata$TestInf))*0.75*max(groupinfdata$TestInf)), colour="green",
                 size=0.1) +
    geom_segment(aes(x=min(pmin(subset(group2$theta, weight!=0)), subset(group3$theta, weight!=0)-0.3), y=0, xend=max(pmax(group2$theta, group3$theta)+0.5), yend=0), arrow = arrow(length = unit(0.25, "cm"))) 
}

Imap(imapdata$P==1&imapdata$R==1&imapdata$T==1, infdata$P==1&infdata$R==1&infdata$T==1) #Choose subgroup
