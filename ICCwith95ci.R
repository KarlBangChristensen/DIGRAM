# Author: Pedro Henrique Ribeiro Santiago
# Description: Item Characteristic Curve with 95% regions for Expected Scores

###Loading libraries###

install.packages("ggplot2")  #Install ggplot2 package
library(ggplot2) #Load ggplot2 package

dbci = file.choose() #Choose the Ari_dot.csv
confidence <- read.csv(dbci, header = TRUE, sep = ";") #Inputs the data into dataframe
dbicc = file.choose() #Choose the ICC.txt
iccdata <- read.table(dbicc, header = TRUE, sep = "", dec = ",") #Inputs the data into dataframe

# NOTE: The Ari_dot.csv and ICC.txt should display THE SAME ITEM

#Execute the function below

  ICCwith95 <- function(sub) {
  confidence[18] <- as.numeric(confidence$ExpVar)
  ggplot(data=subset(confidence, sub), aes(x=Score, y=ObsMean)) +
  theme_light() +
  ggtitle("Item Characteristic Curve") + #Choose title
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
  labs(y = "Item Score", x = "Theta") +
  scale_color_brewer(palette = 'Paired', name="", labels=c("Expected Item Score", "Average Observed Item Score")) +
  geom_point(data=subset(confidence, sub), aes(x=Score, y=ObsMean), colour="black") +
  geom_line(data=subset(confidence, sub), aes(x=Score, y=ExpMean), colour="black") +
  geom_ribbon(data=subset(confidence, sub), aes(x=Score, 
                        ymin=ExpMean - 1.96*sqrt(ExpVar/n), ymax=ExpMean + 1.96*sqrt(ExpVar/n)), fill="gray", alpha=0.3) +
scale_x_continuous(breaks = confidence$Score[1:11], 
                   labels=format(round(subset(iccdata$Theta, iccdata$type=="1"), digits=2), nsmall=2)) +
  annotate("text", x = (min(confidence$Score)+min((3*confidence$Score/10))), 
           y = max(confidence$ObsMean[confidence$ItemNo=="2"]+0.1), 
           label = "Item 1")
  }
  
ICCwith95(confidence$ItemNo=="2")
