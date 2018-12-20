
################################################
### Categories Probability Curves - Design 1 ###
################################################

dbcat = file.choose() #Choose the ItemProbs.txt
probdata <- read.table(dbcat, header = TRUE, sep = "", dec = ",") #Inputs the data into dataframe infdata

CatProbCurves <- function(sub) {
ggplot(data=subset(probdata, sub)) +
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C0), colour="blue") + #Each line corresponds to one category
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C1), colour="red") +
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C2), colour="orange") +
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C3), colour="green") +
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C4), colour="yellow") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
  ggtitle("Categories Probability Curves") +
  labs(y = "Probability") + 
  scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) + 
  scale_y_continuous(breaks = round(seq(min(0), max(1.1), by = 0.1),1))
}

CatProbCurves(probdata$P==1&probdata$R==1&probdata$T==1)

################################################
### Categories Probability Curves - Design 2 ###
################################################

CatProbCurves2 <- function(sub) {
  ggplot(data=(subset(probdata, sub))) +
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C0)) +
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C1)) +
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C2)) +
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C3)) +
  geom_line(data=subset(probdata, sub), aes(x=Theta, y=C4)) +
  geom_area(data=subset(probdata, sub), aes(x=Theta, y=C0), fill="#96CDFF", alpha=0.25) +
  geom_area(data=subset(probdata, sub), aes(x=Theta, y=C1), fill="#96CDFF", alpha=0.25) +
  geom_area(data=subset(probdata, sub), aes(x=Theta, y=C2), fill="#96CDFF", alpha=0.25) +
  geom_area(data=subset(probdata, sub), aes(x=Theta, y=C3), fill="#96CDFF", alpha=0.25) +
  geom_area(data=subset(probdata, sub), aes(x=Theta, y=C4), fill="#96CDFF", alpha=0.25) +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
  ggtitle("Categories Probability Curves") +
  labs(y = "Probability") + 
  scale_x_continuous(breaks = round(seq(min(probdata$Theta), max(probdata$Theta), by = 1.0),1)) +
  scale_y_continuous(breaks = round(seq(min(0), max(1.3), by = 0.1),1))
}
CatProbCurves2(probdata$P==1&probdata$R==1&probdata$T==1)

