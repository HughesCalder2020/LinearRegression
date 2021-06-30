# my regression code in one place
pacman::p_load(datasets, pacman, rio, tidyverse, matrixStats, wesanderson, hexbin, psych, pastecs, QuantPsyc, car, boot, readxl, ggcorrplot)
forwardSkaters <- read_excel("~/Desktop/R/Evolving Hockey csv/Box SCore/EH forward box score 07-21.xls")
View(forwardSkaters)  

#Gonna make a quick dataset suplicate

forSkat <- forwardSkaters
#Now gonna make time standardised variables for the stats. It goes to show that that the more time on ice a player has,
#The more likely they are to increase their stats, so we need a way to standardise these variables.

forSkat$p_rel_toi <- forSkat$Points/forSkat$TOI
forSkat$icf_rel_toi <- forSkat$iCF/forSkat$TOI
forSkat$tot_pend <- (2 * forSkat$iPEND2) + (5 *forSkat$iPEND5)
forSkat$tot_pent <- (2 * forSkat$iPENT2) + (5 * forSkat$iPENT5)
forSkat$weightedPenDiff <- forSkat$tot_pend - forSkat$tot_pent
forSkat$tot_pend_rel_toi <- forSkat$tot_pend/forSkat$TOI
forSkat$fo_pct <- forSkat$FOW / (forSkat$FOW + forSkat$FOL)

#This produces a nicer larger number. Basically, stat per 60?
#I'm worried about the justification for this stat. I don't like this

forSkat$avg_toi <- forSkat$TOI/forSkat$GP
forSkat$p_rel_avgtoi <- forSkat$Points/forSkat$avg_toi


#This is the code for exploring each variable explored and the histo/density graph



#Template for other variables

#mean(forSkat$)
#mean(forSkat$, trim = 0.1)
#median(forSkat$)
#sd(forSkat$)
#mad(forSkat$)
#IQR(forSkat$)
#quantile(forSkat$i, p=c(.05, .25, .5, .75, .95))
#boxplot(forSkat$, ylab="")

#Template for the histogram and density lines

#x <- ggplot(forSkat, aes(x=)) +
  #geom_histogram(aes(y=..density..), binwidth = 0.5, color="#0E0E0E", fill="#E8BF41")  +
  #geom_density(alpha=.2, color="#1740A6", fill="#4BA564") +
  #labs(x="", y="", title="")
#print(x)          

#I think conceptually, especially for heirarchical linear analysis, it would make more sense to introduce each statistic, so exploring


mean(forSkat$p_rel_avgtoi)
mean(forSkat$p_rel_avgtoi, trim = 0.1)
median(forSkat$p_rel_avgtoi)
sd(forSkat$p_rel_avgtoi)
mad(forSkat$p_rel_avgtoi)
IQR(forSkat$p_rel_avgtoi)
quantile(forSkat$p_rel_avgtoi, p=c(.05, .25, .5, .75, .95))
boxplot(forSkat$p_rel_avgtoi, ylab="Points per average Time on Ice per game played")

#Now lets try for a frequency table



#Come back to fix this mess of a table. Honestly you are a disgrace
pHisto <- ggplot(forSkat, aes(x=p_rel_avgtoi)) +
          geom_histogram(aes(y=..density..), binwidth = 0.5, color="#0E0E0E", fill="#E8BF41")  +
          geom_density(alpha=.2, color="#1740A6", fill="#4BA564") +
          labs(x="Points per average time on ice in 1 game", y="Density", title="Histogram of Points per average time on ice per game")
print(pHisto)          


#Now lets look at corsi per minute, should be a better variable, but can adjust with average toi

mean(forSkat$icf_rel_toi)
mean(forSkat$icf_rel_toi, trim = 0.1)
median(forSkat$icf_rel_toi)
sd(forSkat$icf_rel_toi)
mad(forSkat$icf_rel_toi)
IQR(forSkat$icf_rel_toi)
quantile(forSkat$icf_rel_toi, p=c(.05, .25, .5, .75, .95))

boxplot(forSkat$icf_rel_toi, ylab="Individual Corsi For per minute")
histIcf <- ggplot(forSkat, aes(x=icf_rel_toi)) +
  geom_histogram(aes(y=..density..), binwidth = 0.05, color="#0E0E0E", fill="#E8BF41")  +
  geom_density(alpha=.2, color="#1740A6", fill="#4BA564") +
  labs(x="Individual Corsi For per Minute on Ice (iCF/minute)", y="Density", title="Histogram of Individual Corsi For per Minute on Ice")
print(histIcf) 

#Data for penalties per minute?

mean(forSkat$tot_pend)
mean(forSkat$tot_pend, trim = 0.1)
median(forSkat$tot_pend)
sd(forSkat$tot_pend)
mad(forSkat$tot_pend)
IQR(forSkat$tot_pend)
quantile(forSkat$tot_pend, p=c(.05, .25, .5, .75, .95))

boxplot(forSkat$tot_pend, ylab="")
histPenD<- ggplot(forSkat, aes(x=weightedPenDiff)) +
  geom_histogram(aes(y=..density..), binwidth = 10, color="#0E0E0E", fill="#E8BF41")  +
  geom_density(alpha=.2, color="#1740A6", fill="#4BA564") +
  labs(x="Total number of penalties drawn (minutes)", y="Density", title="Histogram of Total number of penalties drawn (minutes)")
print(histPenD) 


#Data for face off percentage, although could just do raw faceoff wins? Should a weight being number of faceoff taken be applied?

mean(forSkat$`FO±`)
mean(forSkat$`FO±`, trim = 0.1)
median(forSkat$`FO±`)
sd(forSkat$`FO±`)
mad(forSkat$`FO±`)
IQR(forSkat$`FO±`)
quantile(forSkat$`FO±`, p=c(.05, .25, .5, .75, .95))


boxplot(forSkat$`FO±`, ylab="Face Off Wins")
histFOPM<- ggplot(forSkat, aes(x=`FO±`)) +
  geom_histogram(aes(y=..density..), binwidth = 10, color="#0E0E0E", fill="#E8BF41")  +
  geom_density(alpha=.2, color="#1740A6", fill="#4BA564") +
  labs(x="Face-off wins", y="Density", title="Histogram of Face-off Wins")
print(histFOPM) 

#Now doing some exploratory analysis for giveaway/takeaways
forSkat$givtakdiff <- forSkat$GIVE - forSkat$TAKE
mean(forSkat$givtakdiff)
mean(forSkat$givtakdiff, trim = 0.1)
median(forSkat$givtakdiff)
sd(forSkat$givtakdiff)
mad(forSkat$givtakdiff)
IQR(forSkat$givtakdiff)
quantile(forSkat$givtakdiff, p=c(.05, .25, .5, .75, .95))


#Template for the histogram and density lines
boxplot(forSkat$givtakdiff, ylab="")
histGTD <- ggplot(forSkat, aes(x=givtakdiff)) +
      geom_histogram(aes(y=..density..), binwidth = 10, color="#0E0E0E", fill="#E8BF41")  +
      geom_density(alpha=.2, color="#1740A6", fill="#4BA564") +
      labs(x="", y="", title="")
print(histGTD)  

#Assessing the effect of our curated variables on our adjusted points values. Now lets do correlation stuff. This is a correlation matrix
varAnalysis <- c("p_rel_avgtoi", "icf_rel_toi", "weightedPenDiff" , "givtakdiff", "FO±")
keyData <- forSkat[varAnalysis]
corrMatrix <- round(cor(keyData), 1)
colnames(corrMatrix) <- c("Faceoff Differential", "Giveaway/Takeaway Differential", "Total Penalty Minute Differential", "Points per average ice time", "Individual Corsi For per Minute")
rownames(corrMatrix) <- c("Faceoff Differential", "Giveaway/Takeaway Differential", "Total Penalty Minute Differential", "Points per average ice time", "Individual Corsi For per Minute")
cor_pmat(keyData)
ggcorrplot(corrMatrix, hc.order = TRUE, 
           outline.col = "#EEEEEE",
           ggtheme = ggplot2::theme_gray,
           colors = c("#1740A6", "#EEEEEE", "#4BA564"))

#Some Graphs exploring the factors that seem to have a greater predictive ability 
#IN this instance, there is correlation between
#Random graphs here










#Now lets start regression baby!!. Do we have a suitable sample size?
#predictors -> 4
summary(forSkat)
#n=7762
50+(8*4)
#We have a large enough sample to be able to test the model
104+4
#And to be able to test individual predictors

simpleRegression <- lm(p_rel_toi ~ icf_rel_toi, data=forSkat)
summary(simpleRegression)
multipleRegression <- lm(p_rel_toi ~ icf_rel_toi + `FO±` + givtakdiff + weightedPenDiff, data=forSkat)
summary(multipleRegression)
#Work out adjusted R^2 with Steins equation

lm.beta(multipleRegression)
conflint(multipleRegression)
anova(simpleRegression, multipleRegression)
dwt(multipleRegression)
vif(multipleRegression)
mean(vif(multipleRegression))
print(1/vif(multipleRegression))

forSkat$residuals <- resid(multipleRegression)
forSkat$standardised.residuals <- rstandard(multipleRegression)
forSkat$cook <- cooks.distance(multipleRegression)
forSkat$leverage <- hatvalues(multipleRegression)
forSkat$cov <- covratio(multipleRegression)
forSkat$studentised.residuals <- rstudent(multipleRegression)
forSkat$fitted <- multipleRegression$fitted.values
forSkat$large.residual <- forSkat$standardised.residuals > 2 | forSkat$standardised.residuals < -2
sum(forSkat$large.residual)
forSkat[forSkat$large.residual, c("cook", "leverage", "cov")]
write.table(forSkat, file="Regression-variable adjusted forward skater data table.csv", sep="\t", row.names = FALSE)
#Publication quality plots.
histRegress <- ggplot(forSkat, aes(studentised.residuals)) +
        geom_histogram(aes(y=..density..), color="#B92626", fill="#E8BF41") +
        labs(x="Studentised residuals", y="Density")
print(histRegress)
histRegress + stat_function(fun=dnorm, args=list(mean=mean(forSkat$studentised.residuals, na.rm=TRUE), sd=sd(forSkat$studentised.residuals, na.rm = TRUE)), color="#B92626", size=1)

#QQ PLot
qqresid <-qplot(sample=forSkat$studentised.residuals) +
          labs(x="Theoretical Values", y="Observed Values")
print(qqresid)

#Scatterplot

scatter <- ggplot(forSkat, aes(fitted, studentised.residuals)) +
            geom_point() +
            geom_smooth(method="lm", color="1740A6") +
            labs(x="Fitted Values", y="Studentised Residuals")
print(scatter)
