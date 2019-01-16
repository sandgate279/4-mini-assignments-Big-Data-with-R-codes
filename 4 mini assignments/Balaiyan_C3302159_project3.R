##Install the following packages and libraries to execute the following codes.
install.packages("rtf")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("plyr")
install.packages("moments")
library(moments)
library(plyr) 
library(ggplot2)
library(corrplot)
library(rtf) 
#########################################################################################################################
##Q1.
#########################################################################################################################
##set the working directory and the read the CSV.
setwd("C:/INFT6201")
forestfires = read.csv("forestfires.csv", header=TRUE, sep=",", dec = ".", stringsAsFactors = FALSE)

##create a factor of the column month in the above CSV into 4 quarters of an year asa new column called quarters.

monthlist <- rep(c("Q1",'Q2','Q3','Q4'),each = 3)
names(monthlist) <- tolower(month.abb)
forestfires$quarter<-monthlist[forestfires$month]

##create a density plot by using the column temp and RH from the above CSV and export into the working directory as a png file with 900 * 900 resolution.
png(file="rh_quarters.png", 
    width=900, height=900)

ggplot(forestfires, aes(x=RH,  fill=quarter)) + 
  geom_density(alpha = .75) + # Make it a Density Plot
  scale_fill_brewer(palette="Set2", name="Season") + # Use a filling color from RColorBrewer
  labs(title = "Relative humidity for Q1-4") + # Add a Title
  theme_bw() +             # Change Background Color
  ylab("Density") +        # Label for Y-Axis
  xlab("Relative Humidity")     # Label for X-Axis

dev.off()




########################################################################################################################################
###q2.
#########################################################################################################################################

###create a data frame "summarystat" with the means of FFMC, DMC, DC, ISI, temp, RH, wind, rain, and area from the CSV. 
##Also included the number of observations N for #each of the four quarters

summarystat=data.frame(ddply(forestfires, c("quarter"), summarise, N = length(quarter), FFMC_avg=round(mean(FFMC),digits=2),DMC_avg=round(mean(DMC),digits=2),DMC_avg=round(mean(DMC),digits=2),
                             DC_avg=round(mean(DC),digits=2),ISI_avg=round(mean(ISI),digits=2),temp_avg=round(mean(temp),digits=2),RH_avg=round(mean(RH),digits=2),wind_avg=round(mean(wind),digits=2),rain_avg=round(mean(rain),digits=2),
                             area_avg=round(mean(area),digits=2)))

## create a rtf document called "output.rtf" and include the newly created data frame "summarystat" as a table in this document.

output.rtf<-RTF("output.rtf", width=10, height=12, font.size=9, omi=c(1,1,1,1))

addTable(output.rtf ,summarystat , font.size=8, row.names=TRUE, NA.string="-")

done(output.rtf)
######################################################################################################################################
##Q3.
#####################################################################################################################################

## create a Pearson correlation matrix for FFMC, DMC, DC, ISI,temp, RH, wind, and area, separately for each of the four quarters (Q1-4)




corr1 = tapply(rownames(forestfires),forestfires$quarter, 
               function(x) cor(forestfires[x,c(5,6,7,8,9,10,11,13)], 
                               method="pearson", use="pairwise.complete.obs"))

##creating plots for differnt seasons and export it to a pdf file with 9*9 resolution.plots are diplayed as 2 rows and 2 columns.

pdf(file="corrplots.pdf", width=9, heigh=9)
par(mfrow =c(2,2))
corrplot.mixed(corr1$Q1,lower="number",upper="circle",
               order="hclust",title="Q1",mar=c(0,0,0,0))
corrplot.mixed(corr1$Q2,lower="number",upper="circle",
               order="hclust",title="Q2",mar=c(0,0,0,0))
corrplot.mixed(corr1$Q3,lower="number",upper="circle",
               order="hclust",title="Q3",mar=c(0,0,0,0))
corrplot.mixed(corr1$Q4,lower="number",upper="circle",
               order="hclust",title="Q4",mar=c(0,0,0,0))

dev.off()

##. Identify and interpret at least three differences across two or more quarters
##Difference between  FFMC and DC across Q1 and Q2.

##Here in Q1 FFMC and DC has a negative relationship this means increase in DC has a negative effect on FFMC which is  -0.10657106.The relationship 
##lies inbetween -1 and +1 in correlations matrix.
##While in Q2 FFMC and DC has a positive relationship this means increase in DC has a positive effect on FFMC which is  0.29328439 .


##Difference between  FFMC and DC across Q3 and Q4.
##FFMC and DC has a negative relationship in Q3 ,this means increase in DC has a negativde effect on FFMC which is  -0.038016255 .In other words increase in the value of DC
##decreases the value of FFMC.
##while in Q4 FFMC and DC has a positive relationship this means increase in DC has a positive effect on FFMC which is 0.8772426.



##Differences betweeen wind and DMC across Q2 and Q3.

##In Q2 relationship between wind and DMC has a negative relationship this means increase in wind  has a negativde effect on DMC  which is  -0.13924588 .
##In other words increase in the value of wind decreases the value of DMC.
##While in  Q3 relationship between wind and DMC has a positive relationship 
##this means increase in wind  also increases the value of  DMC which is  0.05267370.




##############################################################################################################################################
##Q4.
#############################################################################################################################################

##create a scatterplot showing RH and temp and add a blue regression line.
lm.fit=lm(RH~temp, data=forestfires)
with(forestfires, plot(temp, RH))
abline(lm.fit, lwd=3, col="blue")

############################################################################################################################################
##Q5.
###########################################################################################################################################
##create a simple linear regression that regresses RH on temp. 


lm.fit=lm(RH~temp, data=forestfires)

#find the coefficients of the linear regression line.
coef(lm.fit)

##Interpret the coefficients
##Beta 1  -1.482044 is negative and that is the reason the blue line goes down.This means for every degree of celsius increase
##in temperture there is a percentage decrease in relative humidity.

##Beta 0 is  72.282784.This means whether there is a increase or decrease in temperature the standard relative humidity is  always  72.282784.

###R square

summary(lm.fit)$r.squared

###R sqaure is  0.2781406 which means that there is nearly 27 %  chance that response values fall close to the regression line.

#########################################################################################################################################
##Q6
#########################################################################################################################################
## Use function "skewness" of the package "moments" to investigate the distribution of the variable "area"

skewness(forestfires$area)


##The result of the above function is  12.80963,which means the "area" data column of forestfires CSV is positively skewed.In other words the data "area" values are highly 
##ditributed towards its lower end or towards left side and therefore there is a long  tail of data distribution towards right .


##The above asymmetric distribution  can be verified by using a simple boxplot and see the data distribution.
##boxplot(forestfires$area)






