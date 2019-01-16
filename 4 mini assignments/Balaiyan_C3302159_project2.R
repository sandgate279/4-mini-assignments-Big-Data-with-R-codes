############################################################################
##Install the following packages to work on this assignment
 install.packages("ggplot2")
 install.packages("RColorBrewer")
 install.packages("wesanderson")
 install.packages("plyr")

# Load the following library files
library(ggplot2)##To create plots such as Box,Violin,scatter in this assignment.
library(RColorBrewer) # A color scale package
library(wesanderson)  # Another color scale package
library(plyr)        #To use ddply function
###########################################################################################################
 # Set Working Directory and Load CSV datafile
setwd("C:/INFT6201")
moviedata = read.csv("moviedata.csv", header=TRUE, sep=",", dec = ".", na.strings ="NA")

#############################################################################################################
###question 1
#############################################################################################################

##create a factor for the year in the movie data with labels such as Y2014,Y2015.
moviedata$year <- factor(moviedata$year, levels = c(2014, 2015),
                         labels = c("Y2014", "Y2015"))




#create a box plot with the screens value and the factored year value.
ggplot(moviedata[!is.na(moviedata$screens),],aes(x=year, y=screens, fill=year))+
  stat_boxplot(geom= 'errorbar')+
  geom_boxplot()+
  theme_bw()+
  labs(title = "movie screens by the year")+
  xlab("year")+
  ylab("screens")+     ylim(0,5000)

##################################################################################################################
#question 2
##################################################################################################################

##options function is to  to print numerical data on the y axis rather than exponential numbers in the following violin plot.
options(scipen=999)

##calculate profit from gross and budget from the CSV.

moviedata$profit = moviedata$gross-moviedata$budget

#create a factor level labels for the sequel data column in the CSV file.
moviedata$sequelcat <- factor(moviedata$dummy_sequel, levels = c(0, 1),
                              labels = c("ORIGINAL", "SEQUEL"))

#create a violin plot with that of the profit and sequel labels.
ggplot(moviedata[!is.na(moviedata$profit),], aes(x = sequelcat, y = profit,  fill = sequelcat)) + 
  geom_violin() +# Make it a Violin Plot
  theme_bw() +   # Change Background Color
  labs(title = "Profit for ORIGINAL and SEQUEL movies") + # Add a Title     
  xlab("Movie category")+ #label for X=axis 
  ylab("Profit(=Gross-Budget)in USD") + # Label for Y-Axis
  guides(fill=FALSE) +# Remove the legend
  scale_fill_brewer(palette="YlOrRd")+ # Change Fill Color to Yellow orange and red from  RColorBrewer library
  geom_boxplot(width=0.2)+
  stat_summary(fun.y = mean, geom = "point",
               shape = 18, size = 2.5, color = "#FC4E07")  # Add a Box Plot on Top with red color on the mean.
######################################################################################################################

##question 3
######################################################################################################################

#create a subset from the moviedata CSV without NA values from budget,screens and aggregate_followers column data.
moviedatasub<-subset(moviedata, !is.na(moviedata$budget&moviedata$screens&moviedata$aggregate_followers))

#winsor function to winsorize the high values of the likes column in the newly created subset called moviedatasub.

winsor <- function(x, multiplier) {
  if(length(multiplier) != 1 || multiplier <= 0) {
    stop("bad value for 'multiplier'")}
  
  quartile1 = summary(x)[2] # Calculate lower quartile
  quartile3 = summary(x)[5] # Calculate upper quartile
  iqrange = IQR(x) # Calculate interquartile range
  
  y <- x
  boundary1 = quartile1 - (iqrange * multiplier)
  boundary2 = quartile3 + (iqrange * multiplier)
  
  y[ y < boundary1 ] <- boundary1
  y[ y > boundary2 ] <- boundary2
  
  y
}

#call the function winsor with input as likes from moviedatasub and assign the output to a  variable likes_winsor.
moviedatasub <- within(moviedatasub, {
  likes_winsor <- winsor(moviedatasub$likes, 1.5)
})

##create box plots before and after winsorising to check the outliers.

with(moviedatasub, boxplot(likes))##Box plot before winsorising
with(moviedatasub, boxplot(likes_winsor))##Box plot after winsorising
#####################################################################################################################

##Question.4
######################################################################################################################

#Use cut function to label the ratings column with < 6 as "negative",< 6.8 as "neutral" and all greater values than that as "positive".

moviedatasub$ratingscat <- with(moviedatasub, cut(ratings, breaks = c(-Inf, 5.9, 6.7, Inf), labels = c("negative", "neutral", "positive")))

##use scatter ggplot to see the distribution of the ratings across winsorized likes with that of the gross income.
##FantasticFox1 color pallete is used from wesanderson library package to distinguish among the ratings.

ggplot(moviedatasub, aes(x=likes_winsor, y = gross, 
                         color=factor(ratingscat))) +
  geom_point(size=2.5) + # Make it a scatterplot 
  labs(title = "gross income by likes") + # Add a Title 
  theme_bw() +                    # Change Background Color
  ylab("Gross income in USD") +          # Label for Y-Axis
  xlab("Number of likes") + # Label for X-Axis
  scale_color_manual(values=wes_palette("FantasticFox1"),  # Use FantasticFox color from wesanderson
                     name="Ratingscat") # Set the title of the legend
######################################################################################################################
##Question.5
######################################################################################################################

##Make a data frame to show mean and SD for budget,gross and profit across the 3 different ratings and for the 2 different sequels.
ddply(moviedatasub, c("ratingscat", "sequelcat"), summarise, N = length(ratingscat), profit_avg=mean(profit),
      profit_sd=sd(profit), gross_avg=mean(gross), gross_sd=sd(gross),budget_avg=mean(budget),budget_sd=sd(budget))



######################################################################################################################
######################################################################################################################
##Question .6

#Test the varianve homogenity for profit across the 3 ratings.

##Bartlett's test 
bartlett.test(profit ~ ratingscat, data=moviedatasub)

##As the P value is 0.00000009763 < 0.05 ,therefore variances are not  homogenous and we should reject the null hypothesis.
##This means that there is evidence that variance in moviedatasub in terms of profit is different for the three ratingscat.

##one-way Analysis of Variance (ANOVA) to test whether there is a difference in mean profit across the three different ratings categories

aovResult = aov(profit ~ ratingscat, data=moviedatasub)
 summary(aovResult)
 
 
 ##As the p-value (0.00102)is less than the significance level 0.05, 
 ##we can conclude that there is significant differences between the mean of profit across the three ratingscat.
 ##In one-way ANOVA test, a significant p-value indicates that some of the group means are different.
 ##Also the F-statistic( 7.154) is more than "one" that means there is significant difference between the means.
 #But we don't know which pairs of groups are different.
 ##PostHoc analysis is to determine which groups are significantly different from each other.
 ##Conduct Pairwise t-Tests with Bonferroni Adjustments rather than Tukey HSD test because the later test works only if the variance are  homogenous.
 
 ##Conduct Pairwise t-Tests with Bonferroni Adjustments 
 with(moviedatasub, pairwise.t.test(profit,ratingscat, p.adjust="bonferroni", pool.sd=FALSE))
 

 
 ##p value is 0.036<0.05 (for the pairs positive and neutral),which means there is a significant difference between the means of profit across positive and neutral group.
 ##p value is 0.002<0.05 (for the pairs positive and negative),which means there is a significant difference between the means of profit across positive and negative group.
 ##p value is 0.585>0.05 (for the pairs neutral and negative),which means there is no significant difference between the means of profit across neutral and negative group.
 
 ##Therefore the variances are different between the above mentioned groups except for the neutral and negative group with that of the profit data.
 
 
 
##########################################################################################################################
##########################################################################################################################

##question 7
 ##We conduct t-test to determine whether means of profit across the sequelcat are equal,
## with the assumptions that the data is distributed normally and homogenous variances across the sequel cat.
 
 with(moviedatasub, t.test(profit[ sequelcat=="ORIGINAL"], profit[sequelcat=="SEQUEL"], var.equal=TRUE))
 
 
 ##t value is -3.2646,which measures the difference in means  for both sequel cat in terms of standard error.
 #So therefore the means are different for both sequal cat with that of the profit.
 ##p value is  0.001306 ,which means the probabaility of making mistake in calculating the mean value of -3.2646 is approximately one in thousand times,which is significantly low.
