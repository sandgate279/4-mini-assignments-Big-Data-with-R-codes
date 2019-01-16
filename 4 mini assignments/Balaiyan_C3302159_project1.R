#####################################################################################################################
##Question 1
#####################################################################################################################
##set the working directory

##setwd()is used to set the working directory

setwd("C:/INFT6201")

##pimadata is a variable that is assigned to represent pimadata.csv file

pimadata = read.csv("pimadata.csv", header=TRUE, sep=",", dec = ".",
                    na.strings ="?")
##nrow function is used to find the number of rows in the file pimadata

numberoflines<-nrow(pimadata)
##ncol function is used to find the number of columns in the file pimadata

numberofcolumns<-ncol(pimadata)

##cat function displays the values of number of rows and columns respectively.
cat(numberofcolumns)
cat(numberoflines)

###################################################################################################################
##Question 2.
###################################################################################################################
##Compute mean of the variable BMI with and without diabetes

#Following code computes mean with diabetes

Mean_BMI_withdiabetes=sum(pimadata$BMI[pimadata$diabetes
                                   == 1])/length(pimadata$BMI[pimadata$diabetes==1])

#Following code computes mean witout diabetes
Mean_BMI_withoutdiabetes=sum(pimadata$BMI[pimadata$diabetes
                                      == 0])/length(pimadata$BMI[pimadata$diabetes==0])

##Display the difference in the mean of the BMI with and without diabetes
cat(Mean_BMI_withdiabetes-Mean_BMI_withoutdiabetes)
##################################################################################################################
#question 3.
##################################################################################################################
##create a subset with pimadata values where TSFT >0 called sort_TSFT

sort_TSFT <- subset(pimadata, pimadata$TSFT>0)

#create a subset with sort_TSFT values where TSFT >0 and Diabetes ==1 called sort_TSFT_withdiabetes


sort_TSFT_withdiabetes <- subset(sort_TSFT, sort_TSFT$diabetes==1)

#Standard deviation of sort_TSFT_withdiabetes for TSFT with Diabetes ==1

sd_TSFT_withdiabetes=sd(sort_TSFT_withdiabetes$TSFT)

#variance of TSFT>0 and diabetes==1

var_TSFT_withdiabetes=var(sort_TSFT_withdiabetes$TSFT)

#Display both SD and Variance
 cat( sd_TSFT_withdiabetes)

 cat(var_TSFT_withdiabetes)


#TSFT means under arm skin thickness.TSFT value 'o' means value has no meaning,because under arm skin thickness has to be greater than 0.


####################################################################################################################################################
#question 4.
#####################################################################################################################################################

#Difference between standard error of the mean and standard deviation is that standard error shows the difference between the
#sample mean and the population mean.Thus with standard error we can figure out the accuracy of the sample to that of the actual 
#set of the data.Whereas standard deviation shows the value's above and below the mean value.In our pimadata the standard deviation of the TSFT >0 with 
#diabetes==1 is 10.32759 and the mean is 33.`Thus + or - of 10.32759 with that of the mean 33 represents the disturbution of the TSFT with 
#diabetes==1 .

######################################################################################################################################################
#question 5.
#####################################################################################################################################################
#Create a new column in the pimadata  with pimadata$age as "pimadata$agecat" which represents the age brackets such as(21 to 35)&(26 to 55) & (56 to 85)
pimadata$agecat <- ifelse(pimadata$age >=21 & pimadata$age<=35, "21 to 35",ifelse(pimadata$age>=36 & pimadata$age<=55,"36 to 55",ifelse(pimadata$age>=56 & pimadata$age<=85,"56 to 85","N")))

#convert the new column pimadata$agecat into a factor using as.factor()
pima_agecat_fact<-as.factor(pimadata$agecat)

#pima data BMI > 0.
Pima_BMI_Gzero<-subset(pimadata,pimadata$BMI>0)

#median of BMI where age >20 & <36
median_21_35<-median(Pima_BMI_Gzero$BMI[Pima_BMI_Gzero$age>="21" & Pima_BMI_Gzero$age<="35"])

#median of BMI where age >35 & <56
median_36_55<-median(Pima_BMI_Gzero$BMI[Pima_BMI_Gzero$age>="36" & Pima_BMI_Gzero$age<="55"])

#median of BMI where age >55 & <86
median_56_85<-median(Pima_BMI_Gzero$BMI[Pima_BMI_Gzero$age>="56" & Pima_BMI_Gzero$age<="85"])

#create a vector with all the  3 medain variables called medianBMI
medianBMI<-c(median_21_35,median_36_55,median_56_85)


#min of 3 medains
min(medianBMI)

#max of 3 medians
max(medianBMI)
#########################################################################################################################################################
#question 6.
#########################################################################################################################################################
#compare the median and display the highest of the median_21_35 and the median_36_55
ifelse(median_21_35>median_36_55,"21 to 35 median is greater than median 36 to 55 ","median 36 to 55 is greater than median 21 to 35")
##########################################################################################################################################################
#question 7
##########################################################################################################################################################
# create a vector say from 1 to 10
x=c(1:10)

#create a function to find the  standard error for x
stderr <- function(x1) sqrt(var(x)/length(x))

#function 1 to find the lower limit of 99% (99%=2.575)Confidence interval of x
fun1<-function(y) (mean(x) - (stderr(x)) * 2.575)

#function 2 to find the upper limit of 99% (99%=2.575)Confidence interval of x
fun2<-function(z) (mean(x) + (stderr(x)) * 2.575)

#function that calculates both lower and upper limit of the 99% confidence interval for variable x
calc99CI<-function(z) c(fun1(),fun2())

#create a vector using  both the intervals

pima_vec<-c(calc99CI())

#99% CI for Pimadata$BMI with pimadata$age 36 to 55
#BMI not equal to zero

BMI_GZero<-subset(pimadata,pimadata$BMI>0)


#Assign BMI > 0 to variable x with age between 36 and 55
x=BMI_GZero$BMI[BMI_GZero$age>="36" & BMI_GZero$age<="55"]

#create a function to find the standard error of the variable x
stderr <- function(x1) sqrt(var(x)/length(x))

#function 1 to find the lower limit of Confidence interval for BMI within the age bracket 36 to 55
fun1<-function(y) (mean(x) - (stderr(x)) * 2.575)

#function 2 to find the upper limit of Confidence interval for BMI within the age  bracket 36 to 55
fun2<-function(z) (mean(x) + (stderr(x)) * 2.575)

#Function that calculates both lower and upper limit of the 99% confidence interval for BMI >0 and within the age  bracket 36 and 55
calc99CI_BMI<-function(z) c(fun1(),fun2())
#Display the 99 % confidence interval for the BMI >0 within the age bracket 36 and 55
cat(calc99CI_BMI())