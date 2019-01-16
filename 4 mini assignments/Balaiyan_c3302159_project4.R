##install the following packages and load the libraries.
install.packages("ggplot2")
install.packages("boot")
library(ggplot2)
library(boot)
################################################################################################################################################
#Q1

setwd("C:/INFT6201")
#Read the housing data set.
housing = read.csv("Housing.csv", header=TRUE, sep=",", dec = ".", na.strings ="?")

## perform a multiple linear regression that regresses MEDV on CRIM , RM , NOX , DIS, and AGE
lm.fit <- lm(MEDV~CRIM+RM+NOX+DIS+AGE, data=housing)

##. Interpret the coefficients and report the results of the regression in APA style (including a regression table and reporting of F-values).


#The Beta 1,3,4,5 values  are negative that means for every unit value increase in these values will decrease the Beta o value.
#In oter words increase in values of CRIM,NOX,DIS,AGE will decrease the value of MEDV.
#As RM coefficient is positive that means for every unit value increase Beta 2 value will also increase Beta 0.
#In other words increase in value of RM will incraese the value of MEDV.

##Null hypotheis is Beta 1,Beta 2,Beta 3,Beta 4,Beta 5=0

##F value is 145.3 which is greater than 1,therefore atleast one of the predictor coefficients is strongly correlated to the MEDV.Therefore we
#reject null hypothesis.
#                        MEDV

#Independent      B         SE      t-value  Sig.
#Variables
#CRIM         -0.20808    0.03404  -6.113   <.001
#RM            7.73531    0.39542  19.562   <.001
#NOX          -18.05089    3.94709  -4.573  <.001
#DIS          -1.19104    0.21675  -5.495   <.001
#AGE          -0.06662    0.01514  -4.400   <.001
#Constant     -6.22734    4.01469  -1.551     0.122 
#                 N = 506
 #                R²=.5924



#RESULT:"A multiple linear regression was calculated to predict MEDV  based on 
#CRIM,RM,NOX,DIS and AGE values. A significant regression equation was found (F(3, 506) = 145.3, p < .001), with an R² of .5924.
###########################################################################################################################################
#Q2.

##Use R to create a new factor variable called NOXCAT that categorizes the suburbs into towns with LOW,MEDIUM, and HIGH

##LOW (<= 30% Quantile)- MEDIUM (> 30% Quantile & <= 70% Quantile)- HIGH (> 70% Quantile)

housing$NOXCAT <- factor(ifelse(housing$NOX <= quantile(housing$NOX,0.3) , "LOW",
                                ifelse(housing$NOX <= quantile(housing$NOX,0.7) , "MEDIUM",
                                       "HIGH")),ordered = T, levels = c("LOW","MEDIUM","HIGH"))

##use ggplot to create a boxplot that shows MEDV for the different values of NOXCAT
ggplot(housing[!is.na(housing$NOXCAT),],aes(x=housing$NOXCAT, y=housing$MEDV, fill=housing$NOXCAT))+
  stat_boxplot(geom= 'errorbar')+
  geom_boxplot()+
  theme_bw()+
  labs(title = "MEDV by nox category")+
  xlab("NOX Category")+
  ylab("MEDV")+     ylim(0,50)

######################################################################################################################

#Q.3
##manually create a set of dummy variables,use only MEDIUM level to regress MEDV on NOXCAT(MEDIUM)
housing$dummy.MEDIUM[housing$NOXCAT=="MEDIUM"]<-1
housing$dummy.MEDIUM[housing$NOXCAT!="MEDIUM"]<-0

#Multiple regression is peformed  using newly created dummy variable
lm.fit=lm(MEDV ~ housing$dummy.MEDIUM,data=housing)


#summary() is used to see the coeffiecients values.
summary(lm.fit)
#Call:#
#  lm(formula = MEDV ~ housing$dummy.MEDIUM, data = housing)

#Coefficients:
#  (Intercept)  housing$dummy.MEDIUM  
#22.092                 1.114  

##Interpret the coefficients
##Beta 1   1.114  is positive and this means for every unit increase
##in NOX MEDIUM there is a percentage increase in relative MEDV

##Beta 0 is  22.092 .This means whether there is a increase or decrease in NOX MEDIUM the standard MEDV is  always  22.092.

  
  
  
############################################################################################################################



##Q.4

##Use ggplot() to create a scatterplot of MEDV by LSTAT. Add a linear fit (red), a quadratic fit (green), and a cubic fit (blue) to the plot.


ggplot(housing, aes(x=LSTAT, y=MEDV)) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, size = 1) + # Add a 3 rd order polynomial fit
  stat_smooth(method = "lm", formula = y ~ poly(x, 1),color="red", se = FALSE, size = 1) + # Add a linear fit
  
  stat_smooth(method = "lm", formula = y ~ poly(x, 2),color="green", se = FALSE, size = 1) + # Add a 2 nd order polynomial fit
  
geom_point() +  # Make it a scatterplot
  labs(title = "MEDV of a suburb by LSTST") + # Add a Title
  theme_bw() +                    # Change Background Color
  ylab("LSTAT") +          # Label for Y-Axis
  xlab("MEDV")           # Label for X-Axis



################################################################################################################################


#Q.5

##Use Leave-One-Out Cross-Validation (LOOCV) to compare a linear model, a quadratic model, a cubic
#model, and a quartic model to regress MEDV on LSTAT



# Fit a linear, quadratic, cubic and quartic model
glm.fit1 <- glm(MEDV ~ LSTAT,data=housing)
glm.fit2 <- glm(MEDV ~ poly(LSTAT,2), data=housing)
glm.fit3 <- glm(MEDV ~ poly(LSTAT,3), data=housing)
glm.fit4 <- glm(MEDV ~ poly(LSTAT,4), data=housing)

# Use LOCV to Estimate MSE for each model
cv.err1 <- cv.glm(housing,glm.fit1)$delta
cv.err2 <- cv.glm(housing,glm.fit2)$delta
cv.err3 <- cv.glm(housing,glm.fit3)$delta
cv.err4 <- cv.glm(housing,glm.fit4)$delta

# Have a look at the calculated MSEs
cv.err1[1]
cv.err2[1]
cv.err3[1]
cv.err4[1]

#[1] 38.8901
#[1] 30.73622
#[1] 29.42262
#[1] 28.25187

##Interpret the results based on MSE 
#We see a sharp drop in the estimate test MSE between the linear, quadratic,cubic and quartic model.In other words as the degree of the polynomial increases
#the accuracy of the regression models increases by minimising the MSE.


####################################################################################################################################
#Q.6

##Use 11-fold cross-validation to compare 8 different degrees of polynomials to regress MEDV on LSTAT

system.time({
  cv.error.8 <- rep(0,8)
  for (i in 1:8){
    glm.fit <- glm(MEDV~poly(LSTAT,i),data=housing)
    cv.error.8[i] <- cv.glm(housing,glm.fit,K=11)$delta[1]
  }
  cv.error.8})
cv.error.8
##[1] 39.14112 30.58545 29.36073 28.22127 27.94918 27.64539 27.80427 28.49745
###Why is 11-fold crossvalidation in this particular case advantageous compared to 10-fold cross-validation?

##As the number of obervations in the housing data set is 506,doing a 10 fold cross validation actually regresses 500 of MEDV on LSTAT and 6 of the observation 
##is not been regressed.Therefore 11 fold cross validation regresses  all of the obervations of MEDV on LSAT.



