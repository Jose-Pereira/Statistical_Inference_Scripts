#NOW FOR THE TOOTH PART
Tooth <- ToothGrowth
#As per the documentation we have:
#Description: The response is the length of odontoblasts (teeth) in each of 10 guinea pigs at each of three dose levels of Vitamin C (0.5, 1, and 2 mg) with each of two delivery methods (orange juice or ascorbic acid).
#FORMAT : A data frame with 60 observations on 3 variables.
#len  numeric  Tooth length
#supp  factor	Supplement type (VC or OJ).
#dose  numeric	Dose in milligrams

#There seems to be a mild positive relation between dose size and tooth len
#Also it seems as if on lower dosages the VC has a stronger relationship with tooth length that on the max dose available
plot (Tooth$len, Tooth$dose, main = "Tooth Length by Dose Size", xlab = "Tooth Length", ylab = "Dose Size", col=c("red","blue")[Tooth$supp]) 
legend("topleft",c("OJ","VC"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))
#lets see if there is a difference between supp
boxplot(len~supp, data = Tooth, main = "Data points by Supplement type", xlab = "Supplement type", ylab = "Tooth Length", col=(c("red","blue")))
#Summarizing the data
#As we can see below, the ToothGrowth dataset contains, 60 observations of 03 variables: len, supp, and dose
str (Tooth)
#len and dose, are numeric variables while supp is a factor variable
# we have exactly 30 readings for each Supplement type
#there are no missing values in the dataset
sum(is.na (Tooth))
# And we have the same number of observations per dose size
sum (Tooth$dose == 0.5)
sum (Tooth$dose == 1 )
sum (Tooth$dose == 2 )
# Below we can look at a summary of the values
summary (Tooth)
#if we aggregate the data by Tooth length and dose size, it looks like there is a positive relationship between those factors
aggregate(Tooth$len ~ Tooth$dose, FUN = mean)
# Also it looks like the Orange Juice could have a more significant relation with the Toothlength than the Vitamin C 
aggregate(Tooth$len ~ Tooth$supp, FUN = mean)


#Confidence Intervals
#As per the documentation we can infere that only 10 subjects took part on the experiment, 
#and on each a total of 6 different tests were performed. This is important since it means
# that we should use a paired t-test. Since we are going to compare the same subject response to different stimulus
# Test One is there a difference between the mean tooth length by supp administrated?
OJ <- subset(Tooth, Tooth$supp == "OJ", select = 1)
VC <- subset(Tooth, Tooth$supp == "VC", select = 1)
t.test(OJ$len,VC$len, paired=TRUE)
# As showed by the t-test, the p-value is less than 0,05 therefore we reject the null hypothesis: "the difference in means is zero"
#Per the intervals (values larger than zero) it looks like supplement via OJ is more effective than via VC 

#Now we are going to test for different in supplementation method when dose size is the same 
OJ_5 <- subset(Tooth, Tooth$supp == "OJ" & Tooth$dose == 0.5 , select = 1)
VC_5 <- subset(Tooth, Tooth$supp == "VC" & Tooth$dose == 0.5, select = 1)
t.test(OJ_5$len,VC_5$len, paired=TRUE)
#We Reject the null hypothesis : "the difference in means is zero when dose is  = 0,5"

OJ_1 <- subset(Tooth, Tooth$supp == "OJ" & Tooth$dose == 1 , select = 1)
VC_1 <- subset(Tooth, Tooth$supp == "VC" & Tooth$dose == 1, select = 1)
t.test(OJ_1$len,VC_1$len, paired=TRUE)
#We Reject the null hypothesis : "the difference in means is zero when dose is  = 1"

OJ_2 <- subset(Tooth, Tooth$supp == "OJ" & Tooth$dose == 2 , select = 1)
VC_2 <- subset(Tooth, Tooth$supp == "VC" & Tooth$dose == 2, select = 1)
t.test(OJ_2$len,VC_2$len, paired=TRUE)
#We FAIL to reject the null hypothesis : "the difference in means is zero when dose is  = 2"

#Finally, i would like to see if the dose size has any effect on len length regardless of supp method

supp_5 <- subset(Tooth, Tooth$dose == 0.5 , select = 1)
supp_1 <- subset(Tooth, Tooth$dose == 1 , select = 1)
supp_2 <- subset(Tooth, Tooth$dose == 2 , select = 1)

t.test(supp_2$len,supp_5$len, paired=TRUE)
#We reject the Null hypothesis : "the difference in means is zero when one group receives supp with dose = 2 and the other with dose = 0.5"
t.test(supp_2$len,supp_1$len, paired=TRUE)
#We reject the Null hypothesis : "the difference in means is zero when one group receives supp with dose = 2 and the other with dose = 1"
t.test(supp_1$len,supp_5$len, paired=TRUE)
#We reject the Null hypothesis : "the difference in means is zero when one group receives supp with dose = 1 and the other with dose = 0.5"

png("pruebaSI.png")
par (mfrow =c (1,2) )  
plot (Tooth$len, Tooth$dose, main = "Tooth Length by Dose Size", xlab = "Tooth Length", ylab = "Dose Size", col=c("red","blue")[Tooth$supp]) 
legend("topleft",c("OJ","VC"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))
boxplot(len~supp, data = Tooth, main = "Data points by Supplement type", xlab = "Supplement type", ylab = "Tooth Length", col=(c("red","blue")))
dev.off()
