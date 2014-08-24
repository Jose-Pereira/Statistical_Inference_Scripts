#rexp (n, lambda)
#mean = 1/lamba
#SD = 1/lambda
#n = 40
lambda <- 0.2
#De esta forma hago la simulación
m_datos <- NULL
for (i in 1:1000){
  m_datos[i] <-mean(rexp(40, 0.2))
}
#imágen del resultado
hist (m_datos)
#show where the distribution is centered
#As we already know although the values we are using, came from an exponential distribution
# the data we are working with form a sampling distribution of the sample mean
# therefore we know that the center of this distribution must be the mean for the population studied
#as per the values given in the assesment the population mean is lambda
media <- 1/0.2
#now, by the histogram it looks like our sampling distribution has some "normal" features
#as n is a large number we can use the CLT theorem
#by the CLT theorem the mean of the sampling distribution is
m_dist <- mean (m_datos)
#the variance for the exponential pop is
var_exp <- (1/0.2)^2
#the variance of the distribution is 
var_dist <- (var_exp/length (m_datos)) 
#the Standard Error is 
SE_dist <-sqrt(var_exp/length (m_datos))

#http://math.bu.edu/people/nkatenka/MA115_FALL2010/QQPlot.pdf
#as the link states, the qqnorm function gives a set points that are graphed 
#as a straight line with positive slope
qqnorm(m_datos); qqline(m_datos)
#Confidence Interval

#I create a matrix x with A THOUSAND (N = 40) SAMPLES, from the exponential distribution with lambda = 0.2 
# each sample is a collumn of my matrix
#using the function apply i get the mean for each of the samples
x <- matrix(rexp(1000*40, rate = 0.2), 40)
x_mean <- apply(x,2,mean)
x_sd <- apply(x,2,sd)
# ahora debo crear 40 intervalos
ll <- x_mean - qnorm (.975) * (x_sd/sqrt(40)) 
up <- x_mean + qnorm (.975) * (x_sd/sqrt(40))   

lambda <- 0.2
interval <- (ll < 5 & up > 5)
#Proportion of times that the true value of the population mean is between the confidence intervals
sum(interval[interval == TRUE])/length(interval)