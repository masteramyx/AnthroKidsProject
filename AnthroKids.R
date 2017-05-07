data1 <- read.table("anthrokids.csv",header=T,sep=",")
data1 <- na.omit(data1)


newdata <- data.frame(age=seq(from=min(data1$age), to=max(data1$age), length=10))

basic.lm.fit <- lm(mass~age, data=data1)
plot(data1$age, data1$mass, xlab = "Age", ylab = "Mass", main = "Basic Linear Model")
basic.pred <- data.frame(predict(basic.lm.fit, newdata, interval="predict", level=0.90))
lines(newdata$age, basic.pred$fit, col="red", lty=1, lwd=2)
lines(newdata$age, basic.pred$lwr, col="red", lty=2, lwd=2)
lines(newdata$age, basic.pred$upr, col="red", lty=2, lwd=2)




#question1


newdata1 <- data.frame(age=seq(from=min(data1$age), to=max(data1$age), length=10),
                       age.sq.=seq(from=min(data1$age.sq.), to=max(data1$age.sq.), length=10))

quadratic.lm.fit <- lm(mass~age+age.sq., data = data1)
plot(data1$age, data1$mass, main = "Linear & Quadratic Effect", xlab="Age", ylab="Mass")
quadratic.pred <- data.frame(predict(quadratic.lm.fit, newdata1, interval="predict", level=0.90))
lines(newdata1$age, quadratic.pred$fit, col="red", lty=1, lwd=2)
lines(newdata1$age, quadratic.pred$lwr, col="red", lty=2, lwd=2)
lines(newdata1$age, quadratic.pred$upr, col="red", lty=2, lwd=2)



#question2
data1$massLog <- log10(data1$mass)
log.lm.fit <- lm(massLog~age, data=data1)
plot(data1$age, data1$massLog, xlab = "Age", ylab = "Mass Log-Transformed", main = "Log Transformed Model")
log.pred <- data.frame(predict(log.lm.fit, newdata, interval="predict", level=0.90))
lines(newdata1$age, log.pred$fit, col="red", lty=1, lwd=2)
lines(newdata1$age, log.pred$lwr, col="red", lty=2, lwd=2)
lines(newdata1$age, log.pred$upr, col="red", lty=2, lwd=2)






#question3

install.packages("quantreg")
library(quantreg)

                        #LINEAR FOR QUANTILES [0.05, 0.5, 0.95]
#0.05 QUANTILE
Quantile.linear.05 <- rq(mass ~ age, tau = .05, data = data1)
Quantile.linear.05.pred <- predict.rq(Quantile.linear.05,newdata=newdata)
#0.5 QUANTILE
Quantile.linear.5 <- rq(mass ~ age, tau = .5, data = data1)
Quantile.linear.5.pred <- predict.rq(Quantile.linear.5,newdata=newdata)
#0.95 QUANTILE
Quantile.linear.95 <- rq(mass ~ age, tau = .95, data = data1)
Quantile.linear.95.pred <- predict.rq(Quantile.linear.95,newdata=newdata)

#PLOT
plot(data1$age, data1$mass, xlab = "Age", ylab = "Mass", main = "Quantile Linear Effect")

#SUPERIMPOSED
lines(newdata$age, Quantile.linear.05.pred, col="yellow", lty=1, lwd=2)
lines(newdata$age, Quantile.linear.5.pred, col="red", lty=1, lwd=2)
lines(newdata$age, Quantile.linear.95.pred, col="green", lty=1, lwd=2)
legend(x="topleft", legend=c("0.05", "0.5", "0.95"), lty=1,col=c("yellow", "red", "green"))




                        #LINEAR && QUADRATIC FOR QUANTILES [0.05, 0.5, 0.95]


#QUANTILE[0.05]
Quantile.quadratic.05 <- rq(mass ~ age+age.sq., tau = .05, data = data1)
Quantile.quadratic.05.pred <- predict.rq(Quantile.quadratic.05,newdata=newdata1)
#QUANTILE[0.5]
Quantile.quadratic.5 <- rq(mass ~ age+age.sq., tau = .5, data = data1)
Quantile.quadratic.5.pred <- predict.rq(Quantile.quadratic.5,newdata=newdata1)
#QUANTILE[0.95]
Quantile.quadratic.95 <- rq(mass ~ age+age.sq., tau = .95, data = data1)
Quantile.quadratic.95.pred <- predict.rq(Quantile.quadratic.95,newdata=newdata1)

#PLOT
plot(data1$age, data1$mass, xlab = "Age", ylab = "Mass", main = "Quantile Linear & Quadratic Effect")
#SUPERIMPOSED
legend(x="topleft", legend=c("0.05", "0.5", "0.95"), lty=1,col=c("yellow", "red", "green"))
lines(newdata1$age, Quantile.quadratic.05.pred, col="yellow", lty=1, lwd=2)
lines(newdata1$age, Quantile.quadratic.5.pred, col="red", lty=1, lwd=2)
lines(newdata1$age, Quantile.quadratic.95.pred, col="green", lty=1, lwd=2)


                                #QUESTION 4
                                #ONE-MONTH
#SMALLER BANDWIDTH(h) MEANS MORE VARIANCE AND MORE JAGGED LINES
#.05 QUANTILE
nonlinear.05.oneMonth <- lprq(data1$age, data1$mass, 1/12, .05)
#.5 QUANTILE
nonlinear.5.oneMonth <- lprq(data1$age, data1$mass, 1/12, .5)
#.95 QUANTILE
nonlinear.95.oneMonth <- lprq(data1$age, data1$mass, 1/12, .95)
#PLOT
plot(data1$age, data1$mass, xlab = "Age", ylab = "Mass", main = "One Month")
#SUPERIMPOSED
lines(nonlinear.05.oneMonth$xx, nonlinear.05.oneMonth$fv, col="yellow", lty=1, lwd=2)
lines(nonlinear.5.oneMonth$xx, nonlinear.5.oneMonth$fv, col="red", lty=1, lwd=2)
lines(nonlinear.95.oneMonth$xx, nonlinear.95.oneMonth$fv, col="green", lty=1, lwd=2)
legend(x="topleft", legend=c("0.05", "0.5", "0.95"), lty=1,col=c("yellow", "red", "green"))


                                    #6-MONTHS
#.05 QUANTILE
nonlinear.05.sixMonths <- lprq(data1$age, data1$mass, 1/2, .05)
#.5 QUANTILE
nonlinear.5.sixMonths <- lprq(data1$age, data1$mass, 1/2, .5)
#.95 QUANTILE
nonlinear.95.sixMonths <- lprq(data1$age, data1$mass, 1/2, .95)
#PLOT
plot(data1$age, data1$mass, xlab = "Age", ylab = "Mass", main = "Six Months")
#SUPERIMPOSED
lines(nonlinear.05.sixMonths$xx, nonlinear.05.sixMonths$fv, col="yellow", lty=1, lwd=2)
lines(nonlinear.5.sixMonths$xx, nonlinear.5.sixMonths$fv, col="red", lty=1, lwd=2)
lines(nonlinear.95.sixMonths$xx, nonlinear.95.sixMonths$fv, col="green", lty=1, lwd=2)
legend(x="topleft", legend=c("0.05", "0.5", "0.95"), lty=1,col=c("yellow", "red", "green"))



                                      #1 YEAR
#.05 QUANTILE
nonlinear.05.oneYear <- lprq(data1$age, data1$mass, 1, .05)
#.5 QUANTILE
nonlinear.5.oneYear <- lprq(data1$age, data1$mass, 1, .5)
#.95 QUANTILE
nonlinear.95.oneYear <- lprq(data1$age, data1$mass, 1, .95)
#PLOT
plot(data1$age, data1$mass, xlab = "Age", ylab = "Mass", main = "One Year")
#SUPERIMPOSED
lines(nonlinear.05.oneYear$xx, nonlinear.05.oneYear$fv, col="yellow", lty=1, lwd=2)
lines(nonlinear.5.oneYear$xx, nonlinear.5.oneYear$fv, col="red", lty=1, lwd=2)
lines(nonlinear.95.oneYear$xx, nonlinear.95.oneYear$fv, col="green", lty=1, lwd=2)
legend(x="topleft", legend=c("0.05", "0.5", "0.95"), lty=1,col=c("yellow", "red", "green"))

                                  #3 YEARS
#.05 QUANTILE
nonlinear.05.threeYears <- lprq(data1$age, data1$mass, 3, .05)
#.5 QUANTILE
nonlinear.5.threeYears <- lprq(data1$age, data1$mass, 3, .5)
#.95 QUANTILE
nonlinear.95.threeYears <- lprq(data1$age, data1$mass, 3, .95)
#PLOT
plot(data1$age, data1$mass, xlab = "Age", ylab = "Mass", main = "Three Years")
#SUPERIMPOSED
lines(nonlinear.05.threeYears$xx, nonlinear.05.threeYears$fv, col="yellow", lty=1, lwd=2)
lines(nonlinear.5.threeYears$xx, nonlinear.5.threeYears$fv, col="red", lty=1, lwd=2)
lines(nonlinear.95.threeYears$xx, nonlinear.95.threeYears$fv, col="green", lty=1, lwd=2)
legend(x="topleft", legend=c("0.05", "0.5", "0.95"), lty=1,col=c("yellow", "red", "green"))


                                    #QUESTION 5
#LPRQ --> REPRESENTS THE MEDIAN QUANTILE THE BEST, HAS A SLIGHT CONCAVE UP THAT
# FITS THE CURVE NICELY, whereas RQ makes assumption about distribution and applies a 
# "GLOBAL LINEAR FIT" instead of a "LOCAL LINEAR FIT" which is what allows
# the curve to fit nicely


curve.05 <- lprq(data1$age, data1$mass, 3, .05)
curve.1 <- lprq(data1$age, data1$mass, 3, .1)
curve.25 <- lprq(data1$age, data1$mass, 3, .25)
curve.5 <- lprq(data1$age, data1$mass, 3, .5)
curve.75 <- lprq(data1$age, data1$mass, 3, .75)
curve.9 <- lprq(data1$age, data1$mass, 3, .9)
curve.95 <- lprq(data1$age, data1$mass, 3, .95)
curve.list <-data.frame(matrix(nrow=50,ncol=7))
colnames(curve.list) <- c(".05", ".1", ".25", ".5", ".75", ".9", ".95")
curve.list[,1] <- curve.05$fv
curve.list[,2] <- curve.1$fv
curve.list[,3] <- curve.25$fv
curve.list[,4] <- curve.5$fv
curve.list[,5] <- curve.75$fv
curve.list[,6] <- curve.9$fv
curve.list[,7] <- curve.95$fv


plot(data1$age, data1$mass, xlab = "Age", ylab = "Mass", main = "GROWTH CURVES")
cols <- c(1,2,3,4,5,6,7)
for(i in 1:length(cols)){
  lines(curve.05$xx, curve.list[,i], col=i, lty=1, lwd=2)
}

#the list of [curve.n$xx] is the same for every quantile, the function picks the same x-values
#therefore it does not matter which quantile$xx we pick
legend(x="topleft", legend=c(".05", ".1", ".25", ".5", ".75", ".9", ".95"), lty=1,col=c(1,2,3,4,5,6,7))

