sat.df=read.csv("http://goo.gl/HKnl74")
str(sat.df)
sat.df$weekend=factor(sat.df$weekend,levels=c("no","yes"),labels=c(0,1))
sat.df$weekend=as.numeric(as.character(sat.df$weekend))
str(sat.df)
sat.df$weekend=as.numeric(sat.df$weekend) #we don't need as.character to convert it into numeric
str(sat.df)
hist(sat.df$overall)
hist(sat.df$overall, main="Histogram Overall", xlab="Overall Satisfaction", ylab="Frequency", col="lightgreen")
#the Histogram appears to be normally distributed 
library(lattice)
par(mfrow=c(2,2))
sat.df$weekend
graphics.off() #to troubleshoot the error with the graphs
par(mfrow=c(2,2)) #not working due to some error in the screen. not gonna get mad. doing the graphs one by one.
graphics.off()
hist(sat.df$weekend)
hist(sat.df$num.child)
hist(sat.df$distance)
hist(sat.df$rides)
hist(sat.df$games)
hist(sat.df$wait)
hist(sat.df$clean)
hist(sat.df$overall)
#The histograms for weekend, number of children and distance are not normally distributed
#Why are we doing this as the above three variables are all objective measures and categorical variables
#The histograms for rides, games, wait, clean, and overall
#These are all subjective variables of satisfaction and they are all normally distributed
library(corrplot)
corrplot(cor(sat.df),method='ellipse',type='upper')
corrplot(cor(sat.df),method='number',type='upper')
#There appears to be a strong correlation between the satisfaction variables for clean and rides
#The correlation coefficient value is 0.79 which is the highest amongst all the satisfaction variables
lm(overall~clean,data=sat.df)
model_overall_clean=lm(overall~clean,data=sat.df)
plot(model_overall_clean)
boxplot(overall~clean,data=sat.df)
#this is a linear model to predict overall satisfaction using clean as a predictor
summary(model_overall_clean)
#t value is used to test the null hypothesis
#standard error is sampling variability
#the greater the error the greater the variability
#here in the summary, the standard error for clean is 0.107
#the standard error for overall is 9.417. So overall has greater standard error
#Thus we can say that clean has lesser sampling variability and so it is assumed to be more approximately normal
#Also in the box plot, there are more statistical outliers in the overall satisfaction variable when compared to the cleanliness satisfaction variable
#Hence we can say with some degree of confidence that clean has lesser sampling variability
sqrt(0.4088)
#we take the square root of the Multiple R-squared
#Correlation Co-efficient=sqrt(0.4088)=0.639
#The Correlation Co-efficient is between 0.50 and 0.70
#Hence, there is moderate correlation between overall and cleanliness satisfaction variables
abline(model_overall_clean)
abline(lm(overall~clean,data=sat.df))
library(coefplot)
#we did linear regression for overall and clean
#now we are doing multiple regression for overall, clean, wait, and games
model_overall_clean_wait_games=lm(overall~clean+wait+games, data=sat.df)
summary(model_overall_clean_wait_games)
coefplot(model_overall_clean_wait_games, intercept=FALSE, outerCI=1.96,lwdOuter=1.5,ylab="Rating of Variable Association", xlab="Relation with Overall Satisfaction")
library(coefplot)
coefplot(model_overall_clean_wait_games,intercept=FALSE,outerCI=1.96,lwdOuter=1.5,ylab="Rating of Three Variables Association",xlab="Relation with Overall Satisfaction")
coefplot::coefplot(model_overall_clean_wait_games,intercept=FALSE,outerCI=1.96,lwdOuter=1.5,ylab="Rating of Three Variables Association",xlab="Relation with Overall Satisfaction")
#the coefficient plot clearly shows that clean is the biggest driver of overall satisfaction
#wait and games are the second and third biggest drivers of overall satisfaction
#In order to check this further, we calculate the Correlation Coefficient by taking the square root of the Multiple R Squared Value 
sqrt(0.5463)
#The Correlation Coefficient value from the Multiple Regression is 0.739
#To analyze the impact of clean, wait, and games on overall satisfation, we compare this value with the Correlation Coefficient from the Linear Regression we did earlier
#The Correlation Coefficient for the Linear Regression was 0.639
#So we can determine that there is not much of an impact for the association of wait and games on the overall satisfaction as much as clean has on satisfaction
#Therefore, we can say with some degree of confidence that cleanliness is the biggest driver of overall satisfaction
model_overall_clean_wait_games_rides=lm(overall~clean+wait+games+rides,data=sat.df)
summary(model_overall_clean_wait_games_rides)
sqrt(0.5586)
coefplot::coefplot(model_overall_clean_wait_games_rides,intercept=FALSE,outerCI=1.96,lwdOuter=1.5,ylab="Rating of Four Variable Association",xlab="Relation with Overall Satisfaction")
#when we look at this Coefficient Plot, we take a look at the mean of the four satisfaction variables to determine which has the biggest correlation with overall satisfaction
#We notice that rides is a bigger driver of overall satisfaction than games but wait has a bigger impact on overall satisfaction than rides
#When we take a look at the Correlation Coefficient for this model and compare it to the Correlation Coefficient for the previous model, there is just a slight increase in the value. So the addition of rides makes it only slightly statistically significant.
#So in terms of the drivers of satisfaction, cleanliness has the biggest correlation with overall satisfaction 
#Wait and rides may be considered as the second and third biggest drivers of overall satisfaction
#If I were to make one recommendation, I would state that cleanliness has the biggest correlation with overall satisfaction and the other three satisfaction variables are not as big drivers of overall satisfaction
anova(model_overall_clean_wait_games_rides,model_overall_clean_wait_games)
anova(model_overall_clean_wait_games,model_overall_clean_wait_games_rides)
#To further compare the two models, I run the Anova Test. I can state that we reject the null hypothesis because the p-value is less than 0.05. And even though there is not much of a difference between the Correlation Coefficient values for the last two multiple regression models, the Anova Test tells us that the two models are slightly statistically different.
