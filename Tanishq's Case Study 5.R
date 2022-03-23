# Assignment 5

#1
seg.df <- read.csv("http://goo.gl/qw303p")
tmp.tab <- table(rep(c(1:4), times=c(25,25,25,20)))
tmp.tab
chisq.test(tmp.tab)
# p Value is less than 0.05 so we fail to reject the null hypothesis 
tmp.tab <- table(rep(c(1:4), times=c(25,25,25,10)))
tmp.tab
chisq.test(tmp.tab)
# p Value is less than 0.05 so we reject the null hypothesis 

#2
seg.df <- read.csv("http://goo.gl/qw303p")
seg.df
str(seg.df)
chisq.test(table(seg.df$Segment))
# p Value is less than 0.05 so we reject the null hypothesis 

#3
#3partA
hist(seg.df$income, main="Income Histogram",xlab="Income",ylab="Frequency") 
with(seg.df,hist(income[ownHome=="ownYes"], main="Histogram of Home owners",xlab="Income",ylab="Frequency")) 
with(seg.df,hist(income[ownHome=="ownNo"],main="Histogram of Non-Home owners",xlab="Income",ylab="Frequency"))
#3partB
t.test(income~ownHome,data=seg.df)#p-value = 0.001195 is less than 0.05 so we reject it
#3partC
t.test(income~ownHome,data=seg.df, Segment=="Travelers")
#we fail to reject the hypthesis that mweans income does not differ 

#4 part A
#Null hypothesis: the 2 population means are equal
#Alternative hypothesis: the 2 population means differ
seg.aov.own <- aov(income~ownHome,data=seg.df)
anova(seg.aov.own)
sqrt(10.832)
#Using a significance level of .05, the test is statistically significant
#Decision: Reject the null hypothesis
#Conclude: The sample evidence suggest that the population means differ

#4 part B
# ANNOVA used to compare two or more population means 
# T Means is used to compare only 2 independent groups 
# the results are the same in ANNNOVA and T test 
# the F value is the squared value of the t value from T test
# F value is the test statistic in ANNOVA

#4 part C
#the P value is 0.001195 and P value from ANOVA is 0.001118

#4 part D
table(seg.df$Segment)
seg.aov.seg <- aov(income~Segment,data=seg.df)
anova(seg.aov.seg)
library(lattice)
histogram(~income | Segment, data=seg.df)

