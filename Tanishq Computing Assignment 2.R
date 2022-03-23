x <- c(2, 4, 6, 8)
x
install.packages(c("lavaan", "semPlot", "corrplot", "multcomp"))
satData <- read.csv("http://goo.gl/UDv12g")
satData$Segment <- factor(satData$Segment)
head(satData)
summary(satData)
library(corrplot)
corrplot.mixed(cor(satData[ , -3]))
corrplot.mixed(cor(satData[ , -3]))
aggregate(iProdSAT~Segment,satData,mean)
aggregate(iProdSAT∼Segment,satData,mean)
sat.anova=aov(iProdSAT~-1+Segment,satData)
summary(sat.anova)
library(multcomp)
par(mar=c(4,8,4,2))
plot(glht(sat.anova))
satModel <- "SAT =~iProdSAT + iSalesSAT
REC =~iProdREC + iSalesREC
REC~SAT"
library(lavaan)
sat.fit <- cfa (satModel, data=satData)
summary(sat.fit, fit.m=TRUE)
library(semPlot)
semPaths(sat.fit,what="est",residuals=FALSE,intercepts=FALSE,nCharnodes=9)
x
X
x<-c(2,4,6,8) #start a cheer
# start a cheer
x <- c(2, 4, 6, 8)
xNum <- c(1, 3.14159, 5, 7)
xLog <- c(TRUE,FALSE,TRUE,TRUE)
xChar <- c("foo", "bar", "boo", "far")
xMix <- c(1, TRUE, 3, "Hello, world!")
xNum
x2 <- c(x, x)
x2
summary(xNum)
summary(xChar)
xNum[2]
x2 + 1
x2 * pi
(x+cos(0.5)) * x2
length(x)
length(x2)
c(1, 2, 3.5)
xMix
xNum[1]
xMix[1]
xNum[1] + 1
xMix[1] + 1
as.numeric(xMix[1])+1
str(xNum)
str(xChar)
str(xMix)
?as.numeric
?"?"
xSeq <- 1:10
xSeq
1:5*2
1:(5*2)
xNum
xNum[2:4]
myStart <- 2
xNum[myStart:sqrt(myStart+7)]
seq(from=-5, to=28, by=4)
rep(c(1,2,3), each=3)
rep(seq(from=-3, to=13, by=4), c(1, 2, 3, 2, 1))
xSeq
xSeq[-5:-7]
1:300
1001:1300
xNum[2:4]
xSub <- xNum[2:4]
xSub
xNum
xNum[c(FALSE, TRUE, TRUE, TRUE)]
xNum > 3
xNum[xNum > 3]
my.test.scores <- c(91, NA, NA)
max(my.test.scores)
mean(my.test.scores, na.rm=TRUE)
max(my.test.scores, na.rm=TRUE)
mean(na.omit(my.test.scores))
is.na(my.test.scores)
my.test.scores[!is.na(my.test.scores)]
my.test.scores <- c(91, -999, -999)
mean(my.test.scores)
my.test.scores[my.test.scores < -900] <- NA
mean(my.test.scores, na.rm=TRUE)
log(c(-1, 0, 1))
str(xNum)
str(xChar)
xList <- list(xNum, xChar)
xList
str(xList)
summary(xList[[1]])
lapply(xList  ,summary)
xList <- list(xNum, xChar)
names(xList) <- c("itemnum", "itemchar")
xList <- list(itemnum=xNum, itemchar=xChar)
names(xList)
xList[[1]]
xList$itemnum
xList[["itemnum"]]
x.df <- data.frame(xNum, xLog, xChar)
x.df[2, 1]
x.df[1, 3]
x.df <- data.frame(xNum, xLog, xChar, stringsAsFactors=FALSE)
x.df
x.df[1,3]
x.df[2,]
x.df[,3]
x.df[2:3, ]
x.df[ , 1:2]
x.df[-3, ]
x.df[ , -2] # omit the second column
str(x.df[2, 1])
str(x.df[ , 2])
str(x.df[c(1, 3), ])
x.df$xNum
rm(list=ls())
store.num <-factor(c(3,14,21,32,54))
store.rev <- c(543,654,345,678,234)
store.visits <- c(45,78,32,56,34)
store.manager <- c("Annie","Bert","Carla","Dave","Ella")
(store.df = data.frame(store.num,store.rev,store.visits,store.manager,stringsAsFactors=F))
store.df$store.manager
mean(store.df$store.rev)
cor(store.df$store.rev, store.df$store.visits)
summary(store.df)
save(store.df, file="store-df-backup.RData")
rm(store.df)
load("store-df-backup.RData")
mean(store.df$store.rev)
save(list=c("store.df","store.visits"), file="store-df-backup.RData")
store.df <- 5
store.df
load("store-df-backup.RData")
store.df
save(store.df,file="C:\\Documents and Settings\\user\\My Documents\\R\\
store-df-backup.RData")
getwd()
setwd("∼/Documents/R")
getwd()
save.image()
save.image("mywork.RData")
load("mywork.RData")
list.files()
file.remove("mywork.RData","store-df-backup.RData")
write.csv(store.df, file="store-df.csv", row.names=FALSE)
read.csv("store-df.csv")
store.df2 <- read.csv("store-df.csv", stringsAsFactors=FALSE)
store.df2$store.num <- factor(store.df2$store.num)
store.df == store.df2
all.equal(store.df, store.df2)
rm(store.df2)
se <- function(x) { sd(x) / sqrt(length(x)) }          
se(store.df$store.visits)
mean(store.df$store.visits) + 1.96 * se(store.df$store.visits)
se(store.df$store.manager)
se <- function(x) {
  # computes standard error of the mean
  tmp.sd <- sd(x) # standard deviation
  tmp.N <- length(x) # sample size
  tmp.se <- tmp.sd / sqrt(tmp.N) # std error of the mean return(tmp.se)
}
x <- -2:2
log(x)
ifelse(x > 0, x, NA)
log(ifelse(x > 0, x, NA))
my.data <- matrix(runif(100), ncol=5)
apply(my.data, 2, median) / 2
halfmedian <- function (x) { median(x) / 2 }
apply(my.data, 2, halfmedian)
apply(my.data, 2, function(x) { median(x) / 2 } )
ls()
rm(store.num)
rm(list=c("store.rev", "store.visits"))
rm(list=ls(pattern="store"))
rm(list=ls())
