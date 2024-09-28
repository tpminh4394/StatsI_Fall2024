#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################
##sect 1
#Load data
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#Calculate lower and upper bound of confidence interval 
sampling_mean <- mean(y)
sampling_sd <- sd(y)/sqrt(25) 
t_value <- qt((1-0.9)/2 ,lower.tail = FALSE,df = 25 -1 )
CI <- c(sampling_mean - t_value*sampling_sd,sampling_mean + t_value*sampling_sd )

##sect 2
sample_mean <- mean(y)

#calculate t-statistic
t = (sample_mean - 100)/(sd(y)/sqrt(25))

#calculae p-val
pt(t,df = 24,lower.tail = FALSE)

#p values = 0.721 => not enough evidence to reject null hypothesis at 5% significant level 







#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
##section 1
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
# scatter plot of X1,X2,X3,Y pairwise
pdf("/Users/tpminh/Desktop/trinity asds/stat analysis 1/ps1/pairplot.pdf")
pairs(expenditure[2:5])
dev.off()

#Y and X1 are positively correlated 
#Y and X2 does not seems to be correlated 
#Y and X3 are positively correlated
#X1 and X2 seem uncorrelated 
#X1 and X3 are positively correlated 
#X2 and X3 are uncorrelated

##section 2
pdf("/Users/tpminh/Desktop/trinity asds/stat analysis 1/ps1/boxplot.pdf")
boxplot(expenditure$Y ~ expenditure$Region)
dev.off()
#find avg of each region 
aggregate(list(avg_exp = expenditure$Y), list(Region = expenditure$Region), FUN=mean)

##section 3
pdf("/Users/tpminh/Desktop/trinity asds/stat analysis 1/ps1/scatter1.pdf")
plot( expenditure$X1,expenditure$Y)
dev.off()
#change colour and symbol
pdf("/Users/tpminh/Desktop/trinity asds/stat analysis 1/ps1/scatter2.pdf")
plot( expenditure$X1,expenditure$Y, col = expenditure$Region, pch = expenditure$Region)


