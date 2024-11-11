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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

#Q1
#regression between voteshare and difflog 
model_q1 <- lm(voteshare ~ difflog, data=inc.sub)
summary(model_q1)
#scatterplot and regression line
pdf("/Users/tpminh/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/q1plot.pdf")
plot(inc.sub$difflog,inc.sub$voteshare)
abline(model_q1)
dev.off()
#save model resid
head(model_q1$residuals)
model_resid_votes_difflog <- model_q1$residuals

#Q2
#regression between voteshare and difflog 
model_q2 <- lm(presvote ~ difflog, data=inc.sub)
summary(model_q2)
#scatterplot and regression line
pdf("/Users/tpminh/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/q2plot.pdf")
plot(inc.sub$difflog,inc.sub$presvote)
abline(model_q2)
dev.off()
#save model resid
head(model_q2$residuals)
model_resid_presv_difflog <- model_q2$residuals


#Q3
#regression between voteshare and difflog 
model_q3 <- lm(voteshare ~ presvote, data=inc.sub)
summary(model_q3)
#scatterplot and regression line
pdf("/Users/tpminh/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/q3plot.pdf")
plot(inc.sub$presvote,inc.sub$voteshare)
abline(model_q3)
dev.off()

#Q4
#combine resid q1 q2 data
df <- data.frame(r_votes_difflog  = model_resid_votes_difflog, r_presv_difflog = model_resid_presv_difflog  )
#regression between resid Q1 and resid Q2 
model_q4 <- lm(r_votes_difflog ~ r_presv_difflog, data = df)
summary(model_q4)
#scatterplot and regression line
pdf("/Users/tpminh/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/q4plot.pdf")
plot(df$r_presv_difflog,df$r_votes_difflog)
abline(model_q4)
dev.off()

#Q5
model_q5 <- lm(voteshare ~ presvote + difflog, data = inc.sub )
summary(model_q5)


