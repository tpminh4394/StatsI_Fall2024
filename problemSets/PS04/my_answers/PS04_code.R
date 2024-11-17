#Question 1 
install.packages(car)
library(car)
data(Prestige)
help(Prestige)
df <- Prestige
#create new dummy variable 
df$professional <- ifelse(df$type == 'prof',1,0)
#check data 
head(df[c('professional','type' )])
#run regression and check model
model <- lm(prestige ~ income   + professional + income:professional, data = df)
summary(model)

#Question 2
#Calculate the t-value for assigned precinct 
t_1 = (0.042-0)/0.016
#Calculate the p value for assigned precinct 
p_val_1 <- pt(t_1,lower.tail = FALSE,df = 131-3)*2

#Calculate the t-value for adjacent precinct 
t_2 = (0.042-0)/0.013
#Calculate the p value for adjacent precinct 
p_val_2 <- pt(t_2,lower.tail = FALSE,df = 131-3)*2
