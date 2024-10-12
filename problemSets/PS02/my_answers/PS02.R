##Q1
#import observed data into matrix
observed_val_tab <- matrix( c(14,7,6,7,7,1),2,3 )
#add sum of row and columns to the observed value matrix
observed_val_tab <- addmargins(observed_val_tab)
#calculating the row and column probability 
probability_col <- observed_val_tab[3,]/observed_val_tab[3,4]
probability_row <- observed_val_tab[,4]/observed_val_tab[3,4]
#doing matrix multiplication to calculate probability for each cell
probability_matrix <- matrix(probability_row,3,1)%*%matrix(probability_col,1,4)
#calculate expected value 
expected_value_tab <- probability_matrix*observed_val_tab[3,4]
#calculate the chi square stat
chi_square_stat <- sum((expected_value_tab[1:2,1:3] - observed_val_tab[1:2,1:3])^2/expected_value_tab[1:2,1:3])

#calculate degree of freedom
df_num <- (3-1)*(2-1)
#calculate p-value of test
pchisq(chi_square_stat, df = df_num , lower.tail = FALSE)  

### standardized residual 
#produce matrix of row probability and column probability for each cell 
matrix_row <- matrix(probability_row[1:2],2,3)
matrix_col <- matrix(probability_col[1:3],2,3, byrow = TRUE)
#calculate standardize residual
standardized_residual <- (observed_val_tab[1:2,1:3] - expected_value_tab[1:2,1:3])/(sqrt(expected_value_tab[1:2,1:3]*(1-matrix_row)*(1-matrix_col)))
#add col and row names
rownames(standardized_residual) <- c("Upper class", "Lower class")
colnames(standardized_residual)<- c("Not Stopped", "Bribe requested" , "Stopped/Given warning" )

######Q2
#import data
data <- read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"))
#run regression 
model <- lm(water~reserved, data = data )
#get model result
summary(model)
#plot regression line 
pdf("/Users/tpminh/Documents/GitHub/StatsI_Fall2024/problemSets/PS02/regplot.pdf")
plot(data$reserved,data$water)
abline(model)
dev.off()




