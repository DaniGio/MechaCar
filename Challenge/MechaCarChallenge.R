library(tidyverse)
MechaCar_table <- read.csv(file='MechaCar_mpg.csv',sep=",", header = T)

# Challenge: Using multiple linear regression, design a linear model that predicts the mpg of MechaCar 
# prototypes using a number of variables within the MechaCar mpg dataset.
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table) #generate multiple linear regression model
lm(mpg ~ vehicle.length,data=MechaCar_table) #generate single linear regression model
lm(mpg ~ ground.clearance,data=MechaCar_table) #generate single linear regression model


summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table)) #generate summary statistics
summary(lm(mpg ~ vehicle.length,data=MechaCar_table)) #generate summary statistics for vehicle.length
summary(lm(mpg ~ ground.clearance,data=MechaCar_table)) #generate summary statistics for ground.clearance

cor(MechaCar_table$mpg,MechaCar_table$vehicle.length) #calculate correlation coefficient

# Challenge part 2: create a summary statistics table for the suspension 
# coil’s pounds-per-inch continuous variable.

Suspension_table <- read.csv(file='Suspension_Coil.csv',sep=",", header = T)
Suspension_table %>%
  group_by(Manufacturing_Lot) %>%
  summarise(PSI_mean= mean(PSI), PSI_sd = sd(PSI), PSI_median = median(PSI), PSI_variance = var(PSI))

sample_table <- Suspension_table %>% sample_n(50) #generate 50 randomly sampled data points
sample_table2 <- Suspension_table %>% sample_n(50) #generate another 50 randomly sampled data points


t.test(sample_table$PSI,sample_table2$PSI) #compare means of two samples

#challenge_part 3: determine if the suspension coil’s pound-per-inch results are statistically different from the mean 
#population results of 1,500 pounds per inch.

t.test(sample_table$PSI, mu = 1500)
t.test(sample_table2$PSI, mu = 1500)

t.test(subset(Suspension_table,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

?mpg()
?mtcars()

head(mtcars)
head(mpg)


#challenge_part 4: quantify how the MechaCar outperforms the competition

lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars) #generate multiple linear regression model
lm(mpg ~ cyl,data=mtcars) #generate single linear regression model

summary(lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars)) #generate multiple linear regression model
summary(lm(mpg ~ cyl,data=mtcars)) #generate single linear regression model




mtcars %>%
  summarise(cyl_mean= mean(cyl), cyl_sd = sd(cyl), cyl_median = median(cyl), cyl_variance = var(cyl))

sample_table_mtcars <- mtcars %>% sample_n(16) #generate 16 randomly sampled data points
t.test(sample_table_mtcars$cyl, mu = 6)
