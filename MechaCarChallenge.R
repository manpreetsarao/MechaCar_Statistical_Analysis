library(dplyr)
de_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=de_table) #generate multiple linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=de_table)) #generate summary statistics
de_table2 <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
total_summary <- de_table2 %>%  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') #create summary table with multiple columns
lot_summary <- de_table2 %>% group_by(Manufacturing_Lot) %>%summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')

# Perform a t-test on PSI across all manufacturing lots against the population mean of 1,500
t.test(de_table2$PSI, mu=1500)

# Perform a t-test on PSI for each manufacturing lot individually against the population mean of 1,500
t.test(subset(de_table2, Manufacturing_Lot=='Lot1', PSI), mu=1500)
t.test(subset(de_table2, Manufacturing_Lot=='Lot2', PSI), mu=1500)
t.test(subset(de_table2, Manufacturing_Lot=='Lot3', PSI), mu=1500)

