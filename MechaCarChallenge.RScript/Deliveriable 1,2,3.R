library(dplyr)

#### DELIVERABLE 1 ####

# Import data
mecha_car_mpg <- read.csv(file='MechaCar_mpg.csv',check.names = F, stringsAsFactors = F)

# Perform linear regression to predict MPG using lm() function
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mecha_car_mpg) 

# Use summary() function to determine p-value and r-squared value
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mecha_car_mpg)) 



#### DELIVERABLE 2 ####

# Import data
susp_coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Create summary data frame
total_summary <- susp_coil %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

# Create summary data frame group by Manufacturing Lot
lot_summary <- susp_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI),Variance = var(PSI), SD = sd(PSI))



#### DELIVERABLE 3 ####


# Use t.test() to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 PSI
t.test(susp_coil$PSI, mu=1500) 

# Use t.test() 3 additional times with subset() to determine if PSI for each manufacturing lot is statistically different from the pop. mean of 1,500 PSI

#Lot 1
t.test(subset(susp_coil, Manufacturing_Lot=="Lot1")$PSI, mu=1500) 
# Lot 2
t.test(subset(susp_coil, Manufacturing_Lot=="Lot2")$PSI, mu=1500) 
# Lot 3
t.test(subset(susp_coil, Manufacturing_Lot=="Lot3")$PSI, mu=1500) 