#1
library(tidyverse)


mechaCar <- read.csv('MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)


model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechaCar)
summary(model)
#2

suspension <- read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F)


total_summary <- suspension %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))


lot_summary <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

# T.Test for All Manufacturing Lots
t.test(suspension$PSI, mu = 1500)

# T.Test 
t.test(subset(suspension, Manufacturing_Lot == "Lot1", select = PSI), mu = 1500)
t.test(subset(suspension, Manufacturing_Lot == "Lot2", select = PSI), mu = 1500)
t.test(subset(suspension, Manufacturing_Lot == "Lot3", select = PSI), mu = 1500)
