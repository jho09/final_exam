library(tidyverse)
library(psych) 
library(cocor) 
library(apaTables)
library(pwr)                    
library(predictionInterval)  
library(MBESS)    
library(haven)

read_csv("exam_data_f16.csv")
raw_data <- read_csv("exam_data_f16.csv")

# Determine f2
my.f2 <- .10 / (1 - .20)
print(my.f2)
# [1] 0.125

# Calculate power
pwr.f2.test(u=2, f2=0.125, power=.85)
# u = 2
# v = 87.45243
# f2 = 0.125
# sig.level = 0.05
# power = 0.85

N = 2 + 88 + 1
print(N)
# [1] 91