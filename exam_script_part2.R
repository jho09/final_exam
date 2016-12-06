library(tidyverse)
library(psych) 
library(cocor) 
library(apaTables)
library(pwr)                    
library(predictionInterval)  
library(MBESS)    
library(haven)

# Load data
read_csv("exam_data_f16.csv")
raw_data <- read_csv("exam_data_f16.csv")

# Select data
categorical_variables <- select(raw_data, gender, education)

# Convert integers to factors and then label levels of factors
categorical_variables$gender <- as.factor(categorical_variables$gender)
categorical_variables$education <- as.factor(categorical_variables$education)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)

# Break scale items into separate data frames

age <- select(raw_data,age)
agreeableness_items <- select (raw_data, A1, A2, A3, A4, A5)
conscientiousness_items <- select (raw_data, C1, C2, C3, C4, C5)
performance_items <- select (raw_data, JP1, JP2, JP3, JP4, JP5)
gender <- select(raw_data,gender)

# Check for out of range values
psych::describe(agreeableness_items)
psych::describe(conscientiousness_items)
psych::describe(performance_items)

# Fix bad values
is_bad_value <- agreeableness_items<1 | agreeableness_items>6
agreeableness_items[is_bad_value] <- NA

is_bad_value <- conscientiousness_items<1 | conscientiousness_items>6
conscientiousness_items[is_bad_value] <- NA

is_bad_value <- performance_items<1 | performance_items>6
performance_items[is_bad_value] <- NA

# Fix reverse keyed items
agreeableness_items <- mutate(agreeableness_items, A1=7-A1)
conscientiousness_items <- mutate(conscientiousness_items, C4=7-C4)
conscientiousness_items <- mutate(conscientiousness_items, C5=7-C5)
performance_items <- mutate(performance_items, JP1=7-JP1)
performance_items <- mutate(performance_items, JP2=7-JP2)

# Calculate mean scale scores
agreeableness <- psych::alpha(as.data.frame(agreeableness_items), check.keys=FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(conscientiousness_items), check.keys=FALSE)$scores
performance <- psych::alpha(as.data.frame(performance_items), check.keys=FALSE)$scores

# Combine into analytic_data
analytic_data <- cbind(agreeableness, conscientiousness, performance, age)

# Save analytic_data
save(analytic_data,file="exam_part_2.RData")
write_csv(analytic_data,path="exam_part_2.csv")
write_sav(analytic_data,path="exam_part_2.sav")

# Create APA style correlation table
apa.cor.table(analytic_data,filename="Table1.doc",table.number = 1)

# Create non-publication graph to assess non-linear relations
apa.cor.table(as.data.frame(analytic_data))
psych::pairs.panels(as.data.frame(analytic_data),lm=TRUE)

# Compute multiple regression for performance ~ conscientiousness; conscientiousness + agreeableness

block1 <- lm(performance ~ conscientiousness, data=analytic_data)
block2 <- lm(performance~ conscientiousness + agreeableness, data=analytic_data)

apa.reg.table(block1,block2,filename = "myRegressionTable1.doc")
# ΔR2   = .17**
# 95% CI[.14, .19]

# Create analytic data for men
analytic_data <- cbind(agreeableness, conscientiousness, performance, age, gender)
analytic_data_men <- filter(analytic_data,gender==1)
analytic_data_men1 <- analytic_data_men %>% select(-gender)
                            
# Create analytic data for women
analytic_data_women <- filter(analytic_data,gender==2)
analytic_data_women1 <- analytic_data_women %>% select(-gender)

# Run the multiple regression again for just men
block1m <- lm(performance ~ conscientiousness, data=analytic_data_men1)
block2m <- lm(performance~ conscientiousness + agreeableness, data=analytic_data_men1)

apa.reg.table(block1m,block2m,filename = "myRegressionTable2.doc")
# ΔR2   = .18**
# 95% CI[.14, .23]

# Run the multiple regression again for just men
block1w <- lm(performance ~ conscientiousness, data=analytic_data_women1)
block2w <- lm(performance~ conscientiousness + agreeableness, data=analytic_data_women1)

apa.reg.table(block1w,block2w,filename = "myRegressionTable3.doc")
# ΔR2   = .15**
# 95% CI[.12, .18]

