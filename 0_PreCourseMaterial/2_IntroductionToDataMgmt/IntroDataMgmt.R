## ----setup, include=FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----eval=F-----------------------------------------------------------------------------------
## # ******************************************************************
## # ******************************************************************
## 
## # Project: Introduction to Data Analysis in R
## # Description: A script which details some basic commands on how to manipulate data
## # Author: <Your Name>
## # Date Initialized: <dd month yyyy>
## 
## # ******************************************************************
## # ******************************************************************


## ----eval=F-----------------------------------------------------------------------------------
## # Clean your workspace/remove all objects
## rm(list=ls())
## 
## # You can also remove a specific dataset using the following command
## #rm(dataset)


## ----eval = T, message = F, warning = F-------------------------------------------------------
#install.packages("dplyr")
library(dplyr)


## ----eval = T, echo = T, warning=F, message=F-------------------------------------------------
# Read dataset
panda_data <- read.csv(file="Data/panda_data.csv")

# Look at the data
head(panda_data)


## ----warning=F, message=F, eval=F, echo=F-----------------------------------------------------
## # Print the first 3 rows
## head(panda_data, n = 3)
## 
## # Print rows 4 thru 6
## panda_data[4:6,]
## 
## # Dataframe dimensions
## dim(panda_data)
## nrow(panda_data)
## ncol(panda_data)
## 
## # Print the tail
## tail(panda_data)


## ----warning = F, message = F-----------------------------------------------------------------
# select column called panda_name
select(panda_data, panda_name) 

# select all columns in the data except panda_name
select(panda_data, -panda_name)

# select a range of columns, from age to sex
select(panda_data, age:sex)


## ----warning = F, message = F-----------------------------------------------------------------
# select all columns that start with "genetic" in their column names
select(panda_data, starts_with("genetic")) 


## ----warning = F, message = F, eval = F, echo = F---------------------------------------------
## # Select all the columns that contain "value" in the column name.
## select(panda_data, contains("value"))
## 
## # Select or exclude two columns: `panda_name` and `age`.
## select(panda_data, c(panda_name, age))
## select(panda_data, !c(panda_name, age))


## ----warning=F, message=F---------------------------------------------------------------------
# Select rows where pandas are greater than or equal to 5 years of age
filter(panda_data, age >= 5)

# select rows that have age>5 OR weight_kg >100 
filter(panda_data, age > 5 | weight_kg > 100)

# select rows that have age>5 AND base column has CD has entry
filter(panda_data, age > 5 & base == "CD") 

# Select rows where the panda age is defined by a few values.  Note the use of the %in% function.
filter(panda_data, age %in% c(4,5,7))


## ----warning = F, message = F, eval = F, echo = F---------------------------------------------
## # Select rows with `NA` in the genetic_value2 column.
## filter(panda_data, is.na(genetic_value2))
## 
## # Select rows whose panda_name column are `bao_bao` or `bei_bei`
## filter(panda_data, panda_name == 'bao_bao' | panda_name == 'bei_bei')


## ----message=F, warning=F---------------------------------------------------------------------
# Use a simple pipe to select the panda name and it's sex, and output the result.
# Create a new object named 'pipe_result'
pipe_result <- panda_data %>%
  select(panda_name, sex) %>%
  head()

# Output the result to the screen 
pipe_result


## ----message=F, warning=F, eval=F, echo=F-----------------------------------------------------
## # Select columns that contain genetic and then selection values based on these columns
## panda_data %>%
##   select(contains("genetic")) %>%
##   filter(genetic_value1 > 80 & genetic_value2 < 90)
## 
## # Example of filtering a column that doesn't exist after selecting
## # The field doesn't exist and will cause an error
## # panda_data %>%
## #   select(contains("genetic")) %>%
## #   filter(weight_kg > 90)


## ----warning=F, message=F---------------------------------------------------------------------
# Create a new column, based on values from other columns that exist
# By default, keep = all
new_col_ex1 <- panda_data %>%
  mutate(genetic_value_new = genetic_value1 - genetic_value2,
         .keep = "all")  
# Print
new_col_ex1

# You can create multiple columns at once.  Best to put each new column on a separate line.  This simply makes the code more readable.
new_col_ex2 <- panda_data %>%
	mutate(genetic_dif = genetic_value1 - genetic_value2, 
	       weight_g = weight_kg * 1000) 
# Print
new_col_ex2


## ----warning=F, message=F, eval=F, echo=F-----------------------------------------------------
## # Create a new column called zero
## panda_data %>%
##   mutate(zero = 0)


## ---------------------------------------------------------------------------------------------
# Arrange the panda_data by panda_name in descending order
# Place the sex column after the ID column
order_data_ex1 <- panda_data %>% 
	arrange(desc(panda_name)) %>% 
  relocate(sex, .after = ID)
# Print
head(order_data_ex1)

# Arrange the panda_data by genetic_value 1 and genetic_value2 after select the columns that start with 'genetic'
# Rename the genetic_value1 column to gen_val1
order_data_ex2 <- panda_data %>% 
	select(starts_with("genetic")) %>%
	arrange(genetic_value1, genetic_value2) %>% 
  rename(gen_val1 = genetic_value1)
# Print
head(order_data_ex2)


## ----message=F, warning=F, echo=F, eval=F-----------------------------------------------------
## # Arrange the panda dataset by age and weight, relocating columns and renaming the weight column
## panda_data %>%
##   arrange(age, weight_kg) %>%
##   relocate(year, .before = age) %>%
##   relocate(base, .after = last_col()) %>%
##   rename(panda_wgt_kg = weight_kg)


## ----message=F, warning=F---------------------------------------------------------------------
# Calculate the mean weight of the pandas and provide a count.
sum_table <- panda_data %>% 
  summarise(mean_wgt = mean(weight_kg),
            n = n())
# Print summary table
sum_table

# Calculate the avg weight and minimum weight.  Don't create a new object.
panda_data %>% 
  summarise(avg_wt = mean(weight_kg), 
            min_wt = min(weight_kg))


## ----warning=F, message=F---------------------------------------------------------------------
# Similar to above, calculate average and minimum weight, but summarize based on 'base' column.  Include a count of each group.
panda_data %>%
  summarise(avg_wt = mean(weight_kg),
            min_wt = min(weight_kg),
            n = n(),
            .by = base)

# Group summaries can also be calculated across muultiple groups.
# Here, we calculate the same as above, but based on base and sex
panda_data %>%
  summarise(avg_wt = mean(weight_kg),
            min_wt= min(weight_kg),
            n = n(), 
            .by = c(base, sex))


## ----warning=F, message=F---------------------------------------------------------------------
# Read in the vaccination table
panda_med <- read.csv(file="Data/panda_data_med.csv")

# Join all the rows in table 1 (panda_data) with table 2 (panda_med) to determine which of the pandas were vaccinated.  Since ID exists in both tables, this is a straightforward join.
# Arrange the result by ID and year vaccinated
panda_join_ex1 <- panda_data %>% 
  left_join(panda_med, by = "ID") %>% 
  arrange(ID, year_vaccination)
# Print
head(panda_join_ex1)

# Example of how to join based on multiple fields and fields that don't exactly match
panda_join_ex2 <- panda_data %>%
  left_join(panda_med,
            by = c("ID" = "ID", 
                   "year"= "year_vaccination")) 
# Print
head(panda_join_ex2)


## ----warning=T, message=T, echo=F, eval=F-----------------------------------------------------
## # Read in the CSV
## survey_sample <- read.csv(file = "Data/SampleData.csv")
## 
## # Look at the data
## head(survey_sample)
## 
## # Question 1: What are the dimensions of the dataset?  How many rows and columns?
## dim(survey_sample)
## nrow(survey_sample)
## ncol(survey_sample)
## 
## # Question 2: What is the data structure of each variable?
## str(survey_sample)
## 
## # Question 3: How many species were counted?
## n_distinct(survey_sample$Species)
## length(unique(survey_sample$Species))
## 
## # Question 4: How many transects are there and what are the names of these transects?  Same process, but a different column.
## n_distinct(survey_sample$TransectID)
## unique(survey_sample$TransectID)
## 
## # Question 5: How many unique survey days were completed?  What is the range of dates?
## n_distinct(survey_sample$Date)
## length(unique(survey_sample$Date))
## min(survey_sample$Date)
## max(survey_sample$Date)
## range(survey_sample$Date)
## 
## # Question 6: What is the maximum group size of giraffe?
## survey_sample %>%
##   filter(Species == "Giraffe") %>%
##   summarise(max_grp = max(GroupSize))
## 
## # Question 7: What is the mean group size of wildebeest per year?  Include the number of observations in the calculation.
## survey_sample %>%
##   filter(Species == "Wildebeest") %>%
##   summarise(mean_grp = mean(GroupSize),
##             n_obs = n(), # This counts the observations, it is not a sum of the observations.
##             .by=Year)
## 
## # Question 8: What is the total count of impala observed in 2018?
## survey_sample %>%
##   filter(Species == "Impala" & Year == "2018") %>%
##   summarise(sum_impala = sum(GroupSize))
## 
## # Question 9: What is the total number of groups of each species per year?  Note, not the total animals counted.
## survey_sample %>%
##   summarise(Total_Grps = n(),
##             .by=c(Species, Year))
## 
## # Question 10: What is the total number of groups and the mean group size of each species, per year, and per transect?  Sort these by Species, TransectID, and Year
## survey_sample %>%
##   summarise(Total_Grps = n(),
##             Mean_Grps = mean(GroupSize),
##             .by = c(Species, Year, TransectID)) %>%
##   arrange(Species, TransectID, Year)

