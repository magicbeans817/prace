#### This script serves as a crosswalk when trying to make similar-to-spss output in R
# https://cran.r-project.org/web/packages/summarytools/summarytools.pdf
# Crucial for this is summarytools package and its functions (for more info - documentation)

library(summarytools) #load the library
library(dplyr)
data("tobacco") #simulated dataset from this library
data <- tobacco
head(data)

##### freq
# as input dataframe, factor, nebo vector. Output is not dataframe. If excel needs to be printed,
# turn the frequency table to dataframe or datatable (often datatable desirable or needed)
#example
summarytools::freq(data$gender) #frequency of gender
summarytools::freq(data %>% select(gender, smoker, diseased)) #frequency tables of the 3 vars
summarytools::freq(data %>% select(gender:diseased)) #list of vars -> passed -> frequency tables

##### cross-tabulation
# input categorical variables or marginal sums. 
# Works with numeric, character, as well as factor variables
#summarytools::ctable()

#example: lets create variable for above or below average in math
summarytools::ctable(data$gender, data$diseased)


##### descriptive statistics, mean, std, quartiles, skewness... doesnt't work with weights tho
# input can be dataframe or vector
summarytools::descr(data)


##### overall summary of the dataset (dplyr::glimpse seems like a better choice)
summarytools::dfSummary(data)


##### create a label for a variable or get a label for a variable
label(data$disease) <- "type of the disease"
label(data$disease)
View(data)

##### view in side viewer
y <- summarytools::ctable(data$gender, data$diseased)
y
summarytools::view(y)

x <- summarytools::freq(data$gender) 
summarytools::view(x)

##### How to recode: use dplyr::case_when(). Another option dplyr::recode, but I did not understand
#the documentation and examples.
data <- data %>%
  mutate(age_group = dplyr::case_when(
    age <= 29 ~ "18-30",
    age > 29 & age <= 49 ~ "30 - 49",
    age > 49 & age < 66 ~ "50-64",
    age >= 65 ~ "65+"
  )) 


data %>%
  group_by(age_group, smoker) %>%
  count()  %>% spread(key = age_group, value = n, fill = 0)












y <- summarytools::ctable(data$age.gr, data$age_group)
summarytools::view(y)
y <- as.data.frame(y)
View(y)





head(data)

dplyr::recode(data$age)






##### Zasadni nesmysly pro me
lsf.str("package:summarytools")
ls("package:summarytools")





















