# Load necessary library
library(dplyr)

# Load the dataset
df <- read.csv("D:/Bootcamp VCU datasets/NSSO68.csv")

# Select relevant features
features <- c('hhdsz', 'Religion', 'Social_Group', 'Type_of_land_owned', 'Land_Owned',
              'MPCE_URP', 'Age', 'Sex', 'Education', 'Regular_salary_earner', 'nonvegtotal_q')

df <- df[, features]

