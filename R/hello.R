
# load the library to read Excel files
library(readxl)

# read the excel file (I assume the working directory has
# already been set to the location of the Excel file)
my_file <- read_excel("Transpose.xls", skip = 1)

# inspect the file
head(my_file)

# We want to take the first three rows and transpose to three columns. 
# to get to first few rows
my_file <- my_file[ 1:2 , ]

# inspect the result, the first row is the column names
head(my_file)

# we need to convert the five digit date values from Excel into R format
my_file$DATE <- as.Date(as.numeric(my_file$DATE), origin = "1900-01-30")

# Now we can transpose...

# For this task we can use the function `gather` from the package `tidyr`
# 
# Function:       gather(data, key, value, ..., na.rm = FALSE, convert = FALSE)
# Same as:        data %>% gather(key, value, ..., na.rm = FALSE, convert = FALSE)
# 
# Arguments:
# data:           data frame
# key:            column name representing new variable
# value:          column name representing variable values
# ...:            names of columns to gather (or not gather)
# na.rm:          option to remove observations with missing values (represented by NAs)
# convert:        if TRUE will automatically convert values to logical, integer, numeric, complex or 
# factor as appropriate

library(tidyr)

my_file_transposed <- gather(my_file, time, everything,  -DATE)

# to explain this a little bit:
# my_file: our dataframe to transpose
# time: the name of the new column that will hold the cell values
# everything: a function that selects all columns
# -DATE: excludes the DATE column from being included in the new time column, 
# so it will appear in a column by itself.

# let's see the result

head(my_file_transposed)

# it's ok, but we should conver the character cols to numeric 
# so we can use them for stats and plots and so on

library(purrr)
my_file_transposed_numerics <- map_if(my_file_transposed, is.character, as.numeric) %>% 
  data.frame

# I see that you've sorted your excel sheet by date and then by time, so we can do that too
library(dplyr)
my_file_transposed_numerics_sorted <- my_file_transposed_numerics %>% 
  arrange(DATE, time)
# have a look
head(my_file_transposed_numerics_sorted)

# Now we can do all of this at once and save a bit of typing, 
# assuming you've done up to line 22 above, here we go:
my_file_transposed_numerics_sorted <- my_file %>% 
  gather(time, everything, -DATE) %>%       # transpose
  map_if(is.character, as.numeric) %>%      # character cols to numeric
  data.frame %>%                            # ...
  arrange(DATE, time)                       # sort

# want to have one variable that is date-time

my_file_transposed_numerics_sorted %>% 
  mutate(date_time =  as.POSIXct(paste0(as.Date(DATE), " ", paste0(time, ":00")))) %>% 
  select(-DATE, -time) %>% 
  rename(motor_vehicles = everything)





