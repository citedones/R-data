# Identified Issues:

# 1. Inconsistent column names – Both sheets have similar data structures but with slight variations (e.g., extra commas in "Sector" column in Sheet2).
# 2. Missing values – Some values in "Projected investment" and "Base" are missing.
# 3. Data type issues – "Year" should be an integer, but some values appear to be incorrect (e.g., "0", "17", "1901").
# 4. Spelling errors – "Status of deal" column in Sheet2 contains "Don" instead of "Done".
# 5. Outliers – The "Year" column contains unlikely values like "0" and "1901".
# 6. Extra spaces & formatting issues – Column names and textual data may contain extra spaces or inconsistent capitalization.

# Cleaning Steps:

# Standardize column names across both sheets.
# Convert "Year" to integer and handle invalid values, keep the values between 2012 to 2017
# Standardize the "Status of deal" column (correct typos), just analyze the data under "done"
# Handle missing values appropriately.
# Remove unnecessary "NA" value in columns.



# installing the required libraries
library(readxl) 
library(tidyverse) 

# set the working directory 
path <- "C:\\New folder\\Mcdaniel\\2025 Spring\\ANA515\\practice\\GRAIN---Land-grab-deals---Jan-2012-2.xlsx"
setwd("C:\\New folder\\Mcdaniel\\2025 Spring\\ANA515\\practice") 

# accessing all the sheets 
sheet = excel_sheets("GRAIN---Land-grab-deals---Jan-2012-2.xlsx") 

df1 <- read_excel(path, sheet = 1)
df2 <- read_excel(path, sheet = 2)

# Standardize column names
clean_colnames <- function(df) {
  colnames(df) <- colnames(df) %>%
    str_trim() %>%                     
    tolower() %>%                       
    str_replace_all(" ", "_")            
  return(df)
}

df1 <- clean_colnames(df1)
df2 <- clean_colnames(df2)

# Ensure both datasets have the same column structure
all_columns <- union(colnames(df1), colnames(df2))  
df1 <- df1 %>% select(all_of(all_columns))
df2 <- df2 %>% select(all_of(all_columns))

# Convert "year" column to numeric (handling non-numeric cases as NA)
df1 <- df1 %>% mutate(year = as.numeric(year))
df2 <- df2 %>% mutate(year = as.numeric(year))


# Combine sheet1 and sheet2
combined_df <- bind_rows(df1, df2, .id = "Sheet")

# printing data of all sheets 
print (combined_df)

# Get rid of nonsense data
combined_df <- combined_df %>% select(-Sheet)
combined_df <- combined_df %>% select(-summary, -projected_investment)


# Filter the dataset to keep only rows where year is between 2012 and 2017, to get rid of the missing data, or errors
filtered_df <- combined_df %>% filter(year >= 2012 & year <= 2017)
write.csv(filtered_df, "Combined_Land_Grab_Data.csv", row.names = FALSE)

install.packages("countrycode")
library(countrycode)  # To check valid country names

# Define a function to check if a country name is valid
valid_country <- function(name) {
  return(name %in% countrycode::codelist$country.name.en)  # Matches valid country names
}

# Apply the function and filter out invalid country names
filtered_df <- filtered_df %>% filter(valid_country(landgrabbed))

# Remove "NA" value in "production" and "hectares" colomns, keep "Done" in "status of deal"
filtered_df <- filtered_df %>% 
  filter(!is.na(production) & !is.na(hectares) & status_of_deal == "Done")


write.csv(filtered_df, "Final_Land_Grab_Data_2012_2017.csv", row.names = FALSE)

library(ggplot2)

# Number of Land Deals Per Country
ggplot(filtered_df, aes(x = landgrabbed)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Number of Land Acquisitions by Country",
       x = "Country",
       y = "Number of Deals") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 6))  # Rotate x-axis labels

# Distribution of Land Sizes in Hectares
ggplot(filtered_df, aes(x = hectares)) +
  geom_histogram(binwidth = 5000, fill = "purple", color = "black") +
  labs(title = "Distribution of Land Deals (Hectares)", x = "Hectares", y = "Count") +
  theme_minimal()

# Spread of Land Acquisitions by Sector
ggplot(filtered_df, aes(x = sector, y = hectares)) +
  geom_boxplot(fill = "orange", outlier.color = "red") +
  coord_flip() +  # Rotate for better readability
  labs(title = "Land Acquisitions by Sector", x = "Sector", y = "Hectares") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))
