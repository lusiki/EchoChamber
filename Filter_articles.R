
# Load the required libraries
library(readxl)
library(dplyr)
library(stringi)
library(data.table)
library(stringr)
library(writexl)
library(openxlsx)
library(ggplot2)



file_list <- list.files(path = "C:/Users/lukas/Dropbox/Determ_mediaspace/2024", pattern = "*.xlsx", full.names = TRUE)


# Function to read a single Excel file
read_single_file <- function(file_path) {
  read_excel(file_path)
}

# Read all files and combine into one data frame
all_data <- lapply(file_list, read_single_file) %>%
  bind_rows()

# Convert the combined data to a data table
all_data <- as.data.table(all_data)
# View the combined data
#print(all_data)

# Show the structure of the combined data
all_data %>%
  select(SOURCE_TYPE) %>%
  count(SOURCE_TYPE) 

# filter source type as reddit and forum

komentari <- all_data %>%
  filter(SOURCE_TYPE %in% c("reddit", "forum")) 

#create date variable and show number of comments per date

komentari$DATE <- as.Date(komentari$DATE, format = "%Y-%m-%d")

komentari %>%
  select(DATE) %>%
  count(DATE) %>%
  arrange(DATE)

# show visualisation of number of comments per date

komentari %>%
  select(DATE) %>%
  count(DATE) %>%
  ggplot(aes(x = DATE, y = n)) +
  geom_line() +
  labs(title = "Number of comments per date",
       x = "Date",
       y = "Number of comments")


# filter only dates in the ragen 2024-03-14 to 2024-06-10

komentari <- komentari %>%
  filter(DATE >= "2024-03-14" & DATE <= "2024-06-10")


komentari_select %>%
  select(DATE) %>%
  count(DATE) %>%
  ggplot(aes(x = DATE, y = n)) +
  geom_line() +
  labs(title = "Number of comments per date",
       x = "Date",
       y = "Number of comments")




# Replace 'your_column_name' with the actual name of the column you want to search
your_column_name <- "TITLE"


# Define the function to check for matches
check_matches <- function(text, words_vector) {
  any(stri_detect_regex(text, words_vector, negate = FALSE))
}

# Define the words vector
words_vector <- c("izbor", "glas", "parlament", "kampanj", "debat", "polit", "hdz", "sdp", "most", "politi")

# Define batch size
batch_size <- 1000

# Calculate the number of batches
num_batches <- ceiling(nrow(komentari) / batch_size)

# Loop through each batch
for (i in 1:num_batches) {
  
  start_time <- Sys.time()
  
  # Calculate the start and end row indices for the current batch
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(komentari))
  
  # Print the current batch number and row indices
  cat(sprintf("Processing batch %d (rows %d to %d)...\n", i, start_idx, end_idx))
  
  # Subset the data table for the current batch and apply the operations
  komentari[start_idx:end_idx, `:=` (
    has_word = sapply(.SD[[your_column_name]], check_matches, words_vector),
    matched_word = sapply(.SD[[your_column_name]], function(x) paste(unlist(stri_extract_all_regex(x, words_vector)), collapse=", "))
  ), .SDcols = your_column_name]
  
  batch_data <- komentari[start_idx:end_idx]
  
  end_time <- Sys.time()
  duration <- end_time - start_time
  
  # Print the duration for the current batch
  cat(sprintf("Batch %d processed in %f seconds.\n", i, duration))
  
  # Save or further process batch_data if necessary
}

komentari_select <- komentari[has_word==T,]

# group by SOURCE_TYPE and count
komentari_select %>%
  select(SOURCE_TYPE) %>%
  count(SOURCE_TYPE)

#group by matched word and count exluding NA

komentari_select %>%
  select(matched_word) %>%
  count(matched_word) %>%
  filter(!is.na(matched_word)) %>%
  arrange(desc(n)) 


write.xlsx(komentari_select, "C:/Users/lukas/Dropbox/HS/TAMARA/forum_i_reddit.xlsx")
