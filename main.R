install.packages("curl")
install.packages("jsonlite")
library(curl)
install.packages("httr2")
library(httr2)
install.packages("atrrr")
install.packages(c('readxl', 'writexl'))
library(atrrr)
library(readxl)
library(writexl)

# Authentication
auth("samcardwell44.bsky.social", "qioa-udz7-77ck-q5t5", overwrite = TRUE)

# Get info for a specific handle (followers and posts)
get_info <- function(handle, limitnum = 10) {
  handle <- as.character(handle)
  
  tryCatch({
    # Get followers
    followers <- get_followers(actor = handle, limit = limitnum)
    
    # Get posts
    posts <- get_skeets_authored_by(actor = handle, limit = limitnum)
    
    return(list(followers = nrow(followers), posts = nrow(posts)))
  }, error = function(e) {
    warning(sprintf("Error processing %s: %s", handle, e$message))
    return(list(followers = NA, posts = NA))
  })
}

# General update function for both followers and posts
update_data <- function(follower_data, post_data, follower_file = "bsky_followers.xlsx", post_file = "bsky_posts.xlsx") {
  current_date <- as.character(Sys.Date())
  
  # Initialize new rows with existing column structure
  new_row_followers <- follower_data[1, ]
  new_row_posts <- post_data[1, ]
  
  # Set date for new rows
  new_row_followers$date <- current_date
  new_row_posts$date <- current_date
  
  # Set all numeric columns to NA initially
  numeric_cols_followers <- sapply(follower_data, is.numeric)
  numeric_cols_posts <- sapply(post_data, is.numeric)
  new_row_followers[, numeric_cols_followers] <- NA
  new_row_posts[, numeric_cols_posts] <- NA
  
  # Get accounts (columns excluding the date column)
  accounts <- setdiff(names(follower_data), "date")
  total_accounts <- length(accounts)
  
  # Process each account
  for (i in seq_along(accounts)) {
    account <- accounts[i]
    message(sprintf("Processing account %d/%d: %s", i, total_accounts, account))
    
    # Get data
    info <- get_info(account)
    
    # Update the new rows with the retrieved data
    new_row_followers[[account]] <- info$followers
    new_row_posts[[account]] <- info$posts
    
    # Print debug info
    print(sprintf("Account: %s, Followers: %s, Posts: %s", 
                 account, 
                 info$followers, 
                 info$posts))
  }
  
  # Remove existing row for current date if it exists
  follower_data <- follower_data[follower_data$date != current_date, ]
  post_data <- post_data[post_data$date != current_date, ]
  
  # Append new rows
  follower_data <- rbind(follower_data, new_row_followers)
  post_data <- rbind(post_data, new_row_posts)
  
  # Print debug information
  print("New follower row:")
  print(new_row_followers)
  print("Updated follower data:")
  print(follower_data)
  
  # Save the updated data frames
  write_xlsx(follower_data, follower_file)
  write_xlsx(post_data, post_file)
  
  # Verify files were saved
  print("Files saved successfully:")
  print(file.exists(follower_file))
  print(file.exists(post_file))
  
  return(list(followers = follower_data, posts = post_data))
}

# Load existing data
bsky_followers <- tryCatch(
  read_xlsx("bsky_followers.xlsx"),
  error = function(e) {
    warning("Could not read followers file, creating new data frame")
    data.frame(date = character())
  }
)

bsky_posts <- tryCatch(
  read_xlsx("bsky_posts.xlsx"),
  error = function(e) {
    warning("Could not read posts file, creating new data frame")
    data.frame(date = character())
  }
)

# Print initial data structure
print("Initial data structure:")
print(str(bsky_followers))
print(str(bsky_posts))

# Update both followers and posts
updated_data <- update_data(bsky_followers, bsky_posts)

# Preview final data
print("Final data preview:")
print(head(updated_data$followers))
print(head(updated_data$posts))

# Load existing data or initialize new data frames
bsky_followers <- tryCatch(
  read_xlsx("bsky_followers.xlsx"),
  error = function(e) data.frame(date = character())
)

bsky_posts <- tryCatch(
  read_xlsx("bsky_posts.xlsx"),
  error = function(e) data.frame(date = character())
)

# Print initial data structure
print("Initial data structure:")
print(str(bsky_followers))
print(str(bsky_posts))

# Update both followers and posts
updated_data <- update_data(bsky_followers, bsky_posts)

# Preview final data
print("Final data preview:")
print(head(updated_data$followers))
print(head(updated_data$posts))
