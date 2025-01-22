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
  
  # If data frames are empty, initialize them with the date column
  if (ncol(follower_data) == 0) {
    follower_data <- data.frame(date = character())
  }
  if (ncol(post_data) == 0) {
    post_data <- data.frame(date = character())
  }
  
  # Get list of all accounts (columns) excluding date
  accounts <- unique(c(
    setdiff(names(follower_data), "date"),
    setdiff(names(post_data), "date")
  ))
  
  # Initialize new rows
  new_row_followers <- data.frame(date = current_date)
  new_row_posts <- data.frame(date = current_date)
  
  # Process each account
  for (account in accounts) {
    message(sprintf("Processing account: %s", account))
    
    # Get data
    info <- get_info(account)
    
    # Add to new rows
    new_row_followers[[account]] <- info$followers
    new_row_posts[[account]] <- info$posts
  }
  
  # Ensure all columns exist in both data frames
  for (account in accounts) {
    if (!(account %in% names(follower_data))) {
      follower_data[[account]] <- NA
    }
    if (!(account %in% names(post_data))) {
      post_data[[account]] <- NA
    }
  }
  
  # Update or append rows
  if (current_date %in% follower_data$date) {
    follower_data[follower_data$date == current_date, names(new_row_followers)] <- new_row_followers
  } else {
    follower_data <- rbind(follower_data, new_row_followers[, names(follower_data)])
  }
  
  if (current_date %in% post_data$date) {
    post_data[post_data$date == current_date, names(new_row_posts)] <- new_row_posts
  } else {
    post_data <- rbind(post_data, new_row_posts[, names(post_data)])
  }
  
  # Add debugging information
  print("Data structure before saving:")
  print(str(follower_data))
  print(str(post_data))
  
  # Save the updated data frames
  tryCatch({
    write_xlsx(follower_data, follower_file)
    write_xlsx(post_data, post_file)
    
    # Verify files were saved
    print("Files saved successfully:")
    print(file.exists(follower_file))
    print(file.exists(post_file))
  }, error = function(e) {
    stop(sprintf("Error saving files: %s", e$message))
  })
  
  return(list(followers = follower_data, posts = post_data))
}

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
