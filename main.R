install.packages("curl")
library(curl)
install.packages("httr2")
library(httr2)
install.packages("atrrr")
install.packages(c('readxl', 'writexl'))
library(atrrr)
library(readxl)
library(writexl)

# Get info for a specific handle (followers and posts)
get_info <- function(handle, limitnum = 10000) {
  handle <- as.character(handle)
  
  # Get followers
  followers <- get_followers(actor = handle, limit = limitnum)
  
  # Get posts
  posts <- get_skeets_authored_by(actor = handle, limit = limitnum)
  
  # Save followers and posts as global variables for optional use
  assign(paste0(gsub("[^a-zA-Z0-9]", "_", handle), "_followers_df"), followers, envir = .GlobalEnv)
  assign(paste0(gsub("[^a-zA-Z0-9]", "_", handle), "_posts_df"), posts, envir = .GlobalEnv)
  
  # Return a list containing both followers and posts
  return(list(followers = followers, posts = posts))
}

# General update function for both followers and posts
update_data <- function(follower_data, post_data, follower_file = "bsky_followers.xlsx", post_file = "bsky_posts.xlsx") {
  current_date <- as.character(Sys.Date())
  
  # Initialize new rows for followers and posts
  new_row_followers <- data.frame(date = current_date)
  new_row_posts <- data.frame(date = current_date)
  
  # Get the accounts (columns excluding the date column)
  accounts <- names(follower_data)[-1]  # Exclude 'date'
  total_accounts <- length(accounts)
  
  for (i in seq_along(accounts)) {
    account <- accounts[i]
    message(sprintf("Processing account %d/%d: %s", i, total_accounts, account))  # Status message
    
    tryCatch({
      # Get followers and posts info only once
      info <- get_info(account)
      cat(nrow(info$followers),"\n")
      # Update the followers count
      new_row_followers[[account]] <- nrow(info$followers)
      
      # Update the posts count
      new_row_posts[[account]] <- nrow(info$posts)
      
    }, error = function(e) {
      warning(paste("Error processing account", account, ":", e$message))
      new_row_followers[[account]] <- NA
      new_row_posts[[account]] <- NA
    })
  }
  
  # Update or append rows for followers
  if (current_date %in% follower_data$date) {
    follower_data[follower_data$date == current_date, ] <- new_row_followers
  } else {
    follower_data <- rbind(follower_data, new_row_followers)
  }
  
  # Update or append rows for posts
  if (current_date %in% post_data$date) {
    post_data[post_data$date == current_date, ] <- new_row_posts
  } else {
    post_data <- rbind(post_data, new_row_posts)
  }
  
  # Save the updated data frames
  write_xlsx(follower_data, follower_file)
  write_xlsx(post_data, post_file)
  
  # Return the updated data frames
  return(list(followers = follower_data, posts = post_data))
}

# Load existing data or initialize new data frames
bsky_followers <- tryCatch(read_xlsx("bsky_followers.xlsx"), error = function(e) data.frame(date = character()))
bsky_posts <- tryCatch(read_xlsx("bsky_posts.xlsx"), error = function(e) data.frame(date = character()))

# Update both followers and posts
updated_data <- update_data(bsky_followers, bsky_posts, "bsky_followers.xlsx", "bsky_posts.xlsx")

# Separate updated followers and posts
bsky_followers <- updated_data$followers
bsky_posts <- updated_data$posts

# Preview data
head(bsky_followers)
head(bsky_posts)
