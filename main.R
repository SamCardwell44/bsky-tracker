install.packages("curl")
install.packages("jsonlite")
library(curl)
install.packages("httr2")
library(httr2)
install.packages("atrrr")
install.packages("dplyr")
install.packages(c('readxl', 'writexl'))
library(atrrr)
library(readxl)
library(writexl)
library(dplyr)

# Authentication
auth("zylo42.bsky.social", "j4ex-4ake-oelh-74do", overwrite = TRUE)

get_info <- function(handle, limitnum = 10000, previous_data = NULL, retry_limit = 3, delay_sec = 5) {
  retries <- 0
  
  while (retries <= retry_limit) {
    result <- tryCatch({
      handle <- as.character(handle)
      
      # Get followers
      followers <- get_followers(actor = handle, limit = limitnum)
      follower_count <- as.numeric(nrow(followers))
      if (is.na(follower_count)) follower_count <- 0
      if (follower_count == 0 && !is.null(previous_data)) {
        message("Follower count is 0, using previous day's value for ", handle)
        follower_count <- previous_data$followers
      }
      
      # Get posts
      posts <- get_skeets_authored_by(actor = handle, limit = limitnum)
      post_count <- as.numeric(nrow(posts))
      if (is.na(post_count)) post_count <- 0
      
      return(list(
        followers = follower_count,
        posts = post_count
      ))
      
    }, error = function(e) {
      message("Error in get_info for handle ", handle, ": ", e$message)
      return(NULL)  # Indicate failure
    })
    
    # If successful, return the result
    if (!is.null(result)) {
      return(result)
    }
    
    # If failed, retry
    if (retries < retry_limit) {
      retries <- retries + 1
      message(paste("Retrying (", retries, "/", retry_limit, ")...", sep = ""))
      Sys.sleep(delay_sec)  # Add delay before retrying
    } else {
      message("Max retries reached for handle ", handle, ". Returning default values.")
      break  # Exit the loop
    }
  }
  
  # Fail-safe return after all retries
  return(list(followers = 0, posts = 0))
}
# General update function for both followers and posts
update_data <- function(follower_data, post_data, follower_file = "bsky_followers.xlsx", post_file = "bsky_posts.xlsx") {
  current_date <- as.character(Sys.Date())
  
  # Initialize new rows for followers and posts
  new_row_followers <- data.frame(date = current_date)
  new_row_posts <- data.frame(date = current_date)
  
  # Get the accounts (columns excluding the date column)
  accounts <- setdiff(names(follower_data), "date")  # Exclude 'date'
  total_accounts <- length(accounts)
  previous_data <- if (nrow(follower_data) > 1) follower_data[nrow(follower_data) - 1, ] else NULL
  
  for (i in seq_along(accounts)) {
    account <- accounts[i]
    message(sprintf("Processing account %d/%d: %s", i, total_accounts, account))  # Status message
    
    tryCatch({
      # Get followers and posts info only once
      info <- get_info(account, previous_data = previous_data)
      
      # Update the followers count
      new_row_followers[[account]] <- info$followers
      
      # Update the posts count
      new_row_posts[[account]] <- info$posts
      
    }, error = function(e) {
      warning(paste("Error processing account", account, ":", e$message))
      new_row_followers[[account]] <- NA
      new_row_posts[[account]] <- NA
    })
  }
  
  # Add missing columns to ensure alignment
  for (account in setdiff(names(new_row_followers), names(follower_data))) {
    follower_data[[account]] <- NA
  }
  for (account in setdiff(names(new_row_posts), names(post_data))) {
    post_data[[account]] <- NA
  }
  
  # Ensure column order matches
  follower_data <- follower_data[, union(names(new_row_followers), names(follower_data))]
  post_data <- post_data[, union(names(new_row_posts), names(post_data))]

  
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
