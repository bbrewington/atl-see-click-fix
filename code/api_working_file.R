library(httr); library(tidyverse); library(lubridate)

null_to_na <- function(x, data_type){
  if(!is.null(x)){
    x
  } else {
    if((!(data_type %in% c("character", "integer", "numeric")) & is.null(x))){
      stop("argument data_type must be one of character, integer, or numeric")
    } else {
      na_type <- switch(data_type, character = NA_character_, 
                        integer = NA_integer_, numeric = NA_real_)
      if_else(is.null(x), na_type, x)
    }
  }
}

get_issues_in_bbox <- function(min_lat, min_lng, max_lat, max_lng){
  get_issue_attributes <- 
    function(issue){
      data_frame(
        id = issue$id,
        status = issue$status,
        rating = issue$rating,
        summary = issue$summary,
        reporter_id = null_to_na(issue$reporter$id, "integer"),
        reporter_name = null_to_na(issue$reporter$name, "character"),
        lat = issue$lat,
        lng = issue$lng,
        url = issue$html_url,
        comment_url = null_to_na(issue$comment_url, "character"),
        image_full = null_to_na(issue$media$image_full, "character"),
        image_square = null_to_na(issue$media$image_square_100x100, "character"),
        created_at = ymd_hms(null_to_na(issue$created_at, "character")),
        closed_at = ymd_hms(null_to_na(issue$closed_at, "character")),
        reopened_at = ymd_hms(null_to_na(issue$reopened_at, "character")),
        updated_at = ymd_hms(null_to_na(issue$updated_at, "character"))
      )
    }
  get_num_comments <- function(comment_url){
    GET(comment_url) %>% content() %>% .$comments %>% length() %>% as.integer()
  }
  
  # Get initial api response
  # Extract num pages
  # If num pages > 1, loop through remaining pages
    # Get api response, append to original
  initial_url <- 
    paste0("https://seeclickfix.com/api/v2/issues", 
           "?min_lat=", min_lat, "&min_lng=", min_lng,
           "&max_lat=", max_lat, "&max_lng=", max_lng)
  initial <- content(GET(initial_url))
  return_df <- map_df(initial$issues, get_issue_attributes)
  num_pages <- initial$metadata$pagination$pages
  temp_list <- vector(mode = "list", length = num_pages)
  cat(paste0(1, " of ", num_pages), "\n")
  temp_list[[1]] <- initial
  if(num_pages > 1){
    for(i in 2:num_pages){
      cat(i, "of", num_pages, "\n")
      temp_list[[i]] <- content(GET(temp_list[[i-1]]$metadata$pagination$next_page_url))
      return_df <- bind_rows(return_df, map_df(temp_list[[i]]$issues, get_issue_attributes))
    }
  }
  
  return_df$num_comments <- map_int(return_df$comment_url, get_num_comments)
  
  return(list(raw_data = temp_list, parsed_data = return_df))
}

get_comments <- function(parsed_data){
  num_rows <- nrow(parsed_data)
  comments_list <- vector(mode = "list", length = num_rows)
  for(i in 1:num_rows){
    cat("Getting Comment", i, "of", num_rows, "\n")
    comments_list[[i]] <- content(GET(parsed_data$comment_url[i]))
  }
  
  get_comment_element <- function(comment_subgroup){
    map_df(comment_subgroup$comments, 
           ~data_frame(
             issue_url = null_to_na(.$issue_url, "character"),
             comment_text = null_to_na(.$comment, "character"),
             comment_created_at = null_to_na(.$created_at, "character"),
             comment_updated_at = null_to_na(.$updated_at, "character"),
             commenter_id = null_to_na(.$commenter$id, "integer"),
             commenter_name = null_to_na(.$commenter$name, "character"),
             commenter_role = null_to_na(.$commenter$role, "character"),
             comment_media_image_full = null_to_na(.$media$image_full, "character"),
             comment_media_image_square = null_to_na(.$media$image_square, "character")
           ))
  }
  
  comments_parsed <- map_df(comments_list, get_comment_element)

  return(list(comments_raw = comments_list, comments_parsed = comments_parsed))
}

get_atl311_issue <- function(issue_id){
  require(rvest); require(tidyverse)
  atl_311_url <- paste0("https://www.atl311.com/?page_id=103&SRNumber=",
                        issue_id, "&Email=")
  cat("Reading atl311 page", atl_311_url, "\n")
  p <- read_html(atl_311_url)
  p2 <- p %>% html_nodes(".contact-form") %>% html_nodes(".atl_form_row")
  vec <- p2 %>% html_nodes(".atl_right") %>% html_text()
  names(vec) <- p2 %>% html_nodes(".atl_left") %>% html_text() %>% str_replace(":$", "")
  #if(as.numeric(vec[9]) > 0 & as.numeric(vec[9]) %% 1 != 0) stop("Error: SLA Response Time is not integer")
  return(as.list(vec))
}

# Test bbox1: couple blocks around jackson st bridge (50 issues as of 7/8)
min_lat=33.755451; min_lng=-84.380225; max_lat=33.763122; max_lng=-84.369582
test1 <- get_issues_in_bbox(min_lat, min_lng, max_lat, max_lng)
# Test bbox2: most of intown ATL
min_lng = -84.417175; min_lat = 33.739412; max_lng = -84.344734; max_lat = 33.791071
test2 <- get_issues_in_bbox(min_lat, min_lng, max_lat, max_lng)
test2_comments <- get_comments(test2$parsed_data)

# Test bbox3: entire ATL perimeter
min_lng=-84.510559; min_lat=33.615345; max_lng=-84.222168; max_lat=33.928131
entire_perimeter <- get_issues_in_bbox(min_lat, min_lng, max_lat, max_lng)
entire_perimeter_comments <- get_comments(entire_perimeter$parsed_data)
atl311_request_comments <- 
  entire_perimeter_comments[["comments_parsed"]] %>% 
  filter(str_detect(commenter_name, "311") & str_detect(comment_text, "\\d{9,}")) %>%
  mutate(service_request_id = str_extract(comment_text, "\\d{9,}"))
atl311_requests <- atl311_request_comments %>% select(service_request_id) %>% distinct()
atl311_scrape_list <- vector(mode = "list", length = nrow(atl311_requests))
for(i in seq_along(atl311_scrape_list)){
  cat("Getting ATL311 Issue", i, "of", length(atl311_scrape_list), "\n")
  atl311_scrape_list[[i]] <- get_atl311_issue(atl311_requests$service_request_id[i])
}
atl311_issue_status <- bind_rows(atl311_scrape_list) %>% 
  select(-`\n                                    \n\t\t\t\t\t\n\t\t\t\t\t\n                                `)

# Service Requests mentioned in comments that do not exist (some of these are img filenames...need to filter them out of scraper above)
atl311_requests$service_request_id[which(is.infinite(unlist(lapply(atl311_scrape_list, function(x) max(unlist(lapply(x, length)))))))]

# Join comments to ATL 311 issue status
entire_perimeter_comments2 <- 
  left_join(entire_perimeter_comments$comments_parsed, 
            atl311_request_comments %>% select(comment_text, service_request_id), 
            by = "comment_text") %>% 
  left_join(atl311_issue_status, 
            by = c("service_request_id" = "Service Request #"))

write_rds(entire_perimeter_comments2, "entire_perimeter_comments2_20180708.rds")

write_csv(entire_perimeter_comments2, "ATL_seeclickfix_comments_20180708.csv", na = "")
write_csv(entire_perimeter$parsed_data, "ATL_seeclickfix_issues_20180708.csv", na = "")
