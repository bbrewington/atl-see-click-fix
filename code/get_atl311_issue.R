get_atl311_issue <- function(issue_id){
  atl_311_url <- paste0("https://www.atl311.com/?page_id=103&SRNumber=",
                        issue_id, "&Email=")
  
  p <- read_html(atl_311_url)
  p2 <- p %>% html_nodes(".contact-form") %>% html_nodes(".atl_form_row")
  vec <- p2 %>% html_nodes(".atl_right") %>% html_text() %>% .[1:9]
  names(vec) <- p2 %>% html_nodes(".atl_left") %>% html_text() %>% .[1:9] %>% str_replace(":$", "")
  if(as.numeric(vec[9]) > 0 & as.numeric(vec[9]) %% 1 != 0) stop("Error: SLA Response Time is not integer")
  return(as_data_frame(t(vec)) %>% mutate(
    `SR Opened` = mdy(`SR Opened`), `SLA Response Time` = as.integer(`SLA Response Time`)
  ) %>%
    mutate(`SLA End Date` = `SR Opened` + days(`SLA Response Time`)))
}

# Test Case
#get_atl311_issue(1120975763)
