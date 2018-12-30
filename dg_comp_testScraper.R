(page <- sample(case_pages, 1))

parties <- page %>% 
  read_html() %>%
  html_nodes("strong .ClassLink") %>%
  html_text() %>%
  str_replace_all(., "[[:cntrl:]]", "") %>%
  str_trim() %>%
  paste(., collapse = "<--NEW PARTY-->")

case_details_table <- page %>% 
  read_html() %>%
  html_nodes(xpath = "//table[@class = 'details']") %>%
  html_table() %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>%
  set_names(.[1,] %>%
              str_replace_all(., "[[:punct:]]", "") %>%
              str_to_lower() %>%
              str_replace_all(., "\\s+", "_")) %>%
  as_tibble() %>%
  slice(-1) %>%
  mutate(parties = page %>% 
           read_html() %>%
           html_nodes("strong .ClassLink") %>%
           html_text() %>%
           str_replace_all(., "[[:cntrl:]]", "") %>%
           str_trim() %>%
           paste(., collapse = "/")) %>%
  select(parties, everything())

decision_url <- try(page %>%
  read_html() %>%
  html_nodes(xpath = '//a[@class = "ClassLink" and contains(@href, "decisions")]') %>%
  html_attr("href"),
  silent = TRUE)

case_details_table$decision_url <- ifelse(class(decision_url) != "try-error",
                                          decision_url,
                                          NA_character_)


case_details_table

decision_url

Sys.sleep(3)
