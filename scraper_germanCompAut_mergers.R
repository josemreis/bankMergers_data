########################################################################

# file: scraper_germanCompAut_mergers.R

# Author: J. M. Reis

# Date: 28/12/2018

# Purpose: scrape all relevant data on merger decisions from the Bundeskartellamt website.
########################################################################

#### Setting things up----------------------------------------------------------------------

## Load the relevant packages
require(tidyverse)
require(rvest)
require(xlsx)
require(pdftools)

## data repo
if(!dir.exists("data_repo")){
  dir.create("data_repo")
}

# germany repo
if(!dir.exists("data_repo/germany")){
  dir.create("data_repo/germany")
}


#### Scrape the relevant case metadata----------------------------------------------------------------------

## set the base page
query_url <- "https://www.bundeskartellamt.de/SiteGlobals/Forms/Suche/Entscheidungssuche_Formular.html?nn=3589936&cl2Categories_Format=Entscheidungen&gts=3598628_list%253Dheader_text_sort%252Basc&cl2Categories_Arbeitsbereich=Fusionskontrolle&resultsPerPage=45&sortOrder=score+desc%2C+dateOfIssue_dt+desc"

## subpage vector
subpages_url <- query_url %>%
  read_html() %>% 
  html_nodes(xpath = '//li[following-sibling::li[@class = "forward"]]/a') %>%
  html_text() %>%
  as.numeric() %>%
  max(.) %>%
  seq_len(.) %>%
  paste0("https://www.bundeskartellamt.de/SiteGlobals/Forms/Suche/Entscheidungssuche_Formular.html?nn=3589936&cl2Categories_Format=Entscheidungen&gts=3598628_list%253Dheader_text_sort%252Basc&gtp=3598628_list%253D", .,"&cl2Categories_Arbeitsbereich=Fusionskontrolle&resultsPerPage=45&sortOrder=score+desc%2C+dateOfIssue_dt+desc")


### scrape the case table
cases_table <- map_df(subpages_url, function(page){
  
  ### extract the cases from the table
  output <- page %>%
    read_html() %>%
    html_node("#searchResultTable") %>%
    html_table() %>%
    as_tibble() %>%
    mutate(case_page = page %>%
             read_html() %>%
             html_nodes("td a") %>%
             html_attr("href") %>%
             paste0("https://www.bundeskartellamt.de/" ,.) %>%
             str_trim()) %>%
    set_names(c("case_id", "parties", "date", "product_market", "decision", "case_page"))
  
  ### go to each case page and extract the decision text url
  output$decision_url <- map2_chr(output$case_page, output$case_id, function(page, id){
    
    print(paste0("Scraping case: ", id))
    
    decision_url <- try(page %>%
                          read_html() %>%
                          html_node(".FTpdf") %>%
                          html_attr("href") %>%
                          paste0("https://www.bundeskartellamt.de" ,.), silent = TRUE)
    
    if(class(decision_url) == "try-error"){
      
      decision_url <- NA_character_
      
    }
    
    
    return(decision_url)
    
    Sys.sleep(runif(2,1,3))
    
  })
  
  
  return(output)
  
})

### export it
save(cases_table,
           file = paste0("data_repo/germany/1_", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","germany_merger_cases.RData"))

write.xlsx(cases_table,
                 file = paste0("data_repo/germany/1_", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","germany_merger_cases.xlsx"))


#### Parse all the decisions-----------------------------------------------------

bka_data <- cases_table 

bka_data$decision_txt <- map2_chr(bka_data$decision_url, bka_data$case_id, function(dec_pdf, id){
  
  print(paste0("parsing case: ", id))
  
  ### nto a website, missing the url
  if(!is.na(dec_pdf) & str_detect(dec_pdf, "\\.pdf")){
      
      ## parse it
      parsed_txt <- try(pdf_text(dec_pdf) %>% 
                          paste(., collapse = "\r\n"), silent = TRUE)
      
      # if couldn't parse it, try ocr
      if(class(parsed_txt) == "try-error"){
        
        parsed_txt <- try(tesseract::ocr(dec_pdf) %>% 
                            paste(., collapse = "\r\n"), silent = TRUE)
        
        
      }
      
      # if yet again it fails, assign NA
      if(class(parsed_txt) == "try-error"){
        
        parsed_txt <- NA_character_
        
      }
      
    } else {
        
    
    ## no decision
    parsed_txt <- "no decision text"
    
    
    
  }
  
  
  ## print str for double checking
  str(parsed_txt, nchar.max = 1000)
  
  ## rest time for the server
  Sys.sleep(runif(2,1,3))
  
  return(parsed_txt)
  
})


### remove all the ocr pages
file.remove(list.files()[str_detect(list.files(), regex("\\.png", ignore_case = TRUE))])



### export it
save(bka_data,
     file = paste0("data_repo/germany/2_", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","germany_merger_cases.Rdata"))

write.xlsx(bka_data,
           file = paste0("data_repo/germany/2_", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","germany_merger_cases.xlsx"))

### Save each decision as a .txt file
## decision_repo
if(!dir.exists("data_repo/germany/decision_repo")){
  
  dir.create("data_repo/germany/decision_repo")
  
}

## write them and save them
map2(bka_data$case_id, bka_data$decision_txt, function(id, txt){
  
  print(paste0("parsing case: ", id)) 
  cat(txt, file = paste0("data_repo/germany/decision_repo/", str_replace_all(id, "\\/", "_"), ".txt"))
  
  })
