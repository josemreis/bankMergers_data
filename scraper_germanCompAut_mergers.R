########################################################################

# file: scraper_germanCompAut_mergers.R

# Author: J. M. Reis

# Date: 28/12/2018

# Purpose: scrape all relevant data on merger decisions from the Bundeskartellamt website. Lexidale project. 

########################################################################

#### Setting things up----------------------------------------------------------------------

## Load the relevant packages
require(tidyverse)
require(rvest)
require(xlsx)

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
query_url <- "https://www.bundeskartellamt.de/SiteGlobals/Forms/Suche/EN/Entscheidungssuche_Formular.html?nn=3589950&cl2Categories_Format=Fallberichte+Entscheidungen&cl2Categories_Arbeitsbereich=Fusionskontrolle&resultsPerPage=45&sortOrder=score+desc%2C+dateOfIssue_dt+desc"


## extract the urls of the subpages
subpages_url <- query_url %>%
  read_html() %>% 
  html_nodes("#searchResultIndex li:nth-child(3) a , #searchResultIndex li:nth-child(2) a") %>%
  html_attr("href") %>%
  paste0("https://www.bundeskartellamt.de/", .) %>%
  c(query_url, .) # add the first page

### scrape the case table
cases_table <- map_df(subpages_url, function(page){
  
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
             str_trim())
  
  return(output)
  
  Sys.sleep(3)
})

### export it
write.xlsx(cases_table,
                 file = paste0("data_repo/germany/", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","germany_merger_cases.xlsx"))
