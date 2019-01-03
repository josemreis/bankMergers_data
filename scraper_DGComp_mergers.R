########################################################################

# file: scraper_DGComp_mergers.R

# Author: J. M. Reis

# Date: 28/12/2018

# Purpose: scrape all relevant data on merger decisions (NACE: K) from the DGComp

########################################################################

#### setting things up---------------------------------------------------------------------
## Load the relevant packages
require(tidyverse)
require(rvest)
require(xlsx)
require(pdftools)

## data repo
if(!dir.exists("data_repo")){
  dir.create("data_repo")
}

# DG Comp repo
if(!dir.exists("data_repo/DGcomp")){
  dir.create("data_repo/DGcomp")
}


#### Metadata scraper-----------------------------------------------------------

#### Strategy: We extracted a list of all mergers at the DG comp with the NACE "K" (financial services). We will load that data. Extract the case id, and add it to the following url "http://ec.europa.eu/competition/elojade/isef/case_details.cfm?proc_code=2_M_[numerical part of the id]"


### load the id's and prepare them for the url. Its 7352 cases. Keep only the cases relative to banking matters, using the relevant NACE Codes as regex "K\\.64(\\s+|\\.1\\s+|\\.9\\s+|\\.2|\\.19\\s+|\\.20\\s+|\\.91\\s+|\\.99\\s+)|K\\.66(\\s+|\\.1\\s+||\\.3)" as well as those without nace code for hand-coding.
dgcomp_merger <- read.csv("data_repo/DGcomp/interm_data/allMergers.csv") %>%
  set_names(names(.) %>%
              str_to_lower() %>%
              str_replace_all("ï\\.+", "")) %>%
  filter(str_detect(nace_code, "K\\.64(\\s+|\\.1\\s+|\\.9\\s+|\\.2|\\.19\\s+|\\.20\\s+|\\.91\\s+|\\.99\\s+)|K\\.66(\\s+|\\.1\\s+||\\.3)") | nchar(nace_code) < 1)

case_ids <- str_extract_all(dgcomp_merger$case_code, "(?<=M\\.).*") %>%
  str_trim()

case_pages <- paste0("http://ec.europa.eu/competition/elojade/isef/case_details.cfm?proc_code=2_M_", case_ids)


DGComp_cases <- map2_df(case_pages, case_ids, function(page, id){
  
  cat(paste0("scraping case: ", id, "\r\n\r\n"))
  
  ### scrape the case table
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
             paste(., collapse = "/"),
           case_page = page,
           case_id = id) %>%
    select(case_id, parties, everything())
  
  ### scrape the decision_url
  decision_url <- try(page %>%
                        read_html() %>%
                        html_nodes(xpath = '//a[@class = "ClassLink" and contains(@href, "decisions")]') %>%
                        html_attr("href"),
                      silent = TRUE)
  
  ## if decisions_url > 1
  if(length(decision_url) > 1 & class(decision_url) != "try-error"){
    
    decision_url <- paste(decision_url, collapse = " <--NEW URL--> ")
    
  }
  
  ## conditional assignement of the decision url
  case_details_table$decision_url <- ifelse(class(decision_url) != "try-error",
                                            decision_url,
                                            NA_character_)
  
  ### double checking
  print(case_details_table)
  
  print(decision_url)
  
  ### rest time for the server
  Sys.sleep(runif(2,1,3))
  
  return(case_details_table)
  
})


### Not published cases...
DGComp_cases <- DGComp_cases %>%
  mutate(decision_url = replace(decision_url, which(is.na(decision_url)), "not published"))

DGComp_data <- DGComp_cases

### export it
save(DGComp_data,
     file = paste0("data_repo/DGcomp/", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","dgComp_NACE_K_ALL_mergerCases.Rdata"))
write.xlsx(DGComp_data,
           file = paste0("data_repo/DGcomp/", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","dgComp_NACE_K_ALL_mergerCases.xlsx"))



#### Parse all the decisions-----------------------------------------------------

decision_txt <- map2_chr(DGComp_data$decision_url, DGComp_data$case_id, function(dec_pdf, id){
  
  print(paste0("parsing case: ", id))
  
  ### nto a website, missing the url
  if(!is.na(dec_pdf) & str_detect(dec_pdf, "europa")){
    
    ### check if there are more than one urls
    if(length(str_split(dec_pdf, " <--NEW URL--> ")[[1]]) < 2){
      
      ## parse it
      parsed_txt <- try(pdf_text(dec_pdf)[-1] %>% 
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
      
      ### then it has more than one pdf doc. 
      
      ## split them apart into a vector
      splitted_dec <- str_split(dec_pdf, " <--NEW URL--> ")[[1]]
      
      ## if any has _EN, then the multiple ones are just translated versions, keep the enlgish one.
      if(str_detect(splitted_dec, "\\_EN\\.pdf")){
        
        
        dec_pdf <- splitted_dec[str_detect(splitted_dec, "\\_EN\\.pdf")] 
        
        ## parse it
        parsed_txt <- try(pdf_text(dec_pdf)[-1] %>% 
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
        ## Parse each one individually, and then paste them together.
        
        # container var
        container <- rep(NA, length(splitted_dec))
        
        ## the loop
        for(i in seq_len(length(splitted_dec))){
        
          ## parse it
          parsed_txt <- try(pdf_text(splitted_dec[i])[-1] %>% 
                              paste(., collapse = "\r\n"), silent = TRUE)
        
        # if couldn't parse it, try ocr
        if(class(parsed_txt) == "try-error"){
          
          parsed_txt <- try(tesseract::ocr(splitted_dec[i]) %>% 
                              paste(., collapse = "\r\n"), silent = TRUE)
          
          
        }
        
        # if yet again it fails, assign NA
        if(class(parsed_txt) == "try-error"){
          
          
          parsed_txt <- NA_character_
          
        }
        
        ## assign to the container
        container[i] <- parsed_txt
        
        }
        
        ## remove NAs, and paste all the versions of the decisions together
        parsed_txt <- container %>%
          paste(., collapse = " <<--NEW VERSION-->> ")
        
        
      }
      
      
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

### Assign it to the dataset and save it
DGComp_data$decision_txt <- decision_txt

### remove all the ocr pages
file.remove(list.files()[str_detect(list.files(), regex("\\.png", ignore_case = TRUE))])

### export it
save(DGComp_data,
     file = paste0("data_repo/DGcomp/2_", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","dgComp_NACE_K_ALL_mergerCases.Rdata"))
write.xlsx(DGComp_data,
           file = paste0("data_repo/DGcomp/2_", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","dgComp_NACE_K_ALL_mergerCases.xlsx"))

### Save each decision as a .txt file
## decision_repo
if(!dir.exists("data_repo/DGcomp/decision_repo")){
  
  dir.create("data_repo/DGcomp/decision_repo")
  
}

## write them and save them
map2(DGComp_data$case_id, DGComp_data$decision_txt, function(id, txt) cat(txt,
                                                                          file = paste0("data_repo/DGcomp/decision_repo/", id, ".txt")))


#### Identify decisions related with German retail banks--------------------------------------------------------------------

#### Getting the decisions between german banks in the retail sector (relevant geographic market: Germany).

### first filtering method, get all the decisions written in German. So we code the language of the deciion using cld3::detect_language(decision_txt)
DGComp_data <- DGComp_data %>%
  mutate(decision_lang = cld3::detect_language(decision_txt))

### Second method of filtering, we extract all decisions which mention germany or which are in german.
DGComp_filtered <- DGComp_data %>% 
  filter((str_detect(decision_txt, regex("german|deutsch", ignore_case = TRUE)) & str_detect(decision_txt, regex("bank", ignore_case = TRUE))) | (str_detect(decision_txt, "no decision") == FALSE & decision_lang == "de")) %>%
  mutate(hc_finished = NA,
         relevant = NA,
         case_id = paste0("M.", case_id)) %>%
  select(hc_finished, everything())

## export
save(DGComp_filtered,
     file = paste0("data_repo/DGcomp/3_", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","dgComp_filteredGermanRetailBank_mergerCases.Rdata"))
write.xlsx(DGComp_filtered,
           file = paste0("data_repo/DGcomp/3_", str_extract(Sys.time(), "^.*?(?=\\s)"), "_","dgComp_filteredGermanRetailBank_mergerCases.xlsx"))

