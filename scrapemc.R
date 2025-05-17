library(tidyverse)
library(rvest)
library(httr)

scrape_123mc <- function(max_pages = NULL) {
  
  # Henter første side
  url <- "https://www.123mc.dk/brugt/motorcykel?r=soeg&ot=date&or=desc"
  page <- read_html(url)
  
  # Finder totale antal sider
  antal_sider <- page %>% 
    html_node(".btn.btn-default[title='Gå til sidste side']") %>%
    html_text(trim = TRUE) %>%
    as.numeric()
  
  # Hvis bruger har angivet et max, brug det
  if (!is.null(max_pages)) {
    antal_sider <- min(antal_sider, max_pages)
  }
  
  # Ganger hver side med 36 for at følge side logik
  sider <- (0:(antal_sider - 1)) * 36
  
  # Laver dataframe til at gemme resultater
  result_df <- data.frame(
    title = character(),
    price = numeric(),
    year = character(),
    km = numeric(),
    postnr = numeric(),
    scrape_date = as.Date(character()),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(sider)) {
    
    side <- sider[i]
    
    # Henter nuværende side
    url <- paste0("https://www.123mc.dk/brugt/motorcykel?r=soeg&ot=date&or=desc&p=", side)
    page <- read_html(url)
    
    # Henter alle rækker
    rows <- page %>% html_nodes("table tr")
    
    for (row in rows) {
      
      # Tjekker at <tr> indeholder 6 kolonner, altså en motorcykel
      cells <- html_nodes(row, "td")
      if (length(cells) == 6) {
        
        # Henter titel 
        title <- row %>%
          html_node(".a-text-brand-link, .a-text-brand-link-lg") %>%
          html_text(trim = TRUE)
        
        # Henter pris
        price <- row %>%
          html_node(".price_list_text") %>%
          html_text(trim = TRUE)
        price <- gsub("[^0-9]", "", price)
        
        # Henter årstal
        year <- row %>%
          html_node(".numeric.search-5-col.hidden-xs") %>%
          html_text(trim = TRUE)
        
        # Henter antal kørte km
        km <- row %>%
          html_node(".numeric.search-4-col.hidden-xs") %>%
          html_text(trim = TRUE)
        km <- gsub("[^0-9]", "", km)
        
        # Henter postnummer
        postnr <- row %>%
          html_nodes(".postnr-by-text") %>%
          html_text()
        postnr <- gsub("[^0-9]", "", postnr)
        
        # Henter scrape dato
        scrape_date <- Sys.Date()
        
        # Laver ny række med data og binder den på df
        new_row <- data.frame(
          title = title,
          price = as.numeric(price),
          year = year,
          km = as.numeric(km),
          postnr = as.numeric(postnr),
          scrape_date = scrape_date,
          stringsAsFactors = FALSE
        )
        
        result_df <- bind_rows(result_df, new_row)
      }
    }
    cat(paste0("Page ", i, " out of "), antal_sider, " complete \n")
  }
  
  result_df <- result_df %>% distinct()
  return(result_df)
}

result <- scrape_123mc()