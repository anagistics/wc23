library(rvest)
library(tidyverse)

ihf_base <- "https://www.ihf.info"
ihf_wc2023 <- "/competitions/men/308/28th-ihf-mens-world-championship-2023/101253"
ihfurl <- paste0(ihf_base, ihf_wc2023)

page <- read_html(ihfurl)

team_list <- function(src) {
  src |> html_element(css = "#block-views-block-teams-flags-list-block-1-2 > div > div")
}

team_urls <- page |> team_list() |> html_nodes("a") |> html_attr("href")
  
process_player <- function(player_url, base_url) {
  player_page <- read_html(paste0(base_url, player_url))
  tmp <- player_page |> html_elements("#block-ihf-theme-content > div > div > div > div > div.col-md-7") |>
    html_nodes("h3")
  elems <- tmp |> html_text2() 
  tibble(BIRTHDAY = elems[[3]], AGE = elems[[5]], HEIGHT = elems[[6]], WEIGHT = elems[[7]])
}

process_team <- function(team_url, base_url) {
  team_page <- read_html(paste0(base_url, team_url))
  tmp <- team_page |> html_elements("#block-ihf-theme-content > div > div > section > div > div:nth-child(1) > div > div.row") |>
    html_nodes("a") 
  data <- tmp |> html_text2()
  players <- str_split_fixed(data, "\n", n = 4) |> as_tibble(.name_repair = "unique")
  names(players) <- c("FIRST_NAME", "SECOND_NAME", "CLUB", "POSITION")
  players$HREF <- tmp |> html_attr("href")
  
  details <- players$HREF |> map_dfr(process_player, base_url = base_url, .progress = TRUE)
  bind_cols(players, details)
}

thelist <- team_urls[1:1]

player_table <- thelist |> map_dfr(process_team, base_url = ihf_base, .progress = TRUE) 

cntry_pos <- str_match(player_table$POSITION,"(?<COUNTRY>.+)\\s-\\s(?<POSITION>.+)") |> 
  as_tibble(.name_repair="unique")
player_table$COUNTRY <- cntry_pos$COUNTRY
player_table$POSITION <- cntry_pos$POSITION
clb_name <- str_match(player_table$CLUB,"Club:\\s(?<NAME>.+)") |> as_tibble(.name_repair = "unique")
player_table$CLUB <- clb_name$NAME

player_table |> group_by(CLUB) |> summarise(N = n()) |> slice_max(n = 15, order_by = N)


