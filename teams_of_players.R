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
  
process_team <- function(team_url, base_url) {
  team_page <- read_html(paste0(base_url, team_url))
  team_page |> html_elements("#block-ihf-theme-content > div > div > section > div > div:nth-child(1) > div > div.row") |>
    html_nodes("a") |> html_text2()
}

thelist <- team_urls

tmp <- thelist |> map(process_team, base_url = ihf_base, .progress = TRUE) |> list_c()
player_table <- str_split_fixed(tmp, "\n", n = 4) |> as_tibble(.name_repair = "unique")
names(player_table) <- c("FIRST_NAME", "SECOND_NAME", "CLUB", "POSITION")

player_table |> group_by(CLUB) |> summarise(N = n()) |> slice_max(n = 10, order_by = N)


