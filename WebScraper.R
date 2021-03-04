library(rvest)
library(glue)
library(jsonlite)
library(tidyverse)
library(lubridate)

page_2021=read_html("https://official.nba.com/2020-21-nba-officiating-last-two-minute-reports/")

page_2021


game_links =page_2021 %>% html_nodes("div.entry-content") %>%
  html_nodes("a") 

report_links = game_links %>%
  html_attr("href") 



all_game_reports =page_2021 %>% html_nodes("div.entry-content") %>%
  html_nodes("p") %>% map_dfr(function(x){
    if(x %>% html_nodes("strong") %>% length==1 & x%>%html_nodes("a") %>% length >0){
      date = x %>% html_node("strong") %>% html_text()
      x %>% html_nodes("a") %>% map(function(y){
        game = y %>% html_text()
        link = y %>% html_attr("href")
        game_id = substr(link, nchar(link)-9, nchar(link))
        game_json=fromJSON(glue("https://official.nba.com/l2m/json/{game_id}.json"))
        game_json$l2m %>% mutate(game_id=game_id) %>% select(game_id, everything())
        
      })
    }
  })

write_csv(all_game_reports, file="GameReports.csv")


## get reffing data

days=seq.Date(ymd("2020-12-23"), Sys.Date(), by=1)

ref_df=days %>% map_dfr(function(day){
  refs=fromJSON(glue("https://official.nba.com/wp-json/api/v1/get-game-officials?&date={day}"))
  game_data =refs$nba$Table$rows
  if(is.data.frame(game_data)){
    game_data %>% select(game_id, official1, official2, official3, official4) %>%
    pivot_longer(official1:official4, names_to="Position", values_to="Ref") %>%
    drop_na()
  }
})

write_csv(ref_df, file="AllRefsAllGames.csv")
