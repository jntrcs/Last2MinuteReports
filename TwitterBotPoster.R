library(tidyverse)
library(rvest)

team_prefix="UTA"

### Check for a game tonight
date= Sys.Date()
refs_assigned = jsonlite::fromJSON(glue::glue("https://official.nba.com/wp-json/api/v1/get-game-officials?&date={date}"))
game_table = refs_assigned$nba$Table$rows
if(team_prefix %in% game_table$home_team_abbr | team_prefix%in% game_table$away_team_abbr){
  
  ##Get Refs
  game = game_table %>% filter(home_team_abbr==team_prefix | away_team_abbr==team_prefix)
  
  tonights_refs = c(game$official1, game$official2, game$official3, game$official4)
  tonights_refs = tonights_refs[!is.na(tonights_refs)]
  
  
  ###Update Game Report File
  old_games = read_csv("GameReports.csv") %>%mutate(across(c(PCTime, VideolLink, posStart, posEnd), as.character))
  old_game_ids = unique(old_games$game_id)
  
  page_2021=xml2::read_html("https://official.nba.com/2020-21-nba-officiating-last-two-minute-reports/")
  
  
    
  urls = page_2021 %>% html_nodes("div.entry-content") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  urls=urls[str_detect(urls, "gameId=")]
  
  all_game_ids = urls%>%str_sub(nchar(.)-9, nchar(.))
  
  new_game_ids = setdiff(all_game_ids, old_game_ids)
  
  new_report_rows = new_game_ids %>% map_dfr(function(game_id){
    game_json=jsonlite::fromJSON(glue::glue("https://official.nba.com/l2m/json/{game_id}.json"))
    game_json$l2m %>% mutate(game_id=game_id) %>% select(game_id, everything())
  })
  
  all_reports = bind_rows(old_games, tibble(new_report_rows)) 
  
  all_reports %>% 
    write_csv("GameReports.csv")
  
  
  #Update Reffing File
  all_refs = read_csv("AllRefsAllGames.csv")
  earliest_date_to_fetch = max(all_refs$Date_Fetched)+1
  if(earliest_date_to_fetch<=Sys.Date()){
    dates_to_fetch = seq.Date(earliest_date_to_fetch, Sys.Date(), by=1)
    
    new_ref_rows=dates_to_fetch %>% map_dfr(function(day){
      refs=jsonlite::fromJSON(glue::glue("https://official.nba.com/wp-json/api/v1/get-game-officials?&date={day}"))
      game_data =refs$nba$Table$rows
      if(is.data.frame(game_data)){
        game_data %>% select(game_id, official1, official2, official3, official4) %>%
          pivot_longer(official1:official4, names_to="Position", values_to="Ref") %>%
          drop_na()
      }
    }) %>% filter(!game_id %in% all_refs$game_id) %>%
      mutate(Date_Fetched=Sys.Date())
    
    all_refs = all_refs %>%
      bind_rows(new_ref_rows) %>%
      write_csv("AllRefsAllGames.csv")
  }
  
  ### Calculate Ref Scores
  calls = all_reports %>% group_by(game_id) %>%
    summarize(minutes = length(unique(PeriodName))*2,
              ICs = sum(CallRatingName=="IC")+sum(CallRatingName=="INC"),
              CCs = sum(CallRatingName=="CC")+sum(CallRatingName=="CNC"))
  
  
  ref_scores=calls  %>% mutate(across(ICs:CCs, function(x)replace_na(x, 0))) %>%
    left_join(all_refs) %>%
    group_by(Ref) %>%
    summarize(Games_with_Reports=n(), 
              Minutes = sum(minutes),
              Missed_Calls = sum(ICs),
              Correct_Calls = sum(CCs),
              Total_Calls= sum(ICs+CCs),
              Incorrect_Ratio=Missed_Calls/Total_Calls,
              Bad_Calls_Per_Minute = Missed_Calls/Minutes) %>%
    arrange(desc(Bad_Calls_Per_Minute), Correct_Calls, Minutes, Ref) %>%
    mutate(League_Rank=nrow(.):1, Ref=fct_rev(fct_inorder(Ref)))
  
  #Get Twitter Token
  twitter_token <- rtweet::get_token()
  
  
  ##Compose Tweet
  our_ref_stats = ref_scores %>% filter(Ref %in% tonights_refs) %>% 
    mutate(Bad_Calls_Per_Minute=round(Bad_Calls_Per_Minute, 2))
  
  handles = read_csv("TwitterHandles.csv")
  
  opposing_team = if_else(game$home_team_abbr==team_prefix, game$away_team_abbr, game$home_team_abbr)
  opposing_team_handle = handles %>% filter(Abbreviation==opposing_team) %>% pull
  
  our_handle = handles %>% filter(Abbreviation==team_prefix) %>% pull
  
  intro = glue::glue("Today's {our_handle} vs. {opposing_team_handle} game will be reffed by")
  first_ref = paste0("\n\n1. ", our_ref_stats$Ref[1], ": averages ", our_ref_stats$Bad_Calls_Per_Minute[1],
                     " bad calls per minute in Last 2 Minute Reports, ranking #", our_ref_stats$League_Rank[1],
                     " of ", nrow(ref_scores), " NBA refs\n\n") 
  other_refs = paste0(2:nrow(our_ref_stats), ". ", our_ref_stats$Ref[-1], ": ", our_ref_stats$Bad_Calls_Per_Minute[-1],
                      " bad calls/min - #", our_ref_stats$League_Rank[-1], "/", nrow(ref_scores), collapse="\n\n")

  
  ##Check length and post tweet or tweets
  full_tweet_length = nchar(intro)+nchar(first_ref)+nchar(other_refs)
  
  ### Do a bunch of quality checks to make sure the tweet is in decent shape
  
  ready_to_post = TRUE
  if (nrow(our_ref_stats)<2)ready_to_post=F
  if (anyNA(our_ref_stats))ready_to_post=F
  if(str_count(intro, "@")!=2)ready_to_post=F
  
  if(ready_to_post){
    if (full_tweet_length<280){
      tweet=paste0(intro, first_ref, other_refs)
      #If we have the real estate, add the Jazz #takenote hashtag
      if(nchar(tweet)<269) tweet = paste0(tweet, "\n\n#takenote")
      rtweet::post_tweet(tweet, token=twitter_token)
    }else{
      if (nchar(intro)+nchar(first_ref)>280){
        first_tweet = paste0(substr(paste0(intro, first_ref), 1, 277), "...")
      }else{
        first_tweet = paste0(intro, first_ref)
      }
      if (nchar(other_refs)>280){
        second_tweet = paste0(substr(other_refs, 1, 277), "...")
      }else{
        second_tweet =  other_refs
      }
      rtweet::post_tweet(first_tweet, token=twitter_token)
      rtweet::post_tweet(second_tweet, token=twitter_token)
    }
  }else(stop("TWEET NOT POSTED. SOMETHING WENT WRONG"))
     
  
}else print("No game detected")

