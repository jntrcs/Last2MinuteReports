calls= list.files("Games/") %>%
  map_dfr(function(file){
    read_csv(glue("Games/{file}")) %>%
      pivot_wider(names_from = CallRatingName, values_from=N) %>%
      mutate(game_id = substr(file, nchar(file)-13, nchar(file)-4))
  }) %>%
  select(game_id, everything(), -`NA`) 

ref_scores=calls  %>% mutate(across(CC:IC, function(x)replace_na(x, 0)))%>% left_join(ref_df) %>%
  group_by(Ref) %>%
  summarize(Games_with_Reports=n(), Missed_Calls = sum(INC)+sum(IC), 
            #Missed_Calls_Per_Game = Missed_Calls/Games_with_Reports,
            Total_Calls= sum(INC+IC+CC+CNC),
            Total_Bad_Calls=sum(INC+IC),
            Incorrect_Ratio=sum(INC+IC)/(sum(INC+IC+CC+CNC))) %>%
  arrange(desc(Incorrect_Ratio)) %>%
  mutate(League_Rank=nrow(.):1, Ref=fct_rev(fct_inorder(Ref)))

ref_scores %>% filter(Ref %in% our_refs )


our_refs = c("Sean Corbin", "Michael Smith", "Brandon Adair")

ref_scores %>%
  ggplot(aes(y=Ref, x=Incorrect_Ratio))+geom_col(aes(fill=Ref%in%our_refs))+
  ggtitle("Ref by Last Two Minute Performance")+
  scale_x_continuous(labels=scales::label_percent(1))+
  xlab("Percent of calls deemed incorrect on last two minute report")+
  guides(fill=guide_legend(title="Refs in Jazz/Sixers Game"))+
  scale_fill_manual(values=c("forestgreen", "red"), labels=c("No", "Yes"))+
  theme_minimal()+
  theme(legend.position = "bottom")
