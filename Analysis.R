game_data = read_csv("GameReports.csv")
ref_df = read_csv("AllRefsAllGames.csv")

calls= game_data %>% group_by(game_id) %>%
  summarize(minutes = length(unique(PeriodName))*2,
            ICs = sum(CallRatingName=="IC")+sum(CallRatingName=="INC"),
            CCs = sum(CallRatingName=="CC")+sum(CallRatingName=="CNC"))


ref_scores=calls  %>% mutate(across(ICs:CCs, function(x)replace_na(x, 0)))%>% left_join(ref_df) %>%
  group_by(Ref) %>%
  summarize(Games_with_Reports=n(), 
            Minutes = sum(minutes),
            Missed_Calls = sum(ICs),
            #Missed_Calls_Per_Game = Missed_Calls/Games_with_Reports,
            Total_Calls= sum(ICs+CCs),
            Incorrect_Ratio=Missed_Calls/Total_Calls,
            Bad_Calls_Per_Minute = Missed_Calls/Minutes) %>%
  arrange(desc(Bad_Calls_Per_Minute)) %>%
  mutate(League_Rank=nrow(.):1, Ref=fct_rev(fct_inorder(Ref)))

ggplot(ref_scores, aes(x=Incorrect_Ratio, y=Bad_Calls_Per_Minute))+
  geom_point()

ref_scores %>% filter(Ref %in% our_refs )


our_refs = c("Sean Corbin", "Michael Smith", "Brandon Adair")

ref_scores %>%
  ggplot(aes(y=Ref, x=Bad_Calls_Per_Minute))+geom_col(aes(fill=Ref%in%our_refs))+
  ggtitle("Ref by Last Two Minute Performance")+
  xlab("Bad calls / minute")+
  guides(fill=guide_legend(title="Refs in Jazz/Sixers Game"))+
  scale_fill_manual(values=c("forestgreen", "red"), labels=c("No", "Yes"))+
  theme_minimal()+
  theme(legend.position = "bottom")
