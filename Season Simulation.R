library(dplyr)

TeamStrength = data.frame(Team=paste0("Team"," ",c(1:16)),
                          Ranker=c(1:16),
                          MaxPoints = 30)

matchups = tidyr::crossing(TeamStrength$Team,
         TeamStrength$Team)

colnames(matchups) = c("Home","Away")

matchups = matchups %>%
  filter(Home != Away) %>%
  rename(Team1=Home,
         Team2=Away) %>%
  mutate(Team1Win = 
           as.numeric(gsub("Team ","",as.character(Team1))) /
          (as.numeric(gsub("Team ","",as.character(Team1)))+
             as.numeric(gsub("Team ","",as.character(Team2)))+1),
         Tie = 1 / (as.numeric(gsub("Team ","",as.character(Team1)))+
                      as.numeric(gsub("Team ","",as.character(Team2)))+1),
         Team2Win = 1 - Team1Win - Tie)

W1_data = data.frame()
W1_matchups = data.frame(
  Team=as.character(TeamStrength$Team))

Sim_ev = data.frame()

for(x in 1:10000){
  
set.seed(x)
  
while(nrow(W1_matchups)>0){
  
  H1 = sample(W1_matchups$Team,size=1,replace = F)
  
  W1_matchups = W1_matchups %>%
    filter(Team != H1)
  
  A1 = sample(W1_matchups$Team,size=1,replace = F)
  
  W1_data_temp = cbind(as.character(A1),
                       as.character(H1))
  colnames(W1_data_temp) = c("Home","Away")
  
  W1_data = rbind(W1_data,
                  W1_data_temp)
  
  colnames(W1_data) = c("Home","Away")
  
  W1_matchups = W1_matchups %>%
    filter(!Team %in% W1_data$Home&
             !Team %in% W1_data$Away)
}

colnames(W1_data) = c("Team1","Team2")

W1_results = W1_data %>%
  left_join(matchups,c("Team1",
                       "Team2")) %>%
  mutate(Result="")

for(p in 1:8){
W1_results[p,]$Result = 
  as.numeric(sample(0:2,
         size=1,
         prob=c(W1_results[p,]$Team2Win,
                W1_results[p,]$Tie,
                W1_results[p,]$Team1Win)))
}



W1_results2 = rbind(W1_results %>%
  select(Team1,Team2,Result) %>%
  rename(temp=Team1,
         Team1=Team2) %>%
  rename(Team2=temp) %>%
  select(Team1,Team2,Result) %>%
  mutate(Result = abs(as.numeric(Result)-2)),
  W1_results %>%
    select(Team1,Team2,Result) %>%
    mutate(Result=as.numeric(Result)))

matchups_u = matchups %>%
  anti_join((W1_results2 %>%
            select(-Result)),
            c("Team1",
              "Team2"))

points_w1 = W1_results2 %>%
  select(Team1, Result) %>%
  mutate(Ranker=as.numeric(gsub("Team ","",Team1)),
         games_played = 1,
         points_max=Result+28) %>%
  arrange(desc(points_max),
          desc(Ranker)) %>%
  ungroup()

current_stan = points_w1
  
best_team = points_w1[1,]

team_avail = matchups_u %>%
  filter(Team1 == best_team$Team1) %>%
  left_join(points_w1,c("Team1")) %>%
  arrange(desc(points_max),
          desc(Ranker)) %>%
  filter(points_max==max(points_max)) %>%
  top_n(1,-Team2Win) %>%
  select(Team1,
         Team2,
         Team1Win,
         Tie,
         Team2Win) %>%
  mutate(Result = as.numeric(sample(0:2,
                    size=1,
                    prob=c(Team2Win,
                           Tie,
                           Team1Win)))) %>%
  select(Team1,
         Team2,
         Result)

high_point = max(current_stan$Result)
max_pot = max(current_stan[2:16,]$points_max)


while(high_point<=max_pot){

Res = rbind(team_avail %>%
            rename(ResultP=Result),
            team_avail %>%
            rename(temp=Team1,
                     Team1=Team2) %>%
            rename(Team2=temp) %>%
              mutate(Result = abs(as.numeric(Result)-2)) %>%
              rename(ResultP=Result))

matchups_u = matchups_u %>%
  anti_join((Res %>%
               select(-ResultP)),
            c("Team1",
              "Team2"))

current_stan = current_stan %>%
  left_join((Res %>%
               select(Team1,ResultP)),
            "Team1") %>%
  group_by(Team1) %>%
  mutate(Result=sum(Result,ResultP,na.rm=T),
         games_played=case_when(is.na(ResultP)~games_played,
         TRUE~1+games_played),
         points_max=Result+2*(15-games_played)) %>%
  select(-one_of("ResultP")) %>%
  arrange(desc(points_max),
          desc(games_played),
          desc(Ranker))

best_team = current_stan[1,]

high_point = max(current_stan$Result)
max_pot = as.numeric(current_stan %>%
  filter(Team1 != best_team$Team1&
           Result != points_max) %>%
  arrange(desc(Result)) %>%
  ungroup() %>%
  summarise(max(points_max)))


team_avail = matchups_u %>%
  left_join(current_stan,c("Team1")) %>%
  filter(games_played<15) %>%
  arrange(desc(points_max),
          desc(Ranker)) %>%
  filter(points_max==max(points_max)) %>%
  ungroup() %>%
  top_n(1,Ranker) %>%
  top_n(1,-Team1Win) %>%
  select(Team1,
         Team2,
         Team1Win,
         Tie,
         Team2Win) %>%
  mutate(Result = as.numeric(sample(0:2,
                                    size=1,
                                    prob=c(Team2Win,
                                           Tie,
                                           Team1Win)))) %>%
  select(Team1,
         Team2,
         Result)

}

Result_temp = data.frame(Games=sum(current_stan$games_played)/2)
Sim_ev = rbind(Result_temp,Sim_ev)

}


