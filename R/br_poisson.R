library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# Tratamento dos dados originais: emover acentos; separar coluna do placar em duas;
#   transformar em tipo numérico; padronizar nomes das colunas
br <- brasileirao::matches %>%
  mutate(home = stringi::stri_trans_general(home, "Latin-ASCII")) %>%
  mutate(away = stringi::stri_trans_general(away, "Latin-ASCII")) %>%
  separate(col = score,
           into = c("HG","AG"),
           sep = "x") %>%
  mutate(across(
    .cols = ends_with("g"),
    .fns = as.numeric
  )) %>%
  janitor::clean_names()
#br %>% glimpse()

# Objeto para o modelo: selecionar o time, o adversário e os gols; adicionar
# uma coluna para informar se estes gols foram em casa ou fora
goal_model_data <- br %>%
  select(c("home","away","hg")) %>%
  rename(Team=home,Opponent=away,Goals=hg) %>%
  mutate(home=rep(1,nrow(br))) %>%
  bind_rows(br %>%
              select(c("away","home","ag")) %>%
              rename(Team=home,Opponent=away,Goals=ag) %>%
              mutate(home=rep(0,nrow(br))))

# Modelo: Poisson aproximando os gols a partir dos times e do local do jogo
poisson_model <- glm(formula = "Goals ~ Team + Opponent + home",
                     family = poisson(),
                     data = goal_model_data)

# Função para prever um placar: a função predict devolve o placar esperado pelo
# modelo. Na função, informar os dois time e o local (1 para casa, 0 para fora)
make_prediction <- function(team1,team2,is.home=1){
  home_rate <- predict(poisson_model,
                       type = "response",
                       newdata = data.frame(Team=team1,Opponent=team2,
                                            home=is.home))
  away_rate <- predict(poisson_model,
                       type = "response",
                       newdata = data.frame(Team=team2,Opponent=team1,
                                            home=ifelse(is.home==1,0,1)))
  print(paste(round(home_rate, digits = 2),"-",
              round(away_rate, digits = 2)))
  return(c(home_rate,away_rate))
}
game_pred<-make_prediction("Gremio","Coritiba")

# Simulação de uma partida: com a taxa informada pelo predict, gerar uma
# distribuição de possíveis placares de cada time, e retornar matricialmente
simulate_match <- function(home_team,away_team,max_goals=10){
  rates <- make_prediction(home_team,away_team)
  ho <- dpois(0:max_goals,lambda = rates[1])
  aw <- dpois(0:max_goals,lambda = rates[2])
  return(outer(ho,aw))
}

#Selecionar os times para a simulação e gerar o mapa de probabilidades
home_team<-"Bahia"
away_team<-"Fortaleza"
score_matrix <- simulate_match(home_team,away_team,5)
sm<-melt(score_matrix)
sm$Var1<-sm$Var1*-1
ggplot(sm,aes(x=Var2,y=Var1))+ # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = sm$value, label = round(sm$value, 2))) + # write the values
  scale_fill_gradient2(low = scales::muted("darkred"),
                       mid = "white",
                       high = scales::muted("midnightblue"),
                       midpoint = 0) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) +
  ggtitle("Mapa de probabilidades") +
  theme(legend.title=element_text(face="bold", size=14)) +
  scale_x_discrete(name=away_team) +
  scale_y_discrete(name=home_team) +
  labs(fill="Prob.")
