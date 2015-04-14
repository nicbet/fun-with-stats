library(dplyr)
library(BradleyTerry2)

# Read Data
d1 = read.csv('win-loss-summary-2015.csv')
head(d1)

# Compute HOME Team Wins (1) or Loss (0)
d1$WIN <- with(d1 , ifelse (G.1 > G ,1,0))

# Show Data
head(d1)

# Sum up matchups
d2 <- summarize(group_by(d1, Home, Visitor), home.wins=sum(WIN), away.wins=sum(1 - WIN))
d2$Home <- data.frame(team = d2$Home, at.home = 1)
d2$Visitor <- data.frame(team = d2$Visitor, at.home = 0)
head(d2)



# Fit Bradley-Terry Model
model1 <- BTm(cbind(home.wins, away.wins), Home, Visitor, data=d2, id= "team")
model1
model2 <- update(model1, formula = ~ team + at.home)
model2

# Function to calc matchup prob
calcMatch <- function (t1,t2,mdl) {
  ex_t1 = mdl$coefficients[paste('team', t1,sep='')]
  ex_t2 = mdl$coefficients[paste('team', t2,sep='')]
  ex_home = mdl$coefficients['at.home']
  p_t1_beats_t2 = exp(ex_t1 - ex_t2 + ex_home) / (1+ exp(ex_t1 - ex_t2 + ex_home) )
  p_t1_beats_t2
}

# Senators Home Game
calcMatch('Ottawa Senators', 'Montreal Canadiens', model2)

# Canadiens Home Game
calcMatch('Montreal Canadiens', 'Ottawa Senators', model2)

calcG <- function(t1,t2,mdl) {
  a<- calcMatch(t1,t2,mdl)
  b <-calcMatch(t2,t1,mdl)
  c(a,b)
}

# All-In One ;-)
calcG('Ottawa Senators', 'Montreal Canadiens', model2)