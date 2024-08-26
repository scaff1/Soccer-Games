library(fpp3)
library(dplyr)
library(stats)
library(lmtest)
library(ggplot2)
library(sandwich)
view(data)
data = project_econ_5336_1
# ------------------------ Win Away --------------------------------------------
win_away = (1:378)*0
for (j in (1:378))
{
  if (data$WinTeam[j] == 'A')
  {
    win_away[j] = 0
  }
}
data$win_away = win_away

# ------------------------ Win Home --------------------------------------------
data = project_econ_5336_1

win_home = (1:378)*0
for (j in (1:378))
{
  if (data$WinTeam[j] == 'H')
  {
    win_home[j] = 1
  }
}
data$win_home = win_home

view(data)

cor(data$Goal_H, data$win_home)
cor(data$Possession_H, data$Goal_H)
cor.test(data$Goal_V,data$Possession_H)

# ------------------------ First Goal --------------------------------------------

data$FirstGoal[data$FirstGoal == 'H'] = 1
data$FirstGoal[data$FirstGoal == 'A'] = 0

FirstGoal_H = (1:378)*0
for (j in (1:378))
{
  if (data$WinTeam[j] == 'H')
  {
    FirstGoal_H[j] = 1
  }
}
data$FirstGoal_H = FirstGoal_H


FirstGoal_A = (1:378)*0
for (j in (1:378))
{
  if (data$WinTeam[j] == 'A')
  {
    FirstGoal_A[j] = 0
  }
}
data$FirstGoal_A = FirstGoal_A

FirstGoal_N = (1:378)*0
for (j in (1:378))
{
  if (data$WinTeam[j] == 'N')
  {
    FirstGoal_N[j] = 0
  }
}
data$FirstGoal_N = FirstGoal_N

view(data)
modelHome = lm(win_home ~ Possession_H + Goal_H + YellowCards_H + RedCards_H + Penalties_H +FirstGoal, data)
summary(modelHome)
rg = lm(win_home ~ Goal_H, data )
summary(rg)

model = lm(Possession_H~ WinTeam, data)
summary(model)

# -------------------------- dummy --------------------------------------------------

modelAway = lm(win_away ~ Possession_V + Goal_V + YellowCards_V + RedCards_V + FirstGoal + Penalties_V, data)
summary(modelAway)

data$WinTeam[data$WinTeam == 'H'] = 1
data$WinTeam[data$WinTeam == 'A'] = 0
data$WinTeam[data$WinTeam == 'N'] = 2

model11 = lm(WinTeam ~ Possession_H + Goal_H + YellowCards_H + FirstGoal, data)
summary(model11)

data$uhat = model_away$residuals


ggplot(data,aes(x=Possession_V,y=uhat))+
  geom_point()

bptest(modelAway)



model_home = lm(win_home ~ Possession_H + Possession_V + Goal_H + Goal_V + YellowCards_V + 
             YellowCards_H + RedCards_V + RedCards_H + Penalties_V + Penalties_H + FirstGoal, data)
summary(model_home)


model_away = lm(win_away ~ Possession_H + Possession_V + Goal_H + Goal_V + YellowCards_V + 
                      YellowCards_H + RedCards_V + RedCards_H + Penalties_V + Penalties_H + FirstGoal, data)
summary(model_away)

bptest(model_away)
robust_model_away <- coeftest(model_away, vcov = vcovHC(model_away, type = "HC0"))
robust_model_away
View(data)
