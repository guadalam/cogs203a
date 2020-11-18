#Exercise 2

rm(list=ls()) # Clear the environment
library(tidyverse)
library(modelr)

d = read_tsv("http://socsci.uci.edu/~rfutrell/teaching/MWPND_by_picture.txt")

#1.1.1
d%>%
  ggplot(data=., aes(x=RT_M, y=name_H))+
  geom_point()+
  facet_wrap(~lang)+
  ggsave("H_stat.pdf")

#1.1.2
RT_model = lm(name_H~RT_M, data=d)

#1.1.3
d%>%
  add_predictions(RT_model)%>%
  ggplot(data=., aes(x=RT_M))+
  geom_point(aes(y=name_H))+
  geom_line(aes(y=pred))+
  facet_wrap(~lang)+
  ggsave("H_stat_regression.pdf")
#It seems like the RT's are systematically underestimated for Greek and
#Bulgarian by the regression. On the other hand, the regression systematically
#overestimates the RT's for German. 

#1.1.4
d%>%
  add_residuals(RT_model)%>%
  ggplot(data=., aes(x=RT_M))+
  geom_point(aes(y=resid))+
  facet_wrap(~lang)+
  ggsave("H_stat_residuals.pdf")
#Greek and Bulgarian both have their residuals skewed downwards in comparison 
#to the others. All language appear to still have a linear pattern.

#1.1.5
RT_model2 = lm(name_H~RT_M+lang, data=d)

d%>%
  add_predictions(RT_model2)%>%
  ggplot(data=., aes(x=RT_M))+
  geom_point(aes(y=name_H))+
  geom_line(aes(y=pred))+
  facet_wrap(~lang)+
  ggsave("H_stat_regression_by_lang.pdf")

d%>%
  add_residuals(RT_model2)%>%
  ggplot(data=., aes(x=RT_M))+
  geom_point(aes(y=resid))+
  facet_wrap(~lang)+
  ggsave("H_stat_residuals_by_lang.pdf")

#1.1.6
RT_model3 = lm(name_H  ~ RT_M*lang, data=d)

d%>%
  add_predictions(RT_model3)%>%
  ggplot(data=., aes(x=RT_M))+
  geom_point(aes(y=name_H))+
  geom_line(aes(y=pred))+
  facet_wrap(~lang)+
  ggsave("H_stat_regression_by_lang_slope.pdf")

d%>%
  add_residuals(RT_model3)%>%
  ggplot(data=., aes(x=RT_M))+
  geom_point(aes(y=resid))+
  facet_wrap(~lang)+
  ggsave("H_stat_residuals_by_lang_slope.pdf")

#1.1.7
d%>%
  add_residuals(RT_model3)%>%
  add_predictions(RT_model3)%>%
  write_csv("full_predictions.csv")

# # # # # # # # # # # # # # # # 

url = "https://tinyurl.com/y5fgh9mk"
d = read_csv(url)

#1.2.1
d2 = d%>%
  mutate(Theme.animate=if_else(Theme.animacy == "A", "animate", "inanimate"))%>%
  mutate(Recipient.animate=if_else(Recipient.animacy == "A", "animate", "inanimate"))%>%
  mutate(Theme.definite=if_else(Theme.definiteness == "Definite" | Theme.definiteness == "Definite-pn", "D", "I"))%>%
  mutate(Recipient.definite=if_else(Recipient.definiteness == "Definite" | Recipient.definiteness == "Definite-pn", "D", "I"))

#1.2.2
d2 %>%
  gather(key="Type_animacy", value="animacy", Theme.animate, Recipient.animate)%>%
  ggplot(aes(x=animacy, fill=Response.variable))+
  geom_bar(position="fill")+
  facet_wrap(~Type_animacy)+
  ggsave("animacy_bars.pdf")
  
#1.2.3
d2 %>%
  gather(key="Type_definiteness", value="definiteness", Theme.definite, Recipient.definite)%>%
  ggplot(aes(x=definiteness, fill=Response.variable))+
  geom_bar(position="fill")+
  facet_wrap(~Type_definiteness)+
  ggsave("definiteness_bars.pdf")

#1.2.4
#Animacy seems to have an effect such that an animate recipient prompts a D
#dative alternation while an inanimate theme also shows this effect.

#Definiteness seems to have a more pronounced effect for dative alternation such
#that a definite recipient skews responses for a D response. Theme definite also
#has an effect where an indefinite theme 

#1.2.5
d3 = d2%>%
  mutate(prediction=if_else(Response.variable == "D", 1, 0))

LR_model = glm(prediction~Theme.definite+Recipient.definite+Theme.animate+Recipient.animate, data=d3, family="binomial")

#1.2.6

logistic = function(x) {
  1/(1+exp(-x))
}

d3%>%
  add_predictions(LR_model)%>%
  gather(key="Type_animacy", value="animacy", Theme.animate, Recipient.animate)%>%
  ggplot(aes(x=animacy, y=logistic(pred)))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~Type_animacy)

d3%>%
  add_predictions(LR_model)%>%
  gather(key="Type_definiteness", value="definiteness", Theme.definite, Recipient.definite)%>%
  ggplot(aes(x=definiteness, y=logistic(pred)))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~Type_definiteness)

#1.2.7 



