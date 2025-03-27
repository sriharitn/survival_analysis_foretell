library(readxl)
library(janitor)
library(foretell)
library(tidyr)
library(dplyr)
library(tidymodels)
library(svglite)
library(forcats)
library(ggthemes)
library(ggtext)

surv_bus <- read_xlsx("business_survival.xlsx") 

dat1 <- surv_bus$`Agriculture, forestry, fishing, and hunting`

dat2 <- dat1[1:6]

mod <- foretell::BdW(dat2,h=20)

mod

plot(x=0:10,dat1,type="o",xlim=c(0,30),ylim=c(0,100),bty="n",
     main = "Agriculture, forestry, fishing, and hunting",ylab=" Survival %")
lines(x=0:25,c(mod$fitted,mod$projected),col="red",lty=2)
abline(v=5,lty=2)

mod_xl <- exltrend(dat2,h=20)

matlines(x=0:25,rbind(mod_xl$fitted,mod_xl$projected))


## nested model
glimpse(surv_bus)

surv_nested <- surv_bus %>% 
  pivot_longer(!c(Year, `Age of establishment, in years`),
                                         names_to = "category", 
                                         values_to = "survival") %>% 
  clean_names() %>%
  group_by(category) %>%  
  nest()


mod.f <- function(df,h=10){
  df1 <- unlist(df[,3],use.names = F)
  mod <- BdW(df1,h=h)
  fit_for <- data.frame(year = min(df$year) : (max(df$year)+h), 
                        survival.f = c(mod$fitted,mod$projected),
                        actual = c(pull(df[,3]),rep(NA,h))
  )
  maxl <- mod$max.likelihood
  parms <- mod$params
  list(fcst = fit_for,maxl = maxl,parms = parms)
}  

mod.fbg <- function(df,h=10){
  df1 <- unlist(df[,3],use.names = F)
  mod <- BG(df1,h=h)
  fit_for <- data.frame(year = min(df$year) : (max(df$year)+h), 
                        survival.f = c(mod$fitted,mod$projected),
                        actual = c(pull(df[,3]),rep(NA,h))
  )
  maxl <- mod$max.likelihood
  parms <- mod$params
  list(fcst = fit_for,maxl = maxl,parms = parms)
} 

mod.surv <- surv_nested %>% mutate(model=map(data,mod.f))
mod.surv <- surv_nested %>% mutate(model=map(data,mod.fbg))



mod.surv %>% unnest_longer(model) %>%  filter(model_id == "parms") %>%
  select(category,model) %>%
  unnest_wider(model) %>%
  pivot_longer(!category)

forplot <- mod.surv %>% unnest_longer(model) %>% 
  filter(model_id == "fcst") %>% 
  select(category,model) %>% 
  unnest(model) %>% 
  pivot_longer(!c(category,year),names_to = "act_for",values_to = "survival") %>%
  filter(!is.na(survival)) %>% mutate(survival = round(survival,1))

#rowwise() %>% mutate(category = make_clean_names(category,case = "title")) %>%

lab1 <- forplot %>% group_by(category) %>%  filter(act_for == "actual") %>% slice_max(year)
lab2 <- forplot %>% group_by(category) %>%  filter(act_for == "survival.f") %>% slice_max(year)


forplot %>% 
  arrange(year,survival) %>% as.data.frame() %>% 
  mutate(category = fct_reorder2(category,year,survival,.desc = T)) -> forplot


unique(forplot$act_for)

levels(forplot$category)


ty <- forplot %>%  
  ggplot(mapping = aes(x=year,y=survival,group=act_for,col=act_for)) + 
  geom_line(aes(linetype=act_for))+
  geom_point(size=1)+
  geom_vline(xintercept = 2023,linetype="dotted") +
  geom_text(aes(x=year,y=survival,label=survival),data = lab1,nudge_x = 1.5,nudge_y = 5,size=3)+
  geom_text(aes(x=year,y=survival,label=survival),data = lab2,nudge_x = 0,nudge_y = 7,size=3)+
  scale_colour_manual(values=c("black", "darkgreen"))+
  annotate("text", x=2018, y=80, label= "Actual",colour = "black",size=3) + 
  annotate("text", x =2027, y=80, label = "Predicted",colour = "black",size=3)+
  xlab("Year")+
  ylab("Survival (%)")+
  facet_wrap(~factor(category),labeller = label_wrap_gen())+
  theme_pander()+
  theme(legend.position = "none")

ty

library(svglite)
ggsave("surv.svg",plot = ty,width = 14,height = 10)


