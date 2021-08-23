# Introduction #####

# Absenteeism at Work
# This R code was used for the article: https://www.littalics.com/absenteeism-at-work-a-practice-with-open-data/
# Data source: https://archive.ics.uci.edu/ml/datasets/Absenteeism+at+work


# Setting environment #####

rm(list=ls())
#setwd("~/...")

library(tidyverse)
library(plotly)
library(GGally)
library(gridExtra)
library(gganimate)
library(gifski)
library(magick)


# Loading and munging #####

dat <- read_csv("3 Absenteeism at work.csv")

#Several data entry errors were spotted for employee 29 
dat$Social_drinker[dat$ID == 29] <- 1 
dat$Son[dat$ID == 29] <- 2
dat$Education[dat$ID == 29] <- 3
dat$Month_of_absence[dat$Month_of_absence == 0] <- NA



# 1 - Absenteeism and Employee Characteristics  #####


# 1.1 Employee Age and Tenure: 

dat.fig1 <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Age, Service_time) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Age = mean(Age),
            Tenure = mean(Service_time)) %>%
  select(Absenteeism, Age, Tenure)

my_scatter <- function(data,mapping){
  ggplot(data=data, mapping=mapping) +
    geom_point(color="orange")
}
my_density <- function(data,mapping){
   ggplot(data=data,mapping=mapping) +
   geom_density(alpha=1,
               fill="orange")
}

fig1 <- ggpairs(dat.fig1,
        lower=list(continuous=my_scatter),
        diag=list(continuous=my_density))
fig1 <- fig1 + labs(title="Fig1: Absenteeism, Age and Tenure")
ggplotly(fig1)


# 1.2 Employee Body Attributes

dat.fig2 <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Weight, Height, Body_mass_index) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Weight = mean(Weight),
            Height = mean(Height),
            BMI = mean(Body_mass_index)) %>%
  select(Absenteeism, Weight, Height, BMI)

fig2 <- ggpairs(dat.fig2,
        lower=list(continuous=my_scatter),
        diag=list(continuous=my_density))
fig2 <- fig2 + labs(title="Fig2: Absenteeism and employee body attributes")
ggplotly(fig2)


# 1.3 Employee Social Behaviors

dat.fig3 <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Social_smoker, Social_drinker) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Smoking = mean(Social_smoker),
            Drinking = mean(Social_drinker)) %>%
  select(Absenteeism, Smoking, Drinking) %>%
  mutate(Smoking = as.factor(recode(Smoking, `0`="No", `1`="Yes")))  %>%
  mutate(Drinking = as.factor(recode(Drinking, `0`="No", `1`="Yes"))) 

fig3.1 <- ggplot(dat.fig3, aes(x=Smoking, y=Absenteeism)) +
  geom_boxplot(aes(fill=Smoking)) +
  labs(title="Fig3.1: Absenteeism and social behaviors - Smoking", x="Smoking", y="Absenteeism (hours)") +
  theme(legend.position = "none")
ggplotly(fig3.1)

fig3.2 <- ggplot(dat.fig3, aes(x=Drinking, y=Absenteeism)) +
  geom_boxplot(aes(fill=Drinking)) +
  labs(title="Fig3.2: Absenteeism and social behaviors - Drinking", x="Drinking", y="Absenteeism (hours)") + 
   theme(legend.position = "none")
ggplotly(fig3.2)

grid.arrange(fig3.1, fig3.2, nrow=1)


# 1.4 Employee Family Members

dat.fig4 <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Son, Pet) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Children = mean(Son),
            Pets = mean(Pet)) %>%
  mutate(Children = recode_factor(Children, `0`="No kids", `1`="One kid", `2` = "Two kids plus",
                                   `3` = "Two kids plus", `4` = "Two kids plus")) %>%
  mutate(Pets = recode_factor(Pets, `0`="No pets", `1`="One pet", `2` = "Two pets plus",
                                   `3` = "Two pets plus", `4` = "Two pets plus",
                                   `5` = "Two pets plus", `6` = "Two pets plus",
                                   `7` = "Two pets plus", `8` = "Two pets plus",)) %>%
  select(Absenteeism, Children, Pets)

fig4.1 <- ggplot(dat.fig4, aes(x=Absenteeism, fill=Children, color=Children)) +
  geom_histogram(bins=10,alpha=0.2) +
  transition_states(Children) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  labs(title="Fig4.1: Absenteeism and family members - Children", x="Absenteeism (hours)")
animate(fig4.1,renderer=magick_renderer())

fig4.2 <- ggplot(dat.fig4, aes(x=Absenteeism, fill=Pets, color=Pets)) +
  geom_histogram(position="identity",bins=10,alpha=0.2) +
  transition_states(Pets) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  labs(title="Fig4.2: Absenteeism and family members - Pets", x="Absenteeism (hours)")
animate(fig4.2, renderer=magick_renderer())



# 2 - Absenteeism and Work Characteristics   #####


# 2.1 Absenteeism, Workload and Commute Distance

dat.fig5 <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Work_load_Average_per_day, Distance_from_Residence_to_Work, Education) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Workload = mean(Work_load_Average_per_day ),
            Distance = mean(Distance_from_Residence_to_Work),
            Education = mean(Education)) %>%
  mutate(Education = recode_factor(Education, `1`="Low", `2`="Mid", `3` = "High")) %>%
  select(Absenteeism, Workload, Distance, Education)

fig5.1 <- ggplot(dat.fig5, aes(x=Workload, y=Absenteeism, fill=Education, color=Education)) +
  geom_jitter(size=3) + geom_smooth(method = lm, se = FALSE, aes(colour=Education)) +
  labs(title="Fig5.1: Absenteeism and Workload by Education Levels", x="Workload", y="Absenteeism (hours)")
ggplotly(fig5.1)

fig5.2 <- ggplot(dat.fig5, aes(x=Distance, y=Absenteeism, fill=Education, color=Education)) +
  geom_jitter(size=3) + geom_smooth(method = lm, se = FALSE, aes(colour=Education)) +
  labs(title="Fig5.2: Absenteeism and Comute Distance by Education Levels", x="Comute Distance", y="Absenteeism (hours)")
ggplotly(fig5.2)



# 2.2 Absenteeism, Hitting targets and Disciplinary failures

dat.fig6 <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Hit_target, Disciplinary_failure) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Success = mean(Hit_target),
            Indicipline = mean(Disciplinary_failure)) %>%
  mutate(AbsenteeismDir = ifelse(Indicipline>0.25, -1*Absenteeism, Absenteeism))  %>%
  select(AbsenteeismDir, Success, Indicipline)

fig6.1 <- ggplot(dat.fig6, aes(x=Success, y=AbsenteeismDir)) +
  geom_segment( aes(x=Success, xend=Success, y=0, yend=AbsenteeismDir), color="grey") +
  geom_point( color="orange", size=2) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(title="Fig6: Absenteeism and Hit Target among Disciplined and Indiciplined Employee", x="Hit Target", y="Absenteeism (hours)")
ggplotly(fig6.1)



# 2.3 When Absenteeism Occures? Months and Seasons

dat.fig7 <- dat %>% 
  select(Absenteeism_time_in_hours, Month_of_absence) %>%
  group_by(Month_of_absence) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours))

fig7 <- ggplot(dat.fig7, aes(x=Month_of_absence, y=Absenteeism)) +
  geom_line(color=("red")) + geom_point(color=("red"))+
  scale_x_discrete(name ="Months", 
                    limits=c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")) +
  labs(title="Fig7: Monthly Absenteeism", x="Month", y="Absenteeism (hours)")
ggplotly(fig7)


