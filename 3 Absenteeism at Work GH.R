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


# 1.1 Employee Age and Tenure ===== 

dat.fig1 <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Age, Service_time) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Age = mean(Age),
            Tenure = mean(Service_time)) %>%
  select(Absenteeism, Age, Tenure)

my_scatter <- function(data,mapping){
  ggplot(data=data, mapping=mapping) +
    geom_point(color="#3b93a4")
}
my_density <- function(data,mapping){
   ggplot(data=data,mapping=mapping) +
   geom_density(alpha=0.65,
               fill="#3b93a4")
}

fig1 <- ggpairs(dat.fig1,
        lower=list(continuous=my_scatter),
        diag=list(continuous=my_density))
fig1 <- fig1 + labs(title="Figure 1: Absenteeism, Age and Tenure")
ggplotly(fig1)

ggsave("Absenteeism1.png")

# 1.2 Employee Body Attributes =====

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
fig2 <- fig2 + labs(title="Figure 2: Absenteeism and employee body attributes")
ggplotly(fig2)

ggsave("Absenteeism2.png")

# 1.3 Employee Social Behaviors =====

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
  labs(title="Figure 3: Absenteeism and Smoking", x="Smoking", y="Absenteeism (hours)") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#FFFFFF", "#3b93a4")) 
ggplotly(fig3.1)

fig3.2 <- ggplot(dat.fig3, aes(x=Drinking, y=Absenteeism)) +
  geom_boxplot(aes(fill=Drinking)) +
  labs(title="Figure 4: Absenteeism and Drinking", x="Drinking", y="Absenteeism (hours)") + 
   theme(legend.position = "none") +
  scale_fill_manual(values=c("#FFFFFF", "#3b93a4"))
ggplotly(fig3.2)

grid.arrange(fig3.1, fig3.2, nrow=1)

png("Absenteeism3-4.png", width=732, height=553)
print(grid.arrange(fig3.1, fig3.2, nrow=1))
dev.off()

table(dat.fig3$Smoking, dat.fig3$Drinking)
chisq.test(table(dat.fig3$Smoking, dat.fig3$Drinking))

# 1.4 Employee Family Members =====

dat.fig4a <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Son, Pet) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Children = mean(Son),
            Pets = mean(Pet)) %>%
  mutate(Children = recode_factor(Children, `0`="No kids", `1`="Have kids", `2` = "Have kids",
                                  `3` = "Have kids", `4` = "Have kids")) %>%
  mutate(Pets = recode_factor(Pets, `0`="No pets", `1`="Have pets", `2` = "Have pets",
                              `3` = "Have pets", `4` = "Have pets",
                              `5` = "Have pets", `6` = "Have pets",
                              `7` = "Have pets", `8` = "Have pets",)) %>%
  select(Absenteeism, Children, Pets)

fig4a.1 <- ggplot(dat.fig4a, aes(x=Children, y=Absenteeism)) +
  geom_boxplot(aes(fill=Children)) +
  labs(title="Figure 5: Absenteeism and Children", x="Children", y="Absenteeism (hours)") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#FFFFFF", "#3b93a4")) 
ggplotly(fig4a.1)

fig4a.2 <- ggplot(dat.fig4a, aes(x=Pets, y=Absenteeism)) +
  geom_boxplot(aes(fill=Pets)) +
  labs(title="Figure 6: Absenteeism and Pets", x="Pets", y="Absenteeism (hours)") + 
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#FFFFFF", "#3b93a4"))
ggplotly(fig4a.2)

grid.arrange(fig4a.1, fig4a.2, nrow=1)

png("Absenteeism5-6.png", width=732, height=553)
print(grid.arrange(fig4a.1, fig4a.2, nrow=1))
dev.off()


table(dat.fig4a$Children, dat.fig4a$Pets)
chisq.test(table(dat.fig4a$Children, dat.fig4a$Pets))

# 2 - Absenteeism and Work Characteristics   #####


# 2.1 Absenteeism, Workload and Commute Distance =====

dat.fig5 <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Work_load_Average_per_day, Distance_from_Residence_to_Work, Education) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Workload = mean(Work_load_Average_per_day ),
            Distance = mean(Distance_from_Residence_to_Work),
            Education = mean(Education)) %>%
  mutate(Education = recode_factor(Education, `1`="Low", `2`="Mid", `3` = "High")) %>%
  select(Absenteeism, Workload, Distance, Education)

fig5.1a <- ggplot(dat.fig5, aes(x=Workload, y=Absenteeism)) +
  geom_jitter(size=3, color="#3b93a4") + geom_smooth(method = lm, se = FALSE, color="black") +
  labs(title="Figure 7: Absenteeism & Workload", x="Workload", y="Absenteeism (hours)")
ggplotly(fig5.1a)

fig5.2a <- ggplot(dat.fig5, aes(x=Distance, y=Absenteeism)) +
  geom_jitter(size=3, color="#3b93a4") + geom_smooth(method = lm, se = FALSE, color="black") +
  labs(title="Figure 8: Absenteeism & Commute", x="Commute Distance", y="Absenteeism (hours)")
ggplotly(fig5.2a)

grid.arrange(fig5.1a, fig5.2a, nrow=1)

png("Absenteeism7-8.png", width=732, height=553)
print(grid.arrange(fig5.1a, fig5.2a, nrow=1))
dev.off()

fig5.1 <- ggplot(dat.fig5, aes(x=Workload, y=Absenteeism, fill=Education, color=Education)) +
  geom_jitter(size=3) + geom_smooth(method = lm, se = FALSE, aes(colour=Education)) +
  scale_color_manual(values=c("#92A9BD", "#3b93a4", "#072227")) +
  scale_fill_manual(values=c("#92A9BD", "#3b93a4", "#072227")) +
  labs(title="Figure 9: Absenteeism & Workload", subtitle="by Education Level",
       x="Workload", y="Absenteeism (hours)" ) +
  theme(legend.position="top")
ggplotly(fig5.1)

fig5.2 <- ggplot(dat.fig5, aes(x=Distance, y=Absenteeism, fill=Education, color=Education)) +
  geom_jitter(size=3) + geom_smooth(method = lm, se = FALSE, aes(colour=Education)) +
  scale_color_manual(values=c("#92A9BD", "#3b93a4", "#072227")) +
  scale_fill_manual(values=c("#92A9BD", "#3b93a4", "#072227")) +
  labs(title="Figure 10: Absenteeism & Commute", subtitle="by Education Level",
       x="Commute Distance", y="Absenteeism (hours)" ) +
  theme(legend.position="top")
ggplotly(fig5.2)

grid.arrange(fig5.1, fig5.2, nrow=1)

png("Absenteeism9-10.png", width=732, height=553)
print(grid.arrange(fig5.1, fig5.2, nrow=1))
dev.off()

# 2.2 Absenteeism, Hitting targets and Disciplinary failures =====

dat.fig6 <- dat %>% 
  select(ID, Absenteeism_time_in_hours, Hit_target, Disciplinary_failure) %>%
  group_by(ID) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours),
            Success = mean(Hit_target),
            Indicipline = mean(Disciplinary_failure)) %>%
  mutate(AbsenteeismDir = ifelse(Indicipline>0.15, -1*Absenteeism, Absenteeism))  %>%
  select(AbsenteeismDir, Success, Indicipline)

fig6.1 <- ggplot(dat.fig6, aes(x=Success, y=AbsenteeismDir)) +
  geom_segment( aes(x=Success, xend=Success, y=0, yend=AbsenteeismDir), color="grey") +
  geom_point( color="#3b93a4", size=2) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(title="Figure 11: Absenteeism and Target Hits", x="Hit Target", y="Absenteeism (hours)") +
  annotate("text", x = 90.5, y = 400, label = "Disciplined Employees", colour="#3b93a4", size=5) +
  annotate("text", x = 90.5, y = -200, label = "Undisciplined Employees", colour="#3b93a4", size=5) +
  annotate("rect", xmin = 88, xmax = 98, ymin = -400, ymax = 0, alpha = .1) 
ggplotly(fig6.1)

ggsave("Absenteeism11.png")

# 2.3 When Absenteeism Occures? Months and Seasons =====

dat.fig7 <- dat %>% 
  select(Absenteeism_time_in_hours, Month_of_absence) %>%
  group_by(Month_of_absence) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours))

fig7 <- ggplot(dat.fig7, aes(x=Month_of_absence, y=Absenteeism)) +
  geom_line(color="#3b93a4") + geom_point(color="#3b93a4", size=3)+
  scale_x_discrete(name ="Months", 
                    limits=c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Figure 12: Monthly Absenteeism", x="Month", y="Absenteeism (hours)")
ggplotly(fig7)

dat.fig8 <- dat %>% 
  select(Absenteeism_time_in_hours, Day_of_the_week) %>%
  group_by(Day_of_the_week) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours))

fig8 <- ggplot(dat.fig8, aes(x=Day_of_the_week, y=Absenteeism)) +
  geom_line(color="#3b93a4") + geom_point(color="#3b93a4", size=3)+
  scale_x_discrete(name ="DAYS", 
                   limits=c("Sun" ,"Mon","Tue","Wed","Thu","Fri", "Sat")) + ylim(200,1800) +
  labs(title="Figure 13: Daily Absenteeism", x="Days", y="Absenteeism (hours)")
ggplotly(fig8)

grid.arrange(fig7, fig8, nrow=1)

png("Absenteeism12-13.png", width=732, height=553)
print(grid.arrange(fig7, fig8, nrow=1))
dev.off()

dat.fig9 <- dat %>%
  select(Absenteeism_time_in_hours, Day_of_the_week, Month_of_absence) %>%
  group_by(Day_of_the_week, Month_of_absence) %>% 
  summarize(Absenteeism = sum(Absenteeism_time_in_hours))

fig9 <- ggplot(dat.fig9, aes(x=Day_of_the_week, y=Month_of_absence, fill=Absenteeism)) +
  geom_tile() + 
  scale_fill_gradient(low = "#EDEDED", high = "#3b93a4") +
  #theme_bw() +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_discrete(name ="Months", 
                   limits=c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")) +
  scale_x_discrete(name ="Days", 
                   limits=c("Sun" ,"Mon","Tue","Wed","Thu","Fri", "Sat")) +
  labs(title="Figure 14: Absenteeism by Days and Monthes - Heat Map") +
  geom_text(aes(label = Absenteeism), color = "white", size = 4) 

fig9

png("Absenteeism14.png")
print(fig9)
dev.off()