library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(gridExtra)
library(plotly)
theme_set(theme_minimal())

Sys.setenv("plotly_username"="ldlathrop")
Sys.setenv("plotly_api_key"="QdhN5ajQHfbCUn7n3Myu")

death_penalty <- read.csv("1991_2017_individualFIPS.csv", header = TRUE, stringsAsFactors = FALSE)
str(death_penalty)
death_penalty$sent_date <- ymd(death_penalty$sent_date)
death_penalty$offns_date <- ymd(death_penalty$offns_date)

ggplot(death_penalty, aes(x=year)) +
  geom_bar()

state_dp <- ggplot(death_penalty, aes(x=state)) +
  geom_bar() +
  xlab(NULL)
state_dp + theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) 


homicides <- read.csv("CrimeTrendsInOneVar.csv", header = TRUE)
str(homicides)

homicides_gathered <- homicides %>%
  gather(state, count, -Year)
str(homicides_gathered)
state_hom <- ggplot(homicides_gathered, aes(x=state, y=count)) +
  geom_bar(stat = "identity") +
  xlab(NULL)
state_hom + theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0))

homicides_total <- homicides_gathered %>%
  group_by(Year) %>%
  summarise(total_hom = sum(count))

homicides_total <- homicides_total %>%
  rename(year = Year)


deathpen_total <- death_penalty %>%
  group_by(year) %>%
  count(defendant) %>%
  summarise(total_pen = sum(n))

hom_deathpen <- inner_join(deathpen_total, homicides_total, by = "year")

g1 <- ggplot(hom_deathpen, aes(x=year, y=total_hom)) +
  geom_line() + ylim(10000, 25000) + xlab(NULL) +
  ylab("Homicides")

g2 <- ggplot(hom_deathpen, aes(x=year, y=total_pen)) +
  geom_line() + ylim(0, 350) + xlab(NULL) +
  ylab("Death Penalties")

png("hom-dp.png", width = 640, height = 640)
grid.arrange(g1, g2, ncol=1, top = "U.S. Homicides/Death Penalties Over Time")
dev.off()

pops <- read.csv("statePops.csv", header = TRUE, stringsAsFactors = FALSE)
pops <- rename(pops, state = State)

homPerPop <- inner_join(pops, homicides_gathered, by = "state") %>%
  filter(Year == 2014) %>%
  mutate(rate = (Population / count) * 100000)

homRate <- read.csv("Homicide Rate Over Time.csv", header = TRUE, stringsAsFactors = FALSE)

library(stringr)
homRate <- homRate %>%
  gather(year, rate, -state) %>%
  mutate(year = str_extract(year, "[^X]+"))


homRate$year <- as.integer(homRate$year)
