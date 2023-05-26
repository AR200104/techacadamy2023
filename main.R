install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(readxl)
library(ggplot2)

class <-read.csv("data/class.csv")
index <- read.csv("data/index.csv")
zoo1 <- read_excel("data/zoo1.xlsx")
zoo2 <- read.csv("data/zoo2.csv")
zoo3 <- read.csv("data/zoo3.csv")

fish <- select(index, c("IUCN", "Species", "Country", "Value")) %>%
  filter(Species == "Fish", IUCN == "TOT_KNOWN") %>%
  select(Country, Value)

fish_plot <- ggplot(data = fish, aes(x = Country, y = Value)) + 
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Value") +
  ggtitle("Value of fish species") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10)))

print(fish_plot)


