#load packages
x1 <- c("tidytuesdayR", "tidyverse", "circlize", "countrycode", "cowplot", "extrafont")
x2 <- x1[!(x1 %in% installed.packages()[,"Package"])]
if(length(x2)) install.packages(x2, repos='http://cran.rstudio.com/')
#update.packages(ask = FALSE, oldPkgs = x1)
lapply(x1, require, character.only=TRUE)
loadfonts(device = "win")

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2022-03-08')

erasmus <- tuesdata$erasmus

#Data.Europa
#EU program for education, training, youth, and sport

sending <- erasmus %>% 
            filter(sending_country_code != receiving_country_code) %>% 
            group_by(sending_country_code) %>% 
            summarize(count = sum(participants)) %>% 
            arrange(-count) %>% 
            head(5)
receiving <- erasmus %>% 
              filter(sending_country_code != receiving_country_code) %>% 
              group_by(receiving_country_code) %>% 
              summarize(count = sum(participants)) %>% 
              arrange(-count) %>% 
              head(5)

top_country_codes <- unique(c(sending$sending_country_code, receiving$receiving_country_code))
top_countries<-countrycode(top_country_codes , origin="iso2c", destination="iso.name.en")
top_countries[5]<-"United Kingdom"

df <- erasmus %>% 
        mutate(to= countrycode(receiving_country_code, origin="iso2c", destination="iso.name.en"),
                from= countrycode(sending_country_code , origin="iso2c", destination="iso.name.en")) %>% 
        mutate(to = replace(to, receiving_country_code=="UK","United Kingdom"),
                from = replace(from, sending_country_code=="UK","United Kingdom"), 
                to = replace(to, receiving_country_code=="EL","Greece"),
                from = replace(from, sending_country_code=="EL","Greece")) %>% 
        group_by(from, to) %>% 
        summarize(count = sum(participants)) %>% 
        filter(from != to) %>% 
        arrange(-count) 
        
df2 <- df %>% 
        filter(from %in% top_countries & to %in% top_countries) %>% 
        arrange(-count)

pal <- c("#424B54", "#B38D97", "#D5ACA9", "#EBCFB2", "#C5BAAF", "#A4B494", "#519872")
pal2 <- c("#6A0136", "#3A405A", "#495867", "#C18C5D", "#CE796B", "#E7AD99", "#ECC8AF")
pal3 <- c("#40798C", "#1F363D", "#DB5A42", "#E3A587", "#A57F60", "#E8AE68", "#FFD275")
pal4 <- c("#606C38", "#619B8A", "#C42847", "#DE3C4B", "#000022", "#E28413", "#F0D9D1")

chordDiagram(df2, grid.col = pal)

p <- recordPlot()
as.ggplot(ggdraw(p))+
  labs(title = "EU Student Mobility (ERASMUS program)",
       subtitle = "2014 to 2020",
       caption = "#tidytuesday 03-08-2022 | Data from Data.Europa | Chart by @aftokuyama")+
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 11, margin = margin(t = 10)),
        plot.caption = element_text(size = 9, hjust = 0.95, margin = margin(b = 12)),
        plot.margin = margin(t = 20))

ggsave("2022_03_08.jpeg", height = 9, width = 9)
