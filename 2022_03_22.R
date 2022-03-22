#load packages
x1 <- c("tidytuesdayR", "tidyverse", "extrafont", "showtext", "ggrepel")
x2 <- x1[!(x1 %in% installed.packages()[,"Package"])]
if(length(x2)) install.packages(x2, repos='http://cran.rstudio.com/')
#update.packages(ask = FALSE, oldPkgs = x1)
lapply(x1, require, character.only=TRUE)

font_add_google(name = "Prata", family = "prata")
showtext_auto()

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2022-03-22')

baby <- tuesdata$babynames

cols <- c("#CB7C82", "#C39A32", "#5F666D", "#D7D9CE")

schitt <- baby %>% 
            filter(name == "Johnny" | name == "David" | name == "Moira" |
                   name == "Alexis") %>% 
            group_by(year, name) %>% 
            summarize(n = sum(n),
                      prop = sum(prop))

ann <- schitt %>% 
          filter(year == 2017)

ggplot() +
  geom_line(schitt, 
            mapping = aes(x = year, y = prop, color = name), size = 0.75) +
  labs(title = "Popularity of Schitt's Creek Character Names", 
       x = "\nYear\n", y = "\nProportion\n",
       subtitle = "Proportion of total births per year from 1880-2017",
       caption = "#tidytuesday 03-22-2022 | Data from babynames R package by Hadley Wickham | Chart by @aftokuyama") +
  geom_point(ann, mapping = aes(x = year, y = prop, color = name), size = 1.5) +
  scale_color_manual(values = cols) +
  expand_limits(x = 2025) +
  geom_text_repel(ann, mapping = aes(x = year, y = prop, label = name),
                  nudge_x = 15,
                  force = 8,
                  direction = "y",
                  hjust = 0,
                  segment.size = 0.3,
                  segment.angle = 10,
                  segment.ncp = 2,
                  segment.curvature = -0.1,
                  color = "white",
                  family = "prata",
                  size = 6) +
  theme(text = element_text(family = "prata"),
        plot.background = element_rect(fill = "black", color = "black", linetype = 0),
        panel.background = element_rect(fill = "black", color = NULL),
        panel.border = element_blank(),
        panel.grid = element_line(color = "#252525"),
        panel.grid.major = element_line(color = "#353535"),
        panel.grid.minor = element_line(color = "#101010"),
        axis.ticks = element_line(color = "#969696"),
        axis.title = element_text(color = "white", size = 25),
        axis.text = element_text(color = "#eaeaea", size = 20),
        axis.line = element_line(color = "#969696", linetype = 1),
        plot.title = element_text(hjust = 0.5, face="bold", size = 40, color = "#eaeaea"),
        plot.subtitle = element_text(hjust = 0.5, size = 25, margin = margin(t = 5), color = "#eaeaea"),
        plot.caption = element_text(size = 16, hjust = 0.95, margin = margin(b = 12), color = "#eaeaea"),
        plot.margin = margin(t = 20),
        legend.position = "none")
ggsave(filename = "2022_03_22.jpeg", dpi = 300, device = "jpeg",
       width = 7, height = 5)
        