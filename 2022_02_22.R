# Get the Data

#load packages
x1 <- c("tidytuesdayR", "tidyverse", "countrycode")
x2 <- x1[!(x1 %in% installed.packages()[,"Package"])]
if(length(x2)) install.packages(x2, repos='http://cran.rstudio.com/')
#update.packages(ask = FALSE, oldPkgs = x1)
lapply(x1, require, character.only=TRUE)

tuesdata <- tidytuesdayR::tt_load('2022-02-22')

freedom <- tuesdata$freedom %>% 
            mutate(country_code = countrycode::countrycode(
              country, origin = "country.name.en", destination = "iso3c"
            ))



world <- map_data("world") %>% 
            as_tibble() %>% 
            mutate(country_code = countrycode::countrycode(
                    region, origin = "country.name.en", destination = "iso3c")) %>% 
            filter(region != "Antartica") %>% 
            left_join(freedom, by = "country_code")

NF <- world %>% 
        filter(Status == "NF") %>% 
        mutate(CL = ((CL-min(CL))/(max(CL)-min(CL))))
Fr <- world %>% 
  filter(Status == "F") %>% 
  mutate(CL = ((CL-min(CL))/(max(CL)-min(CL))))
PF <- world %>% 
  filter(Status == "PF") %>% 
  mutate(CL = ((CL-min(CL))/(max(CL)-min(CL))))

#CL = Civil Liberties
#PL = political rights
#Status = Free (F), Not Free (NF), Partially Free (PF)

ggplot(world) +
  geom_polygon(NF, mapping = aes(x = long, y = lat, alpha = CL,
                                    group = group), color = "black", fill = "red") +
  geom_polygon(PF, mapping = aes(x = long, y = lat, alpha = CL,
                                    group = group), color = "black", fill = "yellow") +
  geom_polygon(Fr, mapping = aes(x = long, y = lat, alpha = CL,
                                    group = group), color = "black", fill = "green")
