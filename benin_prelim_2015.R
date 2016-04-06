# benin prelim data

library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

district = c( 'Aliboni', 'Atacora', 'Atlanatic', 'Borgou', 'Collines', 
              'Couffo', 'Donga', 'Littoral', 'Mono', 'Oueme', 'Plateau', 'Zou')

DHS2012 = c(47.4,  51.1, 21.9, 34.1, 22.8, 37.8, 43.6, 22.6,12.2,14.7,22,26.1 )
MIS2015 = c(43,62.1,17.9,57.1,54.2,51.4,54.8,7.5,27.6, 9.8,44.1,49.1 )

b =  data.frame( district, DHS2012, MIS2015) 


b %>% gather( survey, Parasitemia, -district) %>%
  ggplot() +
  geom_bar( aes(x = district, y = Parasitemia, fill = survey), stat = 'identity', position = "dodge") 

bg = b %>% mutate( change = ifelse(MIS2015>DHS2012, 'bad', 'good'),
                   district = factor(district, levels = b[order(b$DHS2012), "district"])
                   ) %>%
  arrange( as.numeric(district)) %>%
  gather( survey, Parasitemia, -district, -change) %>%
  mutate( d = factor(as.numeric(district), order = TRUE),
          survey = factor(survey),
          Parasitemia = Parasitemia / 100) 



bg %>% filter(survey == "DHS2012") %>%
  ggplot() +
  geom_point( aes(x = survey, y = Parasitemia, shape  = survey), size = 3) +
  scale_shape_discrete(solid=T, guide=F) +
  scale_y_continuous("Parasitemia\n" , limits = c(0,.70), labels = percent) +
  scale_x_discrete("", drop=FALSE) + # show both surveys
  theme_bw() +
  theme( text = element_text(size=20, face = "bold") )
ggsave("benin_dhs2012.png")

bg %>% 
  ggplot() +
  geom_line( aes(x = survey, y = Parasitemia, group = district, color = change),  size = 2 )  +
  geom_point( aes(x = survey, y = Parasitemia, shape  = survey), size = 3) +
  scale_shape_discrete(solid=T, guide=F) +
  scale_y_continuous("Parasitemia\n" , limits = c(0,.70), labels = percent) +
  scale_x_discrete("", drop=FALSE) + # show both surveys
  theme_bw()+
  theme( text = element_text(size=20, face = "bold") , legend.position="none")
ggsave("benin_dhs2012_mis2015.png")



bg %>% 
  ggplot() +
  geom_line( aes(x = survey, y = value, group = district, color = change),  size = 2 )  +
  geom_point( aes(x = survey, y = value, shape  = survey), size = 2)  +
  facet_wrap( ~district, nrow = 1)

bg %>% 
  ggplot() +
  geom_line( aes(x = d, y = value, group = district, color = change),  size = 2 )  +
  geom_point( aes(x = d, y = value, shape  = survey), size = 3)  +
  xlab("District") + ylab("Parasitemia") +
  coord_flip()

bg %>% 
  ggplot() +
  geom_line( aes(x = district, y = value, group = survey, color = change),  size = 2 )  +
  geom_point( aes(x = district, y = value, shape  = survey), size = 2)  
 

# total
total = data.frame(survey = c("DHS2012", "MIS2015"), Parasitemia = c(28.4, 38.8))

total %>%  ggplot() +
  geom_line( aes(x = survey, y = Parasitemia) ,  group = "red" , color = "red",  size = 2 )  +
  geom_point( aes(x = survey, y = Parasitemia, shape  = survey), size = 3) +
  scale_y_continuous(limits = c(25,40))