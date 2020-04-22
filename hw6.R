library(dplyr)
library(tidyverse)
install.packages("ggrepel")
library(ggrepel)

sheet1 = readxl::read_xlsx('KIB - Best in Show (public).xlsx', sheet='Best in show')
sheet2 = readxl::read_xlsx('KIB - Best in Show (public).xlsx', sheet='Best in show full sheet')

sheet2 = sheet2[-c(1:3),]
sheet2 = sheet2[,-c(2,4)]
names(sheet2)[1:5] = c("breed","category","score","pop_score1","pop_score2")
names(sheet2)[10] = "intel_rank"
names(sheet2)[30] = "sizes"
sheet2$score = as.numeric(sheet2$score)
sheet2$pop_score1 = as.numeric(sheet2$pop_score1)
sheet2$pop_score2 = as.numeric(sheet2$pop_score2)
sheet2$intel_rank = as.numeric(sheet2$intel_rank)
sheet2$sizes = factor(as.factor(sheet2$sizes), levels= c("small","medium","large"))
sheet2$category = as.factor(sheet2$category)
sheet2 = sheet2[!(is.na(sheet2$category)),]
sheet2["intelligence"] = "clever"
sheet2$intelligence[sheet2$intel_rank < 0.50] = "dumb"
sheet2["pop"] = 0 - sheet2$pop_score1
sheet2 = sheet2 %>%
  subset(select=c(1:3,74,39,75,4:5,10,30,6:9,11:29,31:38,40:73))
                              
xmid = 2.4
ymid = -80

p = sheet2 %>%
  ggplot(aes(x=score,y= pop, shape=intelligence, color=category)) +
  
  geom_segment(x = xmid, y = -165, xend = xmid, yend = 0, arrow=arrow(length=unit(.1,"inches"),ends="both",type="closed"), size=1, color="black") +
  geom_segment(x = 1.5, y = ymid, xend = 3.8, yend = ymid, arrow=arrow(length=unit(.1,"inches"),ends="last",type="closed"), size=1, color="black") +
  geom_segment(x = 0, y = ymid, xend = 1, yend = ymid, size=1,color="black") +
  
  annotate("text", x = 1, y = 0,label= "Inexplicably Overrated",hjust=0) +
  annotate("text", x = 3.7, y = 0, label = "Hot Dogs!",hjust=1) +
  annotate("text",x=1,y=-160,label="The Rightly Ignored",hjust=0) +
  annotate("text", x=3.7, y=-160,label="Overlooked Treasures",hjust=1) +
  
  annotate("text",x = 1.25,y=ymid+1,label= "our data score") +
  annotate("text",x = xmid,y=5,label= "popularity") +
  
  coord_cartesian(xlim=c(0.9,3.7),ylim=c(10,-160)) +
  
  geom_text_repel(aes(label=breed), show.legend =F) +
  geom_point(aes(shape=intelligence, size=sizes)) +
  
  labs(title= "Best in Show: The Ultimate Data Dog") +
  
  scale_color_discrete(name = c("")) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_discrete(name="Intelligence") +
  scale_size_discrete(name="Size") +
  
  theme_void() +
  
  theme(
    legend.position = "top",
    legend.box = "vertical"
  )
  
p

