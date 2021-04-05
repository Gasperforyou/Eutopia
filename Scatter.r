library("ggplot2")
library("readxl")
library(magrittr)

# const3ants
axis_begin  <- 1
axis_end    <- 9
total_ticks <- 9

# DATA ----
# point to plot

my_points <- read_excel("AllData Group C.xlsx", "Average pt1")
my_points <- head(my_points, -1)
group_names <- my_points[1]
rownames(my_points)<- t(group_names)
my_points[1] <- NULL 
colnames(my_points)<- c("FC1_valence", "FC1_arousal", "FC2_valence", "FC2_arousal", "FC3_valence", "FC3_arousal")



# chart junk data
tick_frame <- 
  data.frame(ticks = seq(axis_begin, axis_end, length.out = total_ticks), 
             zero=5) %>%
  subset(ticks != 5)

lab_frame <- data.frame(lab = seq(axis_begin, axis_end),
                        zero = 5) %>%
  subset(lab != 5)

tick_sz <- (tail(lab_frame$lab, 1) -  lab_frame$lab[1]) / 128

# PLOT ----
ggplot(my_points) +
  
  # CHART JUNK
  # y axis line
  geom_segment(x = 5, xend = 5, 
               y = lab_frame$lab[1], yend = tail(lab_frame$lab, 1),
               size = 0.5) +
  # x axis line
  geom_segment(y = 5, yend = 5, 
               x = lab_frame$lab[1], xend = tail(lab_frame$lab, 1),
               size = 0.5) +
  # x ticks
  geom_segment(data = tick_frame, 
               aes(x = ticks, xend = ticks, 
                   y = zero, yend = zero + tick_sz)) +
  # y ticks
  geom_segment(data = tick_frame, 
               aes(x = zero, xend = zero + tick_sz, 
                   y = ticks, yend = ticks)) + 
  
  # labels
  geom_text(data=lab_frame, aes(x=lab, y=zero, label=lab),
            family = 'Times', vjust=1.5) +
  geom_text(data=lab_frame, aes(x=zero, y=lab, label=lab),
            family = 'Times', hjust=1.5) +
  
  xlab("Arousal") +
  ylab("Valence") +
  ggtitle("Arousal and valance score")+
  
  # THE DATA POINT
  
    geom_point(aes(x = FC1_valence,y = FC1_arousal, color='dance', shape=as.factor(c(15, 16, 17, 18))), size=4) +
      
    geom_point(aes(x = FC2_valence,y = FC2_arousal, color='corridor', shape=as.factor(c(15, 16, 17, 18))), size = 4) +
      
    geom_point(aes(x = FC3_valence,y = FC3_arousal, color="restaurant", shape=as.factor(c(15, 16, 17, 18))), size = 4) +
  

  
  scale_colour_manual("Film clips",breaks=c("dance","corridor","restaurant"), values=c("dance"="navy", "corridor"="red","restaurant"="green")) +
  scale_shape_manual("Group Names",values = c(15, 16, 17, 18 ), labels=t(group_names[1])) +
  
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("Example4.png", width = 11, height = 10)



