library("ggplot2")

library(magrittr)

# const3ants
axis_begin  <- 1
axis_end    <- 9
total_ticks <- 9

# DATA ----
# point to plot

# x <- c(9, 4.25, 6, 8.5, 8.75, 9, 8.75, 9, 5, 8.5)
# y <- c(6.75, 5.75, 5.5, 6.5, 5.5, 2.75, 6.75, 7.25, 4.5, 4.75)
# 
# x1 <- c(1.75, 3.75, 3.75, 3.75, 3, 3, 5.5, 1.5, 4, 1.75)
# y1 <- c(8.25, 7.25, 7, 4.5, 7.25, 7.25, 5.75, 8.75, 6.25, 8)
# 
# 
# x2 <- c(7, 3.25, 7, 4, 7.75, 7, 8.5, 6.5, 7.75, 8.5)
# y2 <- c(7.25, 8.25, 5.75, 5.5, 5, 5.5, 8.75, 6, 7.5, 7)

#   x - valence, y-arousal
x0 <- c(4,
        2,
        3,
        5,
        3,
        1)

y0 <- c(8,
        7,
        7,
        3,
        9,
        7)

#   x - valence, y-arousal
x1 <- c(4,
        1,
        4,
        2,
        2,
        2,
        2.5)
y1 <- c(5.6,
        5.4,
        4.3,
        4.5,
        3.3,
        6.3,
        4.9)

#   x - valence, y-arousal
x2 <- c(2.33,
        6,
        4.67,
        5.33,
        5,
        3.33,
        5,
        1.67)
y2 <- c(7.67,
        6,
        5.33,
        4.67,
        6.33,
        6.67,
        4.67,
        8.67)

#   x - valence, y-arousal
x3 <- c(1.75,
        3.75,
        3.75,
        3.75,
        3,
        3,
        5.5,
        1.5,
        4,
        1.75)
y3 <- c(8.25,
        7.25,
        7,
        4.5,
        7.25,
        7.25,
        5.75,
        8.75,
        6.25,
        8)
 


group_A <- data.frame(x0, y0)
group_B <- data.frame(x1, y1)
group_C <- data.frame(x2, y2)
group_D <- data.frame(x3, y3)


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
ggplot(group_A) +
  
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
  ggtitle("'corridor'- fear")+
  
  # THE DATA POINT
  
  

  geom_point(group_A, mapping = aes(x0, y0, color='Group A'), size = 3) +

  geom_point(group_B, mapping = aes(x1,y1, color='Group B'), size = 3) +

  geom_point(group_C, mapping = aes(x2,y2, color="Group C"), size = 3) +

  geom_point(group_D, mapping = aes(x3,y3, color="Group D"), size = 3) +
  
  scale_colour_manual("Groups",breaks=c("Group A","Group B","Group C", "Group D"), values=c("Group A"="navy", "Group B"="red","Group C"="green", "Group D"="yellow")) +

  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("Example 2.png", width = 11, height = 10)



