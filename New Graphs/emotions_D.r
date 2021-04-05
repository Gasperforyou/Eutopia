library(ggplot2)
library("readxl")



average <- read_excel("AllData Group C (1).xlsx", "Average pt2")

average$Participant <- NULL

lab <- labels(average)[2]
lab_df <- data.frame(lab)
colnames(lab_df)[1]<- "emo"
lab<-substr(lab_df$emo,1,nchar(as.vector(t(lab_df$emo)))-4)


mean <- t(tail(average, 1))



df <- data.frame(dose=lab,len=mean)
colnames(df)[1]<- "custva"

df_t <- split(df, rep(1:3, length.out = nrow(df), each = ceiling(nrow(df)/3)))


df1 <- data.frame(df_t[1])
df1$X1.custva <- factor(df1$X1.custva, levels = df1$X1.custva)
df2 <- data.frame(df_t[2])
df2$X2.custva <- factor(df2$X2.custva, levels = df2$X2.custva)
df3 <- data.frame(df_t[3])
df3$X3.custva <- factor(df3$X3.custva, levels = df3$X3.custva)


# plot FC !
ggplot(df1, aes(x=X1.custva, y=X1.len, fill=X1.custva)) +
  geom_bar(stat="identity" ) + 
  ggtitle("“dance” - happiness")+
  xlab("Emotions") +
  ylab("Average rating") +
  scale_fill_discrete(name = "Emotions")+
  ylim(0, 8)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Example1.png", width = 20, height = 10)

# plot FC !
ggplot(df2, aes(x=X2.custva, y=X2.len, fill=X2.custva)) +
  geom_bar(stat="identity" ) + 
  ggtitle("“corridor” - fear")+
  xlab("Emotions") +
  ylab("Average rating") +
  scale_fill_discrete(name = "Emotions")+
  ylim(0, 8)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Example2.png", width = 20, height = 10)

# plot FC !
ggplot(df3, aes(x=X3.custva, y=X3.len, fill=X3.custva)) +
  geom_bar(stat="identity" ) + 
  ggtitle("“restaurant” - amusement")+
  xlab("Emotions") +
  ylab("Average rating") +
  scale_fill_discrete(name = "Emotions")+
  ylim(0, 8)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Example3.png", width = 20, height = 10)


