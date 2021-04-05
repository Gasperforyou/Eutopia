library(ggplot2)
library("readxl")

emotions <- c("amusement",	"anger",	"contempt",	"disgust",	"fear",	"sadness",	"surprise",	"no emotion")
select_emotions <- c ("amusement_FC1",	"anger_FC1",	"contempt_FC1",	"disgust_FC1",	"fear_FC1",	"sadness_FC1",	"surprise_FC1", "no_emotions_FC1",
                      "amusement_FC2",	"anger_FC2",	"contempt_FC2",	"disgust_FC2",	"fear_FC2",	"sadness_FC2",	"surprise_FC2", "no_emotions_FC2",
                      "amusement_FC3",	"anger_FC3",	"contempt_FC3",	"disgust_FC3",	"fear_FC3",	"sadness_FC3",	"surprise_FC3", "no_emotions_FC3")


group_A <- read_excel("AllData Group C (2).xlsx", "Pt2 Data A")
group_A[1] <-  NULL
df <- data.frame(group = c("Group A"), labels=emotions,data=t(head(group_A, 1)))
df_A <- split(df, rep(1:3, length.out = nrow(df), each = ceiling(nrow(df)/3)))

group_B <- read_excel("AllData Group C (2).xlsx", "Pt2 Data B")
df <- data.frame(group = c("Group B"), labels = emotions, data = colMeans(group_B[select_emotions], na.rm = TRUE))
df_B <- split(df, rep(1:3, length.out = nrow(df), each = ceiling(nrow(df)/3)))


group_C <- read_excel("AllData Group C (2).xlsx", "Pt2 Data C")
# group_C[is.na(group_C)] <- 0
df <- data.frame(group = c("Group C"), labels = emotions, data = colMeans(group_C[select_emotions], na.rm = TRUE))
df_C <- split(df, rep(1:3, length.out = nrow(df), each = ceiling(nrow(df)/3)))

group_D <- read_excel("AllData Group C (2).xlsx", "Pt2 Data D")
df <- data.frame(group = c("Group D"), labels = emotions, data = colMeans(group_D[select_emotions], na.rm = TRUE))
df_D <- split(df, rep(1:3, length.out = nrow(df), each = ceiling(nrow(df)/3)))

fc1 <- dplyr::bind_rows(df_A[1], df_B[1], df_C[1], df_D[1])
fc2 <- dplyr::bind_rows(df_A[2], df_B[2], df_C[2], df_D[2])
fc3 <- dplyr::bind_rows(df_A[3], df_B[3], df_C[3], df_D[3])



fc1$labels <- factor(fc1$labels, levels = emotions)

fc2$labels <- factor(fc2$labels, levels = emotions)

fc3$labels <- factor(fc3$labels, levels = emotions)


# plot FC !
ggplot(fc1, aes(x=labels,y=data,fill=factor(group))) +
  geom_bar(stat="identity",position="dodge") + 
  ggtitle("“dance” - happiness")+
  xlab("Emotions") +
  ylab("Average rating") +
  scale_fill_discrete(name = "Groups", breaks=c("Group A", "Group B", "Group C", "Group D"))+
  ylim(0, 8)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Example1.png", width = 20, height = 10)

# plot FC !
ggplot(fc2, aes(x=labels,y=data,fill=factor(group))) +
  geom_bar(stat="identity",position="dodge") + 
  ggtitle("“corridor” - fear")+
  xlab("Emotions") +
  ylab("Average rating") +
  scale_fill_discrete(name = "Groups", breaks=c("Group A", "Group B", "Group C", "Group D"))+
  ylim(0, 8)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Example2.png", width = 20, height = 10)

# plot FC !
ggplot(fc3, aes(x=labels,y=data,fill=factor(group))) +
  geom_bar(stat="identity",position="dodge") + 
  ggtitle("“restaurant” - amusement")+
  xlab("Emotions") +
  ylab("Average rating") +
  scale_fill_discrete(name = "Groups", breaks=c("Group A", "Group B", "Group C", "Group D"))+
  ylim(0, 8)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Example3.png", width = 20, height = 10)



