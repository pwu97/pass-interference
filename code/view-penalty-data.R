## Filter by type of penalty, and then perform
## simiilar analysis from `10-15-20-penalties.R`

library(ggridges)
testnew_penalty_all <- filter(testnew_save, penalty_var == TRUE)

testnew_penalty_oh <- testnew_save[grep("Offensive Holding", testnew_save$play_desc),]
testnew_penalty_dpi <- testnew_save[grep("Defensive Pass Interference", testnew_save$play_desc),]
testnew_penalty_dh <- testnew_save[grep("Defensive Holding", testnew_save$play_desc),]
testnew_penalty_opi <- testnew_save[grep("Offensive Pass Interference", testnew_save$play_desc),]

testnew_penalty_oh[is.na(testnew_penalty_oh)] <- 0
testnew_penalty_dpi[is.na(testnew_penalty_dpi)] <- 0
testnew_penalty_dh[is.na(testnew_penalty_dh)] <- 0
testnew_penalty_opi[is.na(testnew_penalty_opi)] <- 0

glm.probs.1 <- predict(glm.fit, testnew_penalty_oh, type = "response")
glm.probs.1 <- data.frame(glm.probs.1, "Defensive PI")
colnames(glm.probs.1)[1] <- "prob"
colnames(glm.probs.1)[2] <- "type"

glm.probs.1

glm.probs.2 <- predict(glm.fit, testnew_penalty_dpi, type = "response")
glm.probs.2 <- data.frame(glm.probs.2, "False PI")
colnames(glm.probs.2)[1] <- "prob"
colnames(glm.probs.2)[2] <- "type"

glm.probs.2
glm.probs.2 <- na.omit(glm.probs.2)

glm.probs.3 <- predict(glm.fit, testnew_penalty_dh, type = "response")
glm.probs.3 <- data.frame(glm.probs.3, "3 PI")
colnames(glm.probs.3)[1] <- "prob"
colnames(glm.probs.3)[2] <- "type"

glm.probs.3
glm.probs.3 <- na.omit(glm.probs.3)

glm.probs.4 <- predict(glm.fit, testnew_penalty_opi, type = "response")
glm.probs.4 <- data.frame(glm.probs.4, "4 PI")
colnames(glm.probs.4)[1] <- "prob"
colnames(glm.probs.4)[2] <- "type"

glm.probs.4
glm.probs.4 <- na.omit(glm.probs.4)

a <- data.frame(testnew_penalty_oh$play_result, glm.probs.1[,1])
a[,3] <- "Offensive Holding"
colnames(a)[3] <- "type"
colnames(a)[1] <- "playresult"
colnames(a)[2] <- "prob"

b <- data.frame(testnew_penalty_dpi$play_result, glm.probs.2[,1])
b[,3] <- "dpi"
colnames(b)[3] <- "type"
colnames(b)[1] <- "playresult"
colnames(b)[2] <- "prob"

c <- data.frame(testnew_penalty_dh$play_result, glm.probs.3[,1])
c[,3] <- "dh"
colnames(c)[3] <- "type"
colnames(c)[1] <- "playresult"
colnames(c)[2] <- "prob"

d <- data.frame(testnew_penalty_opi$play_result, glm.probs.4[,1])
d[,3] <- "Opi"
colnames(d)[3] <- "type"
colnames(d)[1] <- "playresult"
colnames(d)[2] <- "prob"

all <- rbind(a, b, c, d)

pr <- ggplot(data=all, aes(x=prob, y=playresult, color=type)) + 
  geom_point(aes(color=type, size=type, shape=type)) +
  # scale_color_manual(values=c('dodgerblue','black','cyan','magenta')) +
  scale_size_manual(values=c(2,2,2,2)) +
  scale_shape_manual(values=c(1,1,1,1)) +
  labs(title="Result of Play vs. Catch Probability on Different Penalized Catch Plays",
       subtitle="Dashed horizontal black line represents the 15-yard mark",
       x="Predicted Catch Probability", y="Play Result (yards)", color="Penalty Observed") +
  scale_color_manual(labels = c("Defensive Holding", "Defensive PI","Offensive Holding","Offensive PI"), values = c("dodgerblue", "orange","seagreen4","palevioletred3")) +
  guides(size=FALSE, shape=FALSE) +
  geom_hline(yintercept=15, linetype='dashed', color = "black") +
  theme_bw()
pr

b[,2]

ggplot(all, aes(x = playresult, y = type)) + 
  geom_density_ridges(rel_min_height = 0.01, aes(color=type)) +
  scale_color_manual(labels = c("Defensive Holding", "Defensive PI","Offensive Holding","Offensive PI"), values = c("dodgerblue", "orange","seagreen4","palevioletred3")) +
  theme_bw()

