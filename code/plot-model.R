## Fit and plot the models after running `generate-testnew.R`
## and `10-15-20-penalties.R`

library(ggpubr)

testnew <- na.omit(testnew_save)

hist(testnew_save$air_distance)
hist(testnew_save$sideline_distance)

testnew <- arrange(testnew, penalty_var)
testnew_save <- arrange(testnew_save, penalty_var)

testnew <- filter(testnew_save, play_result >= 0)

glm.fit <- glm(formula = completion ~ air_distance + time_to_throw +
              passer_speed + sideline_distance, data = testnew, family = binomial)

summary(glm.fit)

testnew_penalty_high <- grepl("PENALTY", testnew$play_desc)

testnew3 <- mutate(testnew, play_result >= 5)
testnew2 <- mutate(testnew, play_result >= 10)
testnew4 <- mutate(testnew, play_result >= 20)
glm.fit.2 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                 passer_speed, data = testnew2, family = binomial)
glm.fit.3 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed, data = testnew3, family = binomial)
glm.fit.4 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed, data = testnew4, family = binomial)


glm.probs <- predict(glm.fit, testnew, type = "response")

z <- data.frame(testnew$air_distance, glm.probs)
ggplot(data=z, aes(x=testnew.air_distance, y=glm.probs)) + 
  geom_point(aes(color=testnew_penalty_high)) +
  theme_bw()

glm.probs <- data.frame(glm.probs, "Spot_of_Foul")
colnames(glm.probs)[1] <- "prob"
colnames(glm.probs)[2] <- "type"

glm.pred <- ifelse(glm.probs > 0.5, 1, 0)


glm.probs.2 <- predict(glm.fit.all.pass.plays, testnew_penalty, type='response')

y <- data.frame(testnew$air_distance, glm.probs)
ad_plot <- ggplot(data=y, aes(x=testnew$air_distance, y=glm.probs)) + 
  geom_point(aes(color=testnew_penalty_high, size=testnew_penalty_high,
                           shape=testnew_penalty_high)) +
  scale_size_manual(values=c(2, 2)) +
  scale_shape_manual(values=c(1,1)) +
  labs(title="Predicted Catch Probability Based on Air Distance", subtitle=
         "Each circle represents one play out of 4839 total catch plays", 
      x="Air Distance (yards)", y="Predicted Catch Probability", color="Penalty on Play?") +
  scale_color_manual(labels = c("No", "Yes"), values = c("dodgerblue", "black")) +
  guides(size=FALSE, shape=FALSE) +
  theme_bw()
ad_plot

v <- data.frame(testnew$sideline_distance, glm.probs)
sd_plot <- ggplot(data=v, aes(x=testnew$sideline_distance, y=glm.probs)) + 
  geom_point(aes(color=testnew_penalty_high, size=testnew_penalty_high,
                 shape=testnew_penalty_high)) +
  scale_color_manual(values=c('dodgerblue','black')) +
  scale_size_manual(values=c(2, 2)) +
  scale_shape_manual(values=c(1,1)) +
  labs(title="Predicted Catch Probability Based on Sideline Distance",
       x="Sideline Distance (yards)", y="Predicted Catch Probability", color="Penalty on Play?") +
  scale_color_manual(labels = c("No", "Yes"), values = c("dodgerblue", "black")) +
  guides(size=FALSE, shape=FALSE) +
  theme_bw()
sd_plot

w <- data.frame(testnew$time_to_throw, glm.probs)
ttt <- ggplot(data=y, aes(x=testnew$time_to_throw, y=glm.probs)) + 
  geom_point(aes(color=testnew_penalty_high, size=testnew_penalty_high,
                 shape=testnew_penalty_high)) +
  scale_color_manual(values=c('dodgerblue','black')) +
  scale_size_manual(values=c(2, 2)) +
  scale_shape_manual(values=c(1,1)) +
  labs(title="Predicted Catch Probability Based on Time to Throw", 
       x="Passer Time to Throw (seconds)", y="Predicted Catch Probability", color="Penalty on Play?") +
  scale_color_manual(labels = c("No", "Yes"), values = c("dodgerblue", "black")) +
  guides(size=FALSE, shape=FALSE) +
  theme_bw()
ttt

k <- data.frame(testnew$passer_speed, glm.probs)
ps <- ggplot(data=y, aes(x=testnew$passer_speed, y=glm.probs)) + 
  geom_point(aes(color=testnew_penalty_high, size=testnew_penalty_high,
                 shape=testnew_penalty_high)) +
  scale_color_manual(values=c('dodgerblue','black')) +
  scale_size_manual(values=c(2, 2)) +
  scale_shape_manual(values=c(1,1)) +
  labs(title="Predicted Catch Probability Based on Thrower Speed",
       x="Thrower Speed (yards/second)", y="Predicted Catch Probability", color="Penalty on Play?") +
  scale_color_manual(labels = c("No", "Yes"), values = c("dodgerblue", "black")) +
  guides(size=FALSE, shape=FALSE) +
  theme_bw()
ps

pcb4 <- ggarrange(ad_plot, sd_plot + rremove("legend") + rremove("y.title"), 
                  ttt + rremove("legend"), ps + rremove("legend") + rremove("y.title"), 
                  labels=c("I","II","III","IV"),ncol=2, nrow=2)
pcb4

ruleimposed4 <- ggarrange(catch_prob, catch_prob_dpi + rremove("legend") + rremove("y.title"), 
                          catch_prob_opi + rremove("legend"), catch_prob_rest + rremove("legend") + rremove("y.title"),
                  ncol=2, nrow=2)

ruleimposed4

dhoh <- ggarrange(catch_prob_dh, catch_prob_oh + rremove("legend"),
                  ncol=2, nrow=1)

dhoh

t <- data.frame(testnew$pa_min, glm.probs)
pa <- ggplot(data=t, aes(x=testnew$pa_min, y=glm.probs)) + 
  geom_point(aes(color=testnew_penalty_high, size=testnew_penalty_high,
                 shape=testnew_penalty_high)) +
  scale_color_manual(values=c('dodgerblue','black')) +
  scale_size_manual(values=c(2, 2)) +
  scale_shape_manual(values=c(1,1)) +
  labs(title="Predicted Catch Probability Based on Defender Separation",
       x="Thrower Speed (yards/second)", y="Predicted Catch Probability", color="Penalty on Play?") +
  scale_color_manual(labels = c("No", "Yes"), values = c("dodgerblue", "black")) +
  guides(size=FALSE, shape=FALSE) +
  geom_smooth() +
  theme_bw()
pa

l <- data.frame(testnew$pr_min, glm.probs)
pr <- ggplot(data=y, aes(x=testnew$completion, y=glm.probs)) + 
  geom_point(aes(color=testnew_penalty_high, size=testnew_penalty_high,
                 shape=testnew_penalty_high)) +
  scale_color_manual(values=c('dodgerblue','black')) +
  scale_size_manual(values=c(2, 2)) +
  scale_shape_manual(values=c(1,1))
pr

glm.probs.2 <- data.frame(glm.probs.2, "15_Yard_Penalty")
colnames(glm.probs.2)[1] <- "prob"
colnames(glm.probs.2)[2] <- "type"

glm.pred.2 <- ifelse(glm.probs.2 > 0.5, 1, 0)

glm.probs.3 <- predict(glm.fit.all.pass.plays, testnew3, type='response')
glm.probs.3 <- data.frame(glm.probs.3, "20_Yard_Penalty")
colnames(glm.probs.3)[1] <- "prob"
colnames(glm.probs.3)[2] <- "type"

glm.pred.3 <- ifelse(glm.probs.2 > 0.5, 1, 0)

glm.probs.4 <- predict(glm.fit.all.pass.plays, testnew4, type='response')
glm.probs.4 <- data.frame(glm.probs.4, "10_Yard_Penalty")
colnames(glm.probs.4)[1] <- "prob"
colnames(glm.probs.4)[2] <- "type"

glm.pred.4 <- ifelse(glm.probs.2 > 0.5, 1, 0)

x <- glm.pred
y <- glm.pred2
a <- glm.pred3
b <- glm.pred4

total <- rbind(glm.probs, glm.probs.2, glm.probs.3, glm.probs.4)
total <- na.omit(total)

mu <- ddply(total, "type", summarise, grp.mean=mean(total1$prob))

catch_prob <- ggplot(data = total, aes(x=prob, fill=type)) + geom_density(alpha=0.25) +
  scale_x_continuous(name="Catch Probability") + 
  ggtitle("Catch Probability Distributions Under Different Rules")

ggsave(pcb4, device = "pdf", file = "/Users/peter/Downloads/Big-Data-Bowl-master/Plots/catch-prob-4.pdf")
ggsave(pcb4, device = "png", file = "/Users/peter/Downloads/Big-Data-Bowl-master/Plots/catch-prob-4.png")

