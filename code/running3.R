## Similar to `10-15-20-penalties.R`

#testnew <- na.omit(testnew)
hist(testnew_save$air_distance)
hist(testnew_save$sideline_distance)

plot(testnew_save$play_result, testnew_save$air_distance)

testnew.ad <- filter(testnew_save, play_result != 0)

nn <- ggplot(data=testnew.ad, aes(x=play_result, y=air_distance)) + geom_point() +
  geom_smooth()
nn

testnew <- filter(testnew_save, passlength >= 10)

testnew_penalty_all <- filter(testnew_save, penalty_var == TRUE)

testnew_penalty_dh <- testnew_save[grep("Defensive Holding", testnew_save$play_desc),]
testnew_penalty_oh <- testnew_save[grep("Offensive Holding", testnew_save$play_desc),]
testnew_penalty_dpi <- testnew_save[grep("Defensive Pass Interference", testnew_save$play_desc),]
testnew_penalty_dall <- rbind(testnew_penalty_dpi, testnew_penalty_dh)
testnew_penalty_opi <- testnew_save[grep("Offensive Pass Interference", testnew_save$play_desc),]

testnew <- testnew_penalty_dpi

testnew_penalty_rest <- testnew_save[grep("Penalty", testnew_save$play_desc),]
testnew_penalty_rest <- testnew_penalty_rest[-grep("Pass Interference", testnew_penalty_rest$play_desc),]

glm.fit <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                 passer_speed + passlength, data = testnew_save, family = binomial)
testnew2 <- testnew_penalty_opi
testnew3 <- testnew_penalty_dh
testnew4 <- testnew_penalty_oh
glm.fit.2 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed + passlength, data = testnew_penalty_dpi, family = binomial)
glm.fit.3 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed + passlength, data = testnew_penalty_opi, family = binomial)
glm.fit.4 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed + passlength, data = testnew_penalty_rest, family = binomial)


glm.probs <- predict(glm.fit, testnew, type = "response")
glm.probs <- data.frame(glm.probs, "Defensive PI")
colnames(glm.probs)[1] <- "prob"
colnames(glm.probs)[2] <- "type"

glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

glm.probs.2 <- predict(glm.fit, testnew2, type='response')
glm.probs.2 <- na.omit(glm.probs.2)
glm.probs.2 <- data.frame(glm.probs.2, "Offensive PI")
colnames(glm.probs.2)[1] <- "prob"
colnames(glm.probs.2)[2] <- "type"

glm.pred.2 <- ifelse(glm.probs.2 > 0.5, 1, 0)

glm.probs.3 <- predict(glm.fit, testnew3, type='response')
glm.probs.3 <- data.frame(glm.probs.3, "Defensive Holding")
colnames(glm.probs.3)[1] <- "prob"
colnames(glm.probs.3)[2] <- "type"

glm.pred.3 <- ifelse(glm.probs.2 > 0.5, 1, 0)

glm.probs.4 <- predict(glm.fit, testnew4, type='response')
glm.probs.4 <- data.frame(glm.probs.4, "Offensive Holding")
colnames(glm.probs.4)[1] <- "prob"
colnames(glm.probs.4)[2] <- "type"

glm.pred.4 <- ifelse(glm.probs.2 > 0.5, 1, 0)

x <- glm.pred
y <- glm.pred2
a <- glm.pred3
b <- glm.pred4

total <- rbind(glm.probs, glm.probs.2, glm.probs.3, glm.probs.4)
total <- na.omit(total)
total

mu <- ddply(total, "type", summarise, grp.mean=mean(total1$prob))

catch_prob_all <- ggplot(data = total, aes(x=prob, fill=type)) +
  geom_density(alpha=0.25) +
  scale_x_continuous(name="Catch Probability") + 
  labs(title="Catch Probability Distributions for Different Penalties Called on Catch Plays ", 
       subtitle="Out of 9 DPI calls, 7 OPI calls, 29 Defensive Holding calls, and 35 Offensive Holding calls",
       x="Predicted Catch Probability", y="Density",
       fill="Penalty Observed") +
  theme_bw()

catch_prob_all

plot(predict(glm.fit, testnew, type = "response"), testnew$play_result)
hist(predict(glm.fit, testnew2, type = "response"))

plot(ecdf(predict(glm.fit, testnew, type='response')),
     xlim=c(0,1),
     col="green")
lines(ecdf(predict(glm.fit, testnew2, type='response')),
      col="blue")
lines(ecdf(predict(glm.fit, testnew3, type='response')),
      col="red")
lines(ecdf(predict(glm.fit, testnew4, type='response')),
      col="magenta")

plot(testnew_save$passlength, testnew_save$completion)

h <- ggplot(data=testnew, aes(x=play_result)) + 
  geom_histogram(aes(y=stat(density)), bins=50, color="gray",fill="white") + 
  geom_density(color="blue") +
  theme_bw() +
  xlab("Air Distance") +
  ylab("Density") +
  ggtitle("Distribution of Air Distance on Catch Plays (91 Games)")

h

ggsave(catch_prob_all, device = "pdf", file = "/Users/peter/Downloads/Big-Data-Bowl-master/Plots/all.pdf")
ggsave(catch_prob_all, device = "png", file = "/Users/peter/Downloads/Big-Data-Bowl-master/Plots/all.png")
