## Get testnew dataframe from `generate-testnew.R`

## Analogous code to `10-15-20-penalties.R`

## Run this to condition on type of penalty and to view
## density plots. 

#testnew <- na.omit(testnew)
hist(testnew_save$air_distance)
hist(testnew_save$sideline_distance)

plot(testnew_save$play_result, testnew_save$air_distance)

testnew.ad <- filter(testnew_save, play_result != 0)

nn <- ggplot(data=testnew.ad, aes(x=play_result, y=air_distance)) + geom_point() +
  geom_smooth()
nn

testnew <- testnew_penalty_dh

testnew_penalty_all <- filter(testnew_save, penalty_var == TRUE)

testnew_penalty_dh <- testnew_save[grep("Defensive Holding", testnew_save$play_desc),]
testnew_penalty_oh <- testnew_save[grep("Offensive Holding", testnew_save$play_desc),]
testnew_penalty_dpi <- testnew_save[grep("Defensive Pass Interference", testnew_save$play_desc),]
testnew_penalty_dall <- rbind(testnew_penalty_dpi, testnew_penalty_dh)
testnew_penalty_opi <- testnew_save[grep("Offensive Pass Interference", testnew_save$play_desc),]

# testnew <- testnew_penalty_dpi

testnew_penalty_rest <- testnew_save[grep("Penalty", testnew_save$play_desc),]
testnew_penalty_rest <- testnew_penalty_rest[-grep("Pass Interference", testnew_penalty_rest$play_desc),]


glm.fit <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                 passer_speed + passlength, data = testnew_save, family = binomial)
testnew2 <- mutate(testnew, passlength = 5)
testnew3 <- mutate(testnew, passlength = 10)
testnew4 <- mutate(testnew, passlength = 15)
glm.fit.2 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed + passlength, data = testnew_penalty_dpi, family = binomial)
glm.fit.3 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed + passlength, data = testnew_penalty_opi, family = binomial)
glm.fit.4 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed + passlength, data = testnew_penalty_rest, family = binomial)


glm.probs <- predict(glm.fit, testnew, type = "response")
glm.probs <- data.frame(glm.probs, "Spot Foul")
colnames(glm.probs)[1] <- "prob"
colnames(glm.probs)[2] <- "type"

glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

glm.probs.2 <- predict(glm.fit, testnew2, type='response')
glm.probs.2 <- na.omit(glm.probs.2)
glm.probs.2 <- data.frame(glm.probs.2, "5 Yard Penalty")
colnames(glm.probs.2)[1] <- "prob"
colnames(glm.probs.2)[2] <- "type"

glm.pred.2 <- ifelse(glm.probs.2 > 0.5, 1, 0)

glm.probs.3 <- predict(glm.fit, testnew3, type='response')
glm.probs.3 <- data.frame(glm.probs.3, "10 Yard Penalty")
colnames(glm.probs.3)[1] <- "prob"
colnames(glm.probs.3)[2] <- "type"

glm.pred.3 <- ifelse(glm.probs.2 > 0.5, 1, 0)

glm.probs.4 <- predict(glm.fit, testnew4, type='response')
glm.probs.4 <- data.frame(glm.probs.4, "15 Yard Penalty")
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

catch_prob_dpi <- ggplot(data = total, aes(x=prob, fill=type)) +
  geom_density(alpha=0.25) +
  coord_cartesian(xlim=c(0, 0.2, 0.4, 0.6, 0.8)) + 
  scale_x_continuous(name="Catch Probability") + 
  labs(title="Catch Probability Distributions for Defensive Holding", 
       subtitle="For all defensive holding catch plays in Big Data Bowl dataset",
       x="Predicted Catch Probability", y="Density",
       fill="Penalty Observed") +
  theme_bw()

catch_prob_dpi

yy <- ggarrange(catch_prob_other, catch_prob_dpi + rremove("legend"), catch_prob_opi +
                  rremove("legend"), catch_prob_all + rremove("legend"),
                nrow=2, ncol=2)

yy

twoplots <- ggarrange(catch_prob_dh, catch_prob_dpi, ncol=1, nrow=2)
twoplots

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

ggsave(twoplots, device = "pdf", file = "/Users/peter/Desktop/nfl-project/figs/save.pdf")
ggsave(twoplots, device = "png", file = "/Users/peter/Desktop/nfl-project/figs/save.png")
