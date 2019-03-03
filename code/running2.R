## Filter by DPI and OPI and then perform similar
## analysis as `10-15-20-penalties.R`

#testnew <- na.omit(testnew)
hist(testnew_save$air_distance)
hist(testnew_save$sideline_distance)

plot(testnew_save$play_result, testnew_save$air_distance)

testnew.ad <- filter(testnew_save, play_result != 0)

nn <- ggplot(data=testnew.ad, aes(x=play_result, y=air_distance)) + geom_point() +
  geom_smooth()
nn

testnew <- filter(testnew_, passlength >= 10)

testnew_penalty_dpi <- testnew_save[grep("Defensive Pass Interference", testnew_save$play_desc),]
testnew_penalty_opi <- testnew_save[grep("Offensive Pass Interference", testnew_save$play_desc),]

testnew_penalty_rest <- testnew_save[grep("Penalty", testnew_save$play_desc),]
testnew_penalty_rest <- testnew_penalty_rest[-grep("Pass Interference", testnew_penalty_rest$play_desc),]

glm.fit <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                 passer_speed + passlength, data = testnew_save, family = binomial)
testnew2 <- mutate(testnew, passlength = 10)
testnew3 <- mutate(testnew, passlength = 15)
testnew4 <- mutate(testnew, passlength = 20)
glm.fit.2 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed + passlength, data = testnew2, family = binomial)
glm.fit.3 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed + passlength, data = testnew3, family = binomial)
glm.fit.4 <- glm(formula = completion ~ air_distance + sideline_distance + time_to_throw +
                   passer_speed + passlength, data = testnew4, family = binomial)


glm.probs <- predict(glm.fit, testnew, type = "response")
glm.probs <- data.frame(glm.probs, "Spot of Foul")
colnames(glm.probs)[1] <- "prob"
colnames(glm.probs)[2] <- "type"

glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

glm.probs.2 <- predict(glm.fit, testnew2, type='response')
glm.probs.2 <- na.omit(glm.probs.2)
glm.probs.2 <- data.frame(glm.probs.2, "10 Yard Penalty")
colnames(glm.probs.2)[1] <- "prob"
colnames(glm.probs.2)[2] <- "type"

glm.pred.2 <- ifelse(glm.probs.2 > 0.5, 1, 0)

glm.probs.3 <- predict(glm.fit, testnew3, type='response')
glm.probs.3 <- data.frame(glm.probs.3, "15 Yard Penalty")
colnames(glm.probs.3)[1] <- "prob"
colnames(glm.probs.3)[2] <- "type"

glm.pred.3 <- ifelse(glm.probs.2 > 0.5, 1, 0)

glm.probs.4 <- predict(glm.fit, testnew4, type='response')
glm.probs.4 <- data.frame(glm.probs.4, "20 Yard Penalty")
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

catch_prob_dpi <- ggplot(data = total, aes(x=prob, fill=type)) + geom_density(alpha=0.25) +
  scale_x_continuous(name="Catch Probability") + 
  labs(title="Catch Probability Distributions for Long Catch Plays", 
       subtitle="For 2139 total catch plays greater than 10 yards",
       x="Predicted Catch Probability", y="Density",
       fill="Penalty Imposed") +
  theme_bw()




#ggsave(catch_prob_dpi, device = "pdf", file = "/Users/peter/Downloads/Big-Data-Bowl-master/Plots/rest-plot.pdf")
#ggsave(catch_prob_dpi, device = "png", file = "/Users/peter/Downloads/Big-Data-Bowl-master/Plots/rest-plot.png")


