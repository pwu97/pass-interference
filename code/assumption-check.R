## Sample code to check t-tests

library(sm)

View(testnew_save)

boxplot(testnew_save$play_result)
boxplot(testnew_save$passlength)

t.test(testnew_save$play_result, testnew_save$passlength, paired=TRUE)


