
days <- seq(as.Date("2015-3-1"), as.Date("2015-5-30"), by = "days");length(days)

df <- data.frame(X = rnorm(length(days), mean = 1000), Y = days)

library (dplyr )
df %>% filter(days > "2015-04-01" , days < "2015-05-30")

