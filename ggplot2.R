

ggplot(iris, aes(x = Species, y = Sepal.Width)) +
    stat_summary(fun.y = mean, geom = "bar")

ggplot(iris, aes(x = Species, y = Sepal.Width)) +
    stat_summary(fun.y = mean, geom = "bar") + scale_x_discrete(limits = c("versicolor","virginica"))#,"setosa"))
# + scale_x_discrete(limits = c("versicolor","virginica","setosa"))

ggplot(iris, aes(x = reorder(x = Species, X = Sepal.Width, FUN = mean), y = Sepal.Width)) +  stat_summary(fun.y = sum, geom = "bar") + xlab( "sum")



df <- data.frame(x1 = c(30,20,30,10,50,60),x2 = c("AA", "BB", "CC", "DD", "EE", "FF")) ;df

ggplot(df, aes (x = x2, y = x1)) + geom_bar(stat="identity") + scale_x_discrete(limits = c("FF","EE", "CC", "AA"))#, "BB", "DD")) #("bar")

