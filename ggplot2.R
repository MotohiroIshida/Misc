

ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  stat_summary(fun.y = mean, geom = "bar")

ggplot(iris, aes(x = reorder(x = Species, X = Sepal.Width, FUN = mean), y = Sepal.Width)) +  stat_summary(fun.y = mean, geom = "bar") + xlab( "mean")
x
