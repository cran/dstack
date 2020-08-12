library(testthat)

library(ggplot2)
library(dstack)
library(plotly)
library(data.table)

test_that("test ggplot", {
  library(ggplot2)
  theme_set(theme_classic())

  # Plot
  g <- ggplot(mpg, aes(cty)) + geom_density(aes(fill=factor(cyl)), alpha=0.8) + labs(title="Density plot",
         subtitle="City Mileage Grouped by Number of cylinders",
         caption="Source: mpg",
         x="City Mileage",
         fill="# Cylinders")

  push_frame("simple_ggplot", g, "My first R plot")
})

# test_that("Simple test", {
#   x <- c(1:100)
#   random_y <- rnorm(100, mean = 0)
#   data <- data.frame(x, random_y)
#
#   fig <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')
#
#   res <- push_frame("plotlyR", fig, "My first plot")
#   expect_equal(res$url, "https://stgn.dstack.ai/khud/plotlyR")
# })
#
# test_that("Test 1", {
#   theme_set(theme_bw())  # pre-set the bw theme.
#   data("midwest", package = "ggplot2")
#   # midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source
#
#   # Scatterplot
#   gg <- ggplot(midwest, aes(x=area, y=poptotal)) +
#     geom_point(aes(col=state, size=popdensity)) +
#     geom_smooth(method="loess", se=F) +
#     xlim(c(0, 0.1)) +
#     ylim(c(0, 500000)) +
#     labs(subtitle="Area Vs Population",
#          y="Population",
#          x="Area",
#          title="Scatterplot",
#          caption = "Source: midwest")
#
#   #plot(gg)
#   f <- create_frame("mixed_contentR")
#   f <- commit(f, gg, "Scatter plot", list(View="Plot"))
#   f <- commit(f, midwest, "Scatter plot", list(View="Data"))
#   res <- push(f)
#   expect_equal(res$url, "https://stgn.dstack.ai/khud/mixed_contentR")
# })
#
# test_that("Test pull", {
#   f <- pull("mixed_contentR", View = "Data")
#   print(f)
#   expect_equal(TRUE, TRUE)
# })
#
# test_that("Test Push Large data.table", {
#   dt <- fread("/Users/vitaly.khudobakhshov/Downloads/geographic-units-by-industry-and-statistical-area-2000-19-descending-order/head.csv")
#   res <- push_frame("large_data_R", dt)
#   expect_equal(TRUE, TRUE)
# })
#
# test_that("Test Push Large data.frame", {
#   df <- read.csv("/Users/vitaly.khudobakhshov/Downloads/geographic-units-by-industry-and-statistical-area-2000-19-descending-order/head.csv")
#   res <- push_frame("large_data_R_df", df)
#   expect_equal(res$url, "https://stgn.dstack.ai/khud/large_data_R_df")
# })
#
# test_that("Test pull large files", {
#   df <- read.csv(pull("large_data_R_df"))
#   print(colnames(df))
#   expect_equal(TRUE, TRUE)
# })
#
#
# test_that("Pull absolute paths", {
#   df <- read.csv(pull("/public_datasets/fusionbase/covid19-germany", profile="prod", filename="/tmp/test.csv", "Bundesland name"="All"))
#   print(colnames(df))
#   expect_equal(TRUE, TRUE)
# })
#
# test_that("Test tibble", {
#   library(tibble)
#   library(readr)
#   tb <- as_tibble(iris)
#   push_frame("simple_data_r", tb, "My first R dataset", message = "test message", dataset = "tb_iris")
#   tb <- read_csv(dstack::pull("/khud/simple_data_r", dataset = "tb_iris"))
# })
#
#
