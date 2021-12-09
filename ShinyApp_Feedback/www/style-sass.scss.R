sprintf("%.0f%% said yes (out of a sample of size %.2f, but with %.0f chance)", 66.666, 3, 100)


t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  # use sprintf() to format t.test() results compactly
  sprintf(
    "p value: %.4f\n[%.4f, %.4f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

x1 <- rnorm(100, mean = 0, sd = 0.5)
x2 <- rnorm(200, mean = 0.15, sd = 0.9)

cat(t_test(x1, x2))

cat(paste(letters, 100* 1:120), fill = TRUE, labels = paste0("{", 1:3, "}:"))
