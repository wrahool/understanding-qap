
# binary graph
bg <- matrix(
  c(1, 1, 0, 0,
    0, 1, 0, 1,
    0, 0, 1, 1,
    1, 1, 0, 1),
  nrow = 4, ncol = 4)

# weighted graph 1
# weight is more than 10 whenever an edge exists in bg,
# and less than 5 otherwise

wg1 <- matrix(
  c(12, 16, 1, 2,
    2, 11, 2, 15,
    4, 3, 12, 12,
    11, 18, 3, 11),
  nrow = 4, ncol = 4)

# in the QAP test, we expect there to be a positive and significant correlation
# between bg and wg1

bg_wg1_qap <- qaptest(list(bg, wg1), gcor, g1=1, g2=2, reps=1000)

summary(bg_wg1_qap)

# 0.95, p = 0.044

# weighted graph 1
# weight is less than 5 whenever an edge exists in bg,
# and more than 10 otherwise

wg2 <- matrix(
  c(1, 3, 14, 12,
    12, 2, 16, 1,
    11, 16, 2, 2,
    2, 2, 13, 1),
  nrow = 4, ncol = 4)

# in the QAP test, we expect there to be a negative and significant correlation
# between bg and wg2

bg_wg2_qap <- qaptest(list(bg, wg2), gcor, g1=1, g2=2, reps=1000)

summary(bg_wg2_qap)

# -0.967, p = 0.04

# weighted graph 3
# weight is random number between 1 and 20

set.seed(9)
wg3 <- matrix(round(runif(16, 1, 20)),
              nrow = 4, ncol = 4)

# in the QAP test, we expect there to no significant correlation
# between bg and wg3

bg_wg3_qap <- qaptest(list(bg, wg3), gcor, g1=1, g2=2, reps=1000)

summary(bg_wg3_qap)

# -0.26, p = 0.921, 0.131