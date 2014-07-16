library(testthat)

library(cmd.aet)
library(hydromad)

data(Cotter)
x <- Cotter[1:1000]

orig <- hydromad(x,sma="cmd",f=0.6781,e=0.1271,d=506.9382)
x$aET <- predict(orig,U=TRUE,return_state=TRUE)$ET
x$aU <- predict(orig,U=TRUE)

context("C cmd_aet with synthetic aET reproduces cmd")

hydromad.options("pure.R.code"=FALSE)

mod <- hydromad(x,sma="cmd_aet",e=1,d=506.9382)

expect_that(fitted(mod,U=TRUE),equals(fitted(orig,U=TRUE)))
expect_that(predict(mod,U=TRUE,return_state=TRUE),equals(predict(orig,U=TRUE,return_state=TRUE)))

context("pure.R.code cmd_aet with synthetic aET reproduces cmd")

hydromad.options("pure.R.code"=TRUE)

mod <- hydromad(x,sma="cmd_aet",e=1,d=506.9382)

expect_that(fitted(mod,U=TRUE),equals(fitted(orig,U=TRUE)))
expect_that(predict(mod,U=TRUE,return_state=TRUE),equals(predict(orig,U=TRUE,return_state=TRUE)))

context("check potential ET is not used")

x2 <- x
x2$E <- NULL
mod <- hydromad(x2,sma="cmd_aet",e=1,d=506.9382)

hydromad.options("pure.R.code"=TRUE)
expect_that(fitted(mod,U=TRUE),equals(fitted(orig,U=TRUE)))
expect_that(predict(mod,U=TRUE,return_state=TRUE),equals(predict(orig,U=TRUE,return_state=TRUE)))
hydromad.options("pure.R.code"=FALSE)
expect_that(fitted(mod,U=TRUE),equals(fitted(orig,U=TRUE)))
expect_that(predict(mod,U=TRUE,return_state=TRUE),equals(predict(orig,U=TRUE,return_state=TRUE)))


context("estimate parameters of cmd_aet")
modx <- hydromad(x2,sma="cmd_aet")
fitx <- fitByOptim(modx,objective=~-hmadstat("RMSE")(model$data$aU,U))

expect_that(coef(fitx)[["e"]],equals(1))
expect_that(coef(fitx)[["d"]],equals(506.9382))