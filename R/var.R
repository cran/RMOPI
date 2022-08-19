#' VaR Calculation and Coverage Test
#'
#' Calculate VaR with three method and implement unconditional and conditional coverage test.
#'
#' @param data vector of returns
#' @param method the VaR method, one of "param", "hist" and "mc"
#' @param alpha the VaR confidence level
#' @param fun function calculating VaR, limited by \code{method}
#' @param ... the extra parameters of \code{fun}
#'
#' @importFrom rugarch VaRTest
#' @importFrom stats quantile
#' @importFrom lubridate days
#'
#' @return A list of VaR and coverage test outcome
#' @references Christoffersen P. F. 1998. "Evaluating Interval Forecasts", International Economic Review, 841-862. doi: 10.2307/2527341.
#' @references Kupiec PH. 1995. "Techniques for Verifying the Accuracy of Risk Measurement Models", The Journal of Derivatives, 3(2), 73-84. doi: 10.3905/jod.1995.407942.
#' @export
#'
#'
#' @examples
#' swan <- rGarch(len = 30)
#' date <- as.Date("2015-01-01") + days(0:(length(swan) - 1))
#' tswan <- tibble(garch = swan, date = date)
#' tsswan <- as.xts(swan, date)
#' alpha = 0.05
#' num = 100000
#' mu = mean(tsswan)
#' sd = sd(tsswan)
#' VaRSimTest(tsswan, "mc", alpha , rnorm, 100000, mu, sd)
VaRSimTest <- function(data, method, alpha, fun, ...) {
  if (method == "param") {
    varmodel <- fun(...)
    var <- quantile(varmodel, alpha)
  } else if (method == "hist") {
    var <- rep(fun(...), length(data))
  } else if (method == "mc") {
    varmodel <- fun(...)
    var <- rep(quantile(varmodel, alpha), length(data))
  }
  vartest <- VaRTest(alpha, actual = data, VaR = var)
  vartable <- sapply(c(1:12), function(x) vartest[[x]])
  names(vartable) <- rownames(as.data.frame(unlist(vartest)))
  vartable <- as.data.frame(vartable)
  return(list(var, vartable))
}
