parallel.trends.plot.example = function() {
  library(ParallelTrendsPlot)
  T = 100 # no of periods
  N = 40 # no of subjects

  dat = expand.grid( t = 1:T,i = 1:N)

  # Simulate a common AR(1) time trend
  time.trend = as.numeric(arima.sim(n=T,list(ar = c(0.4,0.5), ma=c(0.6,0.5))))*3+0.3*(1:T)

  dat = mutate(dat,
    group = ifelse(i > N/2,"treat","control"),
    treat = 1L*(group == "treat"),
    exp = 1L*(t >= T/2),
    treat_exp = exp*treat,
    mu.t = time.trend[t],
    eps = rnorm(length(t)),
    x = ifelse(treat,-t, t)+runif(n())*2,
    y = mu.t + treat*40 + treat_exp*50 + x + eps
  )

  pt.dat = parallel.trends.data(dat,cvars="x")
  parallel.trends.plot(pt.dat, exp.line.opt = list(color="black")) + theme_bw()
  pt.dat %>%
    group_by(.group) %>%
    summarize(y = mean(y))

  pt.dat = parallel.trends.data(dat,cvars="x", constant.type="zero")
  parallel.trends.plot(pt.dat, exp.line.opt = list(color="black")) + theme_bw()

  dat %>%
    group_by(group) %>%
    summarize(y = mean(y))
}
