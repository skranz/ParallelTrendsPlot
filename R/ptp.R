
#' Generates plots to diagnose parallel trends for difference-in-difference regressions with additional control variables
#'
#' @param pt.dat A data frame as returned by the function parallel.trends.data
#' @param facet.mode If TRUE generate a facet plot with one facet for trends of control group and experimental group with and without correction of the control variables
#' @param add.exp.line How shall vertical lines be added to indicate the experimental periods? "first" means a line at the first experimental period, "all" means a line at all experimental periods. Use "none" to add no lines
#' @param exp.line.opt Arguments passed to geom_vert to customize the vertical lines.
#' @returns A ggplot object
parallel.trends.plot = function(pt.dat, facet.mode = ".mode" %in% colnames(pt.dat), add.exp.line = c("first","all", "none")[1], exp.line.opt = list(color="black")) {
  restore.point("parallel.trends.plot")
  if (!".mode" %in% colnames(pt.dat))
    pt.dat$.mode = "with control variables"
  d = pt.dat %>%
    group_by(.t, .group, .mode) %>%
    summarize(
      .groups = "drop",
      y = mean(.y, na.rm=TRUE),
      exp = first(.exp)
    ) %>%
    ungroup() %>%
    rename(t = .t, group=.group, mode=.mode)

  if (add.exp.line == "first") {
    d = d %>%
      group_by(group, mode) %>%
      mutate(
        exp = 1L*(exp & cumsum(exp)==1)
      ) %>%
      ungroup()
  }


  gg= ggplot(d, aes(x=t, y=y, color=group)) + geom_line()
  if (facet.mode) {
    gg=gg+facet_wrap(~mode,scale="free_y")
  }
  if (add.exp.line %in% c("all","first")) {
    exp.df = filter(d,exp == 1)
    gg = gg + do.call(geom_vline, c(list(data = exp.df, aes(xintercept=t)),exp.line.opt))
  }
  gg
}

#' Creates a data frame used for generating plots to diagnose parallel trends for difference-in-difference regressions with additional control variables
#'
#' The resulting data frame can be directly passed to parallel.trends.plot or be used to generate a more customized manual plot.
#'
#' @param df The data frame used for the DID regression
#' @param timevar Name of the time variable, e.g. "year"
#' @param yvar Name of the dependent variable
#' @param treatdummy Name of the 0-1 dummy variable indicating whether the observation belongs to the treatment group
#' @param expdummy Name of the 0-1 dummy variable indicating whether the observation is in an experimental period, i.e. a period in which the treatment group gets treatment.
#' @param treat_exp_dummy Optional name of the interaction variable treatdummy * expdummy. If not given a column with name "treat_exp_dummy" is added to the data frame.
#' @param cvars Names of the additional control variables used in the DID regression.
#' @param extravars Names of variables that are not used in the regression but shall be included in the returned data frame.
#' @param add.no.control If TRUE (default) add rows that are used for a parallel trends diagnostic plot that assumes that no control variables are added.
#' @returns A data frame that can be passed to parallel.trends.plot. Essential columns are .t, .y, .group, .exp, .mode. Note that the data frame only contains rows that have no NA for the specified columns.
#'
parallel.trends.data = function(df, timevar="t", yvar="y", treatdummy = "treat", expdummy="exp",treat_exp_dummy=NULL, cvars=NULL, extravars=NULL, add.no.control=TRUE, constant.type = c("means","zero")[1]) {
  restore.point("parallel.trends.data")

  library(glueformula)
  if (is.null(treat_exp_dummy)) {
    treat_exp_dummy = paste0(treatdummy,"_", expdummy)
    df[[treat_exp_dummy]] = df[[treatdummy]]*df[[expdummy]]
  }

  xvars = c(treatdummy, expdummy, treat_exp_dummy, cvars)
  vars = c(timevar, xvars,extravars)

  form.mf = gf({yvar} ~ {vars})
  mf = model.frame(form.mf, df)
  form = gf({yvar} ~ {xvars})
  mm = model.matrix(form, mf)[,-1]
  mm0 = mm

  if (constant.type == "zero") {
    mm0[,-c(1:3)] = 0
  } else {
    # Set control variables to means in each group
    treat.rows = mf[[treatdummy]] == 1
    mm0[treat.rows,-(1:3)] = set.matrix.to.col.means(mm[treat.rows,-(1:3), drop=FALSE])
    mm0[!treat.rows,-(1:3)] = set.matrix.to.col.means(mm[!treat.rows,-(1:3), drop=FALSE])
  }

  dat = bind_cols(mf[,c(1),drop=FALSE], as.data.frame(mm))
  dat0 = bind_cols(mf[,c(1), drop=FALSE], as.data.frame(mm0))


  reg = lm(gf({yvar}~.), dat)

  dat$.y = predict(reg, dat0) + resid(reg)
  dat$.group = ifelse(dat[treatdummy]==1, "treatment","control")
  dat$.t = as.numeric(mf[[timevar]])
  dat$.exp = mf[[expdummy]]
  if (!is.null(extravars))
    dat[,extravars] = mf[, extravars, drop=FALSE]

  if (add.no.control) {
    dat$.mode ="with control variables"
    dat.nc = mf
    dat.nc$.y = mf[[1]]
    dat.nc$.group = dat$.group
    dat.nc$.t = as.numeric(mf[[timevar]])
    dat.nc$.exp = mf[[expdummy]]

    dat.nc$.mode ="no control variables"

    #dat.nc.alt = parallel.trends.data(mf, timevar=timevar, yvar=yvar, treatdummy = treatdummy, expdummy=expdummy, treat_exp_dummy=treat_exp_dummy, cvars=NULL, extravars=extravars, add.no.control=FALSE)
    #dat.nc.alt$.mode ="alt no"

    dat = bind_rows(dat, dat.nc)
    #dat = bind_rows(dat, dat.nc, dat.nc.alt)
  }

  dat
}

set.matrix.to.col.means = function(m) {
  restore.point("set.matrix.to.col.means")
  if (nrow(m)==0 | ncol(m)==0) return(m)
  cm = colMeans(m)
  matrix(cm, nrow(m), ncol(m),byrow = TRUE)
}

constantify.cols = function(dat, use.cols = colnames(dat), ignore.cols = NULL, group.col = NULL) {
  constantify = function(x) {
    rows = !is.na(x)
    if (is.numeric(x)) {
      x[rows] = mean(x[rows])
    } else {
      rev(sort(table(c(1:10,1:5))))
      x[rows] = first(x[rows])
    }
    x
  }
  cols = setdiff(use.cols, ignore.cols)

}
