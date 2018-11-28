# data(sysdata, envir = environment())

#' Dates and times with more or less detail
#'
#' @param start_date start date of date/time expansion
#' @param end_date end date of date/time expansion
#'
#' @return a list of 4 objects to be accessed with $ (time.long, time.short,
#'   time.segment, time.days)
#' @export
expandDates <- function(start_date = NA, end_date = NA){

  library(timeDate)
  library(lubridate)
  library(dplyr)

  # number of days from start_date to end_date
  N <- as.numeric(as.Date(end_date) - as.Date(start_date))

  # sequence of dates from start_date to end_date
  Date <- as.Date(start_date) + 0:N
  DateMatrix <-as.data.frame(Date) # data frame output

  # get NERC holidays from start_date to end_date
  holidays <- vector()
  for (y in year(start_date):year(end_date)){
    holidays_y <- holidayNERC(year = y)
    holidays_y <- holidays_y@Data
    holidays_y <- as.Date(holidays_y)
    holidays <- c(holidays,holidays_y)
  }

  holidays <- as.Date(holidays, origin="1970-01-01")

  DateMatrix <- DateMatrix %>%
    mutate(DayOfWeek = weekdays(as.Date(Date)),
           POWSEG = 1 + (as.numeric(is.element(DayOfWeek,"Saturday")) +
                            as.numeric(is.element(DayOfWeek,"Sunday")) +
                            as.numeric(is.element(Date,holidays))), # weekday (1) or weekend(2)
           Delmo = as.Date(paste(year(Date), month(Date), 1, sep = "-")), # start of month
           EndOfMonth = Delmo + days_in_month(Delmo) - 1,
           SoM_EoM_Flag = as.numeric(Date==Delmo) +
             2 * as.numeric(Date==EndOfMonth), # start of month (1) and end of month(2) indicator
           PKHrs = 16 * (as.numeric(is.element(POWSEG,1))),
           OPHrs = 8 * (as.numeric(is.element(POWSEG,1)) +
                           as.numeric(is.element(POWSEG,2))),
           WKNDPKHrs = 16 * (as.numeric(is.element(POWSEG,2))))

  DateMatrix.short <- DateMatrix %>% group_by(Delmo, POWSEG) %>%
    summarise(numPkDays = sum(POWSEG),
              numOffDays = sum(POWSEG)/2,
              sumPkhrs = sum(PKHrs),
              sum7x8 = sum(OPHrs),
              sum2x16 = sum(WKNDPKHrs))

  DateMatrix.segment <- DateMatrix.short %>% group_by(Delmo) %>%
    summarise(sumPk = sum(sumPkhrs),
              sum7x8 = sum(sum7x8),
              sum2x16 = sum(sum2x16))

  DateMatrix.days <- DateMatrix.short %>% filter(POWSEG == 1) %>% select(Delmo, numPkDays) %>%
    left_join(DateMatrix.short %>% filter(POWSEG == 2) %>% select(Delmo, numOffDays), by = 'Delmo') %>%
    mutate(numTotDays = numPkDays + numOffDays)

  DateMatrix.list <- list(time.long = DateMatrix, time.short = DateMatrix.short,
                          time.segment = DateMatrix.segment, time.days = DateMatrix.days)
  return(DateMatrix.list)

}


#' Simulate (forward) prices of multiple commodities based on inputs
#'
#' @param s0 a vector of initial prices
#' @param mu a vector of drifts
#' @param sigma covariance matrix (usually obtained by multiplying correlation
#'   matrix with volatility)
#' @param nsims the number of simulations
#' @param periods time period as fraction of a year; it accepts both c(0, 1)
#'   format using already converted fractions or c('2018-01-01', '2018-12-01')
#'   format where the conversion will be done by the function using beginning
#'   and end time periods, 365 day count convention and system date
#'
#' @return a matrix with commodities vertically and number of simulations
#'   horizontally
#' @export
asset.paths <- function(s0, mu, sigma,
                        nsims = 10000,
                        periods = c(0, 1)   # time periods at which to simulate prices
)
{
  if(class(periods) == "character"){
  periods = as.numeric((seq.Date(as.Date(periods[1]), as.Date(periods[2]), by = 'month') - Sys.Date())/365)
  } else{
  periods = periods
  }
  s0 = as.vector(s0)
  nsteps = length(periods)
  dt = c(periods[1], diff(periods))

  if( length(s0) == 1 ) {
    drift = mu - 0.5 * sigma^2
    if( nsteps == 1 ) {
      s0 * exp(drift * dt + sigma * sqrt(dt) * rnorm(nsims))
    } else {
      temp = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
      for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
      s0 * temp
    }
  } else {
    drift = mu - 0.5 * diag(sigma)
    n = length(mu)

    if( nsteps == 1 ) {
      as.numeric(matrix(rep(s0, nsims), ncol = nsims)) * exp(matrix(rep(drift * dt, nsims), ncol = nsims) +
                                                               sqrt(dt) * t(MASS::mvrnorm(nsims, rep(0, n), sigma))) # had to correct from source
    } else {
      temp = array(exp(as.vector(drift %*% t(dt)) + t(sqrt(dt) * MASS::mvrnorm(nsteps * nsims, rep(0, n), sigma))), c(n, nsteps, nsims))
      for(i in 2:nsteps) temp[,i,] = temp[,i,] * temp[,(i-1),]
      s0 * temp
    }
  }
}


#' Wrapper of monthly forward simulations of power and gas
#'
#' A wrapper function around asset.paths that uses aligne data for forward
#' prices and volatilities. NYMEX/NG is included by default, power curves need
#' to be entered as a list of c('MARKET-COMPONENT'). Note that the requested
#' power curves must have valid volatility entries not mapped from other curves.
#'
#' User needs to supply folder location where three input files are stored. For
#' example, file_loc = "C:/R_input/". One for power futures, one for gas
#' futures, one for the volatilties. The format of these csv files can be found
#' in the "upload_template" folder of this project.
#'
#' @param nsims number of simulations
#' @param curve.date.begin beginning of curve dates used to calculate
#'   inter-curve correlations
#' @param curve.date.end end of curve dates used to calculate inter-curve
#'   correlations (cannot fall on a weekend/holiday as the final available
#'   forward prices are being converged to)
#' @param start_date beginning of forward simulation window
#' @param end_date end of forward simulation window
#' @param marketcomponentstr list of market-component pairs for power curves
#' @param terminal option to choose whether to return the terminal simulated
#'   values only (default is TRUE)
#' @param segment option to choose to simulate offpeak wrap only or weekend and
#'   weeknight segments separately
#'
#' @return three data objects outputSim (for simulated results), outputFwd
#'   (compilation of forward curve inputs) and outputVol (ATM vols of last curve
#'   date)
#' @export
#'
#' @examples fwdSimWrapper(500, '2017-01-01', '2017-04-18', '2018-01-01', '2018-12-01', c('ERCOT-ZONE N', 'PJM-WESTRT'))
fwdSimWrapper <- function(nsims = NA, curve.date.begin = NA, curve.date.end = NA,
                          start_date = NA, end_date = NA, marketcomponentstr = NA,
                          terminal = TRUE, segment = 2, file_loc = NA){
  library(readr)
  
  input_path_pwr <- paste0(file_loc, "pwr.csv")
  input_path_gas <- paste0(file_loc, "gas.csv")
  input_path_vols <- paste0(file_loc, "vols.csv")

  # testing
  # nsims = 10
  # curve.date.begin = '2017-07-01'
  # curve.date.end = '2017-10-31'
  # start_date = '2018-01-01'
  # end_date = '2018-12-01'
  # marketcomponentstr = 'ERCOT-ZONE N'
  # terminal = TRUE
  # segment = 2
  # end of testing

  market <- unlist(strsplit(marketcomponentstr, '-'))[c(TRUE, FALSE)]
  component <- unlist(strsplit(marketcomponentstr, '-'))[c(FALSE, TRUE)]
  numofmonth <- (as.yearmon(end_date) - as.yearmon(start_date))*12 + 1
  # first forward month
  first.month <- start_date
  # forward delivery months included
  month.used <- seq(as.Date(first.month), by = "month", length.out = as.integer(numofmonth))

  # day counts
  end_date <- timeLastDayInMonth(end_date)
  out.days <- expandDates(start_date, end_date)$time.days
  period.pre <- as.numeric(as.Date(first.month) - as.Date(curve.date.end))/365
  period.rtc.inter <- cumsum(out.days$numTotDays)
  # day count convention to be consistent with option quotes
  period.pk <- c(period.pre, period.rtc.inter/365 + period.pre)
  period.pk <- period.pk[-length(period.pk)]

  ## price generation process starts here
  set.seed(123)

  # input historical forward prices for power and gas curves
  pwr.curves.all <- read_csv(input_path_pwr, col_names = TRUE)
  pwr.curves <- pwr.curves.all %>% filter(Segment != 'rtcPrice')
  
  ng.curve <- read_csv(input_path_gas, col_names = TRUE)

  curves.comb <- rbind.data.frame(pwr.curves, ng.curve) %>%
    mutate(Component = trimws(Component), Market = NULL)

  curves.comb.all <- rbind.data.frame(pwr.curves.all, ng.curve) %>%
    mutate(Component = trimws(Component), Market = NULL)

  # make delmo as subscript of distinct commodities
  if(segment == 2){
    curves.comb.pivot <- dcast(curves.comb, Date~Component + Delmo + Segment, value.var = 'Price')
  } else{
    curves.comb.pivot <- dcast(curves.comb.all, Date~Component + Delmo + Segment, value.var = 'Price')
  }

  curves.comb.pivot <- cbind(select(curves.comb.pivot[, order(colnames(curves.comb.pivot))], Date),
                             select(curves.comb.pivot[, order(colnames(curves.comb.pivot))], -Date))
  curves.comb.pivot <- curves.comb.pivot[complete.cases(curves.comb.pivot),]
  curves.comb.ret <- log(curves.comb.pivot[-1,-1]/curves.comb.pivot[-dim(curves.comb.pivot)[1],-1])
  rownames(curves.comb.ret) <- curves.comb.pivot$Date[-1]
  curves.comb.cor <- cor(curves.comb.ret)

  price.fwd <- curves.comb.pivot[which(curves.comb.pivot$Date == curve.date.end), -1]

  vol.fwd <- read_csv(input_path_vols, col_names = TRUE)

  ## simulate the forwards
  mu <- rep(0,length(price.fwd))
  s0 <- as.vector(t(price.fwd))
  sigma <- vol.fwd[["VOLATILITY"]]%*%t(vol.fwd[["VOLATILITY"]])*curves.comb.cor

  # the dimensions of the array are commodity, nsteps, and nsims
  sim.prices <- asset.paths(s0, mu, sigma, nsims, periods = period.pk)
  # flatten the array
  sim.prices.long <- do.call('rbind.data.frame',
                             lapply(1:dim(sim.prices)[1], function(x) cbind.data.frame(sim.prices[x,,], idx = x)))
  sim.prices.long$Monthsout <- month.used # cycling
  sim.prices.long <- left_join(data.frame(idx = 1:length(price.fwd), NewComponent = rownames(t(price.fwd))),
                          sim.prices.long, by = "idx")
  sim.prices.long$idx <- NULL # remove idx after joining as key
  sim.prices.long <- melt(sim.prices.long, id.vars = c("Monthsout", "NewComponent"),
                          variable.name = "SimNo", value.name = "Price")
  long.comp.names <- as.data.frame(str_match(sim.prices.long$NewComponent, "^(.*)_(.*)_(.*)$")[, -1])
  sim.prices.long <- cbind.data.frame(sim.prices.long, long.comp.names) %>%
    rename(Component = V1, Delmo = V2, Segment = V3) %>%
    mutate(Delmo = as.Date(Delmo), Monthsout = as.Date(Monthsout), NewComponent = NULL)

  price.fwd.bind <- cbind.data.frame(str_match(names(price.fwd), "^(.*)_(.*)_(.*)$")[, -1], unname(t(price.fwd)))
  colnames(price.fwd.bind) <- c('Component', 'Delmo', 'Segment', 'Forward_Price')
  price.fwd.bind <- mutate(price.fwd.bind, Delmo = as.Date(Delmo))

  # filter out expired contracts if terminal flag is FALSE, otherwise Delmo == Monthsout (cannot use ifelse with dataframe)
  if(terminal == TRUE){
    sim.prices.final.long <- filter(sim.prices.long, Delmo == Monthsout)
  } else {
    sim.prices.final.long <- filter(sim.prices.long, Delmo >= Monthsout)
  }

  sim.prices.final.long <- sim.prices.final.long %>% left_join(price.fwd.bind, by = c('Component', 'Delmo', 'Segment'))

  return(list(outputSim = sim.prices.final.long, outputFwd = curves.comb.all,
              outputCorr = curves.comb.cor))
}

#' Adds column with strike price - INTERNAL
#'
#' round at-the-money underlying price to the increment that matches the
#' option product type.
#'
#' @param data a dataframe that contains the current forward prices
#' @param col a text field that contains the column name in the data frame that
#'   stores the forward prices
#' @param increm a number that specifies what's the increment (eg., 1, 0.5,
#'   0.25) by which the option instrument trades at
#'
#' @return an additional column with the ATM strikes
#' @keywords internal
atmStrike <- function(data = NA, col = NA, increm = 1){
  price <- data[,c(col)]
  price.quotient <- price %/% increm / (1/increm)
  # if equidistant, the lower strike is printed
  price.remainder <- ifelse((price %% increm) <= increm/2, 0, 1*increm)
  data$atmStrike <- price.quotient + price.remainder
  return(data)
}
