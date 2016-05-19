###############################################################################
## Sit & Go rake calculation.
##
## Copyright 2016 Omni Poker Labs <support@omni.poker>
###############################################################################

###_* Declarations =====================================================
library(xtable)

###_* Notes ============================================================
# Dollar values are represented as non-negative doubles.
# Percentage values, frequencies, and probabilities are represented as
# doubles from 0.0 to 1.0.

# TODO
# ====
# take structure into account:
#   - rake per hand
#   - hands per stack depth (-> possible edges/winrates)

###_* HUSNGs ===========================================================
# Input : prizepool contribution and rake in $, rakeback in %
# Output: effective (post-rakeback) rake in $, pre-rakeback rake
#         in % of buyin, post-rakeback rake in % of buyinh
husng.rake.calculate <- function(pp, rake, rakeback) {
  post.rakeback  <- rake * (1.0 - rakeback)
  base.rake      <- rake / (pp + rake)
  effective.rake <- post.rakeback / (pp + post.rakeback)
  c(post.rakeback, base.rake, effective.rake)
}

# Input : prizepool contribution and effective rake in $
# Output: winrate percentage needed to break even
husng.breakeven.winrate <- function(pp, post.rakeback) {
  buyin     <- pp + post.rakeback
  prizepool <- pp * 2
  # buyin = prizepool * winrate
  buyin / prizepool
}

# Input : prizepool contribution and effective rake in $, winrate in %
# Output: ROI implied by winrate, in %
husng.roi <- function(pp, post.rakeback, winrate) {
  buyin     <- pp + post.rakeback
  prizepool <- pp * 2
  ev        <- prizepool * winrate
  profit    <- ev - buyin
  profit / buyin
}

# Input : prizepool contribution and effective rake in $, ROI in %
# Output: winrate (in %) required to achieve ROI
#husng.winrate <- function(pp, post.rakeback, roi) {
#  buyin     <- pp + post.rakeback
#  prizepool <- pp * 2
#  # roi = ((prizepool * winrate) - buyin) / buyin
#  ((roi + 1) * buyin) / prizepool
#}

# Input : buyin table (each row contains prizepool contribution and rake),
#         rakeback deal
# Output: annotated buyin table
husng.table <- function(tab, rakeback) {
  util.map(
    function(x) sapply(seq(from=51, to=55), function(y) husng.roi(x[1], x[3], y / 100)),
    util.map1(
      function(x) husng.breakeven.winrate(x[1], x[3]),
      util.map(
        function(x) husng.rake.calculate(x[1], x[2], rakeback),
        tab)))
}

########################################################################
husng.table.print <- function(tab, rakeback) {
  table           <- husng.table(tab, rakeback)
  table[,4:11]    <- round(table[,4:11] * 100, digits=2)
  colnames(table) <- list(
      "pp", "rake",
      "effective rake", "rake %", "effective %",
      "breakeven",
      "ROI 51%", "ROI 52%", "ROI 53%", "ROI 54%", "ROI 55%"
      )
  util.print(table)
}

husng.turbos <- function() {
  print("stars turbo husng")
  husng.table.print(stars.husng.turbos, stars.rakeback)

  print("ipoker turbo husng")
  husng.table.print(ipoker.husng.turbos, ipoker.rakeback)

  print("wpn turbo husng")
  husng.table.print(wpn.husng.turbos, wpn.rakeback)

  print("mpn turbo husng")
  husng.table.print(mpn.husng.turbos, mpn.rakeback)

  print("party turbo husng")
  husng.table.print(party.husng.turbos, party.rakeback)

  print("eee turbo husng")
  husng.table.print(eee.husng.turbos, eee.rakeback)
}

husng.hypers <- function() {
  print("stars hyper husng")
  husng.table.print(stars.husng.hypers, stars.rakeback)

  print("ipoker hyper husng")
  husng.table.print(ipoker.husng.hypers, ipoker.rakeback)

  print("wpn hyper husng")
  husng.table.print(wpn.husng.hypers, wpn.rakeback)

  print("mpn hyper husng")
  husng.table.print(mpn.husng.hypers, mpn.rakeback)

  print("party hyper husng")
  husng.table.print(party.husng.hypers, party.rakeback)

  print("eee hyper husng")
  husng.table.print(eee.husng.hypers, eee.rakeback)
}

###_* 6max hypers ======================================================
# Rake calculation is same as for HUSNGs
sixmax.rake.calculate <- husng.rake.calculate

# Payout structure
# TODO: store per-site in data.R
sixmax.payouts.first  <- 0.65
sixmax.payouts.second <- 0.35

# Input : prizepool contribution in $, ITM freq and headsup win freq
# Output: expected profit in $
sixmax.payout <- function(pp, itm, win) {
  prizepool <- pp * 6
  fst       <- prizepool * sixmax.payouts.first
  snd       <- prizepool * sixmax.payouts.second
  hu        <- win * fst + (1.0 - win) * snd #$EV once headsup
  itm * hu                                   #probability of getting headsup
}

# Input : prizepool contribution and effective rake in $, win freq
# Output: minimum ITM freq needed to break even with the given win freq
sixmax.breakeven.itm <- function(pp, post.rakeback, win) {
  buyin <- pp + post.rakeback
  util.search(function(itm) sixmax.payout(pp, itm, win), buyin)
}

# Input : prizepool contribution and effective rake in $, ITM freq
# Output: minimum win freq needed to break even with the given ITM freq
#sixmax.breakeven.win <- function(pp, post.rakeback, itm) {
#  buyin <- pp + post.rakeback
#  util.search(function(win) sixmax.payout(pp, itm, win), buyin)
#}

# Input : prizepool contribution and effective rake in $, ITM and win freqs
# Output: ROI implied by ITM and win freqs
sixmax.roi <- function(pp, post.rakeback, itm, win) {
  buyin  <- pp + post.rakeback
  ev     <- sixmax.payout(pp, itm, win)
  profit <- ev - buyin
  profit / buyin
}

# Input : buyin table (each row contains prizepool contribution and rake),
#         rakeback deal
# Output: annotated buyin table
sixmax.table <- function(tab, rakeback) {
  util.map(
    function(x) sapply(seq(from=35, to=39),
                       function(y) sixmax.roi(x[1], x[3], y / 100, 0.50)),
    util.map(
      function(x) sapply(seq(from=45, to=55, by=5),
                         function(y) sixmax.breakeven.itm(x[1], x[3], y / 100)),
      util.map(
        function (x) sixmax.rake.calculate(x[1], x[2], rakeback),
        tab)))
}

########################################################################
sixmax.table.print <- function(tab, rakeback) {
  table           <- sixmax.table(tab, rakeback)
  table[,4:13]    <- round(table[,4:13] * 100, digits=2)
  colnames(table) <- list(
      "pp", "rake",
      "effective rake", "rake %", "effective %",
      "breakeven 45%", "breakeven 50%", "breakeven 55%",
      "ROI 35%", "ROI 36%", "ROI 37%", "ROI 38%", "ROI 39%"
      )
  util.print(table)
}

sixmax.hypers <- function() {
  print("stars 6max hypers")
  sixmax.table.print(stars.sixmax.hypers, stars.rakeback)

  print("ipoker 6max hypers")
  sixmax.table.print(ipoker.sixmax.hypers, ipoker.rakeback)

  print("wpn 6max hypers")
  sixmax.table.print(wpn.sixmax.hypers, wpn.rakeback)

  print("mpn 6max hypers")
  sixmax.table.print(mpn.sixmax.hypers, mpn.rakeback)

  print("party 6max hypers")
  sixmax.table.print(party.sixmax.hypers, party.rakeback)

  print("eee 6max hypers")
  sixmax.table.print(eee.sixmax.hypers, eee.rakeback)
}

###_* Spins ============================================================
# Input : buyin in $ and multiplier table (each row contains multiplier,
#         numerator and denominator)
# Output: average payout in $
spin.payout <- function(buyin, tab) {
  sum(apply(tab, 1, function(x) buyin * x[1] * (x[2] / x[3])))
}

# Input : buyin, multiplier table (see above), and rakeback deal (in %)
# Output: rake in $, pre- and post-rakeback rake in %
spin.rake.calculate <- function(buyin, tab, rakeback) {
  pp.plus.rake   <- buyin * 3
  prizepool      <- spin.payout(buyin, tab)
  rake           <- (pp.plus.rake - prizepool) / 3.0
  pp             <- buyin - rake
  stopifnot(pp == prizepool / 3.0)

  post.rakeback  <- rake * (1.0 - rakeback)
  base.rake      <- rake / (pp + rake)
  effective.rake <- post.rakeback / (pp + post.rakeback)

  c(rake, base.rake, effective.rake)
}

spin.rake.calculate.pessimistic <- function(buyin, tab, rakeback)
  spin.rake.calculate(buyin, spin.pessimistic(tab), rakeback)

# Input : multiplier table
# Output: multiplier table with the least likely multipliers removed
#
# TODO  : normalize probs and remove multipliers with probability less
#         than some cutoff
spin.pessimistic <- function(tab) tab[-1,][-1,]

# Input : buyin and rake in $, rakeback deal in %, multiplier table
# Output: winrate needed to break even (in %)
spin.breakeven.winrate <- function(buyin, rake, rakeback, tab) {
  cost      <- buyin - (rake * rakeback)
  prizepool <- spin.payout(buyin, tab)
  # cost = prizepool * winrate
  cost / prizepool
}

spin.breakeven.winrate.pessimistic <- function(buyin, rake, rakeback, tab)
  spin.breakeven.winrate(buyin, rake, rakeback, spin.pessimistic(tab))

# Input : buyin and rake in $, rakeback deal in %, multiplier table, winrate
# Output: ROI in %
spin.roi <- function(buyin, rake, rakeback, tab, winrate) {
  cost      <- buyin - (rake * rakeback)
  prizepool <- spin.payout(buyin, tab)
  ev        <- prizepool * winrate
  profit    <- ev - cost
  profit / cost
}

spin.roi.pessimistic <- function(buyin, rake, rakeback, tab, winrate)
  spin.roi(buyin, rake, rakeback, spin.pessimistic(tab), winrate)

# Input : buyin and rake in $, rakeback deal in %, multiplier table, desired ROI %
# Output: required winrate in %
#spin.winrate <- function(buyin, rake, rakeback, tab, roi) {
#  cost      <- buyin - (rake * rakeback)
#  prizepool <- spin.payout(buyin, tab)
#  # roi    = (profit / cost) - 1.0
#  # profit = prizepool * winrate
#  ((roi + 1.0) * cost) / prizepool
#}

# Input : buyin table, multiplier table, rakeback deal
# Output: annotated buyin table
#
# TODO  : handle one table per buyin class
spin.table <- function(buyins, mults, rakeback) {
  util.map(
    function(x) sapply(seq(from=35, to=39),
                       function(y) spin.roi.pessimistic(x[1], x[2], rakeback, mults[x[1]][[1]], y / 100)),
    util.map(
        function(x) sapply(seq(from=35, to=39),
                           function(y) spin.roi(x[1], x[2], rakeback, mults[x[1]][[1]], y / 100)),
        util.map1(
            function(x) spin.breakeven.winrate.pessimistic(x[1], x[2], rakeback, mults[x[1]][[1]]),
            util.map1(
                function(x) spin.breakeven.winrate(x[1], x[2], rakeback, mults[x[1]][[1]]),
                util.map(
                    function(x) spin.rake.calculate.pessimistic(x[1], mults[x[1]][[1]], rakeback),
                    util.map(
                        function (x) spin.rake.calculate(x[1], mults[x[1]][[1]], rakeback),
                        buyins))))))
}

########################################################################
spin.table.print <- function(buyins, mults, rakeback) {
  table                <- spin.table(buyins, mults, rakeback)
  table[,c(3:4, 6:18)] <- round(table[,c(3:4, 6:18)] * 100, digits=2)
  colnames(table)      <- list(
      "buyin",
      "rake", "rake %", "effective %",
      "pessimistic rake", "pessimistic rake %", "pessimistic effective %",
      "breakeven", "pessimistic breakeven",
      "ROI 35%", "ROI 36%", "ROI 37%", "ROI 38%", "ROI 39%",
      "pessimistic ROI 35%", "pessimistic ROI 36%", "pessimistic ROI 37%", "pessimistic ROI 38%", "pessimistic ROI 39%"
      )
  util.print(table)
}

spins <- function() {
  print("stars spins")
  spin.table.print(stars.spins, stars.spins.multipliers, stars.rakeback)

  print("ipoker spins")
  spin.table.print(ipoker.spins, ipoker.spins.multipliers, ipoker.rakeback)

  print("wpn spins")
  spin.table.print(wpn.spins, wpn.spins.multipliers, wpn.rakeback)
}

###_* util =============================================================
# Input :
# Output:
util.map <- function(f, xs) {
  cbind(xs, t(apply(xs, 1, f)))
}

# Input :
# Output:
util.map1 <- function(f, xs) {
  cbind(xs, apply(xs, 1, f))
}

# Input : a function f and a cutoff value y
# Output: the smallest x for which f(x) >= y
util.search <- function(f, y) {
  for (x in seq(from=0.0, to=1.0, by=0.01))
    if (f(x) >= y) return(x)
}

# Input :
# Output:
util.print <- function(table) {
  if (html)
    print(xtable(table), type = "html", include.rownames=FALSE)
  else
    print(table)
}

html <- FALSE

###_* Emacs ============================================================
# Local Variables:
# allout-layout: t
# ess-indent-level: 2
# End:
