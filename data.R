###_* PokerStars/FullTilt ==============================================
stars.husng.turbos <- rbind(
  c(   6.67, 0.33),
  c(  14.39, 0.61),
  c(  28.78, 1.22),
  c(  57.67, 2.33),
  c(  96.32, 3.68),
  c( 193.85, 6.15),
  c( 291.25, 8.75),
  c( 487.60, 12.40),
  c( 979.20, 20.80),
  c(1962.50, 37.50),
  c(4937.00, 63.00)
)

stars.husng.hypers <- rbind(
  c(  6.85,  0.15),
  c( 14.69,  0.31),
  c( 29.37,  0.63),
  c( 58.74,  1.26),
  c( 98.12,  1.88),
  c(196.66,  3.34),
  c(295.51,  4.49),
  c(493.34,  6.66),
  c(988.83, 11.17)
)

stars.sixmax.hypers <- rbind(
  c(  6.71,  0.29),
  c( 14.41,  0.59),
  c( 28.83,  1.17),
  c( 57.66,  2.34),
  c( 96.49,  3.51),
  c(193.52,  6.48),
  c(291.40,  8.60),
  c(487.52, 12.48),
  c(979.14, 20.86)
)

# N.B: dropping first two rows to get probability cutoff > 1 / 20k
# conveniently works for here as well
# TODO: model 2nd and 3rd place payouts:
#   ``The majority of Jackpot Sit & Go's are played as winner-takes-all
#   tournaments, except for when the spinner hits one of the three
#   highest prize tiers. In these Jackpot Sit & Go's no player walks
#   away empty-handed as the second and third-place finishers will also
#   receive 10% of the first-place prize.''

stars.spins <- rbind(
  c(7),
  c(15),
  c(30),
  c(60),
  c(100)
)

stars.spins.low <- rbind(
  c(12000,      1, 1000000),
  c(  240,     50, 1000000),
  c(  120,    100, 1000000),
  c(   25,   1000, 1000000),
  c(   10,   5000, 1000000),
  c(    6,  75000, 1000000),
  c(    4, 195651, 1000000),
  c(    2, 723198, 1000000)
)

stars.spins.mid <- rbind(
  c(12000,      1, 1000000),
  c(  240,     50, 1000000),
  c(  120,    100, 1000000),
  c(   25,   1000, 1000000),
  c(   10,   5000, 1000000),
  c(    6,  75000, 1000000),
  c(    4, 210651, 1000000),
  c(    2, 708198, 1000000)
)

stars.spins.high <- rbind(
  c(12000,      1, 1000000),
  c(  240,     50, 1000000),
  c(  120,    100, 1000000),
  c(   25,   1000, 1000000),
  c(   10,   5000, 1000000),
  c(    6,  75000, 1000000),
  c(    4, 225651, 1000000),
  c(    2, 693198, 1000000)
)

stars.spins.multipliers <- {
  lst        <- list()
  lst[[7]]   <- stars.spins.low
  lst[[15]]  <- stars.spins.mid
  lst[[30]]  <- stars.spins.mid
  lst[[60]]  <- stars.spins.high
  lst[[100]] <- stars.spins.high
  lst
}

stars.rakeback <- 0.30

###_* iPoker ===========================================================
ipoker.husng.turbos <- rbind(
  c(   4.65,  0.35),
  c(   9.40,  0.60),
  c(  18.80,  1.20),
  c(  28.30,  1.70),
  c(  47.50,  2.50),
  c(  95.00,  5.00),
  c( 192.00,  8.00),
  c( 290.00, 10.00),
  c( 485.00, 15.00),
  c( 979.00, 21.00),
  c(1960.00, 40.00),
  c(4950.00, 50.00)
)

ipoker.husng.hypers <- rbind(
  c(   4.80,  0.20),
  c(   9.60,  0.40),
  c(  19.20,  0.80),
  c(  28.80,  1.20),
  c(  48.70,  1.30),
  c(  97.50,  2.50),
  c( 195.00,  5.00),
  c( 294.00,  6.00),
  c( 491.00,  9.00),
  c( 982.00, 18.00),
  c(1970.00, 30.00),
  c(4950.00, 50.00)
)

ipoker.sixmax.hypers <- rbind(
  c( 4.60, 0.40),
  c( 9.20, 0.80),
  c(18.40, 1.60),
  c(46.00, 4.00)
)

ipoker.spins <- rbind(
  c(1),
  c(2),
  c(5),
  c(10)
)

ipoker.spins.all <- rbind(
  c(1000,     1, 20000),
  c( 200,     1, 20000),
  c( 100,     2, 20000),
  c(  50,     5, 20000),
  c(  20,    20, 20000),
  c(  10,   160, 20000),
  c(   6,  1600, 20000),
  c(   4,  3025, 20000),
  c(   2, 15186, 20000)
)

ipoker.spins.multipliers <- {
  lst       <- list()
  lst[[1]]  <- ipoker.spins.all
  lst[[2]]  <- ipoker.spins.all
  lst[[5]]  <- ipoker.spins.all
  lst[[10]] <- ipoker.spins.all
  lst
}

# TODO: model source-based method
ipoker.rakeback <- 0.40

###_* WPN (e.g. America's Cardroom) ====================================
wpn.husng.turbos <- rbind(
  c(   5.50,  0.25),
  c(  11.00,  0.50),
  c(  22.00,  1.00),
  c(  33.00,  1.50),
  c(  55.00,  2.50),
  c( 110.00,  4.50),
  c( 220.00,  9.00),
  c( 330.00, 12.00),
  c( 550.00, 20.00),
  c(1100.00, 30.00)
)

wpn.husng.hypers <- rbind(
  c(   6.00,  0.18),
  c(  12.00,  0.25),
  c(  24.00,  0.50),
  c(  36.00,  0.75),
  c(  60.00,  1.25),
  c( 120.00,  2.25),
  c( 240.00,  4.50),
  c( 360.00,  6.00),
  c( 600.00, 10.00),
  c(1200.00, 15.00)
)

wpn.sixmax.hypers <- rbind(
  c(  5.00, 0.22),
  c( 10.00, 0.41),
  c( 15.00, 0.60),
  c( 20.00, 0.75),
  c( 30.00, 0.97),
  c( 50.00, 1.55),
  c( 80.00, 2.20),
  c(100.00, 2.75)
)

# TODO: Default distribution is: 1st: 75%, 2nd: 15% and 3rd: 10%.
# Deal option which must be approved by each player: 1st: 40%, 2nd: 30% and 3rd: 30%

wpn.spins <- rbind(
  c(2),
  c(10),
  c(25),
  c(40)
)

wpn.spins.all <- rbind(
  c(2500,     1, 100000),
  c( 200,     5, 100000),
  c( 100,    10, 100000),
  c(  20,   100, 100000),
  c(   8,   500, 100000),
  c(   6,  7500, 100000),
  c(   4, 21366, 100000),
  c(   2, 70518, 100000)
)

wpn.spins.multipliers <- {
  lst       <- list()
  lst[[2]]  <- wpn.spins.all
  lst[[10]] <- wpn.spins.all
  lst[[25]] <- wpn.spins.all
  lst[[40]] <- wpn.spins.all
  lst
}

wpn.rakeback <- 0.27

###_* MPN (e.g. betsson) ===============================================
mpn.husng.turbos <- rbind(
  c(   5.00,  0.30),
  c(  10.00,  0.60),
  c(  20.00,  1.20),
  c(  30.00,  1.80),
  c(  50.00,  3.00),
  c( 100.00,  6.00),
  c( 200.00,  8.00),
  c( 300.00, 12.00),
  c( 500.00, 15.00),
  c(1000.00, 30.00)
)

mpn.husng.hypers <- rbind(
  c(   5.00,  0.20),
  c(  10.00,  0.40),
  c(  20.00,  0.80),
  c(  30.00,  1.20),
  c(  50.00,  2.00),
  c( 100.00,  4.00),
  c( 200.00,  5.00),
  c( 300.00,  7.50),
  c( 500.00, 10.00),
  c(1000.00, 20.00)
)

mpn.sixmax.hypers <- rbind(
  c(  5.00,  0.30),
  c( 10.00,  0.60),
  c( 20.00,  1.20),
  c( 30.00,  1.80),
  c( 50.00,  3.00),
  c(100.00,  6.00),
  c(200.00, 10.00)
)

# TODO: fish party

mpn.rakeback <- 0.30

###_* PartyPoker =======================================================
party.husng.turbos <- rbind(
  c(  5.06,  0.44),
  c( 10.25,  0.75),
  c( 20.50,  1.50),
  c( 51.00,  4.00),
  c(101.10,  7.90),
  c(205.00, 10.00),
  c(520.00, 10.00)
)

party.husng.hypers <- rbind(
  c(  5.12, 0.38),
  c( 10.25, 0.75),
  c( 20.50, 1.50),
  c( 51.25, 3.75),
  c(101.60, 7.40),
  c(206.40, 8.60)
)

party.sixmax.hypers <- rbind(
  c(  5.00,  0.50),
  c( 10.00,  1.00),
  c( 20.00,  2.00),
  c( 51.00,  4.00),
  c(101.50,  7.50),
  c(200.00, 15.00)
)

# TODO: sit and go hero

party.rakeback <- 0.212 #max 0.305

###_* 888 ==============================================================
eee.husng.turbos <- rbind(
  c(  4.72,  0.28),
  c(  9.52,  0.48),
  c( 19.05,  0.95),
  c( 28.60,  1.40),
  c( 47.75,  2.25),
  c( 96.25,  3.75),
  c(194.00,  6.00),
  c(488.00, 12.00),
  c(980.00, 20.00)
)

eee.husng.hypers <- rbind(
  c(  4.69,  0.31),
  c(  9.60,  0.40),
  c( 19.22,  0.78),
  c( 28.85,  1.15),
  c( 48.10,  1.90),
  c( 97.10,  2.90),
  c(196.50,  3.50),
  c(494.00,  6.00),
  c(988.00, 12.00)
)

eee.sixmax.hypers <- rbind(
  c(  4.70, 0.30),
  c(  9.45, 0.55),
  c( 19.05, 0.95),
  c( 28.60, 1.40),
  c( 47.80, 2.20),
  c( 96.30, 3.70),
  c(193.40, 6.60)
)

eee.rakeback <- 0.27 # max 0.36

###_* Emacs ============================================================
# Local Variables:
# allout-layout: t
# ess-indent-level: 2
# End:
