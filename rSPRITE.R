# Written by Nick Brown (nicholasjlbrown@gmail.com), 2018-2021.
# This work is licensed under a Creative Commons Attribution 4.0 International License (CC-BY).
#  See http://creativecommons.org/licenses/by/4.0/
# Thanks to Cédric Batailler for help with the X-axis.

# Version history
# 2018-02-19 16:08 UTC 0.01
#   First Shiny version released.
# 2018-02-19 17:23 UTC 0.02
#   Improved the look of the X-axis (numbers can go sideways if they start to bunch up).
#   Added an upper limit for the X-axis for when no items get close to the scale maximum.
#   Added code to use human-friendly intervals on the Y-axis.
# 2018-02-19 18:31 UTC 0.03
#   Fixed a bug that caused a previous error/warning message to hang around on the next run.
#   Added version number display
# 2018-02-19 21:43 UTC 0.04
#   Added input elements to allow a specific response value to appear a fixed number of times.
# 2018-02-20 17:21 UTC 0.05
#   Fixed a bug that caused a crash 50% of the time when scaleMin and scaleMax were both negative.
#   Added dynamic updates of upper/lower bounds of input controls, depending on the values of others.
# 2018-02-21 15:07 UTC 0.06
#   Fixed a bug that caused spurious error messages with certain fixed-value configurations.
#   Plots now appear sorted from smallest to largest skewness.
#   Added a rudimentary help feature.
#   Fixed a bug that meant that the user couldn't easily type in a SD close to the permitted minimum.
#   Fixed a bug that could cause errors in extreme cases with delta=2 in rSprite.delta.
#   Improved performance by taking a pragmatic decision about when to stop looking for duplicates.
# 2018-02-21 23:22 UTC 0.07
#   Added link to enable user to download the data that went into the plots.
#   Fixed a bug that was preventing solutions from being found with very large means and very small SDs.
# 2018-03-03 23:46 UTC 0.08
#   Increased the size of the plot area.
#   Increased maximum grid size to 10 x 10.
#   Changed plot bar colour for better visibility if black text encroaches on bars.
#   Reduced the chances of missing a valid solution when only a few (requested number > n > 1) exist.
#   Changed displayed name to rSprite.
# 2018-03-24 15:00 UTC 0.09
#   Display solutions on the smallest grid that they will fit onto.
#   User now chooses the number of results they want, not the grid size.
#   Moved the decimal places input field to just below the mean and SD.
#   Fixed a bug that could cause spurious solutions to be returned if none were possible.
# 2018-03-27 20:23 UTC 0.10
#   Fixed a bug that could cause the "No solution found" message to be split into two.
#   Fixed a bug that prevented entering 0 or a negative number as the fixed value.
#   Fixed a bug that prevented a solution from being found in some extreme circumstances.
#   Fixed a bug that produced variable bar widths with large X-axis ranges.
# 2018-04-18 13:50 UTC 0.11
#   Fixed a bug that prevented the SD granularity from being changed.
#   Tightened the restrictions on the maximum SD that can be entered.
#   Moved the scale limit fields to the top of the list.
#   Fixed a small bug that sometimes showed more ticks than necessary on the X-axis.
#   Allow fixed values to be outside the scale range.
#   Changed displayed name to rSPRITE.
# 2018-05-22 13:32 UTC 0.12
#   Fixed a bug that caused a failure to calculate the possible SD range in some extreme samples.
# 2018-05-26 19:27 UTC 0.13
#   Added note about privatcy to the help text.
#   Added blank line before download link.
#   Added "loading" spinner image.
# 2018-11-08 23:40 UTC 0.14
#   Increased the size of the plot area.
#   Changed help text to point to preprint article instead of James's blog post.
#   Added CC-BY license.
#   Fixed a small bug that caused slightly different X-axis widths depending on the data.
# 2019-06-02 20:56 UTC 0.15
#   Fixed a bug that could cause valid SDs to be rejected as too small with means near the scale limits.
# 2020-06-23 21:34 UTC 0.16
#   Increased maximum sample size to 10000.
#   Fixed a bug that could make it impossible to enter a target SD in a small number of cases.
#   Added code to make input fields slightly less reactive.
#       I am trying two ways to avoid the Shiny reactivity issue whereby with, for example,
#       scale value min=3 and max=50, the user wants to type 25 for the mean but the 2 immediately gets changed to 3.
#       #limits: include/exclude these lines to toggle automatic checking of mean/SD limits
#       #bounce: include/exclude these lines to toggle debouncing of mean/SD values
#       In version 0.16, I'm including the "debounce" code. The alternative is to not check the limits.
#   Fixed a bug that could cause the samples to be biased towards smaller numbers in the range, especially with more extreme SDs
#    (thanks to Frank Gootjes for pointing this out).
#   Changed performance parameters to reduce the chance of missing a valid solution, especially with fixed values.
#   Added a progress counter to keep track of unique solutions as they are found.
#   Added a message to indicate when searching has finished and plotting of the results has started.
# 2021-07-20 17:10 UTC 0.17
#   Allowed the number of a fixed value to be set to zero (i.e., that value will not appear in the generated sample).
#   Allowed decimal places to be set to zero.
#   Added link to GitHub.
#   Fixed a bug that could cause the display to be distorted if the X-axis started at a value above 0.
#   Removed bounds on scale maximum/minimum input values. Caveat usor.
#   Fixed a bug that could cause no solution to be found when fixed values were used and there was a small number of possible solutions.
# 2022-08-25 20:35 UTC 0.18
#   Documented the fact that the forced count of a value can be zero.
#   Force input values for scale minimum and maximum to be integers.
#   Improvemed the determination of the limits of the SD, cf.
#    https://github.com/sTeamTraen/rSPRITE/issues/1 (h/t Lukas Wallrich).

# To do:

library(ggplot2)
library(gridExtra)
library(moments)
library(shiny)

# Parameters that trade off speed versus completeness.
# maxDeltaLoopsLower controls how many times we tweak pairs of numbers before giving up hope of finding any solution;
#  it is the lower bound on a formula that includes the sample size and range.
# maxDeltaLoopsUpper is the absolute upper bound on that formula (a sanity check, in effect).
# maxDupLoops controls how many times we try to find another unique solution, when we know that at least one exists.
rSprite.maxDeltaLoopsLower <- 20000
rSprite.maxDeltaLoopsUpper <- 1000000
rSprite.maxDupLoops <- 20

# Code taken from https://gist.github.com/jcheng5/6141ea7066e62cafb31c
# Returns a reactive element that debounces the given expression by the given time in milliseconds.
#
# This is not a true debounce in that it will not prevent \code{expr} from being called many times
#  (in fact it may be called more times than usual), but rather, the reactive invalidation signal that is
#  produced by expr is debounced instead. This means that this function should be used when \code{expr} is
# cheap but the things it will trigger (outputs and reactives that use \code{expr}) are expensive.
debounce <- function(expr, millis, env=parent.frame(), quoted=FALSE, domain=getDefaultReactiveDomain()) {
  force(millis)

  f <- exprToFunction(expr, env, quoted)
  label <- sprintf("debounce(%s)", paste(deparse(body(f)), collapse = "\n"))

  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )

  # Responsible for tracking when f() changes.
  observeEvent(f(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis/1000
  }, ignoreNULL = FALSE)

  # This observer is the timer. It rests until v$when elapses, then touches v$trigger.
  observe({
    if (is.null(v$when)) {
      return()
    }

    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    }
    else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })

  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
}

rSprite.huge <- 1e15
rSprite.dust <- 1e-12

# Store a message for future display (unless otherwise specified).
# If we have unloaded Shiny for debugging, show the message immediately.
rSprite.message <- function (s, shinyType="default", showNow=FALSE) {
  if (!exists("shinyUI")) {
    cat("rSPRITE message: |", s, "| (shinyType=", shinyType, ")", "\n", sep="")
    return()
  }

  message <- paste(shinyType, s, sep="%%")
  if (showNow) {
    rSprite.shinyMessages(list(message))
  }
  else {
    rSprite.messageList <<- append(rSprite.messageList, message)
  }
}

# See if a mean is GRIM-consistent. If not, return the nearest mean that is.
rSprite.checkGrim <- function (N, tMean, dp) {
  gMean <- tMean
  int <- round(tMean * N)           # nearest integer; doesn't matter if this rounds up or down
  frac <- int / N
  dif <- abs(tMean - frac)
  granule <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors
  if (dif > granule) {
    gMean <- round(frac, dp)
    dpformat <- paste("%.", dp, "f", sep="")
    s <- paste("Mean ", sprintf(dpformat, tMean), " fails GRIM test - using ", sprintf(dpformat, gMean), sep="")
    rSprite.message(s, shinyType="warning")
  }

  return(gMean)
}

# Determine minimum and maximum SDs for given scale ranges, N, and mean.
rSprite.sdLimits <- function (N, tMean, scaleMin, scaleMax, dp) {
  result <- c(rSprite.huge, -rSprite.huge)        # impossible values

  aMax <- scaleMin                                # "aMax" means "value of a to produce the max SD"
  aMin <- floor(tMean)
  bMax <- max(scaleMax, scaleMin + 1, aMin + 1)   # sanity check (just scaleMax would normally be ok)
  bMin <- aMin + 1
  total <- round(tMean * N)
  for (abm in list(c(aMin, bMin, 1), c(aMax, bMax, 2))) {
    a <- abm[1]
    b <- abm[2]
    m <- abm[3]

    k <- round((total - (N * b)) / (a - b))
    k <- min(max(k, 1), N - 1)               # ensure there is at least one of each of two numbers
    vec <- c(rep(a, k), rep(b, N - k))
    diff <- sum(vec) - total

    if (diff < 0) {
      vec <- c(rep(a, k - 1), a + abs(diff), rep(b, N - k))
    }
    else if (diff > 0) {
      vec <- c(rep(a, k), b - diff, rep(b, N - k - 1))
    }

    result[m] <- round(sd(vec), dp)
  }

  return(result)
}

# Make a single tweak to the data to try and move the SD in the desired direction.
# This will usually subtract 1 (or 2) from one element and add the same to another,
#  thus preserving the mean, but occasionally it will just do one of those things,
#  if the resulting mean is still GRIM-consistent.
rSprite.delta <- function (vec, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c(), never=c()) {
  originalVec <- vec
  avoid <- NA
  if (length(fixed) > 0) {
    avoid <- fixed[1]
  }
  else if (length(never) > 0) {
    avoid <- never[1]
  }

# Decide if we want to increment or decrement first.
  incFirst <- (runif(1) < 0.5)

# Decide if we need to increase or decrease the SD.
  fullVec <- c(vec, fixed)
  increaseSD <- (sd(fullVec) < tSD)

# Most of the time we change a pair of numbers by +/- 1, but changing by +/- 2 allows us to jump over fixed or "never" numbers.
  absDelta <- 1
  if (    !is.na(avoid)
       && (runif(1) < 0.2)
     ) {
    # Check there is at least one number that we can increment or decrement by 2!
    TwoFromEnd <- if (incFirst) (vec < (scaleMax - 1)) else (vec > (scaleMin + 1))
    if ((length(vec[TwoFromEnd])) > 0) {
      absDelta <- 2
    }
  }

  maxToInc <- scaleMax - absDelta                       # maximum value that we can increment
  minToDec <- scaleMin + absDelta                       # minimum value that we can decrement

# Select an element to increment or decrement.
# For better performance, we select from unique elements only; this means that any number that appears in the vector is
#  equally likely to be chosen regardless of how often it appears. I'm not sure if this is good or bad.
  uniqueCanBump1 <- !duplicated(vec)
  delta1 <- if (incFirst) absDelta else -absDelta     # actual value we will add when bumping first number

# The element that we change should be less than the maximum (increment) or greater than the minimum (decrement).
# It should also not be <delta> less/greater than a fixed/"never" value (because adding delta would give us that value).
  notFixed1 <- if (!is.na(avoid)) (vec != (avoid - delta1)) else TRUE
  notEdge1 <- if (delta1 > 0) (vec <= maxToInc) else (vec >= minToDec)
  indexCanBump1 <- uniqueCanBump1 & notFixed1 & notEdge1

# If we can't find an element to change, just return the original vector and let our caller sort it out.
  if (sum(indexCanBump1) == 0) {
    return(originalVec)
  }

# Unless we have no other choice:
# - If we want to make the SD larger, there is no point in incrementing the smallest element, or decrementing the largest
# - If we want to make the SD smaller, there is no point in decrementing the smallest element, or incrementing the largest
  if (increaseSD) {
    noPoint1 <- if (incFirst) (vec == min(vec)) else (vec == max(vec))
  }
  else {
    noPoint1 <- if (incFirst) (vec == maxToInc) else (vec == minToDec)
  }
  indexCanBump1Try <- indexCanBump1 & (! noPoint1)
  if (sum(indexCanBump1Try) > 0) {
    indexCanBump1 <- indexCanBump1Try
  }

  whichCanBump1 <- which(indexCanBump1)
  whichWillBump1 <- whichCanBump1[as.integer(runif(1) * length(whichCanBump1)) + 1];
  willBump1 <- vec[whichWillBump1]
  vec[whichWillBump1] <- vec[whichWillBump1] + delta1

# At this point we can decide to only change one of the elements (decrement one without incrementing another, or vice versa).
# This enables us to explore different means that still round to the same target value.
# So here we perform the first increment or decrement first, and see if the mean is still GRIM-consistent with the target mean.
# If it is, then in a proportion of cases we don't adjust the other cell.
  newFullVec <- c(vec, fixed)
  newMean <- mean(newFullVec)
  meanChanged <- (round(newMean, dp) != tMean) # new mean is no longer GRIM-consistent

  if (meanChanged || (runif(1) < 0.4)) {
    delta2 <- -delta1                          # apply the opposite delta to a different element
    vecBump2 <- vec                            # make a scratch copy of the input vector so we can change it
    vecBump2[whichWillBump1] <- rSprite.huge   # remove the element chosen in part 1...
    uniqueCanBump2 <- !duplicated(vecBump2)    # ... but if there was more than one copy of that, it's still a candidate
    notFixed2 <- if (!is.na(avoid)) (vec != (avoid - delta2)) else TRUE
    notEdge2 <- if (delta2 > 0) (vec <= maxToInc) else (vec >= minToDec)
    indexCanBump2 <- uniqueCanBump2 & notFixed2 & notEdge2 & (vecBump2 != rSprite.huge)

# If we can't find an element to change in the opposite direction to the first, then if the mean with the first change is still OK,
#  we return either the vector with that change. Otherwise we return the original vector and let our caller sort it out.
    if (sum(indexCanBump2) == 0) {
      return(if (meanChanged) originalVec else vec)
    }

# Unless we have no other choice:
# - If we want to make the SD larger:
#   - If in step 1 we chose an element to increment, there is no point in now changing (decrementing) a larger one
#   - If in step 1 we chose an element to decrement, there is no point in now changing (incrementing) a smaller one
# - If we want to make the SD smaller:
#   - If in step 1 we chose an element to increment, there is no point in now changing (decrementing) a smaller one
#   - If in step 1 we chose an element to decrement, there is no point in now changing (incrementing) a larger one
# There is also no point in incrementing an element that is equal to the new value of the one that we have already chosen.
    noPoint2 <- (   (if (increaseSD == incFirst) (vec > willBump1) else (vec < willBump1))
                  | (vec == (willBump1 + delta1))
                )
    indexCanBump2Try <- indexCanBump2 & (! noPoint2)
    if (sum(indexCanBump2Try) > 0) {
      indexCanBump2 <- indexCanBump2Try
    }

    whichCanBump2 <- which(indexCanBump2)
    whichWillBump2 <- whichCanBump2[as.integer(runif(1) * length(whichCanBump2)) + 1];
    vec[whichWillBump2] <- vec[whichWillBump2] + delta2
  }

  return(vec)
}

# Build the label to go across the top of each results chart.
rSprite.chartLabel <- function (N, tMean, tSD, scaleMin, scaleMax, dp, splitLine) {
  dpformat <- paste("%.", dp, "f", sep="")
  label <- paste("N=", N
                 , " (", scaleMin, "-", scaleMax, ")%%"
                 , "M=", sprintf(dpformat, tMean)
                 , " SD=", sprintf(dpformat, tSD)
                 , sep=""
  )

  if (splitLine) {
    label <- unlist(strsplit(label, "%%"))
  }
  else {
    label <- gsub("%%", " ", label)
  }

  return (label)
}

# Find a single vector of responses that matches the target mean and SD.
# Assumes that the mean has been checked for GRIM consistency (see rSprite.getSample).
rSprite.seekVector <- function (N, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c(), never=c(), label) {
  avoid <- NA
  if (length(fixed) > 0) {
    avoid <- fixed[1]
  }
  else if (length(never) > 0) {
    avoid <- never[1]
  }

# Generate some random starting data.
  rN <- N - length(fixed)
  scaleMinZB <- 0
  scaleMaxZB <- scaleMax - scaleMin
  tMeanZB <- tMean - scaleMin
  vec <- pmax(pmin(as.integer(runif(rN) * (2 * tMean + 1)), scaleMax), scaleMin)
  result <- c()

  if (!is.na(avoid)) {         # replace any of the fixed numbers with a random non-fixed number
    whichFixed <- which(vec == avoid)
    notFixed <- sample(setdiff(min(vec):max(vec), avoid), length(whichFixed), replace=TRUE)
    vec[whichFixed] <- notFixed
  }

# Adjust mean of starting data.
  granule <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors
  meanOK <- FALSE
  maxStartLoops <- N * (scaleMax - scaleMin)

  for (i in 1:maxStartLoops) {
    fullVec <- c(vec, fixed)
    cMean <- mean(fullVec)
    dif <- abs(cMean - tMean)
    if (dif < granule) {
      meanOK <- TRUE
      break;
    }

# Identify numbers that we can increment or decrement.
# This should exclude numbers that would become one of the fixed values.
    deltaMean <- 1
    if (    !is.na(avoid)
         && (runif(1) < 0.2)
    ) {
      deltaMean <- 2       # This allows us to "jump over" the fixed/"never" values, if they are not at the extremities.
    }

    increaseMean <- (cMean < tMean)
    if (increaseMean) {
      filter <- (vec < (scaleMax - deltaMean + 1))
      if (!is.na(avoid)) {
        filter <- filter & (vec != (avoid - deltaMean))
      }
    }
    else {
      filter <- (vec > (scaleMin + deltaMean - 1))
      if (!is.na(avoid)) {
        filter <- filter & (vec != (avoid + deltaMean))
      }
    }

    canBumpMean <- which(filter)
    bumpMean <- canBumpMean[as.integer(runif(1) * length(canBumpMean)) + 1]   # select a changeable number
    vec[bumpMean] <- vec[bumpMean] + (if (increaseMean) deltaMean else -deltaMean)
  }

  if (!meanOK) {
    s <- "Couldn't initialize data with correct mean"  # this probably indicates a coding error, if the mean is in range
    rSprite.message(s, shinyType="error")
    return(result)
  }

  maxLoops <- min(max(round(N * ((scaleMax - scaleMin) ^ 2)), rSprite.maxDeltaLoopsLower), rSprite.maxDeltaLoopsUpper)
  found <- FALSE

  for (i in 1:maxLoops) {
    cSD <- sd(c(vec, fixed))
    if (abs(cSD - tSD) <= granule) {
      result <- vec
      break
    }

    vec <- rSprite.delta(vec, tMean, tSD, scaleMin, scaleMax, dp, fixed, never)
  }

  return(result)
}

# Generate a sample of one or more unique SPRITE solutions.
rSprite.getSample <- function (maxCases, N, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c(), never=c()) {
  result <- list(rows=c(), label="")

# Check mean is possible with GRIM; if not, identify the nearest valid mean.
  tMean <- rSprite.checkGrim(N, tMean, dp)

# Determine minimum and maximum SDs.
  sdLimits <- rSprite.sdLimits(N, tMean, scaleMin, scaleMax, dp)

  for (m in 1:2) {
    mSD <- sdLimits[m]
    s <- ""
    if ((m == 1) && (mSD > tSD)) {
      s <- "small; minimum="
    }
    else if ((m == 2) && (mSD < tSD)) {
      s <- "large; maximum="
    }

    if (s != "") {
      dpformat <- paste("%.", dp, "f", sep="")
      s <- paste("Target SD ", sprintf(dpformat, tSD), " is too ", s, sprintf(dpformat, mSD), sep="")
      rSprite.message(s, shinyType="warning")
      return(result)
    }
  }

  if (scaleMin >= scaleMax) {
    s <- paste("Scale minimum should be less than maximum")
    rSprite.message(s, shinyType="warning")
    return(result)
  }

  result$rows <- c()
  nCases <- 0
  result$label <- rSprite.chartLabel(N, tMean, tSD, scaleMin, scaleMax, dp, (maxCases > 9))
  for (i in 1:(maxCases * rSprite.maxDupLoops)) {
    vec <- rSprite.seekVector(N, tMean, tSD, scaleMin, scaleMax, dp, fixed, never, result$label)
    if (length(vec) == 0) {
      break                                 # we failed to find a case despite many tries
    }

    fullVec <- sort(c(vec, fixed))          # sorting lets us find duplicates more easily
    if (length(result$rows) == 0) {
      result$rows <- matrix(fullVec, nrow=1)
    }
    else {
      newRows <- rbind(result$rows, fullVec)
      if (tail(duplicated(newRows), 1)) {   # the solution we just found is a duplicate
        dups <- dups + 1
        if (dups > maxDups) {
          break
        }
        else {
          next
        }
      }

      result$rows <- newRows
    }

    nCases <- nrow(result$rows)
    s <- paste("Found ", nCases, " unique solution", (if (nCases == 1) "" else "s"), sep="")
    rSprite.message(s, showNow=TRUE)    # progress counter

    if (nCases >= maxCases) {           # we have enough cases now
      break
    }

# Calculate the maximum number of consecutive duplicates we will accept before deciding to give up.
# The value of 0.00001 below is our nominal acceptable chance of missing a valid solution;
#  however, it's extremely likely that all possible solutions are not all equally likely to be found.
# So we also set a floor of 100 attempts.
    maxDups <- max(round(log(0.00001) / log(nCases / (nCases + 1))), 100)
    dups <- 0
  }

  if (nCases < maxCases) {
    if (nCases == 0) {
      s <- paste("No solution found for ", paste(result$label, collapse=" "), sep="")
    }
    else {
      was <- if (nCases == 1) "was" else "were"
      s <- paste(maxCases, " unique solutions were requested, but only ", nrow(result$rows), " ", was, " found", sep="")
    }

    rSprite.message(s, shinyType="warning")
  }

  return(result)
}

# Build a single results chart (grob).
rSprite.buildOneChart <- function (vec, scaleMin, scaleMax, gridSize, xMax, yMax, label) {
  df <- data.frame(vec)

# Avoid showing a large number of empty elements on the right of the X-axis if our upper bound is very large.
  xLimit <- if (((scaleMax - scaleMin) <= 11) || (xMax > scaleMax))
              max(scaleMax, xMax)
            else
              min(scaleMax, (xMax + 2))
  xBreaks <- scaleMin:xLimit

# Allow for room above the highest bar to display the label.
  yLimit <- yMax
  llen <- length(label)
  if (llen > 0) {
    yBump <- round(llen * max(2, yMax * 0.1) * (((gridSize >= 4) + 2) / 2))
    yLimit <- yMax + yBump
  }

  yTicks <- c(10, 8, 6, 5, rep(4, 6))[gridSize]
  yTickSize <- round((yMax / (yTicks - 1)) + 1)
  yLabelGaps <- c(1, 2, 3, 4, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000)
  yGap <- yLabelGaps[yLabelGaps >= yTickSize][1]
  yBreaks <- (0:(yTicks - 1)) * yGap

  axisTitleSize <- c(20, 14, 12, 11, 10, rep(8, 5))[gridSize]
  axisTextSize <- c(16, 12, 10, 9, 8, rep(7, 5))[gridSize]

  grob <- ggplot(df, aes(x=factor(vec, levels=xBreaks))) +
          geom_bar(fill="#0099ff", width=0.9) +
          scale_x_discrete(drop=FALSE) +
          scale_y_continuous(limits=c(0, yLimit), breaks=yBreaks) +
          theme(axis.title=element_text(size=axisTitleSize)) +
          theme(axis.text=element_text(size=axisTextSize)) +
          labs(x="response", y="count")

  if (llen > 0) {
    if (gridSize <= 10) {
      labelTextSize <- axisTitleSize * 0.352778 * (1 - (0.1 * (gridSize >= 8)))     # see StackOverflow 36547590
      labelText <- paste(label, collapse="\n")
      labelY <- (yLimit + 1 - llen) - (gridSize >= 5) - (gridSize >= 7)
      labelX <- round((xLimit - scaleMin) / 2) + 1
      grob <- grob + annotate("text", x=labelX, y=labelY, label=labelText, size=labelTextSize)
    }
  }

  flipXThreshold <- c(50, 30, 10, 15, 10, rep(3, 5))[gridSize]
  if (length(xBreaks) > flipXThreshold) {
    grob <- grob + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  }

  return(grob)
}

# Build a grid containing all the results charts.
rSprite.buildCharts <- function (sample, scaleMin, scaleMax, gridSize) {
  rows <- sample$rows

  nCases <- nrow(rows)
  s <- paste("Plotting ", nCases, " unique solution", (if (nCases == 1) "" else "s"), "...", sep="")
  rSprite.message(s, showNow=TRUE)

  if (nCases > 1) {
    rows <- rows[order(apply(rows, 1, skewness)),]
  }

  xMax <- max(rows)
  yMax <- max(unlist(apply(rows, 1, table)))
  grobs <- apply(rows, 1, function (x) {
    rSprite.buildOneChart(x, scaleMin, scaleMax, gridSize, xMax, yMax, sample$label)
  })
  layoutMatrix <- matrix(1:(gridSize ^ 2), nrow=gridSize, ncol=gridSize, byrow=TRUE)
  grid.arrange(grobs=grobs, layout_matrix=layoutMatrix)
}

# Function to display one or more notification messages.
rSprite.shinyMessages <- function (messageList) {
  lapply(rSprite.notifIdList, function (x) {
    removeNotification(x)
  })
  rSprite.notifIdList <<- list()

  uniqueMessages <- unique(unlist(messageList))
  sapply(uniqueMessages, function (x) {
    split <- unlist(strsplit(x, "%%"))
    messageType <- split[1]
    messageText <- split[2]
    id <- showNotification(messageText, type=messageType, duration=NULL, closeButton=FALSE)
    rSprite.notifIdList <<- append(rSprite.notifIdList, id)
  })
}

rSprite.helpText <- c(
  "rSPRITE is an implementation by Nick Brown of SPRITE, an idea by James Heathers."
  , "<br/><br/>"
  , "rSPRITE simulates data from an integer (e.g., Likert-type) scale in the form of bar charts."
  , "<br/><br/>"
  , "You can request up to 100 samples to be presented on a square grid."
  , " You need to specify the minimum and maximum item values of the scale,"
  , " and the mean, standard deviation, and size of the sample."
  , " The charts are presented in increasing order of skewness, from top left to bottom right."
  , "<br/><br/>"
  , "Optionally, you can provide a fixed value and a count;"
  , " this forces every sample to contain exactly that many occurrences of that value,"
  , " which may be outside the scale range. That number of occurrences can be zero."
  , "<br/><br/>"
  , "You can also download the individual values that make up the bar charts to a CSV file."
  , "<br/><br/>"
  , "If you check the box labeled 'Use fixed seed', you will get the same results on every run;"
  , " this can be useful when reporting problems, but otherwise, leave this box unchecked."
  , "<br/><br/>"
  , "rSPRITE may not always find every solution when there are only a few to be found."
  , " If you get a message saying that fewer results were found than you hoped for,"
  , " please try a couple more times to see if one or two more solutions show up."
  , "<br/><br/>"
  , "A general observation: rSPRITE is a tool and not a complete system."
  , " Like any tool, it has the potential to be used incorrectly."
  , " If you ask it do something silly, it will do it, very probably without warning you."
  , "<br/><br/>"
  , "If you want to run rSPRITE on your own computer, or just see how it works, the source code is available <a href=https://github.com/sTeamTraen/rSPRITE>here</a>."
  , "<br/><br/>"
  , "For more information on SPRITE in general, see <a href=https://peerj.com/preprints/26968v1/>here</a>."
  , "<br/><br/>"
  , "Please report bugs to nicholasjlbrown@gmail.com"
  , "<br/><br/>"
  , "Privacy policy: rSPRITE does not collect any information about you whatsoever."
  , " If you are using this code in a web browser at shinyapps.io, you can find the RStudio"
  , " terms of use <a href='https://www.rstudio.com/about/rstudio-service-terms-of-use/'>here</a>."
)

rSprite.notifIdList <<- list()
rSprite.messageList <<- list()
rSprite.prevGo <<- 0
rSprite.prevHelp <<- 0
rSprite.plotData <<- c()

server <- function (input, output, session) {
  debounced_tMean <- debounce(input$tMean, 1000)
  debounced_tSD <- debounce(input$tSD, 1000)

  fixedCount <- reactive({
    result <- NA
    sn <- gsub(" ", "", input$fixedCount)
    if (sn != "") {
      result <- rSprite.huge
      if (grepl("^[0-9]+$", sn)) {
        f <- as.numeric(sn)
        if ((f >= 0) && (f < input$N)) {
          result <- f
        }
      }

      if (result == rSprite.huge) {
        s <- paste("Fixed count must be an integer from 0 to ", (input$N - 1)
                 , "; input |", input$fixedCount
                 , "| ignored"
                 , sep=""
        )
        rSprite.message(s, shinyType="warning")
        result <- NA
      }
    }

    result
  })

  fixedResponse <- reactive({
    result <- NA
    sn <- gsub(" ", "", input$fixedResponse)
    if (sn != "") {
      result <- rSprite.huge
      if (grepl("^-?[0-9]+$", sn)) {
        result <- as.numeric(sn)
      }

      if (result == rSprite.huge) {
        s <- paste("Fixed value must be an integer from ", input$scaleMin
                 , " to ", input$scaleMax
                 , "; input |", input$fixedResponse
                 , "| ignored"
                 , sep=""
        )
        rSprite.message(s, shinyType="warning")
        result <- NA
      }
    }

    result
  })

  reactiveSample <- eventReactive(input$go, {
    rSprite.message("Calculating...", showNow=TRUE)
    set.seed(if (input$fixedSeed) 1 else as.numeric(Sys.time()))
    fResponse <- fixedResponse()
    fCount <- fixedCount()
    fixed <- c()
    never <- c()
    if (!is.na(fCount) && !is.na(fResponse)) {
      if (fCount == 0) {
        never <- fResponse
      }
      else {
        fixed <- rep(fResponse, fCount)
      }
    }

    gridSize <- sqrt(as.numeric(input$gridSize))

    rSprite.getSample(
      gridSize ^ 2
    , input$N
    , input$tMean
    , input$tSD
    , input$scaleMin
    , input$scaleMax
    , input$dp
    , fixed
    , never
    )
  })

# This element is just a placeholder to catch and handle changes in the input controls and their relations to each other.
# We never actually output anything to a text box.
  output$dummy <- renderText({
    N <- input$N
#bounce    tMean <- input$tMean
    tMean <- debounced_tMean()    #bounce
#bounce    tSD <- input$tSD
    tSD <- debounced_tSD()        #bounce
    scaleMin <- round(input$scaleMin, 0)
    scaleMax <- round(input$scaleMax, 0)
    dp <- input$dp
    dstep <- c(1, 0.1, 0.01, 0.001)[(dp + 1)]

    updateNumericInput(session, inputId="scaleMin", max=(scaleMax - 1), value=scaleMin)
    updateNumericInput(session, inputId="scaleMax", min=(scaleMin + 1), value=scaleMax)
    updateNumericInput(session, inputId="tMean", min=scaleMin, max=scaleMax, step=dstep)  #limits
    updateNumericInput(session, inputId="tSD", min=0, max=(((scaleMax - scaleMin) / 2) + 1), step=dstep)  #limits

# It is tempting to force the mean value to a GRIM-consistent one here
#  (cf. what we do for the SD below), but this would be an error,
#  as we would be unable to "scroll" from one valid mean to another using the
#  input spinners if there were any invalid intermediate values
#  (we would constantly be forced back).
# However, we do force the mean to be between scaleMin and scaleMax.
    if (!is.na(tMean)) {
      newMean <- max(min(round(tMean, dp), scaleMax), scaleMin)
      if (newMean != tMean) {
        updateNumericInput(session, inputId="tMean", value=newMean) #limits
      }
    }

# Similarly, it would be nice to have the range for the SD limited by the current mean,
#  but this leads to all sorts of complications. So we allow the user to enter an SD
#  that is too small or large, and tell them later.
    if (!is.na(tSD)) {
      newSD <- max(min(round(tSD, dp), scaleMax), 0)
      if (newSD != tSD) {
        updateNumericInput(session, inputId="tSD", value=newSD) #limits
      }
    }

    return()      # just falling out at the end gives an Shiny error message the first time we come here
  })

  output$plotDisplayed <- reactive({
    input$go
    input$help

    (length(rSprite.plotData) > 0)
  })
  outputOptions(output, "plotDisplayed", suspendWhenHidden=FALSE, priority=-1)

  output$downloadData <- downloadHandler(
    filename=function () {
      "spritedata.csv"
    }
  , content=function (file) {
      write.table(rSprite.plotData, file, row.names=FALSE, col.names=FALSE, sep=",")
    }
  )

  output$help <- renderUI({
    input$go                  # creates a dependency on the Go button

    helpText <- ""            # Unless the user clicked Help, we will clear any existing help text.
    if (input$help > rSprite.prevHelp) {    # user clicked the Help button
      rSprite.prevHelp <<- input$help
      helpText <- HTML(paste(rSprite.helpText, collapse=""))
    }

    isolate({
      helpText
    })
  })

  output$plot <- renderPlot({
    input$help                          # creates a dependency on the Help button
    rSprite.plotData <<- c()

    if (input$go > rSprite.prevGo) {    # user clicked the Go button
      rSprite.prevGo <<- input$go
    }
    else {
      return()           # this clears the plot area (which conveniently allows the help text to show)
    }

    isolate({
      N <- input$N
      tMean <- input$tMean
      tSD <- input$tSD
      scaleMin <- input$scaleMin
      scaleMax <- input$scaleMax
      dp <- input$dp
      gridSize <- sqrt(as.numeric(input$gridSize))

      sample <- reactiveSample()
      if (length(sample$rows) > 0) {
        if (    (gridSize == 10)
             && (session$clientData$url_hostname == "127.0.0.1")        # On developer's local screen...
           ) {                                                          # ... don't show 10x10 grid...
          rSprite.message("Skipping rendering", showNow=TRUE)           # ... to speed up generation of test data.
        }
        else {
          gridSize <- floor(sqrt(nrow(sample$rows)) + 0.999)
          rSprite.buildCharts(sample, scaleMin, scaleMax, gridSize)
          rSprite.message("Rendering...", showNow=TRUE)
        }

        rSprite.plotData <<- sample$rows
      }

      rSprite.shinyMessages(rSprite.messageList)
      rSprite.messageList <<- list()
    })
  }, height=function () {
    min(session$clientData$output_plot_width, 780)
  }, width=1200)
}

#---------- Two-file version: End of server.R ----------
#---------- Two-file version: Beginning of ui.R (but remove trailing if(1)) ----------
library(shinycssloaders)

N <- 50
tMean <- 4.12
tSD <- 1.48
dp <- 2
scaleMin <- 1
scaleMax <- 7
fixedValue <- ""
fixedCount <- ""
fixedSeed <- 0

dstep <- c(1, 0.1, 0.01, 0.001)[(dp + 1)]

ui <- fluidPage(
  titlePanel("rSPRITE beta 0.18")
, sidebarLayout(
    position="left"
  , sidebarPanel(
      width=2
    , numericInput(inputId="scaleMin", label="Minimum scale value", value=scaleMin, step=1) ##, min=-20, max=1)
    , numericInput(inputId="scaleMax", label="Maximum scale value", value=scaleMax, step=1) ##, min=2, max=50)
    , numericInput(inputId="N", label="Sample size", value=N, min=2, max=10000, step=1)
    , numericInput(inputId="tMean", label="Target mean", value=round(tMean, dp), min=scaleMin, max=scaleMax, step=dstep)
#limits    , numericInput(inputId="tMean", label="Target mean", value=round(tMean, dp), step=dstep)  #limits
    , numericInput(inputId="tSD", label="Target SD", value=round(tSD, dp), min=0, max=(((scaleMax - scaleMin) / 2) + 1), step=dstep)
#limits    , numericInput(inputId="tSD", label="Target SD", value=round(tSD, dp), step=dstep)  #limits
    , numericInput(inputId="dp", label="Decimal places", value=dp, min=0, max=3, step=1)
    , selectInput(inputId="gridSize", label="Number of results", choices=(c(1:10) ^ 2), selected=9)
    , fluidRow(
        column(
          6
        , textInput(inputId="fixedResponse", label="Fixed value", value=fixedValue)
      )
      , column(
          6
        , textInput(inputId="fixedCount", label="Fixed count", value=fixedCount)
      )
    )
    , checkboxInput(inputId="fixedSeed", label="Use fixed seed", value=fixedSeed)
    , fluidRow(
        column(
          6
        , actionButton(inputId="go", label="Go!")
        )
      , column(
          6
        , actionButton(inputId="help", label="Help")
        )
      )
    , conditionalPanel(
        condition="output.plotDisplayed"
      , br()
      , downloadLink(outputId="downloadData", label="Download data")
    )
  )
  , mainPanel(
      withSpinner(
        plotOutput(outputId="plot", width="100%"), type=5
      )
    , absolutePanel(        # allows help panel to overlay plot panel
        top="50"
      , textOutput(outputId="dummy")
      , htmlOutput(outputId="help")
      )
    )
  )
, tags$head(
    tags$style(
      HTML(".shiny-notification { position:relative; bottom:2px; left:-200px; width:125% }")
    )
  )
)

if (1) {
  shinyApp(ui=ui, server=server)
}

#rsconnect::deployApp("~/Academic/rSPRITE/rSPRITE")