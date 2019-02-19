#' Single-case data generator
#'
#' The \code{rSC} function generates random single-case data frames
#' for monte-carlo studies and demonstration purposes.
#' \code{design.rSC} is used to set up a design matrix with all parameters needed for the \code{rSC} function.
#'
#' @param design A design matrix which is created by design.rSC and specifies
#'   all paramters.
#' @param round Rounds the scores to the defined decimal. To round to the second
#'   decimal, set \code{round = 2}.
#' @param random.names Is \code{FALSE} by default. If set \code{random.names =
#'   TRUE} cases are assigned random first names. If set \code{"male" or
#'   "female"} only male or female names are chosen. The names are drawn from
#'   the 2,000 most popular names for newborns in 2012 in the U.S. (1,000 male
#'   and 1,000 female names).
#' @param seed A seed number for the random generator.
#' @param ... Paramteres that are directly passed from the rSC function to the design.rSC function for a more concise coding.
#' @param n Number of cases to be created (Default is \code{n = 1}).
#' @param phase.design A vector defining the length and label of each phase.
#' E.g., \code{phase.length = c(A1 = 10, B1 = 10, A2 = 10, B2 = 10)}.
#' @param MT Number of measurements (in each study). Default is \code{MT = 20}.
#' @param B.start Phase B starting point. The default setting \code{B.start = 6}
#'   would assign the first five scores (of each case) to phase A, and all
#'   following scores to phase B. To assign different starting points for a set
#'   of multiple single-cases, use a vector of starting values (e.g.
#'   \code{B.start = c(6, 7, 8)}). If the number of cases exceeds the length of
#'   the vector, values will be repeated.
#' @param m Mean of the sample distribution the scores are drawn from. Default
#'   is \code{m = 50}. To assign different means to several single-cases, use a
#'   vector of values (e.g. \code{m = c(50, 42, 56)}). If the number of cases
#'   exceeds the length of the vector, values are repeated.
#' @param s Standard deviation of the sample distribution the scores are drawn
#'   from. Set to \code{s = 10} by default. To assign different variances to
#'   several single-cases, use a vector of values (e.g. \code{s = c(5, 10,
#'   15)}). If the number of cases exceeds the length of the vector, values are
#'   repeated.
#' @param prob If \code{distribution} (see below) is set \code{"binomial"},
#'   \code{prob} passes the probability of occurrence.
#' @param trend Defines the effect size \emph{d} of a trend per MT added
#'   across the whole data-set. To assign different trends to several
#'   single-cases, use a vector of values (e.g. \code{trend = c(.1, .3, .5)}).
#'   If the number of cases exceeds the length of the vector, values are
#'   repeated. While using a binomial or poisson distribution, \code{d.trend}
#'   indicates an increase in points / counts per MT.
#' @param level Defines the level increase (effect size \emph{d}) at the
#'   beginning of phase B. To assign different level effects to several
#'   single-cases, use a vector of values (e.g. \code{d.level = c(.2, .4, .6)}).
#'   If the number of cases exceeds the length of the vector, values are
#'   repeated. While using a binomial or poisson distribution, \code{d.level}
#'   indicates an increase in points / counts with the onset of the B-phase.
#' @param slope Defines the increase in scores - starting with phase B -
#'   expressed as effect size \emph{d} per MT. \code{d.slope = .1} generates an
#'   incremental increase of 0.1 standard deviations per MT for all phase B
#'   measurements. To assign different slope effects to several single-cases,
#'   use a vector of values (e.g. \code{d.slope = c(.1, .2, .3)}). If the number
#'   of cases exceeds the length of the vector, values are repeated. While using
#'   a binomial or poisson distribution, \code{d.slope} indicates an increase in
#'   points / counts per MT.
#' @param rtt Reliability of the underlying simulated measurements. Set
#'   \code{rtt = .8} by default. To assign different reliabilities to several
#'   single-cases, use a vector of values (e.g. \code{rtt = c(.6, .7, .8)}). If
#'   the number of cases exceeds the length of the vector, values are repeated.
#'   \code{rtt} has no effect when you're using binomial or poisson distributed
#'   scores.
#' @param extreme.p Probability of extreme values. \code{extreme.p = .05} gives
#'   a five percent probability of an extreme value. A vector of values assigns
#'   different probabilities to multiple cases. If the number of cases exceeds
#'   the length of the vector, values are repeated.
#' @param extreme.d Range for extreme values, expressed as effect size \emph{d}.
#'   \code{extreme.d = c(-7,-6)} uses extreme values within a range of -7 and -6
#'   standard deviations. In case of a binomial or poisson distribution,
#'   \code{extreme.d} indicates points / counts. Caution: the first value must
#'   be smaller than the second, otherwise the procedure will fail.
#' @param missing.p Portion of missing values. \code{missing.p = 0.1} creates
#'   10\% of all values as missing). A vector of values assigns different
#'   probabilities to multiple cases. If the number of cases exceeds the length
#'   of the vector, values are repeated.
#' @param distribution Distribution of the scores. Default is \code{distribution
#'   = "normal"}. Possible values are \code{"normal"}, \code{"binomial"}, and
#'   \code{"poisson"}. If set to \code{"normal"}, the sample of scores will be
#'   normally distributed with the parameters \code{m} and \code{s} as mean and
#'   standard deviation of the sample, including a measurement error defined by
#'   \code{rtt}. If set to \code{"binomial"}, data are drawn from a binomial
#'   distribution with the expectation value \code{m}. This setting is useful
#'   for generating criterial data like correct answers in a test. If set to
#'   \code{"poisson"}, data are drawn from a poisson distribution, which is very
#'   common for count-data like behavioral observations. There's no measurement
#'   error is included. \code{m} defines the expectation value of the poisson
#'   distribution, lambda.
#' @return A single-case data frame or a list of single-case data frames. See
#'   \code{\link{scdf}} to learn about this format.
#' @author Juergen Wibert
#' @keywords datagen
#' @examples
#'
#' ## Create random single-case data and inspect it
#' design <- design.rSC(n = 3, rtt = 0.75, slope = 0.1, extreme.p = 0.1,
#'        missing.p = 0.1)
#' dat <- rSC(design, round = 1, random.names = TRUE, seed = 123)
#' describeSC(dat)
#' plotSC(dat)
#'
#' ## And now have a look at poisson-distributed data
#' design <- design.rSC(n = 3, B.start = c(6,10,14), MT = c(12,20,22), m = 10,
#'                     distribution = "poisson", level = -5, missing.p = 0.1)
#' dat <- rSC(design, seed = 1234)
#' pand(dat, decreasing = TRUE, correction = FALSE)
#' @name random
NULL
## NULL

#' @rdname random
rSC <- function(design = NULL, round = NA, random.names = FALSE, seed = NULL, ...) {
  if(!is.null(seed))
    set.seed(seed)
  
  if(is.numeric(design)) {
    warning("The first argument is expected to be a design matrix created by design.rSC. If you want to set n, please name the first argument with n = ...")
    n <- design
    design <- NULL
  }
  if(is.null(design)) {
    design <- design.rSC(...)
  }
  
  if(FALSE) {
    MT  <- rep(MT, length.out = n)
    m   <- rep(m, length.out = n)
    s   <- rep(s, length.out = n)
    rtt <- rep(rtt, length.out = n)
    extreme.p <- rep(extreme.p, length.out = n)
    missing.p <- rep(missing.p, length.out = n)
    d.level   <- rep(d.level, length.out = n)
    d.slope   <- rep(d.slope, length.out = n)
    d.trend   <- rep(d.trend, length.out = n)
    
    if(!is.na(B.start[1])) {
      if (B.start[1] == "rand") {
        tmp.start <- round(as.numeric(B.start[2]) * MT)
        tmp.end   <- round(as.numeric(B.start[3]) * MT)  
        B.start   <- round(runif(n, tmp.start, tmp.end))
      }
      
      if(any(B.start < 1) && any(B.start >= 1)) stop("A B.start vector must not include values below and above 1 at the same time.")
      if(B.start[1] < 1 && B.start[1] > 0) B.start <- round(B.start * MT) + 1
      B.start <- rep(B.start, length.out = n)  
      cases <- list()
      for(i in 1:length(B.start)) {
        cases[[i]] <- data.frame(phase = c("A","B"), length = c(B.start[i] - 1, 1 + MT[i] - B.start[i]))
      }
      
    }
    
    error <- sqrt(((1-rtt)/rtt) * s^2)
    
    for(i in 1:n) {
      cases[[i]]$mt             <- sum(cases[[i]]$length)
      cases[[i]]$rtt            <- rtt[i]
      cases[[i]]$error          <- error[i]
      cases[[i]]$missing.p      <- missing.p[i]
      cases[[i]]$extreme.p      <- extreme.p[i]
      cases[[i]]$extreme.low    <- extreme.d[1]
      cases[[i]]$extreme.high   <- extreme.d[2]
      cases[[i]]$trend          <- d.trend[i]
      cases[[i]]$level          <- c(0,d.level[i])
      cases[[i]]$slope          <- c(0,d.slope[i])
      cases[[i]]$m              <- m[i]
      cases[[i]]$s              <- s[i]
      
      cases[[i]]$start  <- c(1, cumsum(cases[[i]]$length) + 1)[1:length(cases[[i]]$length)]
      cases[[i]]$stop   <- cumsum(cases[[i]]$length)
      
    }
    design <- list(cases = cases)
    design$distribution <- distribution
  }
  
  n <- length(design$cases)
  
  cases <- design$cases
  distribution <- design$distribution
  prob <- design$prob
  
  dat <- list()
  for (i in 1:n) {
    if(distribution == "normal") {
      start_values <- c(cases[[i]]$m[1], rep(0, cases[[i]]$mt[1] - 1))
      trend_values <- c(0, rep(cases[[i]]$trend[1] * cases[[i]]$s[1] , cases[[i]]$mt[1] - 1))
      slope_values <- c()
      level_values <- c()
      for(j in 1:nrow(cases[[i]])) {
        slope_values <- c(slope_values, rep(cases[[i]]$slope[j] * cases[[i]]$s[j], cases[[i]]$length[j]))
        level_values <- c(level_values, cases[[i]]$level[j] * cases[[i]]$s[j], rep(0, cases[[i]]$length[j] - 1))
      }
      
      true_values     <- start_values + trend_values + slope_values + level_values
      true_values     <- cumsum(true_values)
      error_values    <- rnorm(cases[[i]]$mt[1], mean = 0, sd = cases[[i]]$error[1])
      measured_values <- true_values + error_values
      
    }
    
    if(distribution == "poisson" || distribution == "binomial") {
      start_values <- c(cases[[i]]$m[1], rep(0, cases[[i]]$mt[1] - 1))
      trend_values <- c(0, rep(cases[[i]]$trend[1], cases[[i]]$mt[1] - 1))
      slope_values <- c()
      level_values <- c()
      
      for(j in 1:nrow(cases[[i]])) {
        slope_values <- c(slope_values, rep(cases[[i]]$slope[j], cases[[i]]$length[j]))
        level_values <- c(level_values, cases[[i]]$level[j], rep(0, cases[[i]]$length[j] - 1))
      }
      
      true_values     <- start_values + trend_values + slope_values + level_values
      true_values     <- round(cumsum(true_values))
      true_values[true_values < 0] <- 0
      
      if(distribution == "poisson")
        measured_values <- rpois(n = length(true_values), true_values)
      if(distribution == "binomial")
        measured_values <- rbinom(n = length(true_values), size = round(true_values * (1/prob)), prob = prob)
    } 
    

    if(cases[[i]]$extreme.p[1] > 0){
      ra <- runif(cases[[i]]$mt[1])
      if(distribution == "normal")
        multiplier <- cases[[i]]$s[1]
      if (distribution == "binomial" || distribution == "poisson")
        multiplier <- 1
      for(k in 1:cases[[i]]$mt[1]) {
        if(ra[k] <= cases[[i]]$extreme.p[1])
          measured_values[k] <- measured_values[k] + (runif(1, cases[[i]]$extreme.low[1], cases[[i]]$extreme.high[1]) * multiplier) 
      }
    }
    
    if(cases[[i]]$missing.p[1] > 0){
      measured_values[sample(1:cases[[i]]$mt[1], cases[[i]]$missing.p[1]*cases[[i]]$mt[1])] <- NA
    }
    
    if(!is.na(round))
      measured_values <- round(measured_values, round) 
    
    if (distribution == "binomial" || distribution == "poisson") 
      measured_values[measured_values < 0] <- 0
    
    condition <- rep(cases[[i]]$phase, cases[[i]]$length)
    
    dat[[i]] <- data.frame(phase = condition, values = measured_values, mt = 1:cases[[i]]$mt[1])
  }
  
  if(random.names == "male")
    names(dat) <- sample(.male.names,n)
  if(random.names == "female")
    names(dat) <- sample(.female.names,n)
  
  if(isTRUE(random.names))
    names(dat) <- sample(.names,n)
  
  
  attributes(dat) <- .defaultAttributesSCDF(attributes(dat))    #class(dat) <- c("scdf","list")
  return(dat)
}

#' @rdname random
design.rSC <- function(n = 1, phase.design = list(A = 5, B = 15), trend = list(0), level = list(0), slope = list(0), rtt = list(0.80), m = list(50), s = list(10), extreme.p = list(0), extreme.d = c(-4,-3), missing.p = list(0), distribution = "normal", prob = 0.5, MT = NULL, B.start = NULL) {
  
  if(!is.null(B.start)) {
    MT  <- rep(MT, length.out = n)
    if (B.start[1] == "rand") {
      tmp.start <- round(as.numeric(B.start[2]) * MT)
      tmp.end   <- round(as.numeric(B.start[3]) * MT)  
      B.start   <- round(runif(n, tmp.start, tmp.end))
    }
    
    if(any(B.start < 1) && any(B.start >= 1)) 
      stop("A B.start vector must not include values below and above 1 at the same time.")
    if(B.start[1] < 1 && B.start[1] > 0) 
      B.start <- round(B.start * MT) + 1
    B.start <- rep(B.start, length.out = n)  
    
    phase.design <- rep(list(A = rep(NA,n), B = rep(NA,n)))
    for(i in 1:length(B.start)) {
      phase.design$A[i] <- B.start[i] - 1
      phase.design$B[i] <- 1 + MT[i] - B.start[i]
    }
  }
  
  if(length(m) != n)
    m <- rep(m, length = n)
  if(length(s) != n)
    s <- rep(s, length = n)
  if(length(rtt) != n)
    rtt <- rep(rtt, length = n)
  #if(length(trend) != n)
  #  trend <- rep(list(trend), length = n)
  if(is.list(trend))
    trend <- unlist(trend)
  trend <- .check.designSC(trend,n)
  level <- .check.designSC(level,n)
  slope <- .check.designSC(slope,n)
  phase.design <- .check.designSC(phase.design,n)
  
  if(length(extreme.p) != n)
    extreme.p <- lapply(numeric(n),function(y) unlist(extreme.p))
  if(length(extreme.d) != n)
    extreme.d <- lapply(numeric(n),function(y) unlist(extreme.d))
  if(length(missing.p) != n)
    missing.p <- lapply(numeric(n),function(y) unlist(missing.p))
  
  out <- list()
  out$cases <- vector("list", n)
  out$distribution <- distribution
  out$prob <- prob
  
  for(case in 1: n) {
    
    error <- sqrt(((1-rtt[[case]])/rtt[[case]]) * s[[case]]^2)
    design <- data.frame(phase = names(phase.design))
    design$length       <- unlist(lapply(phase.design, function(x) x[case]))
    design$mt           <- sum(design$length)
    design$rtt          <- rtt[[case]]
    design$error        <- error
    design$missing.p    <- missing.p[[case]]
    design$extreme.p    <- extreme.p[[case]]
    design$extreme.low  <- extreme.d[[case]][1]
    design$extreme.high <- extreme.d[[case]][2]
    design$trend        <- trend[[1]][case] #unlist(lapply(trend, function(x) x[case])) #trend[[case]]
    design$level        <- unlist(lapply(level, function(x) x[case])) #level[[case]]
    design$level[1]     <- 0
    design$slope        <- unlist(lapply(slope, function(x) x[case])) #slope[[case]]
    design$slope[1]     <- 0
    design$m            <- m[[case]]
    design$s            <- s[[case]]#unlist(lapply(s, function(x) x[case])) #s[[case]]
    
    design$start <- c(1,cumsum(design$length)+1)[1:length(design$length)]
    design$stop <- cumsum(design$length)
    
    out$cases[[case]] <- design
  }
  
  return(out)
}

.check.designSC <- function(data, n) {
  if(is.numeric(data))
    data <- list(data)
  for(phase in 1:length(data)) {
    if(length(data[[phase]]) != n)
      data[[phase]] <- rep(data[[phase]], length = n)
  }
  data
}


.male.names <- c("Jacob", "Mason", "Ethan", "Noah", "William", "Liam", "Jayden", "Michael", "Alexander", "Aiden", "Daniel", "Matthew", "Elijah", "James", "Anthony", "Benjamin", "Joshua", "Andrew", "David", "Joseph", "Logan", "Jackson", "Christopher", "Gabriel", "Samuel", "Ryan", "Lucas", "John", "Nathan", "Isaac", "Dylan", "Caleb", "Christian", "Landon", "Jonathan", "Carter", "Luke", "Owen", "Brayden", "Gavin", "Wyatt", "Isaiah", "Henry", "Eli", "Hunter", "Jack", "Evan", "Jordan", "Nicholas", "Tyler", "Aaron", "Jeremiah", 
                 "Julian", "Cameron", "Levi", "Brandon", "Angel", "Austin", "Connor", "Adrian", "Robert", "Charles", "Thomas", "Sebastian", "Colton", "Jaxon", "Kevin", "Zachary", "Ayden", "Dominic", "Blake", "Jose", "Oliver", "Justin", "Bentley", "Jason", "Chase", "Ian", "Josiah", "Parker", "Xavier", "Adam", "Cooper", "Nathaniel", "Grayson", "Jace", "Carson", "Nolan", "Tristan", "Luis", "Brody", "Juan", "Hudson", "Bryson", "Carlos", "Easton", "Damian", "Alex", "Kayden", "Ryder", "Jesus", "Cole", "Micah", "Vincent", "Max", "Jaxson", "Eric", "Asher", "Hayden", "Diego", "Miles", "Steven", "Ivan", "Elias", "Aidan", "Maxwell", "Bryce", "Antonio", "Giovanni", "Timothy", "Bryan", "Santiago", "Colin", "Richard", "Braxton", "Kaleb", "Kyle", "Kaden", "Preston", "Miguel", "Jonah", "Lincoln", "Riley", "Leo", "Victor", "Brady", "Jeremy", "Mateo", "Brian", "Jaden", "Ashton", "Patrick", "Declan", "Sean", "Joel", "Gael", "Sawyer", "Alejandro", "Marcus", "Leonardo", "Jesse", "Caden", "Jake", "Kaiden", "Wesley", "Camden", "Edward", "Brantley", "Roman", "Axel", "Silas", "Jude", "Grant", "Cayden", "Emmanuel", "George", "Maddox", "Malachi", "Bradley", "Alan", "Weston", "Gage", "Devin", "Greyson", "Kenneth", "Mark", "Oscar", "Tanner", "Rylan", "Nicolas", "Harrison", "Derek", "Peyton", "Ezra", "Tucker", "Emmett", "Avery", "Cody", "Calvin", "Andres", "Jorge", "Abel", "Paul", "Abraham", "Kai", "Collin", "Theodore", "Ezekiel", "Omar", "Jayce", "Conner", "Bennett", "Trevor", "Eduardo", "Peter", "Maximus", "Jaiden", "Jameson", "Seth", "Kingston", "Javier", "Travis", "Garrett", "Everett", "Graham", "Xander", "Cristian", "Damien", "Ryker", "Griffin", "Corbin", "Myles", "Luca", "Zane", "Francisco", "Ricardo", "Alexis", "Stephen", "Zayden", "Iker", "Drake", "Lukas", "Charlie", "Spencer", "Zion", "Erick", "Josue", "Jeffrey", "Trenton", "Chance", "Paxton", "Elliot", "Fernando", "Keegan", "Landen", "Manuel", "Amir", "Shane", "Raymond", "Zander", "Andre", "Israel", "Mario", "Cesar", "Simon", "King", "Jaylen", "Johnathan", "Troy", "Dean", "Clayton", "Dominick", "Tyson", "Jasper", "Martin", "Kyler", "Hector", 
                 "Edgar", "Marco", "Cash", "Edwin", "Shawn", "Judah", "Andy", "Donovan", "Kameron", "Elliott", "Dante", "Braylon", "Anderson", "Johnny", "Drew", "Sergio", "Cruz", "Dalton", "Rafael", "Gregory", "Lane", "Erik", "Skyler", "Finn", "Reid", "Gunner", "Jared", "Caiden", "Holden", "Emilio", "Fabian", "Aden", "Brendan", "Rowan", "Emiliano", "Braden", "Jase", "Jax", "Emanuel", "Lorenzo", "Roberto", "Amari", "Angelo", "Beau", "Louis", "Derrick", "Beckett", "Dawson", "Felix", "Pedro", "Brennan", "Frank", "Maximiliano", 
                 "Quinn", "Dallas", "Romeo", "Braylen", "Joaquin", "Waylon", "Allen", "Colt", "Ruben", "Milo", "Julius", "Grady", "August", "Dakota", "Cohen", "Brock", "Kellen", "Brycen", "Desmond", "Malik", "Colby", "Nehemiah", "Leland", "Jett", "Marcos", "Taylor", "Karter", "Marshall", "Ty", "Phillip", "Corey", "Ali", "Adan", "Dillon", "Arthur", "Maverick", "Leon", "Brooks", "Tristen", "Titus", "Keith", "Dexter", "Karson", "Emerson", "Landyn", "Armando", "Pablo", "Knox", "Enrique", "Cade", "Gerardo", "Reed", "Kellan", 
                 "Jayson", "Barrett", "Walter", "Dustin", "Kolton", "Ronald", "Trent", "Phoenix", "Ismael", "Julio", "Danny", "Kason", "Scott", "Messiah", "Jay", "Esteban", "Gideon", "Tate", "Abram", "Trey", "Keaton", "Jakob", "Jaime", "Devon", "Braydon", "Izaiah", "Donald", "Albert", "Raul", "Darius", "Archer", "Colten", "Damon", "River", "Gustavo", "Philip", "Atticus", "Walker", "Matteo", "Randy", "Saul", "Rocco", "Davis", "Enzo", "Noel", "Orion", "Jamari", "Remington", "Bruce", "Darren", "Larry", "Mathew", "Russell", "Dennis", "Tony", "Chris", "Porter", "Rodrigo", "Armani", "Zaiden", "Kade", "Ari", "Hugo", "Zachariah", "Kamden", "Mohamed", "Quentin", "Solomon", "Curtis", "Leonel", "Issac", "Khalil", "Alberto", "Jerry", "Alec", "Gianni", "Moises", "Gunnar", "Adriel", "Lawrence", "Alijah", "Chandler", "Ronan", "Prince", "Payton", "Arturo", "Jimmy", "Orlando", "Ricky", "Mitchell", "Maximilian", "Cason", "Malcolm", "Muhammad", "Kasen", "Marvin", "Jalen", "Cyrus", "Mauricio", "Warren", "Jonas", "Kendrick", "Rhys", "Dane", "Ryland", "Pierce", "Johan", "Rory", "Uriel", "Major", "Bryant", "Reece", "Casey", "Ibrahim", "Nikolas", "Arjun", "Sullivan", "Finnegan", "Alfredo", "Royce", "Ahmed", "Amare", "Lance", "Ramon", "Jamison", "Brayan", "Brenden", "Dominik", "Case", "Kristopher", "Maurice", "Mekhi", "Kobe", "Zackary", "Rhett", "Jensen", "Jaxton", "Deandre", "Isaias", "Channing", "Yahir", "Ezequiel", "Tobias", "Talon", "Sam", "Justice", "Kash", "Nash", "Alvin", "Jacoby", "Ace", "Nico", "Quinton", "Cannon", "Franklin", "Raiden", "Joe", "Lawson", "Beckham", "Gary", "Aldo", "Raylan", "Frederick", "London", "Boston", "Carl", "Byron", "Ernesto", "Moshe", "Terry", "Eddie", "Kane", "Moses", "Finley", "Salvador", "Reese", "Kelvin", "Cullen", "Madden", "Wade", "Clark", "Mohammed", "Kieran", "Jagger", "Dorian", "Korbin", "Nelson", "Roy", "Asa", "Matias", "Nasir", "Nickolas", "Roger", "Alonzo", "Jaxen", "Skylar", "Callen", "Malakai", "Douglas", "Ahmad", "Uriah", "Conor", "Kristian", "Carmelo", "Blaine", "Kayson", "Bentlee", "Braeden", "Julien", "Nathanael", "Aarav", "Keagan", "Lucian", "Morgan", "Chad", "Terrance", "Benson", "Noe", "Rodney", "Francis", "Layne", "Mohammad", "Zayne", "Tatum", "Brett", "Wilson", "Kian", "Marc", "Rohan", "Dayton", "Braiden", "Harper", "Luciano", "Nikolai", "Kamari", "Camron", "Joey", "Santino", "Ellis", "Layton", "Xzavier", "Jefferson", "Winston", "Guillermo", "Demetrius", "Bowen", "Daxton", "Melvin", "Soren", "Neil", "Sylas", "Jon", "Raphael", "Rex", "Yusuf", "Shaun", "Brodie", "Tommy", "Harley", "Quincy", "Dax", "Trace", "Adonis", "Bently", "Giovani", "Jeffery", 
                 "Odin", "Luka", "Kylan", "Willie", "Lewis", "Tripp", "Vihaan", "Davion", "Kendall", "Arian", "Cory", "Jamarion", "Jonathon", "Nixon", "Rayan", "Emery", "Jermaine", "Reginald", "Tomas", "Emmitt", "Ayaan", "Zechariah", "Billy", "Hamza", "Micheal", "Urijah",
                 "Aryan", "Lee", "Jasiah", "Landry", "Crosby", "Mathias", "Toby", "Tristian", "Will", "Felipe", "Triston", "Eden", "Terrell", "Deacon", "Matthias", "Jamal", "Makai", "Maxim", "Sterling", "Hank", "Gerald", "Alessandro", "Jaydon", "Hayes", "Niko", "Branson", 
                 "Flynn", "Kody", "Marlon", "Mayson", "Allan", "Augustus", "Jessie", "Neymar", "Adrien", "Aydan", "Leonard", "Sincere", "Kyson", "Terrence", "Jerome", "Jadiel", "Kole", "Aron", "Aydin", "Omari", "Ronnie", "Zain", "Vicente", "Bobby", "Yosef", "Alexzander", 
                 "Harry", "Kale", "Rogelio", "Casen", "Ray", "Clay", "Masen", "Sage", "Ulises", "Kymani", "Chaim", "Javon", "Brent", "Jadon", "Elisha", "Stanley", "Jovanni", "Princeton", "Alonso", "Darian", "Conrad", "Dwayne", "Eugene", "Gauge", "Rene", "Kareem", "Roland", "Ben", "Vincenzo", "Abdullah", "Camren", "Kenny", "Brentley", "Memphis", "Blaze", "Edison", "Osvaldo", "Teagan", "Westin", "Deshawn", "Rayden", "Cedric", "Marquis", "Samir", "Steve", "Draven", "Jairo", "Giovanny", "Brennen", "Bronson", "Crew", "Davin", "Kolten", "Ronin", "Ariel", "Semaj", "Alden", "Isiah", "Lennox", "Davian", "Jaylin", "Cain", "Wayne", "Craig", "Lamar", "Leonidas", "Cristopher", "Otto", "Bo", "Darrell", "Kolby", "Marcelo", "Bruno", "Fletcher", "Justus", "Alfonso", "Theo", "Tyrone", "Aidyn", "Harvey", "Rudy", "Brendon", "Tristin", "Dominique", "Kaeden", "Samson", "Kyree", "Jovani", "Lionel", "Amos", "Giancarlo", "Misael", "Callum", "Quintin", "Valentino", "Gavyn", "Lennon", "Jamir", "Kamron", "Zavier", "Arlo", "Junior", "Killian", "Leandro", "Konnor", "Hezekiah", "Jordyn", "Markus", "Ramiro", "Callan", "Chace", "Johnathon", "Lyric", "Fisher", "Rashad", "Kamryn", "Legend", "Duncan", "Harold", "Camilo", "Hendrix", "Seamus", "Coleman", "Vance", "Rylee", "Elian", "Jaeden", "Jamie", "Krish", "Abdiel", "Antoine", "Camdyn", "Van", "Branden", "Cayson", "Gibson", "Javion", "Izayah", "Darwin", "Jamar", "Mike", "Randall", "Brecken", "Hassan", "Thiago", "Heath", "Arnav", "Kingsley", "Kyrie", "Xavi", "Damari", "Deangelo", "Jionni", "Joziah", "Makhi", "Vaughn", "Zeke", "Konner", "Ean", "Frankie", "Yael", "Benton", "Oakley", "Efrain", "Marcel", "Rolando", "Maxton", "Jaycob", "Keenan", "Rowen", "Yousef", "Ishaan", "Jedidiah", "Remy", "Todd", "Reagan", "Bodhi", "Damarion", "Juelz", "Valentin", "Austyn", "Broderick", "Anders", "Alvaro", "Mustafa", "Thaddeus", "Brenton", "Cale", "Clinton", "Derick", "Jorden", "Gilberto", "Jabari", "Rey", "Salvatore", "Freddy", "Donte", "Ernest", "Aaden", "Axton", "Blaise", "Lucca", "Maximo", "Sidney", "Dario", "Rodolfo", "Trevon", "Camryn", "Deegan", "Sonny", "Cassius", "Truman", "Brice", "Brogan", "Hugh", "Yehuda", "Agustin", "Eliot", "Stefan", "Zaid", "Bridger", "Damion", "Eliseo", "Houston", "Johann", "Leroy", "Sheldon", "Dariel", "Darryl", "Isai", "Tyrell", "Alfred", "Demarcus", "Kohen", "Ignacio", "Rylen", "Santos", "Cael", "Davon", "Kaysen", "Mack", "Darien", "Ross", "Titan", "Tyree", "Ameer", "Zaire", "Aditya", "Briggs", "Immanuel", "Malaki", "Turner", "Bradyn", "Graysen", "Kase", "Reuben", "Yandel", "Gaige", "Jaidyn", "Franco", "Trystan", "Maison", "Simeon", "Anton", 
                 "Darnell", "Emory", "Roderick", "Deon", "Devan", "Graeme", "Howard", "Jael", "Kael", "Karsen", "Jarrett", "Apollo", "Denzel", "Foster", "Gilbert", "Jaylon", "Kylen", "Augustine")	

.female.names <- c("Sophia", "Emma", "Isabella", "Olivia", "Ava", "Emily", "Abigail", "Mia", "Madison", "Elizabeth", "Chloe", "Ella", "Avery", "Addison", "Aubrey", "Lily", "Natalie", "Sofia", "Charlotte", "Zoey", "Grace", "Hannah", "Amelia", "Harper", "Lillian", "Samantha", "Evelyn", "Victoria", "Brooklyn", "Zoe", "Layla", "Hailey", "Leah", "Kaylee", "Anna", "Aaliyah", "Gabriella", "Allison", "Nevaeh", "Alexis", "Audrey", "Savannah", "Sarah", "Alyssa", "Claire", "Taylor", "Riley", "Camila", "Arianna", "Ashley", "Brianna", "Sophie", "Peyton", "Bella", "Khloe", "Genesis", "Alexa", "Serenity", "Kylie", "Aubree", "Scarlett", "Stella", "Maya", "Katherine", "Julia", "Lucy", "Madelyn", "Autumn", "Makayla", "Kayla", "Mackenzie", "Lauren", "Gianna", "Ariana", "Faith", "Alexandra", "Melanie", "Sydney", "Bailey", "Caroline", "Naomi", "Morgan", "Kennedy", "Ellie", "Jasmine", "Eva", "Skylar", "Kimberly", "Violet", "Molly", "Aria", "Jocelyn", "Trinity", "London", "Lydia", "Madeline", "Reagan", "Piper", "Andrea", "Annabelle", "Maria", "Brooke", "Payton", "Paisley", "Paige", "Ruby", "Nora", "Mariah", "Rylee", "Lilly", "Brielle", "Jade", "Destiny", "Nicole", "Mila", "Kendall", "Liliana", "Kaitlyn", "Natalia", "Sadie", "Jordyn", "Vanessa", "Mary", "Mya", "Penelope", "Isabelle", "Alice", "Reese", "Gabrielle", "Hadley", "Katelyn", "Angelina", "Rachel", "Isabel", "Eleanor", "Clara", "Brooklynn", "Jessica", "Elena", "Aliyah", "Vivian", "Laila", "Sara", "Amy", "Eliana", "Lyla", "Juliana", "Valeria", "Adriana", "Makenzie", "Elise", "Mckenzie", "Quinn", "Delilah", "Cora", "Kylee", "Rebecca", "Gracie", "Izabella", "Josephine", "Alaina", "Michelle", "Jennifer", "Eden", "Valentina", "Aurora", "Catherine", "Stephanie", "Valerie", "Jayla", "Willow", "Daisy", "Alana", "Melody", "Hazel", "Summer", "Melissa", "Margaret", "Kinsley", "Kinley", "Ariel", "Lila", "Giselle", "Ryleigh", "Haley", "Julianna", "Ivy", "Alivia", "Brynn", "Keira", "Daniela", "Aniyah", "Angela", "Kate", "Londyn", "Hayden", "Harmony", "Adalyn", "Megan", "Allie", "Gabriela", "Alayna", "Presley", "Jenna", "Alexandria", "Ashlyn", "Adrianna", "Jada", "Fiona", "Norah", "Emery", "Maci", "Miranda", "Ximena", "Amaya", "Cecilia", "Ana", "Shelby", "Katie", "Hope", "Callie", "Jordan", "Luna", "Leilani", "Eliza", "Mckenna", "Angel", "Genevieve", "Makenna", "Isla", "Lola", "Danielle", "Chelsea", "Leila", "Tessa", "Adelyn", "Camille", "Mikayla", "Adeline", "Adalynn", "Sienna", "Esther", "Jacqueline", "Emerson", "Arabella", "Maggie", "Athena", "Lucia", "Lexi", "Ayla", "Diana", "Alexia", "Juliet", "Josie", "Allyson", "Addyson", "Delaney", "Teagan", "Marley",
                   "Amber", "Rose", "Erin", "Leslie", "Kayleigh", "Amanda", "Kathryn", "Kelsey", "Emilia", "Alina", "Kenzie", "Kaydence", "Alicia", "Alison", "Paris", "Sabrina", "Ashlynn", "Lilliana", "Sierra", "Cassidy", "Laura", "Alondra", "Iris", "Kyla", "Christina", "Carly", "Jillian", "Madilyn", "Kyleigh", "Madeleine", "Cadence", "Nina", "Evangeline", "Nadia", "Raegan", "Lyric", "Giuliana", "Briana", "Georgia", "Yaretzi", "Elliana", "Haylee", "Fatima", "Phoebe", "Selena", "Charlie", "Dakota", "Annabella", "Abby", "Daniella", "Juliette", "Lilah", "Bianca", "Mariana", "Miriam", "Parker", "Veronica", "Gemma", "Noelle", "Cheyenne", "Marissa", "Heaven", "Vivienne", "Brynlee", "Joanna", "Mallory", "Aubrie", "Journey", "Nyla", "Cali", "Tatum", "Carmen", "Gia", "Jazmine", "Heidi", "Miley", "Baylee", "Elaina", "Macy", "Ainsley", "Jane", "Raelynn", "Anastasia", "Adelaide", "Ruth", "Camryn", "Kiara", "Alessandra", "Hanna", "Finley", "Maddison", "Lia", "Bethany", "Karen", "Kelly", "Malia", "Jazmin", "Jayda", "Esmeralda", "Kira", "Lena", "Kamryn", "Kamila", "Karina", "Eloise", "Kara", "Elisa", "Rylie", "Olive", "Nayeli", "Tiffany", "Macie", "Skyler", "Addisyn", "Angelica", "Briella", "Fernanda", "Annie", "Maliyah", "Amiyah", "Jayden", "Charlee", "Caitlyn", "Elle", "Crystal", "Julie", "Imani", "Kendra", "Talia", "Angelique", "Jazlyn", "Guadalupe", "Alejandra", "Emely", "Lucille", "Anya", "April", "Elsie", "Madelynn", "Myla", "Julissa", "Scarlet", "Helen", "Breanna", "Kyra", "Madisyn", "Rosalie", "Brittany", "Brylee", "Jayleen", "Arielle", "Karla", "Kailey", "Arya", "Sarai", "Harley", "Miracle", "Kaelyn", "Kali", "Cynthia", "Daphne", "Aleah", "Caitlin", "Cassandra", "Holly", "Janelle", "Marilyn", "Katelynn", "Kaylie", "Itzel", "Carolina", "Bristol", "Haven", "Michaela", "Monica", "June", "Janiyah", "Camilla", "Jamie", "Rebekah", "Audrina", "Dayana", "Lana", "Serena", "Tiana", "Nylah", "Braelyn", "Savanna", "Skye", "Raelyn", "Madalyn", "Sasha", "Perla", "Bridget", "Aniya", "Rowan", "Logan", "Mckinley", "Averie", "Jaylah", "Aylin", "Joselyn", "Nia", "Hayley", "Lilian", "Adelynn", "Jaliyah", "Kassidy", "Kaylin", "Kadence", "Celeste", "Jaelyn", "Zariah", "Tatiana", "Jimena", "Lilyana", "Anaya", "Catalina", "Viviana", "Cataleya", "Sloane", "Courtney", "Johanna", "Amari", "Melany", "Anabelle", "Francesca", "Ada", "Alanna", "Priscilla", "Danna", "Angie", "Kailyn", "Lacey", "Sage", "Lillie", "Brinley", "Caylee", "Joy", "Kenley", "Vera", "Bailee", "Amira", "Aileen", "Aspen", "Emmalyn", "Erica", "Gracelyn", "Kennedi", "Skyla", "Annalise", "Danica", "Dylan", "Kiley", "Gwendolyn", "Jasmin", "Lauryn", 
                   "Aleena", "Justice", "Annabel", "Tenley", "Dahlia", "Gloria", "Lexie", "Lindsey", "Hallie", "Sylvia", "Elyse", "Annika", "Maeve", "Marlee", "Aryanna", "Kenya", "Lorelei", "Selah", "Kaliyah", "Adele", "Natasha", "Brenda", "Erika", "Alyson", "Braylee", "Emilee", "Raven", "Ariella", "Blakely", "Liana", "Jaycee", "Sawyer", "Anahi", "Jaelynn", "Elsa", "Farrah", "Cameron", "Evelynn", "Luciana", "Zara", "Madilynn", "Eve", "Kaia", "Helena", "Anne", "Estrella", "Leighton", "Nataly", "Whitney", "Lainey", "Amara", "Anabella", "Malaysia", "Samara", "Zoie", "Amani", "Phoenix", "Dulce", "Paola", "Marie", "Aisha", "Harlow", "Virginia", "Ember", "Regina", "Jaylee", "Anika", "Ally", "Kayden", "Alani", "Miah", "Yareli", "Journee", "Kiera", "Nathalie", "Mikaela", "Jaylynn", "Litzy", "Charley", "Claudia", "Aliya", "Madyson", "Cecelia", "Liberty", "Braelynn", "Evie", "Rosemary", "Myah", "Lizbeth", "Giana", "Ryan", "Teresa", "Ciara", "Isis", "Lea", "Shayla", "Jazlynn", "Rosa", "Gracelynn", "Desiree", "Elisabeth", "Isabela", "Arely", "Mariam", "Abbigail", "Emersyn", "Brenna", "Kaylynn", "Nova", "Raquel", "Dana", "Laney", "Laylah", "Siena", "Amelie", "Clarissa", "Lilianna", "Lylah", "Halle", "Madalynn", "Maleah", "Sherlyn", "Linda", "Shiloh", "Jessie", "Kenia", "Greta", "Marina", "Melina", "Amiya", "Bria", "Natalee", "Sariah", "Mollie", "Nancy", "Christine", "Felicity", "Zuri", "Irene", "Simone", "Amya", "Matilda", "Colette", "Kristen", "Paityn", "Alayah", "Janiya", 
                   "Kallie", "Mira", "Hailee", "Kathleen", "Meredith", "Janessa", "Noemi", "Aiyana", "Aliana", "Leia", "Mariyah", "Tori", "Alissa", "Ivanna", "Joslyn", "Sandra", "Maryam", "Saniyah", "Kassandra", "Danika", "Denise", "Jemma", "River", "Charleigh", "Emelia", "Kristina", "Armani", "Beatrice", "Jaylene", "Karlee", "Blake", "Cara", "Addilyn", "Amina", "Ansley", "Kaitlynn", "Iliana", "Mckayla", "Adelina", "Briley", "Elaine", "Lailah", "Mercedes", "Chaya", "Lindsay", "Hattie", "Lisa", "Marisol", "Patricia", "Bryanna", "Taliyah", "Adrienne", "Emmy", "Millie", "Paislee", "Charli", "Kourtney", "Leyla", "Maia", "Willa", "Milan", "Paula", "Ayleen", "Clare", "Kensley", "Reyna", "Martha", "Adley", "Elianna", "Emilie", "Karsyn", "Yasmin", "Lorelai", "Amirah", "Aryana", "Livia", "Alena", "Kiana", "Celia", "Kailee", "Rylan", "Ellen", "Galilea", "Kynlee", "Leanna", "Renata", "Mae", "Ayanna", "Chanel", "Lesly", "Cindy", "Carla", "Pearl", "Jaylin", "Kimora", "Angeline", "Carlee", "Aubri", "Edith", "Alia", "Frances", "Corinne", "Jocelynn", "Cherish", "Wendy", "Carolyn", "Lina", "Tabitha", "Winter", "Abril", "Bryn", "Jolie", "Yaritza", "Casey", "Zion", "Lillianna", "Jordynn", "Zariyah", "Audriana", "Jayde", "Jaida", "Salma", "Diamond", "Malaya", "Kimber", "Ryann", "Abbie", "Paloma", "Destinee", "Kaleigh", "Asia", "Demi", "Yamileth", "Deborah", "Elin", "Kaiya", "Mara", "Averi", "Nola", "Tara", "Taryn", "Emmalee", "Aubrianna", "Janae", "Kyndall", "Jewel", "Zaniyah", 
                   "Kaya", "Sonia", "Alaya", "Heather", "Nathaly", "Shannon", "Ariah", "Avah", "Giada", "Lilith", "Samiyah", "Sharon", "Coraline", "Eileen", "Julianne", "Milania", "Chana", "Regan", "Krystal", "Rihanna", "Sidney", "Hadassah", "Macey", "Mina", "Paulina", "Rayne", "Kaitlin", "Maritza", "Susan", "Raina", "Hana", "Keyla", "Temperance", "Aimee", "Alisson", "Charlize", "Kendal", "Lara", "Roselyn", "Alannah", "Alma", "Dixie", "Larissa", "Patience", "Taraji", "Sky", "Zaria", "Aleigha", "Alyvia", "Aviana", "Bryleigh", "Elliot", "Jenny", "Luz", "Ali", "Alisha", "Ayana", "Campbell", "Karis", "Lilyanna", "Azaria", "Blair", "Micah", "Moriah", "Myra", "Lilia", "Aliza", "Giovanna", "Karissa", "Saniya", "Emory", "Estella", "Juniper", "Kairi", "Kenna", "Meghan", "Abrielle", "Elissa", "Rachael", "Emmaline", "Jolene", "Joyce", "Britney", "Carlie", "Haylie", "Judith", "Renee", "Saanvi", "Yesenia", "Barbara", "Dallas", "Jaqueline", "Karma", "America", "Sariyah", "Azalea", "Everly", "Ingrid", "Lillyana", "Emmalynn", "Marianna", "Brisa", "Kaelynn", "Leona", "Libby", "Deanna", "Mattie", "Miya", "Kai", "Annalee", "Nahla", "Dorothy", "Kaylyn", "Rayna", "Araceli", "Cambria", "Evalyn", "Haleigh", "Thalia", "Jakayla", "Maliah", "Saige", "Avianna", "Charity", "Kaylen", "Raylee", "Tamia", "Aubrielle", "Bayleigh", "Carley", "Kailynn", "Katrina", "Belen", "Karlie", "Natalya", "Alaysia", "Celine", "Milana", "Monroe", "Estelle", "Meadow", "Audrianna", "Cristina", "Harlee", "Jazzlyn", "Scarlette", "Zahra", "Akira", "Ann", "Collins", "Kendyl", "Anabel", "Azariah", "Carissa", "Milena", "Tia", "Alisa", "Bree", "Carleigh", "Cheyanne", "Sarahi", "Laurel", "Kylah", "Tinley", "Kora", "Marisa", "Esme", "Sloan", "Cailyn", "Gisselle", "Kasey", "Kyndal", "Marlene", "Riya", "Annabell", "Aubriana", "Izabelle", "Kirsten", "Aya", "Dalilah", "Devyn", "Geraldine", "Analia", "Hayleigh", "Landry", "Sofie", "Tess", "Ashtyn", "Jessa", "Katalina")	

.names <- c(.male.names, .female.names)
.names <- .names[which(!duplicated(.names))]
                                    
                                    