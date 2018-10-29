getEuclideanLength <- function(vec) {
  # Calculate euclidian length of a vector.
  #
  # Args:
  #   x: The input vector.
  #
  # Returns:
  #   The euclidian length of the input vector.
  sqrt(sum(vec**2))
}

getEuclideanDistance <- function(x, y) {
  # Calculate the euclidian distance of two vectors.
  #
  # Args:
  #   x: The first vector.
  #   y: The second vector.
  #
  # Returns:
  #   The distance of the two input vectors.
  sqrt(sum((x - y) ^ 2))
}

getVectorProjection <- function(start, end, point) {
  # Calculate the vector projection of a point onto a line.
  #
  # Args:
  #   start: The starting point of the line.
  #   end: The ending point of the line.
  #   point: The input point.
  #  Returns:
  #    The vector projection of the point to the line.
  diagonal <- end - start
  result <- list(va1=numeric(0), va2=numeric(0))
  result$va1 <- sum((point - start) *
                      (diagonal / getEuclideanLength(diagonal))) *
    (diagonal / getEuclideanLength(diagonal))
  result$va2 <- (point - start) - result$va1
  result
}

createOppositePoint <- function(point) {
  # A helper function to create "opposing" points inside a unit hypercube.
  # "Opposing" in this case means mirrored around the 0.5-vector
  #
  # Args:
  #   point: The point to be mirrored.
  #
  # Returns:
  #   The mirrored / opposing point.
  (point - 0.5) * (-1) + 0.5
}

getCorners <- function(n=2, makePretty=FALSE) {
  # Create object containing the corner vectors of a n-dimensional hypercube.
  #
  # Args:
  #   n: Number of dimensions.
  #   makePretty: If makePretty is FALSE, the value of expand.grid is returned.
  #               If makePretty is TRUE, a list of vectors representing the
  #               corners is returned.
  #
  # Returns:
  #   Either a list or the value of expand.grid containing the corner vectors
  #   of the hypercube.
  arguments <- vector("list", n)  # arguments for expand.grid
  for (i in 1:n) {
    arguments[[i]] <- c(0,1)
  }
  corners <- expand.grid(arguments)
  if (makePretty) {
    result <- vector("list", nrow(corners))
    for (i in 1:nrow(corners)) {
      result[[i]] <- as.vector(corners[i, ], mode="integer")
    }
    result
  } else {
    corners
  }
}

createDiagList <- function(n=2) {
  # Create a list of diagonals.
  #
  # Creates the start and end points of the diagonals
  # in a n-dimensional unit hypercube. The start and
  # end points of a diagonal have flipped values, i.e.
  # 0 -> 1 and 1 -> 0.
  #
  # Args:
  #   n: The number of dimensions
  #
  # Returns:
  #   A list of two lists with start and end points of
  #   the diagonals in a hypercube each.
  result <- list()
  l <- rep(list(0:1), n)
  all.corners <- expand.grid(l)
  for (i in 1:(2^(n-1))) {
    result$starts[[i]] <- all.corners[i,]
    result$ends[[i]] <- all.corners[2^n-(i-1),]
  }
  result
}

sin.star <- function(s) {
  # This function reshapes the standard sin-function so that a full cycle fits
  # inside the [0,1] interval.
  #
  # Args:
  #   s: The input for the reshaped sine function.
  #
  # Returns:
  #   The reshed sine value.
  (sin(s * 2 * pi) + 1) / 2
}

wrap.sin.star <- function(s, n=3) {
  # A wrapper function to create a 3D-vector of the sine dependency.
  #
  # Args:
  #   s: The value of the the first element of the sine dependency vector.
  #      See sin.star for more information.
  #   n: The number of dimensions.
  #
  # Returns:
  #   A vector according to the sine dependency where each vector element
  #   is the reshaped sine value of it's former element, starting with the
  #   first element being s.
  if (n == 3) {
    c(s, sin.star(s), sin.star(sin.star(s)))
  } else {
    c(s, sin.star(s))
  }
}

linDep <- function(direction, suppVec, margin=0.1){
  # This function creates a point around a line.
  #
  # Args:
  #   direction: A directional vector of the line.
  #   suppVec: The support vector of the line.
  #   margin: Half of the provided value is the maximal distance from the
  #           line in one dimension.
  #
  # Returns:
  #   A vector containing the data point.
  if(!(length(direction) == length(suppVec))) {
    stop("Direction and suppVec need to have the same dimension.")
  }
  weight <- runif(1, 0, 1)
  new.val <- c(weight*direction + suppVec)
  start.val <- new.val
  m <- 1-margin
  for (d in 1:length(suppVec)) {
    new.val[[d]] <- new.val[[d]] + runif(1, -m/2, m/2)
    if(new.val[[d]] < 0 || new.val[[d]] > 1) {
      while(new.val[[d]] < 0 || new.val[[d]] > 1) {
        new.val[[d]] <- start.val[[d]] + runif(1, -m/2, m/2)
      }
    }
  }
  new.val
}

getHOPs <- function(start, n, oppDim) {
  corners <- getCorners(n=n, makePretty=FALSE)
  # create the "oppValue" for the desired dimension (oppDim)
  oppValue <- createOppositePoint(start[[oppDim]])
  # replace the value of the oppDim of all corners
  for (i in 1:nrow(corners)) {
    corners[i,][oppDim] <- oppValue
  }
  # remove duplicates
  corners <- unique(corners)
  # rename rows and transform to a list of vectors
  rownames(corners) <- 1:nrow(corners)
  result <- vector("list", nrow(corners))
  for (i in 1:nrow(corners)) {
    result[[i]] <- as.vector(corners[i,], mode="integer")
  }
  result
}

createHOPLines <- function(n=2) {
  # The hourglass lines are the vectors between a corner of the
  # hypercube and its ``HOPs'' (Hourglass Opposing Points). A HOP can be
  # any other corner of the hypercube other than the starting point as long
  # as the value of a pre-specified dimension is different (by only using
  # the corners, the values in question can only be either 0 or 1).
  #
  # Example: Assume we have a 3D Hourglass.
  # We thus have vectors (x,y,z). For finding the HOPs of a corner, we
  # want x to be the opposite. Thus, the point (0,0,0) would have the
  # following HOPs:
  # (1,0,0), (1,1,0), (1,0,1) and (1,1,1)
  #
  # In general we have 2^(n-1) HOPs per corner in a n-dimensional hypercube.
  corners <- getCorners(n=n, makePretty=TRUE)
  result = vector("list", length(corners))
  for (i in 1:length(corners)) {
    result[[i]][["starts"]] <- corners[[i]]
    result[[i]][["endList"]] <- getHOPs(result[[i]][["starts"]], n, 1)
  }
  result
}

########## Actual functions ############

isInHiddenSpace.Wall <- function(row, subspace) {
  all(row[subspaces[[subspace]]] > 1 - margins[[subspace]])
}

isInHiddenSpace.Square <- function(row, subspace) {
  all(abs(row[subspaces[[subspace]]] - 0.5) < margins[[subspace]] / 2)
}

isInHiddenSpace.Donut <- function(row, subspace) {
  sqrt(sum((row[subspaces[[subspace]]] - 0.5)**2)) < margins[[subspace]] /2 |
    sqrt(sum((row[subspaces[[subspace]]] - 0.5)**2)) > 0.5
}

isInHiddenSpace.Linear <-function(row, subspace) {
  p <- row[subspaces[[subspace]]]
  b <- rep(1, length(p))
  # https://en.wikipedia.org/wiki/Vector_projection
  a1 <- sum(p * (b / sqrt(length(b))))
  va1 <- a1 * ( b /sqrt(length(b)))
  vd <- p - va1
  d <- sqrt(sum(vd**2))
  d > 0.5 - margins[[subspace]] / 2
}

isInHiddenSpace.Cross <- function(row, subspace, marginFactor=1) {
  point <- row[subspaces[[subspace]]]
  if (marginFactor != 1) {point <- row}
  n <- length(subspaces[[subspace]])
  diagonals.list <- createDiagList(n)
  diagonals.projections <- vector("list", (2^n))
  diffs <- list()
  for (i in 1:(2^(n-1))) {
    diagonals.projections[[i]] <- getVectorProjection(diagonals.list$starts[[i]],
                                                      diagonals.list$ends[[i]],
                                                      point);
    diffs[[i]] <- getEuclideanLength(diagonals.projections[[i]]$va2)
  }
  all(diffs >  (1 - (margins[[subspace]] * marginFactor)) / 2)
}

isInHiddenSpace.Hourglass <- function(row, subspace, marginFactor=1) {
  if (marginFactor != 1) {
    point <- row  # assume call from ensureOutlyingBehavior
  } else {
    point <- row[subspaces[[subspace]]]  # only look at the relevant subspace
  }
  
  n <- length(subspaces[[subspace]])
  line.list <- createHOPLines(n)
  numberOfCorners <- length(line.list)
  numberOfHOPs <- length(line.list[[1]][["endList"]])
  line.projections <- vector("list", length(line.list))
  diffs <- vector("list", (numberOfCorners * numberOfHOPs))
  k <- 1
  for (i in 1:numberOfCorners) {
    line.projections[[i]] <- vector("list", numberOfHOPs)
    for (j in 1:numberOfHOPs) {
      line.projections[[i]][[j]] <- getVectorProjection(line.list[[i]][["starts"]],
                                                        line.list[[i]][["endList"]][[j]],
                                                        point)
      diffs[k] <- getEuclideanLength(line.projections[[i]][[j]]$va2)
      k <- k + 1
    }
  }
  all(diffs > 0.5 - ((margins[[subspace]]*marginFactor)/2))
}

isInHiddenSpace.Sine <- function(row, subspace, marginFactor = 1) {
  if (marginFactor != 1) {
    point <- row  # assume call from ensureOutlyingBehavior
  } else {
    point <- row[subspaces[[subspace]]]  # only look at the relevant subspace
  }
  
  n <- length(subspaces[[subspace]])
  if (n > 3) {
    n <- 3
  }
  
  # We need to calculate the distance of the point from the Sine Dependency
  # Curve (SDC). This can be achieved with the standard euclidian distance (D).
  # We use the zero points of the derivative to find the point on the SDC with
  # minimal distance to the point. Since the root is a monotonous function
  # we can calculate dD^2 / ds instead of dD / ds in order to get rid of the
  # root.
  #
  # Load requiered package.
  if(require("rootSolve")){
    # print("rootSolve is loaded correctly.")
  } else {
    # print("Trying to install rootSolve.")
    install.packages("rootSolve")
    if(require(rootSolve)){
      # print("rootSolve installed and loaded.")
    } else {
      stop("Could not install rootSolve.")
    }
  }
  
  dsd.3D <- function(s) {  # Derivative Squared Distance
    # The derivative of the squared euclidian distance function for some point
    # s and a predefined point.
    #
    # Args:
    #   s: The input for the reshaped sine function. See sin.star for more
    #      information.
    #
    # Returns:
    #   The derivative of the squared distance function for s and point.
    2 * (s - point[1]) +
      2 * (sin.star(s) - point[2]) * cos(2 * pi * s) * pi +
      2 * ((sin(pi * (sin(2 * pi * s) + 1)) + 1) / 2 - point[3]) *
      cos(pi * (sin(2 * pi * s) + 1)) *
      cos(2 * pi * s) * pi^2
  }
  dsd.2D <- function(s) {  # Derivative Squared Distance
    # The derivative of the squared euclidian distance function for some point
    # s and a predefined point.
    #
    # Args:
    #   s: The input for the reshaped sine function. See sin.star for more
    #      information.
    #
    # Returns:
    #   The derivative of the squared distance function for s and point.
    2 * (s - point[1]) +
      2 * (sin.star(s) - point[2]) * cos(2 * pi * s) * pi
  }
  
  # Calculate all zero points of dsd. Differentiate between 2D and 3D.
  if (n == 2) {
    zero.points <- uniroot.all(dsd.2D, c(0, 1))
  } else {
    zero.points <- uniroot.all(dsd.3D, c(0, 1))
  }
  
  # Save sine dependency vectors obtained from zero.points to a matrix.
  mat <- sapply(zero.points, wrap.sin.star, n)
  
  # Get minimum distance.
  diff <- min(apply(mat, 2, getEuclideanDistance, point))
  diff > 0.5 - ((margins[[subspace]] * marginFactor) / 2)
}

ensureOutlyingBehavior.Wall <- function(row, subspace) {
  row[subspaces[[subspace]]] + (1 - row[subspaces[[subspace]]]) * 0.2
}

ensureOutlyingBehavior.Square <- function(row, subspace) {
  transformed_r <- row[subspaces[[subspace]]]-0.5
  transformed_r <- transformed_r * ((margins[[subspace]] / 2) -
                                      (margins[[subspace]] / 2) * 0.2) /
    (margins[[subspace]] / 2)
  transformed_r + 0.5
}

ensureOutlyingBehavior.Donut <- function(row, subspace) {
  # Distinguish between points "inside" and "outside" of the sphere
  # If it is INSIDE the donut, push it towards the center
  if(sqrt(sum((row[subspaces[[subspace]]]-0.5)**2)) < margins[[subspace]]/2) { 
    absr <- abs(row[subspaces[[subspace]]]-0.5)
    res <- ((row[subspaces[[subspace]]]-0.5) * absr/(absr + absr*0.2)) + 0.5
  } else { # If it is OUTSIDE the Donut, push it towards the corners
    absr <- abs(row[subspaces[[subspace]]]-0.5)
    # I've put 0.3 here because otherwise I find it too close really
    res <- ((row[subspaces[[subspace]]]-0.5) *
              (absr + (0.5-absr)*0.3)/absr) + 0.5  
  }
  res
}

ensureOutlyingBehavior.Linear <- function(row, subspace){
  d <- 0 # This way is a bit suboptimal but it avoids points at the border
  while(!(d > 0.5 - (margins[[subspace]] * 0.8) / 2)) {
    p <- runif(length(row[subspaces[[subspace]]]))
    b <- rep(1, length(p))
    # https://en.wikipedia.org/wiki/Vector_projection
    a1 <- sum(p * (b / sqrt(length(b))))
    va1 <- a1 * (b / sqrt(length(b)))
    vd <- p - va1
    d <- sqrt(sum(vd ** 2))
  }
  p
}

ensureOutlyingBehavior.Cross <- function(row, subspace){
  d <- FALSE
  while(!d) {
    p <- runif(length(row[subspaces[[subspace]]]))
    d <- isInHiddenSpace(p, subspace, marginFactor = 0.8)
  }
  p
}

ensureOutlyingBehavior.Hourglass <- function(row, subspace){
  d <- FALSE
  while(!d) {
    p <- runif(length(row[subspaces[[subspace]]]))
    d <- isInHiddenSpace(p, subspace, marginFactor = 0.8)
  }
  p
}

ensureOutlyingBehavior.Sine <- function(row, subspace) {
  d <- FALSE  # (enough) distance
  while(!d) {
    p <- runif(length(row[subspaces[[subspace]]]))  # Generate a point
    d <- isInHiddenSpace(p, subspace, marginFactor = 0.8)  # Check distance
  }
  p
}

constructPoint.Wall <- function(subspace){
  n <- length(subspaces[[subspace]])
  index <- sample(x = 1:n, size = 1)  # Index of dimension < margin
  point <- runif(n)
  point[index] <- runif(1, 0, 1-margins[[subspace]])
  point
}

construcPoint.Square <- function(subspace) {
  n <- length(subspaces[[subspace]])
  index <- sample(x = 1:n, size = 1)  # Index of dimension < margin
  point <- runif(n)
  point[index] <- sample(c(runif(1, 0, 1-margins[[subspace]]),
                           runif(1, margins[[subspace]], 1)), 1)
  point
}

constructPoint.Donut <- function(subspace) {
  # Still has some random characteristic.
  # Based on Marsaglia 1972.
  # Generate random point inside the unit hypercube.
  # Rescale it to the surface of a n-sphere, add some
  # noise to the point and resacle it to fit into the
  # unit hypercube again.
  n <- length(subspaces[[subspace]])
  x <- runif(n, -1, 1)
  r <- sqrt(sum(x**2))
  while(r >= 1) {
    x <- runif(n, -1, 1)
    r <- sqrt(sum(x**2))
  }
  normalized.x <- x / r
  point <- normalized.x
  for (d in 1:n) {
    point[[d]] <- point[[d]] -  runif(1, 0, 0.1)
  }
  point / 2 + rep(0.5, n)
}

constructPoint.Linear <- function(subspace) {
  n <- length(subspaces[[subspace]])
  point <- linDep(rep(1, n), rep(0, n), margins[[subspace]])
}

constructPoint.Cross <- function(subspace) {
  n <- length(subspaces[[subspace]])
  diagonals.list <- createDiagList(n)
  index <- round(runif(1, 1, 2^(n - 1)))  # Index of random diagonal
  direction <- diagonals.list$ends[[index]] - diagonals.list$starts[[index]]
  point  <- linDep(as.vector(direction),
                   as.vector(diagonals.list$starts[[index]]),
                   margins[[subspace]])
  as.vector(unlist(point))
}

constructPoint.Hourglass <- function(subspace) {
  #browser()
  n <- length(subspaces[[subspace]])
  # Chose opposing dimension
  oppDim <- 1  # For random opposing dim use: sample(x = 1:n, size = 1)
  # Chose starting corner
  start <- sample(x = c(0,1), size = n, replace = TRUE)
  # Create a HOP
  end <- sample(x = c(0,1), size = n, replace = TRUE)
  end[oppDim] <- (start[oppDim] - 1) * (-1)
  point <- linDep(direction = as.vector(end - start, mode = "integer"),
                  suppVec = start,
                  margin = margins[[subspace]])
  as.vector(unlist(point))
}

constructPoint.Sine <- function(subspace) {
  n <- length(subspaces[[subspace]])
  if(n > 3) {
    n <- 3
  }
  m <- 1 - margins[[subspace]]
  point <- wrap.sin.star(s = runif(1), n = n)
  start.point <- point
  for (d in 1:n) {
    point[[d]] <- point[[d]] + runif(1, -m/2, m/2)
    if(point[[d]] < 0 || point[[d]] > 1) {
      while(point[[d]] < 0 || point[[d]] > 1) {
        point[[d]] <- start.point[[d]] + runif(1, -m/2, m/2)
      }
    }
  }
  point
}
