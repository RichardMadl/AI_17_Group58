?seq.along
dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}
basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load  
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}
manualDM=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }  
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}


getH=function(xGoal, yGoal, dim) {
  hArray = matrix(data = 0, ncol = dim, nrow = dim)
  for (x in 1:dim){
    for (y in 1:dim) {
      d = manhattanDist(x, y, xGoal, yGoal)
      hArray[x,y] = d
    }
  }
  return (hArray)
}

expandFrontier=function(frontiers, xnew, ynew, hnew){
  frontiers$xcoords = append(frontiers$xcoords, xnew)
  frontiers$ycoords = append(frontiers$ycoords, ynew)
  frontiers$cost    = append(frontiers$cost, hnew)
  return (frontiers)
}

frontierNotEmpty=function(frontiers) {
  return (length(frontiers$xcoords) > 0)
}

bestFirst=function(frontiers) {
  index = which.min(frontiers$cost)
  node = list(x = frontiers$xcoords[[index]],
              y = frontiers$ycoords[[index]],
              c = frontiers$cost[[index]],
              newFrontiers = NULL)
  frontiers$xcoords[[index]] <- NULL
  frontiers$ycoords[[index]] <- NULL
  frontiers$cost[[index]] <- NULL
  node$newFrontiers = frontiers
  return (node)
}

notVisited=function(visited, node) {
  return(visited[node$y, node$x] == 0)  
}

calculateCost=function(node, h, roads, xnew, ynew){
  #print(paste0("Node_x = ", node$x))
  #print(paste0("Node_y = ", node$y))
  #print(paste0("NewNode_x = ", xnew))
  #print(paste0("NewNode_y = ", ynew))
  #print(roads)
  #print(paste0("roads[X,Y] = ", roads[node$x, node$y]))
  h[xnew, ynew] +roads[node$x, node$y] + node$c - h[node$x, node$y]
  
  return(distanceToGoal + trafficCost + costSoFar - prevDistance)
}

aStar=function(xStart, yStart, xGoal, yGoal, roads, car) {
  # initiate frontiers (x,y,cost), visited, moves & heuristics
  dim = dim(roads$hroads)[1]
  frontiers  = list(xcoords=list(), ycoords=list(), cost=list())
  visited    = matrix(data = 0, nrow = dim, ncol = dim)
  moves      = matrix(data = 0, nrow = dim, ncol = dim)
  h = getH(xGoal, yGoal, dim)
  #print (frontiers)
  #print (heuristics)
  
  # add neighbours to empty frontier list
  frontiers = expandFrontier(frontiers, xStart, yStart, h[xStart, yStart])
  i = 0
  while (frontierNotEmpty(frontiers)) {
    #print (paste0("Iteration:", i)) 
    #print (paste0("Current:", frontiers))
    
    node = bestFirst(frontiers)
    frontiers = node$newFrontiers
    
    if((node$x == xGoal) && 
       (node$y == yGoal)){
      break
    } 
    else {
      # Overlying neighbour 
      if ((node$y < dim) && (visited[node$x, node$y +1] == 0)) {
        frontiers = expandFrontier(frontiers, 
                                   node$x, 
                                   node$y +1,
                                   h[node$x, node$y +1] + roads$vroads[node$y, node$x] + node$c - h[node$x, node$y])
                                   
        moves[node$x, node$y +1]= 8
      }
      
      # Underlying neighbour
      if ((node$y > 1) && (visited[node$x, node$y -1] == 0)) {
        frontiers = expandFrontier(frontiers, 
                                   node$x, 
                                   node$y -1,
                                   h[node$x, node$y -1] + roads$vroads[node$y-1, node$x] + node$c - h[node$x, node$y])
                                   
        moves[node$x, node$y -1] = 2
      }
      
      # Right neighbour
      if ((node$x < dim) && (visited[node$x +1, node$y] == 0)) {
        frontiers = expandFrontier(frontiers, 
                                   node$x +1, 
                                   node$y,
                                   h[node$x+1, node$y] + roads$hroads[node$y, node$x] + node$c - h[node$x, node$y])
        moves[node$x +1, node$y] = 6
      }
      
      # Left neighbour
      if ((node$x > 1) && (visited[node$x -1, node$y] == 0)) {
        frontiers = expandFrontier(frontiers, 
                                   node$x -1, 
                                   node$y,
                                   h[node$x-1, node$y] + roads$hroads[node$y, node$x-1] + node$c - h[node$x, node$y])
        moves[node$x -1, node$y] = 4
      }
      
      # Mark node as visited
      visited[node$x, node$y] = 1
    }
    
  }
  return (moves)
}

getPath = function(xGoal, yGoal, moves){
  x = xGoal
  y = yGoal
  last = 5
  notFinished = TRUE
  print(moves)
  while(notFinished){
    currentMove = moves[x,y]
    if(is.null(currentMove) | currentMove == 0){
      notFinished = FALSE
      currentMove = 5
    } else if(currentMove == 8){
        last = 8
        y = y - 1
    } else if(currentMove == 2){
        last = 2
        y = y + 1
    } else if(currentMove == 6){
        last = 6
        x = x - 1
    } else if (currentMove == 4) {
        last = 4
        x = x + 1
    }
  }
  return (last)
}


manhattanDist = function(x1, y1, x2, y2) {
  dx = abs(x1 - x2)
  dy = abs(y1 - y2)
  return (dx + dy)       
}

getClosestPackage = function(car, packages) {
  package = list(x = NULL, y = NULL, dist = NULL)
  rows = dim(packages)[1]
  for (row in 1:rows) {
    if (packages[row, 5] == 0) {
        package$x = append(package$x, packages[row,1])
        package$y = append(package$y, packages[row,2])
        dist = manhattanDist(car$x, car$y, packages[row,1], packages[row,2])
        package$dist = append(package$dist, dist)
    }
  }

  index = which.min(package$dist)
  closest = list(x = package$x[[index]], y = package$y[[index]])
  #print (paste0("Closest package: ", closest))
  return (closest)
  
}

aStarDM = function(roads,car,packages) {

  if(car$load == 0){
    closest = getClosestPackage(car, packages)
    xGoal   = closest$x
    yGoal   = closest$y
    
  } else if(car$load > 0){
    xGoal = packages[car$load, 3]
    yGoal = packages[car$load, 4]
  }
  
  # add visited set to car
  moves = aStar(car$x, car$y, xGoal, yGoal, roads)
  nextMove = getPath(xGoal, yGoal, moves)
  car$nextMove = nextMove
  #print(paste0("next move: ", nextMove))
  return (car)
}

runDM=function() {
  runDeliveryMan(carReady = aStarDM, dim = 10, turns = 2000, doPlot = T,
                 pause = 0.1, del = 5)
}
#' Run Delivery Man
#' 
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the 
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic 
#' conditional on the vertical roads. (2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car 
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are 
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be 
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this 
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=aStarDM,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5) {
  roads=makeRoadMatrices(dim)

  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list(prev=list(x=NA, y=NA)))
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i) 
      plotRoads(roads$hroads,roads$vroads) 
      points(car$x,car$y,pch=16,col="blue",cex=3)  
      plotPackages(packages)      
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          print (paste("Congratulations! You suceeded in",i,"turns!"))
          return (i)
        }
      }      
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  print (paste("You failed to complete the task. Try again."))
  return (NA)
}
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  } 
  return (0)
}
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$y,car$x]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$y,car$x]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$y,car$x]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$y,car$x]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")    
  }
  car$nextMove=NA
  return (car)
} 

plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0) 
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

makeRoadGrid<-function() {
  
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}

makeRoadGrid<-function() {
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}
#' @export
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @export
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n)
  vroads=matrix(rep(1,(n-1)*n),nrow=n-1)
  list(hroads=hroads,vroads=vroads)
}

#' @export
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(col,col+1),c(row,row),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(col,col),c(row,row+1),col=vroads[row,col])
    }
  }
}

#' @export
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }    
  }
  list (hroads=hroads,vroads=vroads)
}


