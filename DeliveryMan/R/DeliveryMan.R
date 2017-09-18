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
runDeliveryMan <- function (carReady=nextMove,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    #nextMove(roads,car,packages)
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

nextMove = function(roads,car,packages) {
  start <- list(x=car$x, y=car$y)
  goal <- list(x=packages[1,1], y=packages[1,2])
  if(car$load!=0){
    goal <- list(x=packages[car$load,3], y=packages[car$load,4])
  }else{
    nextMove  <- list(move=0, cost=Inf)
    for(i in 1:nrow(packages)){
      goal <- list(x=packages[i,1], y=packages[i,2])
      currentNextMove <- list(move = 1, cost = Inf)
      currentNextMove <- aStar(start, goal, roads)
      print(cat("Current next move cost: ", currentNextMove$cost))
      print(cat("Next move cost: ", nextMove$cost))
      if(currentNextMove$cost < nextMove$cost){
        nextMove = currentNextMove
      }
    }
  }
  car$nextMove = nextMove$move
  return(car)
}


aStar = function(start, goal, roads){
  nextMove = list(move = 0, cost = Inf)
  if(start$x==goal$x & start$y == goal$y){
    
    nextMove$move = 1
    return(1)
  }
  closedSet <- list()
  openSet <- list(start)
  cameFrom <- list()
  gScore <- matrix(Inf, nrow = nrow(roads$hroads), ncol = ncol(roads$vroads))
  gScore[start$x, start$y] = 0
  fScore <- matrix(Inf, nrow = nrow(roads$hroads), ncol = ncol(roads$vroads))
  fScore[start$x, start$y] = manhattanDistance(start,goal,roads)
  while(list.count(openSet)>0){
    
    current = getNextNode(openSet, fScore)
    
    if(current$x == goal$x && current$y == goal$y){
      path = reconstructPath(cameFrom, current)
      nextMove$cost = gScore[current$x, current$y]
      nextMove$move = list.first(path)$step
      return(nextMove)
    }
    
    openSet = list.exclude(openSet, x==current$x & y==current$y)
    
    closedSet = list.append(closedSet, current)
    
    neighbours = getNeighbours(current, roads)
    
    for(neighbour in neighbours){
      if(!existsInList(neighbour, closedSet)){
        if(!existsInList(neighbour, openSet)){
          openSet = list.append(openSet, neighbour)
        }
        tentativeScore = gScore[current$x, current$y] + manhattanDistance(current,neighbour,roads)
        if(tentativeScore < gScore[neighbour$x, neighbour$y]){
          cameFrom = updateCameFrom(cameFrom, neighbour, current)
          gScore[neighbour$x, neighbour$y] = tentativeScore
          fScore[neighbour$x, neighbour$y] = tentativeScore + manhattanDistance(neighbour, goal, roads)
        }
      }
    }
  }
}
updateCameFrom <- function(cameFrom, neighbour, current){
  cameFrom = list.exclude(cameFrom, x==neighbour$x & y==neighbour$y)
  cameFrom = list.append(cameFrom, list(x=neighbour$x, y=neighbour$y, from=current))
  return (cameFrom)
}
reconstructPath <- function(cameFrom, current){
  totalPath = list(current)
  while(existsInList(current, cameFrom)){
    previous = list.first(cameFrom, x==current$x & y==current$y)$from
    step = deriveStepFromNeighbour(current, previous)
    current = list(x=previous$x, y=previous$y, step = step)
    totalPath = list.append(totalPath, current)
  }
  totalPath = list.reverse(totalPath)
  return (totalPath)
}

existsInList <- function(element, list){
  return (list.count(list, x==element$x & y==element$y)>0)
}

getNeighbours <-function(current, roads){
  neighbours = list()
  if(current$x-1>0) {
    neighbours = list.append(neighbours, list(x=current$x-1, y=current$y))
  }
  if(current$x+1<=ncol(roads$hroads)) {
    neighbours = list.append(neighbours, list(x=current$x+1, y=current$y))
  }
  if(current$y-1>0) {
    neighbours = list.append(neighbours, list(x=current$x, y=current$y-1))
  }
  if(current$y+1<=nrow(roads$vroads)) {
    neighbours = list.append(neighbours, list(x=current$x, y=current$y+1))
  }
  return(neighbours)
}

getNextNode <- function(set, fScore){
  bestFScore = Inf
  bestNode = list(x=-1, y=-1)
  for(node in set){
    if(fScore[node$x, node$y]<bestFScore){
      bestFScore = fScore[node$x, node$y]
      bestNode = node
    }
  }
  return (bestNode)
}
manhattanDistance <- function(start, goal, roads){
  distance = 0
  if(start$x<=goal$x){
    for(i in start$x:goal$x){
      if(i!=goal$x){
        distance = distance + roads$hroads[start$y, i]
      }
    }
  }else{
    for(i in goal$x:start$x){
      if(i!=goal$x){
        distance = distance + roads$hroads[start$y, i]
      }
    }
  }
  
  if(start$y<=goal$y){
    for(i in start$y:goal$y){
      if(i!=goal$y){
        distance = distance + roads$vroads[i, goal$x]
      }
    }
  }else{
    for(i in goal$y:start$y){
      if(i!=goal$y){
        distance = distance + roads$vroads[i, goal$x]
      }
    }
  }
  
  return(distance)
}

deriveStepFromNeighbour <- function(current, neighbour){
  if(current$x+1 == neighbour$x) return(4)
  if(current$x-1 == neighbour$x) return(6)
  if(current$y+1 == neighbour$y) return(2)
  if(current$y-1 == neighbour$y) return(8)
}