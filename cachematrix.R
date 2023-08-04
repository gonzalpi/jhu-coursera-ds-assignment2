makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}




setwd("/Users/macbook/laba2/1")
getwd()

# makeCacheMatrix: Эта функция создает специальный "матричный" объект,
# который может кешировать свою обратную матрицу.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # хранит обратную матрицу
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) { 
    # устанавливает значение обратной матрицы и очищает предыдущее значение
    # Оператор "<<-" используется для установки переменной, которая уже существует
    i <<- inverse
  }
  getinverse <- function() {i}
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve: Эта функция вычисляет обратную матрицу для специального "матричного" 
# объекта, возвращенного функцией makeCacheMatrix выше. Если обратная матрица 
# уже была рассчитана (и матрица не изменилась), то функция cacheSolve 
# извлекает обратную матрицу из кеша.
cacheSolve <- function(x, ...) {
  ## Возвращает матрицу, которая является обратной для 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("извлекаю данные из кеша")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i # возвращаем обратную матрицу
}

# Пример использования программы
Mat <- matrix(c(1:8),3,3)
Mat
Mat1 <- makeCacheMatrix(Mat)
Mat1
cacheSolve(Mat1)
