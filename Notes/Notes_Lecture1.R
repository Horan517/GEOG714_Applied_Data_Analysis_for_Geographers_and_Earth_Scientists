# Notes on lecture 1
# 没有 /* ... */这样的注释

length("yiannakoulias")
nchar("yiannakoulias")
# length("yiannakoulias", "Niko") 这个也是错误的

m <- as.matrix(cbind(c(1,2,3), c(4,5,6)))

m <- as.matrix(cbind(c(1,2,3), c("4","5","6")))
  
  x <- 4
print(x)

# float?
if(a<5)
  x <- 4
print(x)


# 定义变量 x
x <- 10

# 现在打印 x
print(x)  # 不会出现错误

v <- c(1, 5, 2, 3, 4)

print(v[1], v[2:3])