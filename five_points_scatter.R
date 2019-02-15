

library(ggplot2)
set.seed(12)
x1 <- rnorm(50, 10, 2)

x2 <- scale(matrix(rnorm(50), ncol=1))
x12 <- cbind(scale(x1),x2)

c1 <- var(x12)
chol1 <- solve(chol(c1))
newx <-  x12 %*% chol1

newc <- matrix(c(1,-0.52, -0.52, 1), ncol=2)
chol2 <- chol(newc)
finalx <- newx %*% chol2 * sd(x1) + mean(x1)

finalx[,2] <- (2*finalx[,2]-5)

giraffe_data <- finalx
colnames(giraffe_data) <- c("Heights", "Celery_Eaten")

giraffe_data <- as.data.frame(giraffe_data)

(points <- giraffe_data[c(12, 50, 14, 43, 32),])


p <-  ggplot(data= points, aes(x= Heights, y=Celery_Eaten)) +
  geom_point() + geom_vline(xintercept = mean(giraffe_data$Heights)) + geom_hline(yintercept = mean(giraffe_data$Celery_Eaten))

p


p <-  ggplot(data= points, aes(x= Heights, y=Celery_Eaten)) +
  geom_point(size = 3) + geom_vline(xintercept = mean(giraffe_data$Heights), col="grey50", linetype="dashed", size = 2) + geom_hline(yintercept = mean(giraffe_data$Celery_Eaten), col= "grey81", linetype="dashed", size= 2) + xlim(5.5, 14.4) + ylim(5.3, 23)


p <- p + theme_light() + theme(panel.border = element_blank(), panel.grid.minor=element_blank())

p

ggsave(filename = "/Users/Desiree/Documents/New R Projects/Cars/points.png", width=10, height=6, p)

#xlim(5.7, 14.4) + ylim(5.3, 22.5)


ggplot_build(p)

xlim(5.5,15)
ylim(5,24)