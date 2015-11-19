##########
library(ggplot2)
library(gtable)

ones <- sample.int(10, size=10,replace = TRUE)
n <- length(ones)
m <- matrix(nrow=n, ncol=max(ones))

# Create gauge matrix 
for(i in 1:n){
	g_len <- max(ones)
	t_ <- rep(1, ones[i])
	f_ <- rep(0, g_len-ones[i])
	ifelse(ones[i]==g_len, 
		m[i,] <- rep(2, ones[i]),
		m[i,] <- factor(t(c(t_, f_)))
		)
}

mdat <- data.frame(t(m))
mdat$var <- factor(1:ncol(mdat))
mdat2 <- melt(mdat)
head(mdat2)

# Set Colors
sgcols <- c("#E4E7EC", "#3A7AA6")

# Run Test
ggplot(data = mdat2, aes(x = variable, y = as.numeric(var)))+ geom_point(aes(color=factor(value)), size=10) +theme( axis.text.x=element_blank(),panel.grid.major.y = element_blank(), axis.line = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + scale_color_manual(values=sgcols) + coord_flip()+theme(axis.line.y=element_blank(), axis.ticks.y=element_blank()) + theme_gdocs2()+theme( axis.text.x=element_blank() ,panel.grid.major.y = element_blank()) +theme(legend.position="none", axis.line = element_blank())

