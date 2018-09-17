p.ext <-
function(x, y)
{
#----------------------------------------------
#  x matrix IxS
#  y matrix JxS
#  resultant matrix (IxJ),S
#  with elements xis * yis
#-----------------------------------------------
	x <- as.matrix(x)
	y <- as.matrix(y)
	xr <- rep(1:nrow(x), nrow(y))
	yr <- rep(1:nrow(y), rep(nrow(x), nrow(y)))
	z <- as.matrix(x[xr,  ] * y[yr,  ])
}
