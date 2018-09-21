
--2
e (x,y) =
	case (x,y) of
		(True, True) -> True
		(_,_) ->False

--3
e2(x, y) =
	case (x,y) of
		(True, b) -> b
		(False,_) -> False

--4

f = (x1,y1,z1) (x2,y2,z2)
	sqrt( (x2-x1)^2 + (y2-y1)^2 (z2-z1)^2)