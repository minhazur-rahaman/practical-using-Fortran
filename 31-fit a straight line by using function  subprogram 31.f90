! Write a Fortran program, which reads n elemrnts in an array and given a set of points(x1,y1),(x2,y2),(x3,y3),
!....(xn,yn) to fit a straight line y=mx+c with the help of function subprogram



program stright_line_function 
IMPLICIT NONE 

real :: sum_x,sum_y,sum_xy,sum_xsq,m,c,slope, intercept
real,allocatable,dimension(:)::x,y
integer :: n,i   
   print*, "Enter the value of n"
   read*, n

   allocate(x(n),y(n))
   print*,"Enter n pairs of x,y values :"

   read*, (x(i),y(i),i=1,n)

    sum_x = 0
	sum_y = 0
	sum_xy= 0
	sum_xsq= 0

	do i=1,n

	 sum_x = sum_x+x(i)
	 sum_y = sum_y +y(i)
	 sum_xy = sum_xy +x(i)*y(i)
	 sum_xsq = sum_xsq+ x(i)*x(i)
	end do

	
	!m,c function ke call kore slope, intercept e assign korlam
	slope = m(sum_x,sum_y,sum_xy,sum_xsq,n)	  
	intercept = c(sum_x,sum_y,slope,n)

	!If you want you can use format.ok?
	print*, "Slope (m) =", slope
    print*, "Intercept (c) =", intercept 
	print*, "Equation of straight line is :"
	print*, "y=",slope,"x+ ",intercept

end program 



real function m(sum_x,sum_y,sum_xy,sum_xsq,n)
IMPLICIT NONE

real,intent(in) :: sum_x,sum_y,sum_xy,sum_xsq
integer,intent(in):: n
real :: nl
nl = n
   m = (nl*sum_xy-sum_x*sum_y)/(nl*sum_xsq-(sum_x)**2)

end function 

real function c(sum_x,sum_y,slope,n)
IMPLICIT NONE

real,intent(in) :: sum_x,sum_y,slope
integer,intent(in):: n
real :: nl
nl = n

    c = (sum_y-slope*sum_x)/nl

end function 
