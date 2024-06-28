! Write a Fortran program, which reads n elemrnts in an array and given a set of points(x1,y1),(x2,y2),(x3,y3),
!....(xn,yn) to fit a straight line y=mx+c

program straight_line 
IMPLICIT NONE 
integer :: n,i,serial_num
real, allocatable, dimension(:):: x,y
real:: sum_x,sum_y,sum_xy,sum_xsq,m,c,temp


   do 
      print*, "Enter serial number (0 to be EXIT)"
	  read*, serial_num
	  if (serial_num == 0) EXIT 

        print*, "Enter the value of N"
        read*, n
		allocate(x(n),y(n))
		print*, "Type the n pairs of x,y value: "
		read*, (x(i),y(i),i=1,n)

		  
		  sum_x = 0
		  sum_y = 0
		  sum_xy =0
		  sum_xsq =0 

	   do i = 1, n
	      sum_x = sum_x + x(i)
		  sum_y = sum_y + y(i)
		  sum_xy = sum_xy + x(i)*y(i)
		  sum_xsq = sum_xsq + x(i)*x(i)
	   end do
	   temp = n
	   m = ((temp*sum_xy - sum_x*sum_y)/(temp*sum_xsq-(sum_x)**2))
	   c = (sum_y-m*sum_x)/n

	   print 72,m,c

	   72 format (2X, "Equation of straight line is : ","Y= ",f8.5, " x+ ",f8.3)
	   deallocate(x)
	   deallocate(y)

	   end do

	   end program 