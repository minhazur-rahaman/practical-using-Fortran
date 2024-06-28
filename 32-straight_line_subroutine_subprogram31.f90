! Write a Fortran program, which reads n elemrnts in an array and given a set of points(x1,y1),(x2,y2),(x3,y3),
!....(xn,yn) to fit a straight line y=mx+c with the help of subroutine subprogram 



program str_line_subroutine_subprogram  
IMPLICIT NONE 
integer :: n,i,serial_num
real, allocatable, dimension(:):: x,y
real :: m,c  


   do 
      print*, "Enter serial number(0 to exit)"
	  read*, serial_num
	  if (serial_num == 0) EXIT 

        print*, "Enter the value of N"
        read*, n
		allocate(x(n),y(n))
		print*, "Type the n pairs of x,y value: "
		read*, (x(i),y(i),i=1,n)

		call str(x,y,n,m,c)  

	   print*, "The fitted line is: y = ",m, " x+ ", c
	   deallocate(x)
	   deallocate(y)

	end do



	  contains 

	   subroutine str(x,y,n,m,c)
 
	   integer,intent(in) :: n
	   real,intent(in):: x(n),y(n)
	   real,intent(out):: m,c
	   real:: sum_x,sum_y,sum_xy,sum_xsq,temp
	   integer :: i 

	   ! Initialize
	      sum_x = 0.0
		  sum_y = 0.0
		  sum_xy =0.0
		  sum_xsq =0.0 

	   do i = 1, n
	      sum_x = sum_x + x(i)
		  sum_y = sum_y + y(i)
		  sum_xy = sum_xy + x(i)*y(i)
		  sum_xsq = sum_xsq + x(i)*x(i)
	   end do

	   ! Calculate the slope (m) and intercept (c)
	   temp = n
	   m = (temp*sum_xy - sum_x*sum_y)/(temp*sum_xsq-(sum_x)**2)
	   c = (sum_y-m*sum_x)/temp
	  end subroutine str

  end program str_line_subroutine_subprogram
 