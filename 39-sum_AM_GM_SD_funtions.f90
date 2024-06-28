!problem : Write a FORTRAN program, which reads n elements and write some Function Subprogram to print the value of Sum, AM, GM and SD
!problem 39

program sum_AM_GM_SD_funtions
IMPLICIT NONE 

real, ALLOCATABLE, DIMENSION(:) :: val

real :: t_sum, am, gm,sd

integer :: i, n, serial_num 

      do 
	       print*, "Enter the serial number: "
		   read*, serial_num 
		   if(serial_num == 0) EXIT
		    
			print*, "Enter the number of Observation : "
			read*, n
			
			allocate(val(n)) 

			print*, "Input the element :"
			read*, (val(i), i= 1,n)

			!gm(val, n)
			!sd(val, n)

			print*, "Sum will be: ", t_sum(val,n)

			print*, "AM value will be: ", am(val,n)

			print*, "GM value will be: ", gm(val,n)

			print*, "SD value will be: ", sd(val,n)


		 deallocate(val)

	  end do


end program sum_AM_GM_SD_funtions


      Real Function t_sum(val,n)

	  IMPLICIT NONE 

	  integer :: i,n

	  real :: val(n)

	  t_sum = 0

	  do i = 1,n
	      t_sum = t_sum + val(i)
	  end do

	  end function 




	     real function am(val, n)
		 
		 IMPLICIT NONE 

		 integer :: i, n 
		 real :: t_sum =0 , val(n)

		 do i =1,n
		    t_sum = t_sum+val(i)

		 end do 

		 am = t_sum / n  

		 end function 




		    real function gm(val,n)

			IMPLICIT NONE 

			integer :: i, n
			real ::  multi = 1, val(n)

			do i = 1, n
			  multi = multi* val(i)
			end do 

			gm = multi**(1/real(n))

			end function 

		       real function sd(val,n)
			   
			   IMPLICIT NONE

			   integer :: i, n
			   real :: t_sum = 0, val(n), am, sd_sum = 0

			   do i = 1, n
			      t_sum = t_sum+val(i)
				 
			   end do

			   am = t_sum / n

			   do i = 1, n
			       sd_sum = sd_sum + (val(i)-am)**2
			   end do
			 
			   sd = sqrt(sd_sum/real(n))

			   end function 

