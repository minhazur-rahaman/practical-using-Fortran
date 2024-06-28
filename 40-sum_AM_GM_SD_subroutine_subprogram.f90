!problem : Write a FORTRAN program, which reads n elements and write some Subroutine Subprogram to print the value of Sum, AM, GM and SD
!problem 40

program sum_AM_GM_SD_subroutine 
IMPLICIT NONE 

real, ALLOCATABLE, DIMENSION(:) :: val

real :: sum, am, gm,sd

integer :: i, n, serial_num 

      do 
	       print*, "Enter the serial number: "
		   read*, serial_num 
		   if(serial_num == 0) EXIT
		    
			print*, "Enter the number of Observation : "
			read*, n
			
			allocate(val(n)) 

			print*, "Input the elements :"
			read*, (val(i), i= 1,n)

			call results(i,n,val,sum,am,gm,sd)

			print 72, sum,am,gm,sd
			
			72 format(2X,"Sum=",f9.3/2x,"Arithmetic mean:",f9.3/2x,"GM:",f9.4/2x,"SD:",f9.2) 


		 deallocate(val)

	  end do


end program 


  subroutine results(i,n,val,sum,am,gm,sd)
  implicit none 

  integer, intent(in):: i,n
  real,intent(in) :: val(n)

  real,intent(out) :: sum,am,gm,sd

  real :: multi, sd_sum


  sum =0
    do i = 1,n
	   sum = sum+val(i)
	end do

  am = sum/n

  multi = 1
    do i= 1,n
	  multi = multi*val(i)
	end do
  gm = multi**(1/real(n))
	
  sd_sum = 0
     do i = 1, n
	   sd_sum = sd_sum + (val(i)-am)**2
	 end do
  sd = sqrt(sd_sum/real(n))

  end subroutine 
	 