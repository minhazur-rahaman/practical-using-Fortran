!Write a FORTRAN program to solve a quadratic equation ax^2+bx^2+cx+d=0 and print 
 !the roots(real or complex) by subroutine Subprogram 


program quadratic_equ_by_subroutine_subpro
IMPLICIT NONE 

real :: a,b,c,d,dis,root,root1,root2
integer :: i, serial_num

      do 
	    print*,"Enter the serial number (0 to exit):"
		read*, serial_num 
		if (serial_num == 0) exit 

		print*, "Input the value of a,b,c,d "
		read*, a,b,c,d

		 call quadratic_root(a,b,c,d,i,dis,root,root1,root2)
		
		select case (i)
		case (2)
			print 22, root 
            22 format (3X, "The two root are equal in the equation so the root is:", f8.3)
		case (3)
			print 33, root1,root2
            33 format (3X, "The two root of the equation where first root = ", f8.3, "Second root = ",f8.3)
		case (4)
			print 44, root1,root2,root1,root2
            44 format (3X, "The two complex root of the equation where first root", f8.3, "+i",f8.3/2x,"second root",f8.3,"-i",f8.3)
		end select 
		end do
 end program 

	  subroutine quadratic_root(a,b,c,d,i,dis,root,root1,root2)
	  IMPLICIT NONE 
	  real:: a,b,c,d,dis,root,root1,root2
	  integer :: i
	  !real :: dis 
	  !real, intent(in) :: a,b,c,d
	  !real, intent(out):: root,root1,root2,i

	  if((a+b)==0)then
	  
	  print*, "It's not a quadratic equation :" 
	  
	  else 
	  
	  dis = (c**2-4*(a+b)*d)
	  
	  if(dis == 0)then
	  
	  root = -c/(2.0*(a+b))

	  i = 2

	  return 
	  
	  else if (dis > 0)then 
	  
	  root1 = (-c+sqrt(dis))/(2.0*(a+b)) 
	  root2 = (-c-sqrt(dis))/(2.0*(a+b))

	  i = 3

	  return 

	  else
	   
	  root1 = -c/(2.0*(a+b))
	  root2 = sqrt(-dis)/(2.0*(a+b))

	  i = 4

	  return 

	   end if 
	   end if 

	   end subroutine 