program biggest_number
IMPLICIT NONE
real :: u, v, w
integer :: serial_num

do
  print*, "Enter the serial number : "
  read*, serial_num
  if (serial_num == 0) EXIT
  print*, "Type the value of u, v, w : "
  read*, u,v,w
     if (u>v) THEN
	   if (u>w) THEN
	    print*, u," is the biggest number "
	 else 
	    print*, "W is the biggest num"
	 end if 
	 else if (v>w) THEN
	   	print*, "v is the biggest num"
	 else 
	    print*, "W is the biggest num"
   
	end if 
	end do

	end program biggest_number