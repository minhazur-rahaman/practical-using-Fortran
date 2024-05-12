! problem : Write a fORTRAN program to print prime numbers and how many prime numbers between a specified range (m,n;m<n)
! problem 10


program prime_number_in_betwee_range

IMPLICIT NONE

integer :: m, n, i, j, is_prime , count 

     print*, "Enter the specified range in between the prime number you want to know, start & end :"
	 read*, m, n 

	 count = 0 

	 ! ay outer loop ta ami je range ta dibo oii tar proti value ke loop chalai read korbe
	 do i = m,n 

	   if (i<=1) CYCLE 
	    ! is_prime = 1 mane hoilo mone koren m=5, first e dore nissi 5 akta prime number abr pore deken jodi prime number na hoi porer loop e is_prime = 0 kore disci 
		is_prime = 1

		do j = 2, i-1			                
		   if(mod(i,j)==0) is_prime = 0			!deken ak line er modde jodi statement likte paren taile then dite hoi na
		                                        ! copmiler oii 1 liner modde if condition er kaj ta shes kore even end if o korte hoi na.
												! sundor na jinis ta ??
		end do


	   if(is_prime == 1) THEN
	    print*, i, " ", "is a prime number"

		count = count+1
	   end if 
	 end do

	 print*, "Total prime number from :", m," ", "to ",n," ", "range is ::::", count
 end program 


 !yes! if you understand the code then now are a programmar ..!!!