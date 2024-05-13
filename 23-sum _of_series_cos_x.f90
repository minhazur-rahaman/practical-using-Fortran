!problem : Write a FORTRAN program to calculate the sum of series such as cox(x)

!problem 23



program sum_of_series_cox

IMPLICIT NONE
real :: x, t_sum
integer :: serial_num, n
real, parameter :: pi= 3.1416

       do 
          print*, "Enter the serial number : "
          read*, serial_num

          if (serial_num == 0) EXIT

             print*, "Enter the value of X:(in degree)" 			! cox series e x er value kinto degree te ni amra. 
             read*, x

			 !radiant e ni e gelam 
             x = pi*(x/180)

             print*, "Enter the N therms of number:"
             read*, n

         print*, "Sum is = ", t_sum(x,n)

       end do 
 
 end program 
 


 real function t_sum(x,n)

 IMPLICIT NONE 

real, intent(in)::x
integer :: i, n
real :: factorial

n = n-1 

t_sum = 1        ! agee jakon sum korsi takon kinto value assign korar time e 0 dorcilam but akane 1 dorsi, why? cinta kore dekben!
factorial = 1
do i = 1, n
  factorial = factorial*(2*i)*(2*i-1)
  t_sum = t_sum +((-1)**(i+2)*x**(2*i))/factorial
end do

end function





