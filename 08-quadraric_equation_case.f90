!Write a FORTRAN program to solve a quadratic equation ax^2+bx^2+cx+d=0 and print 
 !the roots(real or complex) by using case construct


program quadraric_equation_case
IMPLICIT NONE
real :: a,b,c,d,discri,f_root,s_root, real_p, imag
integer :: cond_code, serial_num


do 
  print*, "Enter the serial Number: "
  read*, serial_num
  if(serial_num == 0) EXIT 
    print*, "Enter the value of quadratic coefficient: a, b linear coefficient: c constant coefficient: d : "
    read*, a,b,c,d
    if ((a+b)==0)then
    print*, "The equation is not quadratic "
     else  
    discri = c**2-4*(a+b)*d

  if (discri > 0) THEN
	cond_code = 1
  else if (discri == 0) THEN
    cond_code = 2
  else 
    cond_code = 3 

  end if  

 SELECT CASE(cond_code)
   CASE(1)
     f_root = (-c + sqrt(discri))/(2*(a+b))
     s_root = (-c - sqrt(discri))/(2*(a+b))
     print*, "the root are real and unequal"
     print 72,f_root,s_root
	 72 format(2X, "first root: ", f9.4, "   Second root: " ,f9.4)

   CASE(2)
     s_root = (-c - sqrt(discri))/(2*(a+b))
     print*, "The roots are real and equal: " ,s_root

   CASE(3)
     print*, "the roots are complex: "
     real_p = -c/(2*(a+b))
     imag = sqrt(abs(discri))/(2*(a+b))
     print 66,real_p,imag,real_p,imag
	 66 format(2X, "first root: ",f9.4 , "+i" ,f9.4 , "    second root: ",f9.4 , "-i",f9.4) 
  END SELECT
  end if  
end do	  

print*, "THE END! & and program is really easy "

end program quadraric_equation_case 