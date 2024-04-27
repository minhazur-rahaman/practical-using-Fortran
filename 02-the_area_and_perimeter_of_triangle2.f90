program area_perimeter
IMPLICIT NONE
real ::a,b,c,s,area,perimeter,theta,radian
real, parameter :: pi = 3.1416
integer :: serial_num

Do
  print*, "Enter the serial number: "
  read*, serial_num
  if (serial_num == 0) EXIT
  !if we take angle value in degree we have to change it in radian
  print*,"Type the value of a, b : "
  read*, a,b 
  print*,"Enter the value of theta (remember the value must be in digree) : "
  read*, theta
  radian = theta*(pi/180.0)
  c = sqrt((a)**2+(b)**2-2*a*b*cos(radian))

  !now we have to check the necessary condition to form a triangle
  s = (a+b+c)/2.0
  area = sqrt(s*(s-a)*(s-b)*(s-c))
  perimeter = a+b+c
  !here we use formate sothat how many digit will print after decimal point 
  print 10, area
  10 format("The area of the triangle is: ",f6.2)
  print 12, perimeter
  12 format("The perimeter of the triangle is: ",f6.3) 
end do

end program area_perimeter
