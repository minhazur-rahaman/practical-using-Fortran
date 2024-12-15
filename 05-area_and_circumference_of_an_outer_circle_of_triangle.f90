program area_and_circumference
IMPLICIT NONE
real ::x1,y1,x2,y2,x3,y3,t_area,a,b,c,s,area,perimeter,circumf,radius 
real, parameter :: pi= 3.1416
integer :: serial_num

Do
   print*, "Enter the serial number: "
   read*, serial_num
   if (serial_num == 0) EXIT
   print*, "Type the coordinates of the vertex of x1, y1 : "
   read*, x1,y1
   print*, "Type the coordinates of the vertex of x2, y2 : "
   read*, x2,y2
   print*, "Type the coordinates of the vertex of x3, y3 : "
   read*, x3,y3
a = sqrt((x1-x2)**2+(y1-y2)**2)
b = sqrt((x1-x3)**2+(y1-y3)**2)
c = sqrt((x2-x3)**2+(y2-y3)**2)

!now we have to check the necessary condition to form a triangle
 if ((a+b)>c .and. (b+c)>a .and. (a+c)>b ) THEN
 s = (a+b+c)/2.0
 t_area = sqrt(s*(s-a)*(s-b)*(s-c))
 perimeter = a+b+c

 !radius of outer circle 
 radius = a*b*c/(4*t_area)
 area = pi*radius**2
 circumf = 2*pi*radius

!here we use formate sothat how many digit will print after decimal point 
 print 10, area
 10 format("The area of the inner circle is: ",f6.2)
 print 12, circumf
 12 format("The circumference of the outer circle is: ",f6.3)
 else 
 print*, "Condition is not satisfy so triangle will not form"
 end if 
end do

end program area_and_circumference
