program n_elements_array
IMPLICIT NONE
integer,ALLOCATABLE, DIMENSION (:) :: value  
integer :: n,i, j, temp_v 
print*, "Enter the value of n: "
read*, n
allocate(value(n))
!read*, value
print*, "Enter the array variable: "

!use do loop to take array value 
   do i = 1, n
      read*, value(i)
   end do

do i = 1, n-1
  do j= i+1, n
   if(value(i)> value(j)) THEN
   temp_v = value(i)
   value(i) = value(j)
   value(j)	= temp_v
   end if 
  end do
end do


print*, "Enter the ascending order value is : "
do i= 1, n
    print*, value(i)
end do

do i = 1, n-1
  do j = i+1, n 
  if(value(i) < value(j)) THEN 
  temp_v = value(i)
  value(i) = value(j)
  value(j) = temp_v
  end if 
 end do
end do

print*, "Enter the ascending order value is : "
do i= 1, n
    print*, value(i)
end do


end program n_elements_array
