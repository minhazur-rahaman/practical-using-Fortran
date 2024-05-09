!problem : Write a FORTRAN program, which reads two matrix and print their Subtraction in a matrix form
!problem 34



program matrix_Subtraction
IMPLICIT NONE 

integer, allocatable, dimension(:,:)::a,b,c

integer :: a_row, a_col, b_row, b_col, i, j, serial_num

do 
   print*, "Enter the serial number : "
   read*, serial_num

   if(serial_num == 0) EXIT


   print*, "Type the value of a_row and a_col :"
   read*, a_row, a_col
   print*, "Type the value of b_row and b_col :"
   read*, b_row, b_col
   
   allocate(a(a_row,a_col),b(b_row,b_col),c(a_row,a_col))
   
   print*, "Enter the matrix a row wise :"
   
   read*, ((a(i,j),j=1,a_col),i=1,a_row)			  ! i die je loop chalilam ayta outer loop ar j ta inner loop

   print*, "Enter the matrix b row wise :"
   
   read*, ((b(i,j),j=1,b_col),i=1,b_row)

      print*, "The Matrix A is: "

         do i = 1, a_row
	       print*,(a(i,j),j=1,a_col)

	     end do

      print*, "The Matrix B is: "

         do i = 1, b_row 
	       print*,(b(i,j),j=1,b_col)

	     end do

   !condition must dite hobe 
   if((a_row==b_row) .AND. (a_col==b_col)) THEN
        do i = 1, a_row
		  do j = 1, a_col
		    c(i,j) = a(i,j)-b(i,j)		  
		  end do
		end do
		  ! result 	print 
	      print*, "sum of the two matrix is : "
          do i = 1, a_row
             print*, (c(i,j), j= 1, a_col)

          end do
     else 
        print*, "aha ! its not posible to Subtraction in between this two matrix. sorry yeear . try again plzzz"

   end if 


   deallocate(a)	   !array value deallocate na kore program run kore dekben. tarpor nije nije buje jaben kenno dealloacte use korte hoi
   deallocate(b)
   deallocate(c)
 end do

end program 
