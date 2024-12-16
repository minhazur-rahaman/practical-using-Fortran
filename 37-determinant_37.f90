PROGRAM MatrixDeterminant
IMPLICIT NONE
INTEGER :: n, row, col, index, stopFlag
REAL*8 :: factor, determinant
REAL*8, ALLOCATABLE, DIMENSION(:,:) :: matrix

DO
  PRINT *, "Enter the value of stopFlag (0 to exit):"
  READ *, stopFlag
  IF (stopFlag == 0) EXIT

  PRINT *, "Enter the size of the matrix (n):"
  READ *, n

  ALLOCATE(matrix(n, n))

  PRINT *, "Input the elements of the matrix row by row:"
  READ *, ((matrix(row, col), col = 1, n), row = 1, n)

  DO row = 1, n
    DO col = 1, n
      IF (col > row) THEN
        factor = matrix(row, col) / matrix(row, row)
        DO index = 1, n
          matrix(index, col) = matrix(index, col) - (factor * matrix(index, row))
        END DO
      END IF
    END DO
  END DO

  determinant = 1.000
  DO row = 1, n
    determinant = determinant * matrix(row, row)
  END DO

  PRINT *, "The determinant is:", determinant
  DEALLOCATE(matrix)
END DO

END PROGRAM MatrixDeterminant
