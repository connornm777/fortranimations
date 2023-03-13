complex function f(a)
  integer :: k
  complex :: a, b, h
  h = cmplx(0.0, 1.0)
  b = a
  do k=1, 50
    b = (b**12)/(a+b)**12 + (a**3)/(a+b)**3
  end do
  f = b 
  return
end function f

program draw_image
  implicit none
 
  integer, parameter :: n=2500
  real, parameter :: s=3.0 ! image boundaries
  
  integer :: i, j, t ! loop indices
  real :: ab(n, n)
  complex :: c, f

  ! compute image values
  do i = 1, n
    do j = 1, n
      c = f( cmplx( (s*(1.0*i/n-0.5)), (s*(1.0*j/n-0.5)))) 
      ab(i,j) = 1/(1+exp(abs(c)))
    end do
  end do

  ! output image data as PPM file
  open(unit=1, file='output.ppm', status="replace")
  write(1,'(A)') "P2"
  write(1,*) n, n
  write(1,*) 255

  do i = 1, n
    do j = 1, n
      write(1,*) nint(ab(i,j)*255) 
    end do
  end do
  close(unit=1)
 
end program draw_image
