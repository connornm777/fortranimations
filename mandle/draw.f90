complex function f(a, p)
  integer :: k
  real:: p
  complex :: a, b
  b = a
  do k=1, 5
    b = b**p + a
  end do
  f = b 
  return
end function f

program draw_image
  implicit none
 
  integer, parameter :: n=200, steps = 500
  real, parameter :: s=2.0 ! image boundaries
  
  integer :: i, j, t ! loop indices
  real :: dt=0.05
  real :: ab(n, n), im(n, n), re(n, n)
  character(len=10) :: filename 
  complex :: c, f

  ! compute image values
  do t = 1, steps
    do i = 1, n
      do j = 1, n 
        c = f(cmplx(s*(1.0*(i-1)/(n-1)-0.5), s*(1.0*(j-1)/(n-1)-0.5)), 1.0+t*dt)
        ab(i,j) = abs(c)/(1+abs(c))
        re(i, j) = real(c)/(1+abs(c))
        im(i, j) = aimag(c)/(1+abs(c))
      end do
    end do
  
    ! output image data as PPM file
    write(filename, '(I4.4)') t
    open(unit=1, file='data/'//trim(filename//'.ppm'), status="replace")
    write(1,'(A)') "P3"
    write(1,*) n, n
    write(1,*) 255

    do j = 1, n
      do i = 1, n
        write(1,*) nint(ab(i,j)*255*re(i,j)),  nint(ab(i,j)*255),  nint(ab(i,j)*255*im(i,j))
      end do
    end do
    close(unit=1)
  end do
  
end program draw_image
