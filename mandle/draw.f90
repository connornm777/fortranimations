real function f(a, p)
  integer :: k
  real:: p
  complex :: a, b
  b = a
  do k=1, 20
    b = b**p + a
    if (abs(b)>50) then
      f=1.0
      return 
    end if
  end do
  f = 0.0
  return
end function f

program draw_image
  implicit none
 
  integer, parameter :: n=2000, steps = 1
  real, parameter :: s=1.0 ! image boundaries
  
  integer :: i, j, t ! loop indices
  real :: f, dt=0.05
  real :: z(n, n)
  character(len=10) :: filename 

  ! compute image values
  do t = 1, steps
    do i = 1, n
      do j = 1, n
        z(i,j) = abs(f(cmplx(s*(1.0*(i-1)/(n-1)-0.5), s*(1.0*(j-1)/(n-1)-0.5)), 1.0+t*dt))
      end do
    end do
  
    ! output image data as PPM file
    write(filename, '(I4.4)') t
    open(unit=1, file=trim(filename//'.ppm'), status="replace")
    write(1,'(A)') "P2"
    write(1,*) n, n
    write(1,*) 255

    do j = 1, n
      do i = 1, n
        write(1,*) nint(z(i,j)*100+155)
      end do
    end do
    close(unit=1)
  end do
  
end program draw_image
