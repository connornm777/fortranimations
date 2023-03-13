complex function f(a, p)
  integer :: k
  real:: p(2)
  complex :: a, b
  b = a
  do k=1, 5
    b =(b**p(1))+a
  end do
  f = b 
  return
end function f

program draw_image
  implicit none
 
  integer, parameter :: n=1000, steps = 1
  real, parameter :: s=2.0 ! image boundaries
  
  integer :: i, j, t ! loop indices
  real :: dt=0.05, pi = 4.0*atan(1.0), v(2)=(1.0, -0.3), p(2)=(2.0, 0.0), x(2)=(1.0,0.0), y(2)=(0.0,1.0)
  real :: ab(n, n)
  character(len=10) :: filename 
  complex :: c, f, pot=0.1
  real :: r, den
  real :: start_time, end_time, elapsed_time
  call cpu_time(start_time)

  ! compute image values
  do t = 1, steps
    write(*, '(A,I0,A,I0,A)') "Building ", t, " out of ", steps,  " images."
    do i = 1, n
      do j = 1, n/2
        c = f( cmplx( (s*(i/n-0.5)), (s*(j/n-0.5)))  , p )
        ab(i,j) = abs(c)/(1+abs(c))
        ab(i, n-j) = abs(c)/(1+abs(c))
      end do
    end do

    den = (p(1)-2.0)**2 + p(2)**2
    call random_number(r)
    v = v + x*r-p(1)*pot
    call random_number(r)
    v = v + y*((r-0.5) - p(2)*pot)*dt
    p = p + v*dt

    ! output image data as PPM file
    write(filename, '(I4.4)') t
    open(unit=1, file='data/'//trim(filename//'.ppm'), status="replace")
    write(1,'(A)') "P2"
    write(1,*) n, n
    write(1,*) 255

    do i = 1, n
      do j = 1, n
        write(1,*) nint(ab(i,j)*255)  
      end do
    end do
    close(unit=1)
  end do
 
  call cpu_time(end_time)
  elapsed_time = end_time - start_time
  write(*, *) "Time: ",  elapsed_time
 
end program draw_image
