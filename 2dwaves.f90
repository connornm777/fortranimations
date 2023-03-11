program waves

real, parameter :: pi=4.0*atan(1.0), dt=0.01, dx=0.01, m2=100, l=1
integer, parameter :: n=300, time=200

integer :: x, t
real :: p(n,time), a, v(n)
character(len=20) :: filename


do x=1,n
  p(x,1) = 50*exp(-100*((x-n/2)*dx)**2)
  p(x,2) = 50*exp(-100*((x-n/2)*dx)**2)
  v(x) = -2/((x-n/4)*dx+dx**2)
end do


do t=2,time-1

  do x=2,n-1

    a = (p(x+1,t)+p(x-1,t)-2*p(x,t))/(dx**2) - (m2)*p(x,t) - l*(p(x,t)**3) - p(x,t)*v(x)
    p(x,t+1)=2*p(x,t)-p(x,t-1)+a*0.5*(dt**2)

  end do

end do


do t=1,time
  write(filename, "('timeslice2d/',I4.4,'.dat')") t
  open(unit=t, file=filename)
  do x=1,n
    write(t,*) x, p(x,t) 
  end do
  close(t)
end do 

end program waves
