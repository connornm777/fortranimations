
program waves
implicit none
real, parameter :: pi=4.0*atan(1.0), dt=0.005, dx=0.01, m2=-200, l=0.25, g=100
integer, parameter :: n=100, time=500, deln=2
integer :: x, y , t
real :: p(n,n,time), ap, s(n,n,time), as
character(len=100) :: filename


do x=1,n
  do y=1,n
    p(x,y,2) = 0!100*exp(-500*((float(x-n/2)*dx)**2 + (float(y-n/2)*dx)**2))
    p(x,y,1) = 0!100*exp(-500*((float(x-n/2)*dx)**2 + (float(y-n/2)*dx)**2))
    s(x,y,1) = 0!10*exp(-500*((float(x-n/2)*dx)**2 + (float(y-n/2)*dx)**2))
    s(x,y,2) = 0!10*exp(-500*((float(x-n/2)*dx)**2 + (float(y-n/2)*dx)**2))


  end do
end do


p(n/4,3*n/5,1)=1


do t=2,time-1

  do x=1,n
    do y=1,n

      ap = -(m2)*p(x,y,t) - l*(p(x,y,t)**3) - g*(s(x,y,t)**2)
      as = - p(x,y,t)

      if (0<x) then
        ap = ap + (p(x,y-1,t)-p(x,y,t))/(dx**2)
      end if
      if (x<n+1) then
        ap = ap + (p(x+1,y,t)-p(x,y,t))/(dx**2)
      end if
      if (0<y) then
        ap = ap + (p(x-1,y,t)-p(x,y,t))/(dx**2)
      end if
      if (y<n+1) then
        ap = ap + (p(x,y+1,t)-p(x,y,t))/(dx**2)
      end if
      p(x,y,t+1)=2*p(x,y,t)-p(x,y,t-1)+ap*(dt**2)

      if (0<x) then
        as = as + (s(x,y-1,t)-s(x,y,t))/(dx**2)
      end if
      if (x<n+1) then
        as = as + (s(x+1,y,t)-s(x,y,t))/(dx**2)
      end if
      if (0<y) then
        as = as + (s(x-1,y,t)-s(x,y,t))/(dx**2)
      end if
      if (y<n+1) then
        as = as + (s(x,y+1,t)-s(x,y,t))/(dx**2)
      end if
      s(x,y,t+1)=2*s(x,y,t)-s(x,y,t-1)+as*(dt**2)

    end do
  end do

end do


do t=1,time
  write(filename, "('timeslice3d/',I4.4,'.dat')") t
  open(unit=t, file=filename)
  do x=1,int(n/deln)
    do y=1,int(n/deln)
      write(t,*) x, y, p(deln*x,deln*y,t), s(deln*x,deln*y,t)
    end do
  end do
  close(t)
end do 

end program waves
