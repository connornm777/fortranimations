program waves

real, parameter :: pi=4.0*atan(1.0), dt=0.005, dx=0.01, ma=-1000, mb=100, mc=10, g=1000, l=5000
integer, parameter :: n=100, time=1000, deln=3
integer :: x, y , t
real :: a1(n,n,time), a2(n,n,time), b1(n,n,time), b2(n,n,time), aa1, aa2, ab1, ab2
character(len=20) :: filename


do x=1,n
  do y=1,n
    a1(x,y,1) = 0.5 
    a1(x,y,2) = 0.5 
    b1(x,y,1) = 0.1
    b1(x,y,2) = 0.1 
    a2(x,y,1) = 0.5 
    a2(x,y,2) = 0.5 
    b2(x,y,1) = 0.1
    b2(x,y,2) = 0.1
  end do
end do

a(n/2, n/2, 1) = 0.51

do t=2,time-1

  do x=1,n
    do y=1,n


      !Acceleration terms. Take derivatives in your interaction lagrangian to find what the acceleration of a, b, and c fields should be.

      aa1 = -(ma)*(a(x,y,t)-0.5) - l*(a(x,y,t)-0.5)**3 - g*(a(x,y,t)-0.5)*(b(x,y,t)**2)
      aa2 = -(ma)*(a(x,y,t)-0.5) - l*(a(x,y,t)-0.5)**3 - g*(a(x,y,t)-0.5)*(b(x,y,t)**2)
      ab1 = -(mb)*b(x,y,t) - g*((a(x,y,t)-0.5)**2)*b(x,y,t)
      ab2 = -(mb)*b(x,y,t) - g*((a(x,y,t)-0.5)**2)*b(x,y,t)

      if (1 < x ) then
        aa1 = aa1 + (a1(x-1,y,t)-a1(x,y,t))/(dx**2)
        ab1 = ab1 + (b1(x-1,y,t)-b1(x,y,t))/(dx**2)
      end if 

      if (x < n ) then
        aa1 = aa1 + (a1(x+1,y,t)-a1(x,y,t))/(dx**2)
        ab = ab + (b(x+1,y,t)-b(x,y,t))/(dx**2)
        ac = ac + (c(x+1,y,t)-c(x,y,t))/(dx**2)
      end if 

      if (1 < y ) then
        aa = aa + (a(x,y-1,t)-a(x,y,t))/(dx**2)
        ab = ab + (b(x,y-1,t)-b(x,y,t))/(dx**2)
        ac = ac + (c(x,y-1,t)-c(x,y,t))/(dx**2)
      end if 

      if (y < n ) then
        aa = aa + (a(x,y+1,t)-a(x,y,t))/(dx**2)
        ab = ab + (b(x,y+1,t)-b(x,y,t))/(dx**2)
        ac = ac + (c(x,y+1,t)-c(x,y,t))/(dx**2)
      end if 


      a(x,y,t+1)=2*a(x,y,t)-a(x,y,t-1)+aa*0.5*(dt**2)
      b(x,y,t+1)=2*b(x,y,t)-b(x,y,t-1)+ab*0.5*(dt**2)
      c(x,y,t+1)=2*c(x,y,t)-c(x,y,t-1)+ac*0.5*(dt**2)


    end do
  end do

end do


do t=1,time
  write(filename, "('timeslice3d/',I4.4,'.dat')") t
  open(unit=t, file=filename)
  do x=1,int(n/deln)
    do y=1,int(n/deln)
      write(t,*) x, y, a(deln*x,deln*y,t), b(deln*x,deln*y,t), c(deln*x,deln*y,t)
    end do
  end do
  close(t)
end do 

end program waves
