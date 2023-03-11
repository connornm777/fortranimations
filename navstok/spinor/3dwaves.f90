program waves

real, parameter :: pi=4.0*atan(1.0), dt=0.05, dx=0.01, Temp=5, k=10, pn=1.0/3.0
integer, parameter :: n=100, time=200, deln=3
integer :: x,y,t,s
real :: p(n,n,time), vx(n,n,time), vy(n,n,time), ap, ax, ay
character(len=20) :: filename


do x=1,n
  do y=1,n
    p(x,y,1) = 0.01!exp(-10*(dx**2)*((float(x-n/2))**2+(float(y-n/2))**2))
    p(x,y,2) = 0.01!exp(-10*(dx**2)*((float(x-n/2))**2+(float(y-n/2))**2))
    vx(x,y,1) = 0!-dx*float(y)/(sqrt(float(x-n/2)**2+float(y-n/2)**2))
    vx(x,y,1) = 0!-dx*float(y)/(sqrt(float(x-n/2)**2+float(y-n/2)**2))
    vy(x,y,2) = 0!dx*float(x)/(sqrt(float(x-n/2)**2+float(y-n/2)**2))
    vy(x,y,2) = 0!dx*float(x)/(sqrt(float(x-n/2)**2+float(y-n/2)**2))
  end do
end do



do t=2,time-1

  do x=2,n-1
    do y=2,n-1
           
      ap = -(vx(x,y,t)*(p(x+1,y,t)-p(x-1,y,t)) + vy(x,y,t)*(p(x,y+1,t)-p(x,y-1,t)))/(2*dx)


      ax = -( vx(x,y,t)*(vx(x+1,y,t)-vx(x-1,y,t)) + vy(x,y,t)*(vx(x,y+1,t)-vx(x,y-1,t)) )/(2*dx)
      ay = -( vx(x,y,t)*(vy(x+1,y,t)-vy(x-1,y,t)) + vy(x,y,t)*(vy(x,y+1,t)-vy(x,y-1,t)) )/(2*dx)

      ax = ax + (k*(p(x,y,t)**pn) - Temp)*( p(x+1,y,t) - p(x-1,y,t) )/(2*dx) 
      ay = ay + (k*(p(x,y,t)**pn) - Temp)*( p(x,y+1,t) - p(x,y-1,t) )/(2*dx) 
 

      p(x,y,t+1) = 2*p(x,y,t) - p(x,y,t-1) + (dt**2)*ap
      vx(x,y,t+1) = 2*vx(x,y,t) - vx(x,y,t-1) + (dt**2)*ax
      vx(x,y,t+1) = 2*vy(x,y,t) - vy(x,y,t-1) + (dt**2)*ay






    end do
  end do

end do


do t=1,time
  write(filename, "('timeslice3d/',I4.4,'.dat')") t
  open(unit=t, file=filename)
  do x=1,int(n/deln)
    do y=1,int(n/deln)
      write(t,*) x, y, p(deln*x,deln*y,t)
    end do
  end do
  close(t)
end do 

end program waves
