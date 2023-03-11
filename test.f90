program test

character(len=20) :: filename
integer :: i



do i=1,10  
  write(filename, "('test',I3.3,'.dat')") i
  print *, filename
end do

end program test
