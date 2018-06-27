module BVTK
 integer,parameter :: rt=4

 contains

 subroutine write_vtk(filename,title,nx,ny,nz,x,y,z,var)
  !WRITE VTK FILE
  implicit none
  
 !Passed Variables
  character(len=*),intent(in) :: filename,title
  integer,intent(in)          :: nx,ny,nz
  real(rt),intent(in)         :: x(1:nx),y(1:ny),z(1:nz)
  real(rt),intent(in)         :: var(1:nx,1:ny,1:nz)
 
 !Local Variables
  real(4)                                :: x4
  real(4),allocatable,dimension(:,:,:)   :: var4 
  integer                                :: i,j,k,s1
  character(len=25)                      :: ss
 
 !Open VTK File
  open(13,file=filename,access='stream',form='unformatted',status='new', &
    convert='big_endian',iostat=s1)
   if (s1.NE.0) write(6,'(a,a)') "ERROR_VTK_1 CANNOT OPEN: ",filename
  !HEADER: note termination with char(10)
   write(13) "# vtk DataFile Version 3.0"//char(10)
   write(13) trim(title)//char(10)
   write(13) "BINARY"//char(10)
   write(13) "DATASET RECTILINEAR_GRID"//char(10)
   write(ss,fmt='(A10,3I5)') "DIMENSIONS",nx,ny,nz
   write(13) ss//char(10)
 
   !X-grid
   write(ss,fmt='(A13,I6,A6)') "X_COORDINATES",nx," float"
   write(13) ss//char(10)
   do i = 1, nx
    x4=x(i)
    write(13) x4
   enddo
 
   !Y-grid
   write(ss,fmt='(A13,I6,A6)') "Y_COORDINATES",ny," float"
   write(13) char(10)//ss//char(10)
   do j = 1, ny
    x4=y(j)
    write(13) x4
   enddo
 
   !Z-grid
   write(ss,fmt='(A13,I6,A6)') "Z_COORDINATES",nz," float"
   write(13) char(10)//ss//char(10)
   do k = 1, nz
    x4=z(k)
    write(13) x4
   enddo
   write(13) char(10)
 
   !Field
   write(ss,fmt='(A10,I15)') "POINT_DATA",nx*ny*nz
   write(13) char(10)//ss//char(10)
   write(13) "SCALARS Fraction float 1"//char(10)
   write(13) "LOOKUP_TABLE default"//char(10)
   allocate( var4(nx,ny,nz) , stat=s1 )
    if (s1.NE.0) write(6,'(a)') "ERROR_VTK_2 CANNOT ALLOCATE var4"
 
   var4=var
   write(13) var4
 
  !Close VTK File
  close(13)
 
  deallocate( var4, stat=s1 )
    if (s1.NE.0) write(6,'(a)') "ERROR_VTK_3 CANNOT DEALLOCATE var4"

 return
 end subroutine write_vtk
end module
