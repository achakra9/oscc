!    This is the main file that reads the input file and does the
!    calculations asked

      program main
          use, intrinsic :: iso_fortran_env, only: dp => real64
          ! integer, parameter :: dp = selected_real_kind(15)
          implicit none
          integer :: dt(8)
          real(dp) :: a,b
          integer :: num_args, idx, ierr
          character(len=50), dimension(:), allocatable :: args
          integer, parameter :: inp = 211 ! open input file
          integer, parameter :: io = 221  ! open the output file
          character(len=50) :: f_input, f_output


          call date_and_time(values=dt)


          ! Read the command line arguments
          num_args = command_argument_count()
          allocate(args(num_args))

          do idx=1, num_args
              call get_command_argument(idx,args(idx))
          end do

          if (args(1) .eq. '-h') then
              write(*,*)" oscc inputFile outputFile"
              stop
          else
              write(*,*)"Number of args:", num_args
              f_input = args(1)
              f_output = args(2)
          end if

          ! open the input file
         open(unit=inp,file=f_input,status='old',iostat=ierr)
         if (ierr .ne. 0) then
              write(*,*)"Input file not found"
              stop
          end if
          read(inp,*)a
          open(unit=io,file=f_output,status='new',iostat=ierr)
          if (ierr .ne. 0) then
              write(*,*)"Error in openning output file"
              stop
          end if
          write(io,*)" OSCC program started on:",dt(3),'-',dt(2),'-', &
                     dt(1),' at ',dt(5), ':', dt(6), ':',dt(7)
          write(io,*)a,a
          write(io,*)"Success"

          close(inp)
          close(io)

      end program main
