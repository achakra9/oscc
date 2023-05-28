!    This is the main file that reads the input file and does the
!    calculations asked

      program main
          use, intrinsic :: iso_fortran_env, only: dp => real64
          ! integer, parameter :: dp = selected_real_kind(15)
          implicit none
          real(dp) :: a,b
          integer :: num_args, idx
          character(len=50), dimension(:), allocatable :: args
          integer, parameter :: io = 211

          num_args = command_argument_count()
          allocate(args(num_args))

          do idx=1, num_args
              call get_command_argument(idx,args(idx))
          end do

          write(*,*)args(1)

      end program main
