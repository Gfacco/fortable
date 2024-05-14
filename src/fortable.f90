module fortable
  use iso_varying_string, only: &
    varying_string, var_str, len, operator(//), trim
  use strff, only: join, NEWLINE, split_at
  implicit none
  private
  enum, bind(C)
      enumerator :: left, right
  end enum

  public :: tabulate
contains
  pure function tabulate(list, row_headers) result(table)
    type(varying_string), intent(in) :: list(:,:)
    logical, optional, intent(in) :: row_headers
    type(varying_string) :: padded_list(size(list, dim=1),size(list, dim=2))
    type(varying_string) :: table
    integer :: max_column_width(size(list, dim=2))
    integer :: lines(size(list, dim=2))
    integer :: i

    if (present(row_headers)) then
      if (row_headers) then
        max_column_width = [(maxval(len(list(:,i))), i=1, size(max_column_width))]
        lines = [(1, i=1, size(max_column_width))]
        do i=1, size(list, dim=1)
          padded_list(i,2:) = pad_to(list(i,2:), max_column_width(2:), lines(2:), alignment = right)
        end do
        do i=1, size(list, dim=1)
          padded_list(i,1) = pad_to(list(i,1), max_column_width(1), lines(1), alignment = left)
        end do
        table = join([(join(padded_list(i,:)," "), i=1, size(list,dim=1))], NEWLINE)
      end if
    else
      max_column_width = [(maxval(len(list(:,i))), i=1, size(max_column_width))]
      lines = [(1, i=1, size(max_column_width))]
      do i=1, size(list, dim=1)
        padded_list(i,:) = pad_to(list(i,:), max_column_width, lines, alignment = left)
      end do
      table = join([(join(padded_list(i,:)," "), i=1, size(list,dim=1))], NEWLINE)
    endif
  end function

  elemental function pad_to(string, width, num_lines, alignment) result(padded)
  !taken from https://gitlab.com/everythingfunctional/string_tree -by Brad Richardson
        type(varying_string), intent(in) :: string
        integer, intent(in) :: width, num_lines
        integer, intent(in) :: alignment
        type(varying_string) :: padded
        type(varying_string), allocatable :: padded_lines(:), lines(:)
        integer :: i
        allocate(padded_lines(0))
        allocate(lines(0))
        select case(alignment)
        case(left)
            lines = split_at(string, NEWLINE)
            padded_lines = pad_left(lines, width)
            padded = join([padded_lines, [(var_str(repeat(" ", width)), i = 1, num_lines - size([lines]))]], "")
        case(right)
            lines = split_at(string, NEWLINE)
            padded_lines = pad_right(lines, width)
            padded = join([padded_lines, [(var_str(repeat(" ", width)), i = 1, num_lines - size([lines]))]], "")
        case default   
        end select

    contains
        elemental function pad_left(line, width) result(padded_line)
            type(varying_string), intent(in) :: line
            integer, intent(in) :: width
            type(varying_string) :: padded_line

            padded_line = line // repeat(" ", width - len(line))
        end function
        elemental function pad_right(line, width) result(padded_line)
            type(varying_string), intent(in) :: line
            integer, intent(in) :: width
            type(varying_string) :: padded_line

            padded_line = repeat(" ", width - len(line)) // line
        end function       
    end function

end module fortable
