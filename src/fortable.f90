module fortable
  use iso_varying_string, only: &
    varying_string, var_str, len, operator(//)
  use strff, only: join, NEWLINE, split_at
  implicit none
  private

  public :: tabulate
contains
  pure function tabulate(list) result(table)
    type(varying_string), intent(in) :: list(:,:)
    type(varying_string) :: padded_list(size(list, dim=1),size(list, dim=2))
    type(varying_string) :: table
    integer :: max_column_width(size(list, dim=2))
    integer :: lines(size(list, dim=2))
    integer :: i,j

    max_column_width = [(maxval(len(list(:,i))), i=1, size(max_column_width))]
    lines = [(1, i=1, size(max_column_width))]
    do i=1, size(list, dim=1)
      padded_list(i,:) = pad_to(list(i,:), max_column_width, lines)
    end do
    table = join([(join(padded_list(i,:)," "), i=1, size(list,dim=1))], NEWLINE)
  end function

  elemental function pad_to(string, width, num_lines) result(padded)
        type(varying_string), intent(in) :: string
        integer, intent(in) :: width, num_lines
        type(varying_string) :: padded

        integer :: i

        associate(lines => split_at(string, NEWLINE))
            associate(padded_lines => pad_line(lines, width))
                padded = join([padded_lines, [(var_str(repeat(" ", width)), i = 1, num_lines - size(lines))]], NEWLINE)
            end associate
        end associate
    contains
        elemental function pad_line(line, width) result(padded_line)
            type(varying_string), intent(in) :: line
            integer, intent(in) :: width
            type(varying_string) :: padded_line

            padded_line = line // repeat(" ", width - len(line))
        end function
    end function

end module fortable
