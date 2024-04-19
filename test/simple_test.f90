
module simple_test
  use veggies, only: result_t, test_item_t, describe, it, succeed, fail, assert_equals
  use strff, only: NEWLINE, join
  use iso_varying_string, only: varying_string, var_str, char
  use fortable, only: tabulate

  implicit none
  private
  public :: test_simple

  character(len=*), parameter :: SIMPLE_LIST_FORMAT = &
    "Sun   696000 1.9891e+09" // NEWLINE &
//  "Earth 6371   5973.6    " // NEWLINE &
//  "Moon  1737   73.5      " // NEWLINE &
//  "Mars  3390   641.85    " 
contains
  function test_simple() result(tests)
    type(test_item_t) :: tests

    tests = describe("A simple list", [it("displays properly", check_simple)])
  end function

  function check_simple() result(result_)
    type(result_t) :: result_
    type(varying_string) :: simple_list(4,3)
    type(varying_string) :: simple_array(12)
    type(varying_string) :: tabular_string


    simple_array =  [ var_str("Sun"),   var_str("696000"),  var_str("1.9891e+09") &
                    , var_str("Earth"), var_str("6371"),    var_str("5973.6") &
                    , var_str("Moon"),  var_str("1737"),    var_str("73.5") &
                    , var_str("Mars"),  var_str("3390"),    var_str("641.85")]
    simple_list = reshape(simple_array, shape(simple_list), order=[2,1])                    

    tabular_string = tabulate(simple_list)


    result_ = assert_equals(SIMPLE_LIST_FORMAT, tabular_string, tabular_string)
  end function
end module