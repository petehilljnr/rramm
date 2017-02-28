#' createFilter Function
#'
#' Creates a filter object that can be passed into the getTableData function
#'
#' @param filter_field A valid name of a field in the table you are going to filter
#' @param filter_operator Any, EqualTo, NotEqualTo, GreaterThan, GreaterEqualThan, LessThan, LessEqualThan, In, NotIn, IsNull, IsNotNull, Like, NotLike, Matches
#' @param filter_value A value to include in the filter
#' @export
#' @examples
#' # TODO:  Add example

createFilter = function(filter_field, filter_operator, filter_value) {

  new_filter = list(columnName=filter_field,
                    operator=filter_operator,
                    value=filter_value)


  return(new_filter)

}
