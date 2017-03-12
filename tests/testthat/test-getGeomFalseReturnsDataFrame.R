test_that(
  'SF results returned when geometry requested',
  {
    expect_true(
      ! 'sf' %in% class(
        getTableData(
          getAuthorisedHeaders(userName='api_demo', password='thursday', database='RAMM API Demo'),
          'carr_way',
          get_geometry = FALSE,
          filters = list(
            createFilter('carr_way_no','LessThan',20)
          )
        )
      )
    )
  }
)
