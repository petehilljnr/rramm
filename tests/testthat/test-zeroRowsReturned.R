test_that(
  'Check data.frame is returned when filter creates zero records',
  {
    f = function(){ getTableData(
      getAuthorisedHeaders(userName='api_demo', password='thursday', database='RAMM API Demo'),
      'carr_way',
      get_geometry = TRUE,
      filters = list(
        createFilter('carr_way_no','LessThan',0)
      )
    )
    }

    expect_warning(f())

    req = f()

    expect_equal(
      0,
      nrow(req)
    )

    expect_true(
      'data.frame' %in% class(req)
    )

    expect_true(
      'sf' %in% class(req)
    )

  }
)

