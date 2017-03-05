test_that('RAMM API login works against demo site',
          {
            expect_equivalent(
              substring(
                getAuthorisedHeaders(userName='api_demo', password='thursday', database='RAMM API Demo')$headers['Authorization'],
                1,6
              ),
              'Bearer'
            )
          }
)
