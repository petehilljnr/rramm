#' offsetLeftHandSurfaces Function
#'
#' This function returns raw c_surface records from RAMM that need to be updated or added when doing a Left Hand Side widening
#'
#' @param headers Authorised access headers created using  getHeaders(...).
#' @param site_road_id The RAMM Road ID on which the site resides
#' @param site_start_m The start chainage(m) of the site
#' @param site_end_m The end chainage(m) of the site
#' @param site_offset The width(m) of the offset that you want to implement at the site
#' @export
#' @examples
#' \dontrun{
#' #The following example returns dataframes of the adds and updates when widening road_id 888 between 120m and 400m by 4m
#'
#' surfaces = offsetLeftHandSurfaces(headers = hdrs, site_road_id = 888, start_start_m = 120, site_end_m = 400, site_offset = 4)
#' surfaces$adds
#' surfaces$updates
#' }

offsetLeftHandSurfaces = function(headers,
                    site_road_id,
                    site_start_m,
                    site_end_m,
                    site_offset) {
  surfaces_raw = getTableData(
    headers = headers,
    table_name = "c_surface",
    load_type = "All",
    get_geometry = FALSE,
    filters = list(
      createFilter("road_id", "EqualTo", site_road_id),
      createFilter("start_m", "LessThan", site_end_m),
      createFilter("end_m", "GreaterThan", site_start_m)
    )
  )

  surfaces = bind_rows(
    surfaces_raw %>% select(c_surface_id, start_m) %>% rename(loc = start_m),
    surfaces_raw %>% select(c_surface_id, end_m) %>% rename(loc = end_m),
    surfaces_raw %>% select(c_surface_id) %>% mutate(loc = site_start_m),
    surfaces_raw %>% select(c_surface_id) %>% mutate(loc = site_end_m)
  ) %>%
    arrange(c_surface_id, loc) %>%
    mutate(
      sec_start = loc,
      sec_end = lead(loc),
      next_id = lead(c_surface_id)
    ) %>%
    filter(next_id == c_surface_id) %>%
    select(-next_id,-loc) %>%
    inner_join(
      surfaces_raw %>% select(c_surface_id, start_m, end_m, sealed_area, surf_offset),
      by = "c_surface_id"
    ) %>%
    filter(sec_start < end_m, sec_end > start_m) %>%
    mutate(
      in_site = !(sec_start < site_start_m | sec_end > site_end_m),
      surf_offset = if_else(in_site, surf_offset + site_offset, surf_offset),
      sealed_area = sealed_area * ((sec_end - sec_start) / (end_m - start_m))
    ) %>%
    group_by(c_surface_id) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    mutate(
      original_id = c_surface_id,
      new_c_surface_id = if_else(rank == 1, c_surface_id, as.integer(0))
    ) %>%
    select(-rank,-start_m,-end_m) %>%
    inner_join(
      surfaces_raw %>% select(-start_m,-end_m,-surf_offset,-sealed_area),
      by = ("original_id" = "c_surface_id")
    ) %>%
    select(-c_surface_id,-original_id,-in_site) %>%
    rename(c_surface_id = new_c_surface_id,
           start_m = sec_start,
           end_m = sec_end)

  adds = surfaces %>% filter(c_surface_id == 0)
  updates = surfaces %>% filter(c_surface_id != 0)

  return(list(adds = adds, updates = updates))

}
